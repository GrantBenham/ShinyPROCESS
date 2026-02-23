# ============================================================================
# ASSUMPTION CHECKS MODULE
# ============================================================================
# This module contains all helper functions for assumption checking
# Extracted from gbPROCESS.R as part of Stage 1 modularization

# Helper function to detect if a variable is binary (0/1 or only 2 unique values)
is_binary_variable <- function(data, var_name) {
  if(is.null(data) || is.null(var_name) || !var_name %in% names(data)) {
    return(FALSE)
  }
  var_data <- data[[var_name]]
  var_data <- var_data[!is.na(var_data)]
  unique_vals <- unique(var_data)
  # Check if only 2 unique values
  if(length(unique_vals) == 2) {
    return(TRUE)
  }
  return(FALSE)
}

# Helper to detect continuous variables (numeric/integer/labelled and not binary)
is_continuous_variable <- function(data, var_name) {
  if(is.null(data) || is.null(var_name) || !var_name %in% names(data)) {
    return(FALSE)
  }
  if(is_binary_variable(data, var_name)) return(FALSE)
  v <- data[[var_name]]
  is.numeric(v) || is.integer(v) || inherits(v, "labelled")
}

# Univariate outlier screening (detection only) for selected continuous variables
# Returns one summary row per variable.
compute_univariate_outlier_summary <- function(data, vars,
                                               method = c("iqr", "mad"),
                                               iqr_multiplier = 1.5,
                                               mad_threshold = 3.5) {
  method <- match.arg(method)
  if(is.null(data) || !is.data.frame(data) || is.null(vars) || length(vars) == 0) {
    return(data.frame())
  }

  vars <- unique(vars[vars %in% names(data)])
  if(length(vars) == 0) return(data.frame())

  rows <- lapply(vars, function(v) {
    x <- suppressWarnings(as.numeric(data[[v]]))
    x <- x[!is.na(x)]
    n_non_missing <- length(x)
    if(n_non_missing == 0) {
      return(data.frame(
        variable = v,
        method = if(method == "iqr") "IQR fences" else "MAD robust z",
        threshold = if(method == "iqr") paste0("k = ", format(round(iqr_multiplier, 2), nsmall = 1)) else paste0("|z| > ", format(round(mad_threshold, 2), nsmall = 1)),
        n_non_missing = 0L,
        flagged_n = 0L,
        flagged_pct = 0,
        rule_detail = "No non-missing values",
        stringsAsFactors = FALSE
      ))
    }

    if(method == "iqr") {
      q1 <- as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, type = 7))
      q3 <- as.numeric(stats::quantile(x, 0.75, na.rm = TRUE, type = 7))
      iqr_val <- q3 - q1
      lower <- q1 - (iqr_multiplier * iqr_val)
      upper <- q3 + (iqr_multiplier * iqr_val)
      flagged <- (x < lower) | (x > upper)
      rule_detail <- sprintf("[%.3f, %.3f]", lower, upper)
      threshold_txt <- paste0("k = ", format(round(iqr_multiplier, 2), nsmall = 1))
      method_txt <- "IQR fences"
    } else {
      med <- stats::median(x, na.rm = TRUE)
      mad_val <- stats::mad(x, center = med, constant = 1, na.rm = TRUE)
      if(is.na(mad_val) || mad_val == 0) {
        flagged <- rep(FALSE, length(x))
        rule_detail <- "MAD = 0 (no flags)"
      } else {
        robust_z <- 0.6745 * (x - med) / mad_val
        flagged <- abs(robust_z) > mad_threshold
        rule_detail <- sprintf("|robust z| > %.2f", mad_threshold)
      }
      threshold_txt <- paste0("|z| > ", format(round(mad_threshold, 2), nsmall = 1))
      method_txt <- "MAD robust z"
    }

    flagged_n <- sum(flagged, na.rm = TRUE)
    data.frame(
      variable = v,
      method = method_txt,
      threshold = threshold_txt,
      n_non_missing = n_non_missing,
      flagged_n = flagged_n,
      flagged_pct = if(n_non_missing > 0) (100 * flagged_n / n_non_missing) else 0,
      rule_detail = rule_detail,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Function to identify outliers based on model type
identify_outliers_assumption <- function(data, outcome_var, predictor_var, 
                                         mediator_vars = NULL, moderator_var = NULL,
                                         moderator2_var = NULL, covariates = NULL, 
                                         residual_threshold = 2,
                                         cooks_threshold_type = "conservative", 
                                         cooks_threshold_custom = 0.01) {
  # Build formula based on model type
  formula_terms <- c(outcome_var, "~", predictor_var)
  
  # Add interaction term for moderation models (only if moderator is provided)
  if(!is.null(moderator_var) && moderator_var != "") {
    formula_terms <- c(formula_terms, "*", moderator_var)
  }
  
  # Add second moderator interaction if provided
  if(!is.null(moderator2_var) && moderator2_var != "") {
    # For models with two moderators, add the second moderator interaction
    formula_terms <- c(formula_terms, "*", moderator2_var)
  }
  
  # Add mediators for mediation models
  if(!is.null(mediator_vars) && length(mediator_vars) > 0) {
    formula_terms <- c(formula_terms, "+", paste(mediator_vars, collapse = " + "))
  }
  
  # Add covariates
  if(!is.null(covariates) && length(covariates) > 0) {
    formula_terms <- c(formula_terms, "+", paste(covariates, collapse = " + "))
  }
  
  model_formula <- as.formula(paste(formula_terms, collapse = " "))
  
  # Check if outcome is binary
  outcome_is_binary <- is_binary_variable(data, outcome_var)
  
  if(outcome_is_binary) {
    # For binary outcomes, use logistic regression diagnostics
    model <- glm(model_formula, data = data, family = binomial())
    
    # Calculate leverage (hat values) and Cook's distance
    leverage <- hatvalues(model)
    cooks_d <- cooks.distance(model)
    
    # Get threshold
    n <- nrow(data)
    threshold <- if(cooks_threshold_type == "conservative") {
      4 / n
    } else if(cooks_threshold_type == "liberal") {
      1.0
    } else {
      cooks_threshold_custom
    }
    
    # Identify influential cases based on Cook's distance
    influential_cases <- which(cooks_d > threshold)
    
    return(list(
      cases = influential_cases,
      values = cooks_d[influential_cases],
      leverage = leverage[influential_cases],
      count = length(influential_cases),
      percentage = length(influential_cases) / length(cooks_d) * 100,
      is_binary = TRUE,
      threshold = threshold,
      method = "Cook's Distance"
    ))
  } else {
    # For continuous outcomes, use linear regression
    model <- lm(model_formula, data = data)
    std_resid <- rstandard(model)
    
    # Identify outliers
    outlier_cases <- which(abs(std_resid) > residual_threshold)
    outlier_values <- std_resid[outlier_cases]
    
    return(list(
      cases = outlier_cases,
      values = outlier_values,
      count = length(outlier_cases),
      percentage = length(outlier_cases) / length(std_resid) * 100,
      is_binary = FALSE,
      threshold = residual_threshold,
      method = "Standardized Residuals"
    ))
  }
}

# Function to check normality
check_normality <- function(model) {
  # Shapiro-Wilk test
  sw_test <- shapiro.test(residuals(model))
  # Create Q-Q plot and capture it
  qq_plot <- ggplot(data.frame(residuals = residuals(model)), aes(sample = residuals)) +
    stat_qq() + stat_qq_line() +
    theme_minimal() +
    labs(title = "Normal Q-Q Plot of Residuals")
  
  return(list(
    test = sw_test,
    plot = qq_plot,
    text = sprintf("Normality (Shapiro-Wilk): W = %.3f, p %s",
                  sw_test$statistic,
                  ifelse(sw_test$p.value < .001, "< .001",
                        sprintf("= %.3f", sw_test$p.value)))
  ))
}

# Function to test homoscedasticity
test_homoscedasticity <- function(model) {
  # Breusch-Pagan test
  bp_test <- car::ncvTest(model)
  return(sprintf("Homoscedasticity (Breusch-Pagan): χ²(%d) = %.3f, p %s",
                bp_test$Df,
                bp_test$ChiSquare,
                ifelse(bp_test$p < .001, "< .001", sprintf("= %.3f", bp_test$p))))
}

# Function for diagnostic report
diagnostic_report <- function(model) {
  tryCatch({
    # Basic model diagnostics
    n <- nobs(model)
    
    # Check if model is logistic (glm with binomial family)
    is_logistic <- inherits(model, "glm") && model$family$family == "binomial"
    
    # Handle VIF calculation
    vif_result <- tryCatch({
      # Get model terms
      terms <- attr(terms(model), "term.labels")
      
      if(length(terms) > 1) {
        # Calculate VIF for all predictors
        vif_values <- suppressWarnings(suppressMessages(car::vif(model)))
        
        # Format VIF results
        sprintf("VIF for predictors: %s", 
                paste(names(vif_values), sprintf("%.2f", vif_values), 
                collapse = ", "))
      } else {
        "VIF not calculated (insufficient predictors)"
      }
    }, error = function(e) {
      "VIF calculation unavailable"
    })
    
    # Return diagnostics
    c(
      sprintf("Sample size: %d", n),
      vif_result,
      if(is_logistic) {
        "Note: VIF calculated for all predictors. For binary outcomes, VIF interpretation is similar to linear regression."
      } else {
        "Note: VIF calculated for all predictors"
      }
    )
  }, error = function(e) {
    c(
      "Unable to compute some diagnostic measures",
      paste("Error details:", e$message)
    )
  })
}

# Spec-driven helpers for assumptions validation (with fallback)
assumptions_model_spec_row <- function(model_num) {
  if(is.null(model_num) || is.na(model_num) || !exists("process_model_specs", inherits = TRUE)) {
    return(NULL)
  }
  spec_tbl <- get("process_model_specs", inherits = TRUE)
  spec <- spec_tbl[spec_tbl$model == as.integer(model_num), , drop = FALSE]
  if(nrow(spec) == 1) spec else NULL
}

assumptions_requires_w <- function(model_num) {
  spec <- assumptions_model_spec_row(model_num)
  if(!is.null(spec)) return(isTRUE(spec$requires_w_input))
  model_num %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74, 83:92)
}

assumptions_requires_z <- function(model_num) {
  spec <- assumptions_model_spec_row(model_num)
  if(!is.null(spec)) return(isTRUE(spec$requires_z_input))
  model_num %in% c(2, 3)
}

assumptions_has_m <- function(model_num) {
  spec <- assumptions_model_spec_row(model_num)
  if(!is.null(spec)) return(isTRUE(spec$has_m))
  !is.null(model_num) && model_num >= 4 && model_num <= 92
}

# Helper function to get required variables description for a model
get_required_vars_description <- function(model_num) {
  required <- c("Predictor (X)", "Outcome (Y)")
  
  if(assumptions_requires_z(model_num)) {
    required <- c(required, "Moderator (W)", "Second Moderator (Z)")
  } else if(assumptions_requires_w(model_num)) {
    required <- c(required, "Moderator (W)")
  }
  
  if(assumptions_has_m(model_num)) {
    required <- c(required, "at least one Mediator")
  }
  
  return(required)
}

# Helper function to check if all required variables are selected for assumption checks
check_required_vars_for_assumptions <- function(model_num, predictor_var, outcome_var, 
                                                 moderator_var, moderator2_var, mediator_vars) {
  # Always need predictor and outcome
  if(is.null(predictor_var) || predictor_var == "" || 
     is.null(outcome_var) || outcome_var == "") {
    required_desc <- get_required_vars_description(model_num)
    return(list(valid = FALSE, message = paste0("This model requires: ", paste(required_desc, collapse = ", "), ".")))
  }
  
  # Check for moderator if required
  if(assumptions_requires_w(model_num)) {
    if(is.null(moderator_var) || moderator_var == "") {
      required_desc <- get_required_vars_description(model_num)
      return(list(valid = FALSE, message = paste0("This model requires: ", paste(required_desc, collapse = ", "), ".")))
    }
  }
  
  # Check for second moderator if required
  if(assumptions_requires_z(model_num)) {
    if(is.null(moderator2_var) || moderator2_var == "") {
      required_desc <- get_required_vars_description(model_num)
      return(list(valid = FALSE, message = paste0("This model requires: ", paste(required_desc, collapse = ", "), ".")))
    }
  }
  
  # Mediator models require at least one mediator
  if(assumptions_has_m(model_num)) {
    if(is.null(mediator_vars) || length(mediator_vars) == 0) {
      required_desc <- get_required_vars_description(model_num)
      return(list(valid = FALSE, message = paste0("This model requires: ", paste(required_desc, collapse = ", "), ".")))
    }
  }
  
  return(list(valid = TRUE, message = ""))
}

# Helper function to generate assumption checks HTML
# This function eliminates duplication between render function and download handler
generate_assumption_checks_html <- function(input, rv, mediator_vars_collected_func, outlier_summary_func, include_info_boxes = FALSE) {
  tryCatch({
    # Build formula based on model type
    formula_terms <- c(input$outcome_var, "~", input$predictor_var)
    
    # Add interaction for moderation models (only if moderator is selected)
    if(!is.null(input$moderator_var) && input$moderator_var != "") {
      formula_terms <- c(formula_terms, "*", input$moderator_var)
    }
    
    # Add second moderator interaction if provided
    if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
      formula_terms <- c(formula_terms, "*", input$moderator2_var)
    }
    
    # Add mediators for mediation models
    mediator_vars_current <- mediator_vars_collected_func()
    if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
      formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
    }
    
    # Add covariates
    if(!is.null(input$covariates) && length(input$covariates) > 0) {
      formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
    }
    
    model_formula <- as.formula(paste(formula_terms, collapse = " "))
    
    # Check if outcome is binary
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    
    if(outcome_is_binary) {
      # For binary outcomes, use logistic regression
      model <- glm(model_formula, data = rv$original_dataset, family = binomial())
      
      # Get outlier/influential summary
      outlier_text <- paste(outlier_summary_func(), collapse = "<br>")
      
      # Binary counts
      bin_counts <- c(
        binary_count_lines(rv$original_dataset, input$outcome_var, "Outcome (original)"),
        binary_count_lines(rv$original_dataset, input$predictor_var, "Predictor (original)")
      )
      if(!is.null(input$moderator_var) && input$moderator_var != "") {
        bin_counts <- c(bin_counts,
          binary_count_lines(rv$original_dataset, input$moderator_var, "Moderator (original)"))
      }
      mediator_vars_current <- mediator_vars_collected_func()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        for(med in mediator_vars_current) {
          bin_counts <- c(bin_counts,
            binary_count_lines(rv$original_dataset, med, paste0("Mediator ", med, " (original)")))
        }
      }
      
      # For binary outcomes, skip normality and homoscedasticity tests
      diagnostics <- diagnostic_report(model)
      
      # Build informational boxes if requested (for download handler)
      info_boxes <- ""
      if(include_info_boxes) {
        # Get Cook's distance threshold for reporting
        n <- nrow(rv$original_dataset)
        cooks_threshold <- if(input$cooks_threshold_type == "conservative") {
          sprintf("4/n = %.4f", 4/n)
        } else if(input$cooks_threshold_type == "liberal") {
          "1.0"
        } else {
          sprintf("%.4f", input$cooks_threshold_custom)
        }
        
        note_text <- paste(
          "These assumption checks are always performed on the original dataset. Results update automatically based on your selected variables and Cook's distance threshold (", cooks_threshold, "). ",
          "These assumption checks examine the <strong>outcome model</strong> (Y ~ X + M + W*X + covariates) using <strong>logistic regression diagnostics</strong> (Cook's distance and leverage) for binary outcomes.",
          sep = ""
        )
        
        example_text <- paste(
          "Prior to analysis, we examined assumptions for the outcome model using logistic regression diagnostics. ",
          "Cook's distance and leverage values were calculated from a logistic regression model predicting [outcome] from [predictor], [mediators], and [covariates]. ",
          "Variance inflation factors (VIF) for all predictors were below 5, indicating no multicollinearity concerns. ",
          "[X] cases with Cook's distance > ", cooks_threshold, " were identified as influential [and removed/retained based on your decision]. ",
          "Note: For binary outcomes, standard regression assumptions (normality, homoscedasticity) do not apply. Model fit was assessed using pseudo-R² measures (McFadden, Cox-Snell, Nagelkerke) shown in PROCESS output.",
          sep = ""
        )
        
        info_boxes <- paste(
          "<div style='background-color: #e7f3ff; padding: 10px; margin-bottom: 15px; border-left: 4px solid #2196F3; font-family: Arial, sans-serif;'>",
          "<strong>Note on Assumption Checks:</strong><br>",
          note_text,
          "</div>",
          "<div style='background-color: #fff9e6; padding: 10px; margin-bottom: 15px; border-left: 4px solid #FF9800; font-family: Arial, sans-serif;'>",
          "<strong>Example Reporting Format:</strong><br>",
          "<em>", example_text, "</em>",
          "</div>",
          sep = ""
        )
      }
      
      output_text <- paste(
        "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
        info_boxes,
        "<strong>Note: Binary Outcome Detected</strong><br>",
        "<em>Your outcome variable is binary (0/1). PROCESS will use logistic regression for this analysis.</em><br><br>",
        "<strong>Important:</strong> Standard regression assumptions (normality, homoscedasticity) do not apply to logistic regression.<br>",
        "For binary outcomes, different diagnostic approaches are needed:<br>",
        "<ul>",
        "<li><strong>Linearity:</strong> Check linearity of continuous predictors with the logit of the outcome</li>",
        "<li><strong>Influential observations:</strong> Review leverage values and Cook's distance in the outlier summary above</li>",
        "<li><strong>Model fit:</strong> Use pseudo-R² measures (McFadden, Cox-Snell, Nagelkerke) shown in PROCESS output</li>",
        "<li><strong>Multicollinearity:</strong> VIF can still be calculated for predictors</li>",
        "</ul><br>",
        if(length(na.omit(bin_counts)) > 0) {
          paste(
            "<strong>Binary Variable Counts (original dataset):</strong><br>",
            paste(na.omit(bin_counts), collapse = "<br>"),
            "<br><br>"
          )
        } else { "" },
        outlier_text,
        "<br><br>",
        "<strong>Additional Diagnostics:</strong><br>",
        paste(diagnostics, collapse = "<br>"),
        "<br><em>Note: VIF calculated for all predictors. For binary outcomes, focus on model fit statistics and residual patterns rather than normality/homoscedasticity.</em>",
        "</div>",
        sep = ""
      )
    } else {
      # For continuous outcomes, use linear regression
      model <- lm(model_formula, data = rv$original_dataset)
      
      # Get outlier summary
      outlier_text <- paste(outlier_summary_func(), collapse = "<br>")
      
      # Binary counts (only if binary)
      bin_counts <- c(
        binary_count_lines(rv$original_dataset, input$outcome_var, "Outcome (original)"),
        binary_count_lines(rv$original_dataset, input$predictor_var, "Predictor (original)")
      )
      if(!is.null(input$moderator_var) && input$moderator_var != "") {
        bin_counts <- c(bin_counts,
          binary_count_lines(rv$original_dataset, input$moderator_var, "Moderator (original)"))
      }
      mediator_vars_current <- mediator_vars_collected_func()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        for(med in mediator_vars_current) {
          bin_counts <- c(bin_counts,
            binary_count_lines(rv$original_dataset, med, paste0("Mediator ", med, " (original)")))
        }
      }
      
      # Run other diagnostics
      normality <- check_normality(model)
      homoscedasticity <- test_homoscedasticity(model)
      diagnostics <- diagnostic_report(model)
      
      # Build informational boxes if requested (for download handler)
      info_boxes <- ""
      if(include_info_boxes) {
        note_text <- paste(
          "These assumption checks are always performed on the original dataset. Results update automatically based on your selected variables and standardized residual threshold value (|SR| > ", input$residual_threshold, "). ",
          "These assumption checks examine the <strong>outcome model</strong> (Y ~ X + M + W*X + covariates) using <strong>linear regression diagnostics</strong> (standardized residuals) for continuous outcomes.",
          sep = ""
        )
        
        example_text <- paste(
          "Prior to analysis, we examined assumptions for the outcome model. Standardized residuals were calculated from a regression model predicting [outcome] from [predictor], [mediators], and [covariates]. ",
          "A Q-Q plot indicated residuals were approximately normally distributed, and a Breusch-Pagan test confirmed homoscedasticity, χ²(df) = X.XX, p = .XX. ",
          "Variance inflation factors (VIF) for all predictors were below 5, indicating no multicollinearity concerns. ",
          "[X] cases with standardized residuals > ", input$residual_threshold, " were identified as outliers [and removed/retained based on your decision].",
          sep = ""
        )
        
        info_boxes <- paste(
          "<div style='background-color: #e7f3ff; padding: 10px; margin-bottom: 15px; border-left: 4px solid #2196F3; font-family: Arial, sans-serif;'>",
          "<strong>Note on Assumption Checks:</strong><br>",
          note_text,
          "</div>",
          "<div style='background-color: #fff9e6; padding: 10px; margin-bottom: 15px; border-left: 4px solid #FF9800; font-family: Arial, sans-serif;'>",
          "<strong>Example Reporting Format:</strong><br>",
          "<em>", example_text, "</em>",
          "</div>",
          sep = ""
        )
      }
      
      # Create final output
      output_text <- paste(
        "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
        info_boxes,
        if(length(na.omit(bin_counts)) > 0) {
          paste(
            "<strong>Binary Variable Counts (original dataset):</strong><br>",
            paste(na.omit(bin_counts), collapse = "<br>"),
            "<br><br>"
          )
        } else { "" },
        outlier_text,
        "<br><br>",
        "<strong>Normality Test:</strong><br>",
        normality$text,
        "<br><em>Interpretation: A significant p-value (< .05) suggests non-normality. ",
        "However, with large samples, minor deviations often become significant. ",
        "Visual inspection of the Q-Q plot is often more informative.</em>",
        "<br><br>",
        "<strong>Homoscedasticity Test:</strong><br>",
        homoscedasticity,
        "<br><em>Interpretation: A significant p-value suggests non-constant variance. ",
        "Consider the Residuals vs Fitted plot for visual confirmation.</em>",
        "<br><br>",
        "<strong>Additional Diagnostics:</strong><br>",
        paste(diagnostics, collapse = "<br>"),
        "<br><em>Interpretation:<br>",
        "- VIF > 5 suggests potential multicollinearity issues<br>",
        "- With bootstrapping, these diagnostics become less crucial as bootstrap methods are more robust to violations</em>",
        "</div>",
        sep = ""
      )
    }
    
    return(output_text)
    
  }, error = function(e) {
    return(paste("<div class='alert alert-danger'>Error in assumption checks: ", e$message, "</div>"))
  })
}

# Helper to summarize binary counts
binary_count_lines <- function(data, var, label) {
  if (is.null(var) || var == "" || is.null(data)) return(NULL)
  vals <- data[[var]]
  vals <- vals[!is.na(vals)]
  if (length(unique(vals)) > 2) return(NULL)
  tab <- table(vals)
  levels_sorted <- sort(unique(vals))
  counts <- sapply(levels_sorted, function(x) tab[as.character(x)])
  names(counts) <- levels_sorted
  sprintf("%s (%s): %s", label, var,
          paste(sprintf("%s = %d", names(counts), counts), collapse = "; "))
}

# Helper function to wrap assumption checks content in complete HTML document
# Used by download handler to create standalone HTML file
wrap_assumptions_html <- function(content) {
  sprintf('
    <!DOCTYPE html>
    <html>
    <head>
      <style>
        body { font-family: Arial, sans-serif; padding: 20px; }
        table { border-collapse: collapse; margin: 10px 0; }
        th, td { border: 1px solid #dee2e6; padding: 8px; }
        th { background-color: #f8f9fa; }
      </style>
    </head>
    <body>
      %s
    </body>
  </html>
  ', content)
}
