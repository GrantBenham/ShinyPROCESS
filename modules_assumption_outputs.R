# ============================================================================
# ASSUMPTION OUTPUTS MODULE
# ============================================================================
# Copyright (c) 2026 Dr. Grant Benham. See LICENSE for usage terms.
# This module contains server logic related to assumption check outputs,
# including outlier identification, diagnostic plots, and download handlers.
# Extracted from gbPROCESS.R as part of Stage 4 modularization.
#
# Contents:
# - cooks_threshold_value reactive
# - identify_outliers reactive
# - outlier_summary reactive
# - Assumption output renderers (validation message, info boxes, details)
# - Diagnostic plots (Q-Q, residuals, scale-location)
# - Violin plots (main variables and covariates)
# - Outlier removal button UI
# - Download handler for assumption checks
# ============================================================================

  # Reactive to compute Cook's distance threshold
  cooks_threshold_value <- reactive({
    req(rv$original_dataset, input$outcome_var)
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    
    if(!outcome_is_binary) {
      return(NULL)
    }
    
    n <- nrow(rv$original_dataset)
    
    if(input$cooks_threshold_type == "conservative") {
      return(4 / n)
    } else if(input$cooks_threshold_type == "liberal") {
      return(1.0)
    } else {
      return(input$cooks_threshold_custom)
    }
  })
  
  # Function to identify outliers using assumption checks module
  identify_outliers <- reactive({
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    
    identify_outliers_assumption(
      data = rv$original_dataset,
      outcome_var = input$outcome_var,
      predictor_var = input$predictor_var,
      mediator_vars = mediator_vars_collected(),
      moderator_var = input$moderator_var,
      moderator2_var = input$moderator2_var,
      covariates = input$covariates,
      residual_threshold = input$residual_threshold,
      cooks_threshold_type = input$cooks_threshold_type,
      cooks_threshold_custom = input$cooks_threshold_custom
    )
  })
  
  # Outlier summary reactive
  outlier_summary <- reactive({
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    
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
      mediator_vars_current <- mediator_vars_collected()
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
        # For binary outcomes, use logistic regression diagnostics
        model <- glm(model_formula, data = rv$original_dataset, family = binomial())
        
        leverage <- hatvalues(model)
        cooks_d <- cooks.distance(model)
        
        # Get threshold
        n <- nrow(rv$original_dataset)
        threshold <- if(input$cooks_threshold_type == "conservative") {
          4 / n
        } else if(input$cooks_threshold_type == "liberal") {
          1.0
        } else {
          input$cooks_threshold_custom
        }
        
        # Identify influential cases
        influential_cases <- which(cooks_d > threshold)
        
        if(length(influential_cases) > 0) {
          # Sort by Cook's distance
          sorted_indices <- order(-cooks_d[influential_cases])
          sorted_cases <- influential_cases[sorted_indices]
          sorted_cooks <- cooks_d[sorted_cases]
          sorted_leverage <- leverage[sorted_cases]
          
          case_summaries <- sapply(seq_along(sorted_cases), function(i) {
            sprintf("Case %d: Cook's D = %.4f, Leverage = %.4f", 
                    sorted_cases[i], sorted_cooks[i], sorted_leverage[i])
          })
          
          threshold_label <- if(input$cooks_threshold_type == "conservative") {
            sprintf("4/n = %.4f", threshold)
          } else if(input$cooks_threshold_type == "liberal") {
            "1.0"
          } else {
            sprintf("%.4f", threshold)
          }
          
          return(c(
            "<strong>Influential Case Analysis (Logistic Regression):</strong>",
            "<em>Note: Your outcome variable is binary (0/1). Using leverage and Cook's distance.</em>",
            "<br>",
            sprintf("Cases exceeding threshold (Cook's D > %s):", threshold_label),
            sprintf("Number of cases: %d (%.1f%%)", 
                    length(influential_cases),
                    100 * length(influential_cases) / length(cooks_d)),
            "<br>",
            "<strong>Understanding these metrics:</strong>",
            "<ul>",
            "<li><strong>Cook's Distance:</strong> Measures the influence of each case on all parameter estimates. Values > 4/n (conservative) or > 1.0 (liberal) suggest influential cases.</li>",
            "<li><strong>Leverage:</strong> Measures how unusual a case's predictor values are. High leverage cases can have disproportionate influence on the model.</li>",
            "</ul>",
            "<strong>Top cases (sorted by Cook's D):</strong>",
            paste0('<div style="max-height: 100px; overflow-y: auto; border: 1px solid #ddd; padding: 5px; margin: 5px 0;">', 
                  paste(case_summaries, collapse = "<br>"),
                  '</div>')
          ))
        } else {
          threshold_label <- if(input$cooks_threshold_type == "conservative") {
            sprintf("4/n = %.4f", threshold)
          } else if(input$cooks_threshold_type == "liberal") {
            "1.0"
          } else {
            sprintf("%.4f", threshold)
          }
          
          return(c(
            "<strong>Influential Case Analysis (Logistic Regression):</strong>",
            "<em>Note: Your outcome variable is binary (0/1). Using leverage and Cook's distance.</em>",
            "<br>",
            sprintf("No cases exceed the threshold (Cook's D > %s)", threshold_label)
          ))
        }
      }
      
      # For continuous outcomes, use linear regression
      model <- lm(model_formula, data = rv$original_dataset)
      
      # Get standardized residuals
      sresid <- rstandard(model)
      
      # Find outliers
      outliers <- which(abs(sresid) > input$residual_threshold)
      
      if(length(outliers) > 0) {
        # Sort outliers by absolute value of residuals
        sorted_indices <- order(-abs(sresid[outliers]))
        sorted_cases <- outliers[sorted_indices]
        sorted_resids <- sresid[sorted_cases]
        
        # Create case summaries
        case_summaries <- sapply(seq_along(sorted_cases), function(i) {
          sprintf("Case %d: SR = %.3f", sorted_cases[i], sorted_resids[i])
        })
        
        # Create summary text with scrollable div for cases
        c(
          "<strong>Standardized Residual Analysis:</strong>",
          sprintf("Cases exceeding threshold (|SR| > %.1f):", input$residual_threshold),
          sprintf("Number of cases: %d (%.1f%%)", 
                  length(outliers),
                  100 * length(outliers) / length(sresid)),
          "Top cases (sorted by |SR|):",
          paste0('<div style="max-height: 100px; overflow-y: auto; border: 1px solid #ddd; padding: 5px; margin: 5px 0;">', 
                paste(case_summaries, collapse = "<br>"),
                '</div>')
        )
      } else {
        c(
          "<strong>Standardized Residual Analysis:</strong>",
          sprintf("No cases exceed the threshold (|SR| > %.1f)", input$residual_threshold)
        )
      }
    }, error = function(e) {
      c(
        "<strong>Standardized Residual Analysis:</strong>",
        "Unable to compute residuals. Please check that all variables are numeric.",
        paste("Error details:", e$message)
      )
    })
  })
  
  # Validation message output (shown when required variables are missing)
  output$assumption_validation_message <- renderUI({
    if(is.null(input$process_model) || input$process_model == "" || 
       is.null(rv$original_dataset) || is.null(input$outcome_var) || is.null(input$predictor_var)) {
      return(HTML(""))
    }
    model_num <- as.numeric(input$process_model)
    mediator_vars_current <- mediator_vars_collected()
    validation <- check_required_vars_for_assumptions(
      model_num, input$predictor_var, input$outcome_var,
      input$moderator_var, input$moderator2_var, mediator_vars_current
    )
    
    if(!validation$valid) {
      return(HTML(paste(
        "<div class='alert alert-info' style='font-family: Arial, sans-serif; margin-bottom: 20px;'>",
        "<strong>Please select all required variables:</strong><br>",
        validation$message,
        "<br><br><em>Assumption checks will appear once all required variables for this model are selected.</em>",
        "</div>"
      )))
    }
    return(HTML(""))
  })
  
  # Info boxes output (blue and yellow boxes) - shown when all variables are selected
  output$assumption_info_boxes <- renderUI({
    if(is.null(input$process_model) || input$process_model == "" || 
       is.null(rv$original_dataset) || is.null(input$outcome_var) || is.null(input$predictor_var)) {
      return(HTML(""))
    }
    model_num <- as.numeric(input$process_model)
    mediator_vars_current <- mediator_vars_collected()
    validation <- check_required_vars_for_assumptions(
      model_num, input$predictor_var, input$outcome_var,
      input$moderator_var, input$moderator2_var, mediator_vars_current
    )
    
    if(!validation$valid) {
      return(HTML(""))
    }
    
    # Check if outcome is binary
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    
    # Build note text based on outcome type
    if(outcome_is_binary) {
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
    } else {
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
    }
    
    HTML(paste(
      "<div style='background-color: #e7f3ff; padding: 10px; margin-bottom: 15px; border-left: 4px solid #2196F3; font-family: Arial, sans-serif;'>",
      "<strong>Note on Assumption Checks:</strong><br>",
      note_text,
      "</div>",
      "<div style='background-color: #fff9e6; padding: 10px; margin-bottom: 15px; border-left: 4px solid #FF9800; font-family: Arial, sans-serif;'>",
      "<strong>Example Reporting Format:</strong><br>",
      "<em>", example_text, "</em>",
      "</div>",
      sep = ""
    ))
  })

  # Optional univariate outlier screening summary (detection only)
  output$univariate_outlier_summary <- renderUI({
    req(rv$original_dataset, input$process_model)

    if(!isTRUE(input$use_univariate_outlier_screen)) {
      return(NULL)
    }

    tryCatch({
      model_num <- as.numeric(input$process_model)
      mediator_vars_current <- mediator_vars_collected()
      validation <- check_required_vars_for_assumptions(
        model_num, input$predictor_var, input$outcome_var,
        input$moderator_var, input$moderator2_var, mediator_vars_current
      )

      if(!validation$valid) {
        return(NULL)
      }

      selected_vars <- c(input$outcome_var, input$predictor_var)
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        selected_vars <- c(selected_vars, mediator_vars_current)
      }
      if(!is.null(input$moderator_var) && nzchar(input$moderator_var)) {
        selected_vars <- c(selected_vars, input$moderator_var)
      }
      if(!is.null(input$moderator2_var) && nzchar(input$moderator2_var)) {
        selected_vars <- c(selected_vars, input$moderator2_var)
      }
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        selected_vars <- c(selected_vars, input$covariates)
      }
      selected_vars <- unique(selected_vars[!is.na(selected_vars) & nzchar(selected_vars)])

      cont_vars <- selected_vars[vapply(selected_vars, function(v) {
        is_continuous_variable(rv$original_dataset, v)
      }, logical(1))]

      if(length(cont_vars) == 0) {
        return(
          tags$div(
            style = "background-color: #f7f7f7; padding: 10px; margin-bottom: 15px; border-left: 4px solid #999; font-family: Arial, sans-serif;",
            tags$strong("Univariate Outlier Screening (Detection Only)"),
            tags$div(style = "margin-top: 6px;",
                     "No continuous variables are currently selected, so univariate screening is not shown.")
          )
        )
      }

      method <- if(!is.null(input$univariate_outlier_method) && input$univariate_outlier_method %in% c("iqr", "mad")) {
        input$univariate_outlier_method
      } else {
        "iqr"
      }
      iqr_k <- suppressWarnings(as.numeric(input$univariate_iqr_multiplier))
      if(is.na(iqr_k) || iqr_k < 0.5 || iqr_k > 5) iqr_k <- 1.5
      mad_thr <- suppressWarnings(as.numeric(input$univariate_mad_threshold))
      if(is.na(mad_thr) || mad_thr < 2 || mad_thr > 10) mad_thr <- 3.5

      uni_df <- compute_univariate_outlier_summary(
        data = rv$original_dataset,
        vars = cont_vars,
        method = method,
        iqr_multiplier = iqr_k,
        mad_threshold = mad_thr
      )

      if(is.null(uni_df) || !is.data.frame(uni_df) || nrow(uni_df) == 0) {
        return(NULL)
      }

      display_df <- uni_df
      display_df$`Flagged %` <- sprintf("%.1f%%", display_df$flagged_pct)
      if(method == "iqr") {
        # Keep variable-specific fence cutoffs visible for transparency.
        display_df$`Threshold / Rule` <- paste0(display_df$threshold, " | fences ", display_df$rule_detail)
      } else {
        # For MAD rows, threshold text is sufficient unless MAD is zero.
        display_df$`Threshold / Rule` <- ifelse(
          grepl("^MAD = 0", display_df$rule_detail),
          paste0(display_df$threshold, " | ", display_df$rule_detail),
          display_df$threshold
        )
      }
      display_df <- display_df[, c("variable", "method", "Threshold / Rule", "n_non_missing", "flagged_n", "Flagged %")]
      names(display_df) <- c("Variable", "Method", "Threshold / Rule", "N (non-missing)", "Flagged (n)", "Flagged %")

      header_note <- if(method == "iqr") {
        paste0("Method: IQR fences (Tukey), k = ", format(round(iqr_k, 2), nsmall = 1))
      } else {
        paste0("Method: MAD-based robust z-score, threshold |z| > ", format(round(mad_thr, 2), nsmall = 1))
      }

      table_rows <- lapply(seq_len(nrow(display_df)), function(i) {
        tags$tr(lapply(display_df[i, , drop = FALSE], function(cell) tags$td(as.character(cell), style = "padding: 4px 8px; border: 1px solid #ddd;")))
      })
      header_cells <- tags$tr(lapply(names(display_df), function(nm) tags$th(nm, style = "padding: 6px 8px; border: 1px solid #ddd; background: #f0f0f0;")))

      tags$div(
        style = "background-color: #f7f7f7; padding: 10px; margin-bottom: 15px; border-left: 4px solid #777; font-family: Arial, sans-serif;",
        tags$strong("Univariate Outlier Screening (Detection Only)"),
        tags$div(style = "margin-top: 6px; margin-bottom: 4px;", header_note),
        tags$div(
          style = "font-size: 12px; color: #555; margin-bottom: 8px;",
          "These are variable-by-variable screening flags for review only. They do not remove cases from the analysis dataset and do not replace model-based standardized residual / Cook's distance checks. Bootstrapping improves interval estimation but does not correct miscoding or influential/extreme observations."
        ),
        tags$div(
          style = "overflow-x: auto;",
          tags$table(
            style = "border-collapse: collapse; width: 100%; font-size: 12px;",
            tags$thead(header_cells),
            tags$tbody(table_rows)
          )
        )
      )
    }, error = function(e) {
      HTML(paste0(
        "<div class='alert alert-warning' style='margin-bottom: 15px;'>",
        "<strong>Univariate outlier screening error:</strong> ",
        htmltools::htmlEscape(e$message),
        "</div>"
      ))
    })
  })
  
  # Assumption details output
  output$assumption_details <- renderUI({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$process_model)
    
    tryCatch({
      # Check if all required variables are selected for this model
      model_num <- as.numeric(input$process_model)
      mediator_vars_current <- mediator_vars_collected()
      validation <- check_required_vars_for_assumptions(
        model_num, input$predictor_var, input$outcome_var,
        input$moderator_var, input$moderator2_var, mediator_vars_current
      )
      
      if(!validation$valid) {
        return(HTML(""))
      }
      
      # Generate HTML using helper function (without info boxes for UI display)
      output_text <- generate_assumption_checks_html(
        input, rv, 
        mediator_vars_collected_func = mediator_vars_collected,
        outlier_summary_func = outlier_summary,
        include_info_boxes = FALSE
      )
      
      HTML(output_text)
      
    }, error = function(e) {
      print("Error in assumption checks:")
      print(e$message)
      return(HTML(paste(
        "<div class='alert alert-danger'>",
        "Error in assumption checks: ", e$message,
        "</div>"
      )))
    })
  })
  
  # Diagnostic plots
  output$qq_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$process_model)
    
    tryCatch({
      # Check if all required variables are selected for this model
      model_num <- as.numeric(input$process_model)
      mediator_vars_current <- mediator_vars_collected()
      validation <- check_required_vars_for_assumptions(
        model_num, input$predictor_var, input$outcome_var,
        input$moderator_var, input$moderator2_var, mediator_vars_current
      )
      
      if(!validation$valid) {
        plot.new()
        text(0.5, 0.5, "Please select all required\nvariables for this model", cex = 1.2)
        return(NULL)
      }
      
      # Build formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      if(!is.null(input$moderator_var)) {
        formula_terms <- c(formula_terms, "*", input$moderator_var)
      }
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
      }
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        return(NULL)
      } else {
        model <- lm(model_formula, data = rv$original_dataset)
        
        ggplot(data.frame(residuals = rstandard(model)), aes(sample = residuals)) +
          stat_qq() + 
          stat_qq_line() +
          theme_minimal() +
          labs(title = "Normal Q-Q Plot",
               x = "Theoretical Quantiles",
               y = "Sample Quantiles") +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Error generating Q-Q plot", cex = 1.2)
    })
  })
  
  output$residual_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$process_model)
    
    tryCatch({
      # Check if all required variables are selected for this model
      model_num <- as.numeric(input$process_model)
      mediator_vars_current <- mediator_vars_collected()
      validation <- check_required_vars_for_assumptions(
        model_num, input$predictor_var, input$outcome_var,
        input$moderator_var, input$moderator2_var, mediator_vars_current
      )
      
      if(!validation$valid) {
        plot.new()
        text(0.5, 0.5, "Please select all required\nvariables for this model", cex = 1.2)
        return(NULL)
      }
      
      # Build formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      if(!is.null(input$moderator_var)) {
        formula_terms <- c(formula_terms, "*", input$moderator_var)
      }
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
      }
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        model <- glm(model_formula, data = rv$original_dataset, family = binomial())
        fitted_values <- fitted(model)
        pearson_resid <- residuals(model, type = "pearson")
        
        outliers <- identify_outliers()
        is_outlier <- seq_along(fitted_values) %in% outliers$cases
        
        subtitle_text <- if(outliers$count > 0) {
          sprintf("Cook's D > %.4f: %d influential case%s highlighted",
                  outliers$threshold, outliers$count,
                  ifelse(outliers$count == 1, "", "s"))
        } else {
          sprintf("Cook's D ≤ %.4f: No influential cases detected", outliers$threshold)
        }
        
        plot_data <- data.frame(
          fitted = fitted_values,
          residuals = pearson_resid,
          is_outlier = is_outlier
        )
        
        suppressMessages(
          ggplot(plot_data, aes(x = fitted, y = residuals)) +
            geom_point(aes(color = is_outlier), alpha = 0.6) +
            scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
            geom_smooth(method = "loess", se = FALSE, color = "blue") +
            geom_hline(yintercept = 0, linetype = "dashed") +
            theme_minimal() +
            labs(title = "Residuals vs Fitted (Logistic Regression)",
                 x = "Fitted probabilities",
                 y = "Pearson residuals",
                 subtitle = subtitle_text,
                 color = "Influential (Cook's D)") +
            theme(
              text = element_text(size = 14),
              axis.title = element_text(size = 16),
              axis.text = element_text(size = 14),
              plot.title = element_text(size = 18, hjust = 0.5),
              plot.subtitle = element_text(size = 12, hjust = 0.5),
              legend.position = "none",
              axis.line = element_line(color = "black", linewidth = 0.5),
              axis.ticks = element_line(color = "black", linewidth = 0.5)
            )
        )
      } else {
        model <- lm(model_formula, data = rv$original_dataset)
        
        fitted_values <- fitted(model)
        residuals <- residuals(model)
        std_resid <- rstandard(model)
        is_outlier <- abs(std_resid) > input$residual_threshold
        
        plot_data <- data.frame(
          fitted = fitted_values,
          residuals = residuals,
          is_outlier = is_outlier
        )
        
        suppressMessages(
          ggplot(plot_data, aes(x = fitted, y = residuals)) +
            geom_point(aes(color = is_outlier), alpha = 0.6) +
            scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
            geom_smooth(method = "loess", se = FALSE, color = "blue") +
            geom_hline(yintercept = 0, linetype = "dashed") +
            theme_minimal() +
            labs(title = "Residuals vs Fitted",
                 x = "Fitted values",
                 y = "Residuals") +
            theme(
              text = element_text(size = 14),
              axis.title = element_text(size = 16),
              axis.text = element_text(size = 14),
              plot.title = element_text(size = 18, hjust = 0.5),
              legend.position = "none",
              axis.line = element_line(color = "black", linewidth = 0.5),
              axis.ticks = element_line(color = "black", linewidth = 0.5)
            )
        )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Error generating residuals plot", cex = 1.2)
    })
  })
  
  output$scale_location_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    
    tryCatch({
      # Build formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      if(!is.null(input$moderator_var)) {
        formula_terms <- c(formula_terms, "*", input$moderator_var)
      }
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
      }
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        return(NULL)
      } else {
        model <- lm(model_formula, data = rv$original_dataset)
        
        fitted_values <- fitted(model)
        std_residuals <- rstandard(model)
        sqrt_abs_resid <- sqrt(abs(std_residuals))
        is_outlier <- abs(std_residuals) > input$residual_threshold
        
        plot_data <- data.frame(
          fitted = fitted_values,
          sqrt_abs_resid = sqrt_abs_resid,
          is_outlier = is_outlier
        )
        
        suppressMessages(
          ggplot(plot_data, aes(x = fitted, y = sqrt_abs_resid)) +
            geom_point(aes(color = is_outlier), alpha = 0.6) +
            scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
            geom_smooth(method = "loess", se = FALSE, color = "blue") +
            theme_minimal() +
            labs(title = "Scale-Location Plot",
                 x = "Fitted values",
                 y = expression(sqrt("|Standardized residuals|"))) +
            theme(
              text = element_text(size = 14),
              axis.title = element_text(size = 16),
              axis.text = element_text(size = 14),
              plot.title = element_text(size = 18, hjust = 0.5),
              legend.position = "none",
              axis.line = element_line(color = "black", linewidth = 0.5),
              axis.ticks = element_line(color = "black", linewidth = 0.5)
            )
        )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Error generating scale-location plot", cex = 1.2)
    })
  })

  # Row-level univariate screening flags for violin-plot overlays (detection only)
  build_univariate_flag_overlay_long <- function(data, vars, dataset_label) {
    empty_out <- data.frame(
      variable = character(0),
      value = numeric(0),
      dataset = character(0),
      stringsAsFactors = FALSE
    )
    if(is.null(data) || !is.data.frame(data) || length(vars) == 0 ||
       !isTRUE(input$use_univariate_outlier_screen)) {
      return(empty_out)
    }

    method <- if(!is.null(input$univariate_outlier_method) &&
                 input$univariate_outlier_method %in% c("iqr", "mad")) {
      input$univariate_outlier_method
    } else {
      "iqr"
    }
    iqr_k <- suppressWarnings(as.numeric(input$univariate_iqr_multiplier))
    if(is.na(iqr_k) || iqr_k < 0.5 || iqr_k > 5) iqr_k <- 1.5
    mad_thr <- suppressWarnings(as.numeric(input$univariate_mad_threshold))
    if(is.na(mad_thr) || mad_thr < 2 || mad_thr > 10) mad_thr <- 3.5

    rows <- lapply(vars, function(v) {
      if(!(v %in% names(data))) return(NULL)
      x_raw <- suppressWarnings(as.numeric(data[[v]]))
      keep <- !is.na(x_raw)
      x <- x_raw[keep]
      if(length(x) == 0) return(NULL)

      if(method == "iqr") {
        q1 <- as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, type = 7))
        q3 <- as.numeric(stats::quantile(x, 0.75, na.rm = TRUE, type = 7))
        iqr_val <- q3 - q1
        lower <- q1 - (iqr_k * iqr_val)
        upper <- q3 + (iqr_k * iqr_val)
        flagged_valid <- (x < lower) | (x > upper)
      } else {
        med <- stats::median(x, na.rm = TRUE)
        mad_val <- stats::mad(x, center = med, constant = 1, na.rm = TRUE)
        if(is.na(mad_val) || mad_val == 0) {
          flagged_valid <- rep(FALSE, length(x))
        } else {
          robust_z <- 0.6745 * (x - med) / mad_val
          flagged_valid <- abs(robust_z) > mad_thr
        }
      }

      if(!any(flagged_valid, na.rm = TRUE)) return(NULL)

      data.frame(
        variable = v,
        value = x[flagged_valid],
        dataset = dataset_label,
        stringsAsFactors = FALSE
      )
    })

    rows <- rows[!vapply(rows, is.null, logical(1))]
    if(length(rows) == 0) return(empty_out)
    out <- do.call(rbind, rows)
    if(is.null(out)) return(empty_out)
    out
  }
  
  output$violin_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    
    tryCatch({
      selected_vars <- c(input$outcome_var, input$predictor_var)
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current)) selected_vars <- c(selected_vars, mediator_vars_current)
      if(!is.null(input$moderator_var)) selected_vars <- c(selected_vars, input$moderator_var)
      if(!is.null(input$moderator2_var) && input$moderator2_var != "") selected_vars <- c(selected_vars, input$moderator2_var)
      
      cont_vars <- selected_vars[vapply(selected_vars, function(v) is_continuous_variable(rv$original_dataset, v), logical(1))]
      
      if(length(cont_vars) == 0) {
        plot.new()
        text(0.5, 0.5, "No continuous variables selected.\nViolin plot not shown.", cex = 1.1)
        return(NULL)
      }
      
      # Build data for original dataset
      orig_long <- do.call(rbind, lapply(cont_vars, function(v) {
        data.frame(
          variable = v,
          value = rv$original_dataset[[v]],
          dataset = "Original",
          stringsAsFactors = FALSE
        )
      }))
      
      # Build analysis dataset by removing identified outliers
      outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
      filtered_data <- rv$original_dataset
      if(!is.null(outliers) && length(outliers$cases) > 0) {
        filtered_data <- rv$original_dataset[-outliers$cases, ]
      }
      
      analysis_long <- do.call(rbind, lapply(cont_vars, function(v) {
        data.frame(
          variable = v,
          value = filtered_data[[v]],
          dataset = "After removal",
          stringsAsFactors = FALSE
        )
      }))
      
      plot_data <- rbind(orig_long, analysis_long)
      plot_data <- plot_data[!is.na(plot_data$value), ]
      plot_data$value <- suppressWarnings(as.numeric(plot_data$value))
      plot_data$variable <- factor(plot_data$variable, levels = cont_vars)
      plot_data$dataset <- factor(plot_data$dataset, levels = c("Original", "After removal"))

      flag_points <- data.frame()
      if(isTRUE(input$use_univariate_outlier_screen)) {
        flag_orig <- build_univariate_flag_overlay_long(rv$original_dataset, cont_vars, "Original")
        flag_after <- build_univariate_flag_overlay_long(filtered_data, cont_vars, "After removal")
        flag_list <- list(flag_orig, flag_after)
        flag_list <- flag_list[vapply(flag_list, function(df) is.data.frame(df) && nrow(df) > 0, logical(1))]
        flag_points <- if(length(flag_list) > 0) do.call(rbind, flag_list) else data.frame()
        if(!is.null(flag_points) && nrow(flag_points) > 0) {
          flag_points <- flag_points[!is.na(flag_points$value), , drop = FALSE]
          flag_points$value <- suppressWarnings(as.numeric(flag_points$value))
          flag_points$variable <- factor(flag_points$variable, levels = cont_vars)
          flag_points$dataset <- factor(flag_points$dataset, levels = c("Original", "After removal"))
        }
      }
      
      if(nrow(plot_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for violin plot.", cex = 1.1)
        return(NULL)
      }

      p <- ggplot(plot_data, aes(x = dataset, y = value, fill = dataset)) +
        geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
        geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.6) +
        facet_wrap(~ variable, scales = "free_y", ncol = 1) +
        labs(title = "Continuous Variable Distributions",
             x = "Dataset (Original vs After removal)",
             y = "Value",
             fill = "Dataset") +
        theme_minimal() +
        theme(
          text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.position = "right",
          strip.text = element_text(size = 14)
        )

      if(is.data.frame(flag_points) && nrow(flag_points) > 0) {
        p <- p + geom_point(
          data = flag_points,
          aes(x = dataset, y = value),
          inherit.aes = FALSE,
          shape = 21,
          size = 2.0,
          stroke = 0.4,
          fill = "#d32f2f",
          color = "black",
          alpha = 0.9,
          position = ggplot2::position_jitter(width = 0.05, height = 0),
          show.legend = FALSE
        )
      }

      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error generating violin plot:\n", e$message), cex = 1.1)
    })
  })
  
  # Violin plot for covariates
  output$violin_plot_covariates <- renderPlot({
    req(rv$original_dataset)
    
    tryCatch({
      if(is.null(input$covariates) || length(input$covariates) == 0) {
        return(NULL)
      }
      
      cont_covariates <- input$covariates[vapply(input$covariates, function(v) is_continuous_variable(rv$original_dataset, v), logical(1))]
      
      if(length(cont_covariates) == 0) {
        return(NULL)
      }
      
      # Build data for original dataset
      orig_long <- do.call(rbind, lapply(cont_covariates, function(v) {
        data.frame(
          variable = v,
          value = rv$original_dataset[[v]],
          dataset = "Original",
          stringsAsFactors = FALSE
        )
      }))
      
      # Build analysis dataset by removing identified outliers
      outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
      filtered_data <- rv$original_dataset
      if(!is.null(outliers) && length(outliers$cases) > 0) {
        filtered_data <- rv$original_dataset[-outliers$cases, ]
      }
      
      analysis_long <- do.call(rbind, lapply(cont_covariates, function(v) {
        data.frame(
          variable = v,
          value = filtered_data[[v]],
          dataset = "After removal",
          stringsAsFactors = FALSE
        )
      }))
      
      plot_data <- rbind(orig_long, analysis_long)
      plot_data <- plot_data[!is.na(plot_data$value), ]
      plot_data$value <- suppressWarnings(as.numeric(plot_data$value))
      plot_data$variable <- factor(plot_data$variable, levels = cont_covariates)
      plot_data$dataset <- factor(plot_data$dataset, levels = c("Original", "After removal"))

      flag_points <- data.frame()
      if(isTRUE(input$use_univariate_outlier_screen)) {
        flag_orig <- build_univariate_flag_overlay_long(rv$original_dataset, cont_covariates, "Original")
        flag_after <- build_univariate_flag_overlay_long(filtered_data, cont_covariates, "After removal")
        flag_list <- list(flag_orig, flag_after)
        flag_list <- flag_list[vapply(flag_list, function(df) is.data.frame(df) && nrow(df) > 0, logical(1))]
        flag_points <- if(length(flag_list) > 0) do.call(rbind, flag_list) else data.frame()
        if(!is.null(flag_points) && nrow(flag_points) > 0) {
          flag_points <- flag_points[!is.na(flag_points$value), , drop = FALSE]
          flag_points$value <- suppressWarnings(as.numeric(flag_points$value))
          flag_points$variable <- factor(flag_points$variable, levels = cont_covariates)
          flag_points$dataset <- factor(flag_points$dataset, levels = c("Original", "After removal"))
        }
      }
      
      if(nrow(plot_data) == 0) {
        return(NULL)
      }

      p <- ggplot(plot_data, aes(x = dataset, y = value, fill = dataset)) +
        geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
        geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.6) +
        facet_wrap(~ variable, scales = "free_y", ncol = 1) +
        labs(title = "Covariate Distributions",
             x = "Dataset (Original vs After removal)",
             y = "Value",
             fill = "Dataset") +
        theme_minimal() +
        theme(
          text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.position = "right",
          strip.text = element_text(size = 14)
        )

      if(is.data.frame(flag_points) && nrow(flag_points) > 0) {
        p <- p + geom_point(
          data = flag_points,
          aes(x = dataset, y = value),
          inherit.aes = FALSE,
          shape = 21,
          size = 2.0,
          stroke = 0.4,
          fill = "#d32f2f",
          color = "black",
          alpha = 0.9,
          position = ggplot2::position_jitter(width = 0.05, height = 0),
          show.legend = FALSE
        )
      }

      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error generating covariates violin plot:\n", e$message), cex = 1.1)
    })
  })
  
  # UI renderers for adaptive plot heights
  output$violin_plot_ui <- renderUI({
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    selected_vars <- c(input$outcome_var, input$predictor_var)
    mediator_vars_current <- mediator_vars_collected()
    if(!is.null(mediator_vars_current)) selected_vars <- c(selected_vars, mediator_vars_current)
    if(!is.null(input$moderator_var)) selected_vars <- c(selected_vars, input$moderator_var)
    if(!is.null(input$moderator2_var) && input$moderator2_var != "") selected_vars <- c(selected_vars, input$moderator2_var)
    cont_vars <- selected_vars[vapply(selected_vars, function(v) is_continuous_variable(rv$original_dataset, v), logical(1))]
    n_vars <- length(cont_vars)
    height_px <- max(200 * n_vars, 300)
    plotOutput("violin_plot", height = paste0(height_px, "px"), width = "700px")
  })
  
  output$violin_plot_covariates_ui <- renderUI({
    req(rv$original_dataset)
    if(is.null(input$covariates) || length(input$covariates) == 0) return(NULL)
    cont_covariates <- input$covariates[vapply(input$covariates, function(v) is_continuous_variable(rv$original_dataset, v), logical(1))]
    n_vars <- length(cont_covariates)
    if(n_vars == 0) return(NULL)
    height_px <- max(200 * n_vars, 300)
    plotOutput("violin_plot_covariates", height = paste0(height_px, "px"), width = "700px")
  })
  
  # UI output for outlier removal button
  output$outlier_removal_button <- renderUI({
    req(rv$original_dataset, input$outcome_var)
    
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    
    button_text <- if(outcome_is_binary) {
      "With Cook's Distance Outliers Removed"
    } else {
      "With Standardized Residual Outliers Removed"
    }
    
    # Check if button should be enabled
    button_enabled <- FALSE
    if (!is.null(input$predictor_var) && input$predictor_var != "" && 
        !is.null(input$outcome_var) && input$outcome_var != "") {
      tryCatch({
        outliers <- identify_outliers()
        if(!is.null(outliers) && !is.null(outliers$count) && outliers$count > 0 && 
           !is.null(outliers$cases) && length(outliers$cases) > 0) {
          # Check if removing outliers would leave enough cases
          reduced_data <- rv$original_dataset[-outliers$cases, ]
          all_vars <- c(input$outcome_var, input$predictor_var)
          mediator_vars_current <- mediator_vars_collected()
          if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
            all_vars <- c(all_vars, mediator_vars_current)
          }
          if(!is.null(input$moderator_var) && input$moderator_var != "") {
            all_vars <- c(all_vars, input$moderator_var)
          }
          if(!is.null(input$covariates) && length(input$covariates) > 0) {
            all_vars <- c(all_vars, input$covariates)
          }
          complete_cases <- complete.cases(reduced_data[all_vars])
          n_complete <- sum(complete_cases)
          button_enabled <- (n_complete >= 3)
        }
      }, error = function(e) {
        dbg(paste("DEBUG: Error checking outliers for button:", e$message))
        button_enabled <- FALSE
      })
    }
    
    actionButton("run_analysis_no_outliers", button_text, 
                class = "btn-warning",
                style = "width: 100%;",
                disabled = !button_enabled)
  })
  
  # Download handler for assumption checks
  build_assumption_formula_for_export <- function() {
    formula_terms <- c(input$outcome_var, "~", input$predictor_var)
    if(!is.null(input$moderator_var) && input$moderator_var != "") {
      formula_terms <- c(formula_terms, "*", input$moderator_var)
    }
    if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
      formula_terms <- c(formula_terms, "*", input$moderator2_var)
    }
    mediator_vars_current <- mediator_vars_collected()
    if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
      formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
    }
    if(!is.null(input$covariates) && length(input$covariates) > 0) {
      formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
    }
    as.formula(paste(formula_terms, collapse = " "))
  }

  assumptions_export_validation_ok <- function() {
    model_num <- suppressWarnings(as.numeric(input$process_model))
    mediator_vars_current <- mediator_vars_collected()
    validation <- check_required_vars_for_assumptions(
      model_num, input$predictor_var, input$outcome_var,
      input$moderator_var, input$moderator2_var, mediator_vars_current
    )
    isTRUE(validation$valid)
  }

  build_univariate_summary_export_html <- function() {
    if(!isTRUE(input$use_univariate_outlier_screen)) return("")
    if(!assumptions_export_validation_ok()) return("")

    selected_vars <- c(input$outcome_var, input$predictor_var)
    mediator_vars_current <- mediator_vars_collected()
    if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
      selected_vars <- c(selected_vars, mediator_vars_current)
    }
    if(!is.null(input$moderator_var) && nzchar(input$moderator_var)) {
      selected_vars <- c(selected_vars, input$moderator_var)
    }
    if(!is.null(input$moderator2_var) && nzchar(input$moderator2_var)) {
      selected_vars <- c(selected_vars, input$moderator2_var)
    }
    if(!is.null(input$covariates) && length(input$covariates) > 0) {
      selected_vars <- c(selected_vars, input$covariates)
    }
    selected_vars <- unique(selected_vars[!is.na(selected_vars) & nzchar(selected_vars)])
    cont_vars <- selected_vars[vapply(selected_vars, function(v) {
      is_continuous_variable(rv$original_dataset, v)
    }, logical(1))]

    if(length(cont_vars) == 0) return("")

    method <- if(!is.null(input$univariate_outlier_method) && input$univariate_outlier_method %in% c("iqr", "mad")) {
      input$univariate_outlier_method
    } else {
      "iqr"
    }
    iqr_k <- suppressWarnings(as.numeric(input$univariate_iqr_multiplier))
    if(is.na(iqr_k) || iqr_k < 0.5 || iqr_k > 5) iqr_k <- 1.5
    mad_thr <- suppressWarnings(as.numeric(input$univariate_mad_threshold))
    if(is.na(mad_thr) || mad_thr < 2 || mad_thr > 10) mad_thr <- 3.5

    uni_df <- compute_univariate_outlier_summary(
      data = rv$original_dataset,
      vars = cont_vars,
      method = method,
      iqr_multiplier = iqr_k,
      mad_threshold = mad_thr
    )
    if(!is.data.frame(uni_df) || nrow(uni_df) == 0) return("")

    display_df <- uni_df
    display_df$flagged_pct <- sprintf("%.1f%%", display_df$flagged_pct)
    if(method == "iqr") {
      display_df$rule <- paste0(display_df$threshold, " | fences ", display_df$rule_detail)
    } else {
      display_df$rule <- ifelse(
        grepl("^MAD = 0", display_df$rule_detail),
        paste0(display_df$threshold, " | ", display_df$rule_detail),
        display_df$threshold
      )
    }
    display_df <- display_df[, c("variable", "method", "rule", "n_non_missing", "flagged_n", "flagged_pct")]
    names(display_df) <- c("Variable", "Method", "Threshold / Rule", "N (non-missing)", "Flagged (n)", "Flagged %")

    header_note <- if(method == "iqr") {
      paste0("Method: IQR fences (Tukey), k = ", format(round(iqr_k, 2), nsmall = 1))
    } else {
      paste0("Method: MAD-based robust z-score, threshold |z| > ", format(round(mad_thr, 2), nsmall = 1))
    }

    table_html <- paste0(
      "<div style='overflow-x:auto;'><table style='border-collapse: collapse; width: 100%; font-size: 12px;'>",
      "<thead><tr>",
      paste(sprintf("<th style='padding: 6px 8px; border: 1px solid #ddd; background: #f0f0f0;'>%s</th>", names(display_df)), collapse = ""),
      "</tr></thead><tbody>",
      paste(apply(display_df, 1, function(row) {
        paste0("<tr>", paste(sprintf("<td style='padding: 4px 8px; border: 1px solid #ddd;'>%s</td>", htmltools::htmlEscape(as.character(row))), collapse = ""), "</tr>")
      }), collapse = ""),
      "</tbody></table></div>"
    )

    paste0(
      "<div style='background-color: #f7f7f7; padding: 10px; margin: 15px 0; border-left: 4px solid #777; font-family: Arial, sans-serif;'>",
      "<strong>Univariate Outlier Screening (Detection Only)</strong>",
      "<div style='margin-top: 6px; margin-bottom: 4px;'>", htmltools::htmlEscape(header_note), "</div>",
      "<div style='font-size: 12px; color: #555; margin-bottom: 8px;'>",
      "Variable-by-variable screening flags for review only (original dataset). These do not remove cases from the analysis dataset and do not replace model-based standardized residual / Cook's distance checks.",
      "</div>",
      table_html,
      "</div>"
    )
  }

  save_ggplot_to_data_uri <- function(p, width = 8, height = 6, dpi = 120) {
    if(is.null(p)) return(NULL)
    if(!requireNamespace("base64enc", quietly = TRUE)) return(NULL)
    tmp_png <- tempfile(fileext = ".png")
    on.exit(unlink(tmp_png), add = TRUE)
    ggplot2::ggsave(tmp_png, plot = p, device = "png", width = width, height = height, dpi = dpi, units = "in", bg = "white")
    base64enc::dataURI(file = tmp_png, mime = "image/png")
  }

  build_export_plot_qq <- function() {
    if(!assumptions_export_validation_ok()) return(NULL)
    if(is_binary_variable(rv$original_dataset, input$outcome_var)) return(NULL)
    model_formula <- build_assumption_formula_for_export()
    model <- lm(model_formula, data = rv$original_dataset)
    ggplot(data.frame(residuals = rstandard(model)), aes(sample = residuals)) +
      stat_qq() +
      stat_qq_line() +
      theme_minimal() +
      labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme(
        text = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.line = element_line(color = "black", linewidth = 0.4),
        axis.ticks = element_line(color = "black", linewidth = 0.4)
      )
  }

  build_export_plot_residual <- function() {
    if(!assumptions_export_validation_ok()) return(NULL)
    model_formula <- build_assumption_formula_for_export()
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    if(outcome_is_binary) {
      model <- glm(model_formula, data = rv$original_dataset, family = binomial())
      fitted_values <- fitted(model)
      pearson_resid <- residuals(model, type = "pearson")
      outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
      is_outlier <- if(!is.null(outliers) && !is.null(outliers$cases)) seq_along(fitted_values) %in% outliers$cases else rep(FALSE, length(fitted_values))
      subtitle_text <- if(!is.null(outliers) && !is.null(outliers$count) && outliers$count > 0) {
        sprintf("Cook's D > %.4f: %d influential case%s highlighted",
                outliers$threshold, outliers$count, ifelse(outliers$count == 1, "", "s"))
      } else if(!is.null(outliers) && !is.null(outliers$threshold)) {
        sprintf("Cook's D <= %.4f: No influential cases detected", outliers$threshold)
      } else {
        "No influential cases detected"
      }
      plot_data <- data.frame(fitted = fitted_values, residuals = pearson_resid, is_outlier = is_outlier)
      suppressMessages(
        ggplot(plot_data, aes(x = fitted, y = residuals)) +
          geom_point(aes(color = is_outlier), alpha = 0.6) +
          scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme_minimal() +
          labs(title = "Residuals vs Fitted (Logistic Regression)",
               x = "Fitted probabilities", y = "Pearson residuals",
               subtitle = subtitle_text) +
          theme(
            text = element_text(size = 12), axis.title = element_text(size = 13),
            axis.text = element_text(size = 11), plot.title = element_text(size = 14, hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5),
            legend.position = "none"
          )
      )
    } else {
      model <- lm(model_formula, data = rv$original_dataset)
      plot_data <- data.frame(
        fitted = fitted(model),
        residuals = residuals(model),
        is_outlier = abs(rstandard(model)) > input$residual_threshold
      )
      suppressMessages(
        ggplot(plot_data, aes(x = fitted, y = residuals)) +
          geom_point(aes(color = is_outlier), alpha = 0.6) +
          scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme_minimal() +
          labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
          theme(text = element_text(size = 12), axis.title = element_text(size = 13),
                axis.text = element_text(size = 11), plot.title = element_text(size = 14, hjust = 0.5),
                legend.position = "none")
      )
    }
  }

  build_export_plot_scale_location <- function() {
    if(!assumptions_export_validation_ok()) return(NULL)
    if(is_binary_variable(rv$original_dataset, input$outcome_var)) return(NULL)
    model_formula <- build_assumption_formula_for_export()
    model <- lm(model_formula, data = rv$original_dataset)
    std_residuals <- rstandard(model)
    plot_data <- data.frame(
      fitted = fitted(model),
      sqrt_abs_resid = sqrt(abs(std_residuals)),
      is_outlier = abs(std_residuals) > input$residual_threshold
    )
    suppressMessages(
      ggplot(plot_data, aes(x = fitted, y = sqrt_abs_resid)) +
        geom_point(aes(color = is_outlier), alpha = 0.6) +
        scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
        geom_smooth(method = "loess", se = FALSE, color = "blue") +
        theme_minimal() +
        labs(title = "Scale-Location Plot",
             x = "Fitted values",
             y = expression(sqrt("|Standardized residuals|"))) +
        theme(text = element_text(size = 12), axis.title = element_text(size = 13),
              axis.text = element_text(size = 11), plot.title = element_text(size = 14, hjust = 0.5),
              legend.position = "none")
    )
  }

  build_export_plot_violin_main <- function() {
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    selected_vars <- c(input$outcome_var, input$predictor_var)
    mediator_vars_current <- mediator_vars_collected()
    if(!is.null(mediator_vars_current)) selected_vars <- c(selected_vars, mediator_vars_current)
    if(!is.null(input$moderator_var)) selected_vars <- c(selected_vars, input$moderator_var)
    if(!is.null(input$moderator2_var) && input$moderator2_var != "") selected_vars <- c(selected_vars, input$moderator2_var)
    cont_vars <- selected_vars[vapply(selected_vars, function(v) is_continuous_variable(rv$original_dataset, v), logical(1))]
    if(length(cont_vars) == 0) return(NULL)

    orig_long <- do.call(rbind, lapply(cont_vars, function(v) data.frame(variable = v, value = rv$original_dataset[[v]], dataset = "Original", stringsAsFactors = FALSE)))
    outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
    filtered_data <- rv$original_dataset
    if(!is.null(outliers) && length(outliers$cases) > 0) filtered_data <- rv$original_dataset[-outliers$cases, ]
    analysis_long <- do.call(rbind, lapply(cont_vars, function(v) data.frame(variable = v, value = filtered_data[[v]], dataset = "After removal", stringsAsFactors = FALSE)))

    plot_data <- rbind(orig_long, analysis_long)
    plot_data <- plot_data[!is.na(plot_data$value), , drop = FALSE]
    if(nrow(plot_data) == 0) return(NULL)
    plot_data$value <- suppressWarnings(as.numeric(plot_data$value))
    plot_data$variable <- factor(plot_data$variable, levels = cont_vars)
    plot_data$dataset <- factor(plot_data$dataset, levels = c("Original", "After removal"))

    flag_points <- data.frame()
    if(isTRUE(input$use_univariate_outlier_screen)) {
      flag_orig <- build_univariate_flag_overlay_long(rv$original_dataset, cont_vars, "Original")
      flag_after <- build_univariate_flag_overlay_long(filtered_data, cont_vars, "After removal")
      flag_list <- list(flag_orig, flag_after)
      flag_list <- flag_list[vapply(flag_list, function(df) is.data.frame(df) && nrow(df) > 0, logical(1))]
      flag_points <- if(length(flag_list) > 0) do.call(rbind, flag_list) else data.frame()
      if(is.data.frame(flag_points) && nrow(flag_points) > 0) {
        flag_points <- flag_points[!is.na(flag_points$value), , drop = FALSE]
        flag_points$value <- suppressWarnings(as.numeric(flag_points$value))
        flag_points$variable <- factor(flag_points$variable, levels = cont_vars)
        flag_points$dataset <- factor(flag_points$dataset, levels = c("Original", "After removal"))
      }
    }

    p <- ggplot(plot_data, aes(x = dataset, y = value, fill = dataset)) +
      geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
      geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.6) +
      facet_wrap(~ variable, scales = "free_y", ncol = 1) +
      labs(title = "Continuous Variable Distributions",
           x = "Dataset (Original vs After removal)", y = "Value", fill = "Dataset") +
      theme_minimal() +
      theme(text = element_text(size = 11), axis.title = element_text(size = 12),
            axis.text = element_text(size = 10), plot.title = element_text(size = 13, hjust = 0.5),
            strip.text = element_text(size = 11), legend.position = "right")

    if(is.data.frame(flag_points) && nrow(flag_points) > 0) {
      p <- p + geom_point(
        data = flag_points, aes(x = dataset, y = value), inherit.aes = FALSE,
        shape = 21, size = 1.8, stroke = 0.4, fill = "#d32f2f", color = "black",
        alpha = 0.9, position = ggplot2::position_jitter(width = 0.05, height = 0),
        show.legend = FALSE
      )
    }
    p
  }

  build_export_plot_violin_covariates <- function() {
    if(is.null(input$covariates) || length(input$covariates) == 0) return(NULL)
    cont_covariates <- input$covariates[vapply(input$covariates, function(v) is_continuous_variable(rv$original_dataset, v), logical(1))]
    if(length(cont_covariates) == 0) return(NULL)

    orig_long <- do.call(rbind, lapply(cont_covariates, function(v) data.frame(variable = v, value = rv$original_dataset[[v]], dataset = "Original", stringsAsFactors = FALSE)))
    outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
    filtered_data <- rv$original_dataset
    if(!is.null(outliers) && length(outliers$cases) > 0) filtered_data <- rv$original_dataset[-outliers$cases, ]
    analysis_long <- do.call(rbind, lapply(cont_covariates, function(v) data.frame(variable = v, value = filtered_data[[v]], dataset = "After removal", stringsAsFactors = FALSE)))

    plot_data <- rbind(orig_long, analysis_long)
    plot_data <- plot_data[!is.na(plot_data$value), , drop = FALSE]
    if(nrow(plot_data) == 0) return(NULL)
    plot_data$value <- suppressWarnings(as.numeric(plot_data$value))
    plot_data$variable <- factor(plot_data$variable, levels = cont_covariates)
    plot_data$dataset <- factor(plot_data$dataset, levels = c("Original", "After removal"))

    flag_points <- data.frame()
    if(isTRUE(input$use_univariate_outlier_screen)) {
      flag_orig <- build_univariate_flag_overlay_long(rv$original_dataset, cont_covariates, "Original")
      flag_after <- build_univariate_flag_overlay_long(filtered_data, cont_covariates, "After removal")
      flag_list <- list(flag_orig, flag_after)
      flag_list <- flag_list[vapply(flag_list, function(df) is.data.frame(df) && nrow(df) > 0, logical(1))]
      flag_points <- if(length(flag_list) > 0) do.call(rbind, flag_list) else data.frame()
      if(is.data.frame(flag_points) && nrow(flag_points) > 0) {
        flag_points <- flag_points[!is.na(flag_points$value), , drop = FALSE]
        flag_points$value <- suppressWarnings(as.numeric(flag_points$value))
        flag_points$variable <- factor(flag_points$variable, levels = cont_covariates)
        flag_points$dataset <- factor(flag_points$dataset, levels = c("Original", "After removal"))
      }
    }

    p <- ggplot(plot_data, aes(x = dataset, y = value, fill = dataset)) +
      geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
      geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.6) +
      facet_wrap(~ variable, scales = "free_y", ncol = 1) +
      labs(title = "Covariate Distributions",
           x = "Dataset (Original vs After removal)", y = "Value", fill = "Dataset") +
      theme_minimal() +
      theme(text = element_text(size = 11), axis.title = element_text(size = 12),
            axis.text = element_text(size = 10), plot.title = element_text(size = 13, hjust = 0.5),
            strip.text = element_text(size = 11), legend.position = "right")

    if(is.data.frame(flag_points) && nrow(flag_points) > 0) {
      p <- p + geom_point(
        data = flag_points, aes(x = dataset, y = value), inherit.aes = FALSE,
        shape = 21, size = 1.8, stroke = 0.4, fill = "#d32f2f", color = "black",
        alpha = 0.9, position = ggplot2::position_jitter(width = 0.05, height = 0),
        show.legend = FALSE
      )
    }
    p
  }

  build_assumption_export_plots_html <- function() {
    sections <- list(
      list(title = "Q-Q Plot", plot = tryCatch(build_export_plot_qq(), error = function(e) NULL), width = 7.0, height = 5.5),
      list(title = "Residuals vs Fitted", plot = tryCatch(build_export_plot_residual(), error = function(e) NULL), width = 7.0, height = 5.5),
      list(title = "Scale-Location Plot", plot = tryCatch(build_export_plot_scale_location(), error = function(e) NULL), width = 7.0, height = 5.5),
      list(title = "Continuous Variable Distributions (Violin Plots)", plot = tryCatch(build_export_plot_violin_main(), error = function(e) NULL), width = 8.5, height = 8.0),
      list(title = "Covariate Distributions (Violin Plots)", plot = tryCatch(build_export_plot_violin_covariates(), error = function(e) NULL), width = 8.5, height = 6.5)
    )

    html_parts <- lapply(sections, function(sec) {
      if(is.null(sec$plot)) return(NULL)
      uri <- tryCatch(save_ggplot_to_data_uri(sec$plot, width = sec$width, height = sec$height, dpi = 120), error = function(e) NULL)
      if(is.null(uri) || !nzchar(uri)) {
        return(paste0(
          "<div style='margin: 16px 0;'><h3 style='font-family: Arial, sans-serif;'>", htmltools::htmlEscape(sec$title), "</h3>",
          "<div style='font-family: Arial, sans-serif; color: #666;'>Plot export unavailable (image embedding dependency not available).</div></div>"
        ))
      }
      paste0(
        "<div style='margin: 16px 0;'>",
        "<h3 style='font-family: Arial, sans-serif; margin-bottom: 8px;'>", htmltools::htmlEscape(sec$title), "</h3>",
        "<img src='", uri, "' alt='", htmltools::htmlEscape(sec$title), "' style='max-width: 100%; border: 1px solid #ddd; background: white; padding: 4px;'/>",
        "</div>"
      )
    })
    html_parts <- Filter(Negate(is.null), html_parts)
    if(length(html_parts) == 0) return("")
    paste0(
      "<hr style='margin: 20px 0;'>",
      "<h2 style='font-family: Arial, sans-serif;'>Diagnostic Plots</h2>",
      paste(html_parts, collapse = "")
    )
  }

  output$download_assumptions <- downloadHandler(
    filename = function() {
      model_txt <- if(!is.null(input$process_model) && input$process_model != "") input$process_model else "unknown"
      paste0("model_", model_txt, "_assumption_checks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      req(rv$original_dataset, input$outcome_var, input$predictor_var)
      
      # Generate HTML using helper function (with info boxes for download)
      assumption_html <- generate_assumption_checks_html(
        input, rv,
        mediator_vars_collected_func = mediator_vars_collected,
        outlier_summary_func = outlier_summary,
        include_info_boxes = TRUE
      )
      univariate_html <- tryCatch(build_univariate_summary_export_html(), error = function(e) "")
      plots_html <- tryCatch(build_assumption_export_plots_html(), error = function(e) {
        paste0(
          "<hr style='margin: 20px 0;'><h2 style='font-family: Arial, sans-serif;'>Diagnostic Plots</h2>",
          "<div style='font-family: Arial, sans-serif; color: #b00;'>Unable to generate embedded plots for export: ",
          htmltools::htmlEscape(e$message),
          "</div>"
        )
      })
      export_html <- paste0(assumption_html, univariate_html, plots_html)
      
      # Wrap in complete HTML document
      writeLines(wrap_assumptions_html(export_html), file)
    },
    contentType = "text/html"
  )

