# ============================================================================
# ASSUMPTION OUTPUTS MODULE
# ============================================================================
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
      
      if(nrow(plot_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for violin plot.", cex = 1.1)
        return(NULL)
      }
      
      ggplot(plot_data, aes(x = dataset, y = value, fill = dataset)) +
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
      
      if(nrow(plot_data) == 0) {
        return(NULL)
      }
      
      ggplot(plot_data, aes(x = dataset, y = value, fill = dataset)) +
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
  output$download_assumptions <- downloadHandler(
    filename = function() {
      paste0("assumption_checks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
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
      
      # Wrap in complete HTML document
      writeLines(wrap_assumptions_html(assumption_html), file)
    },
    contentType = "text/html"
  )

