# ============================================================================
# RESULTS MODULE
# ============================================================================
# This module contains all results display and download functionality,
# including formatting functions and output renderers.
# Extracted from gbPROCESS.R as part of Stage 6 modularization.
#
# Contents:
# - create_bivariate_correlations() function
# - create_missing_data_breakdown() function
# - create_formatted_output() function
# - wrap_results_html() function
# - output$analysis_output renderer
# - output$download_results download handler
# - Observer for download button state
# - output$download_filtered_data download handler
# ============================================================================

# Helper function to create bivariate correlations
create_bivariate_correlations <- function(data, predictor_var, outcome_var, settings) {
  outcome_is_binary <- is_binary_variable(data, outcome_var)
  complete_data <- data[complete.cases(data[c(predictor_var, outcome_var)]), ]
  n <- nrow(complete_data)
  
  dataset_desc <- if(settings$outliers_removed) {
    sprintf("the original dataset (before %d outlier%s removed)", 
            settings$outliers_count, ifelse(settings$outliers_count == 1, "", "s"))
  } else {
    "the original dataset (all cases included)"
  }
  
  if(outcome_is_binary) {
    pearson_test <- cor.test(complete_data[[predictor_var]], complete_data[[outcome_var]], 
                             method = "pearson")
    pearson_r <- pearson_test$estimate
    pearson_p <- pearson_test$p.value
    pearson_ci <- pearson_test$conf.int
    
    output <- c(
      "<br><strong>BIVARIATE CORRELATION: PREDICTOR AND OUTCOME</strong>",
      sprintf("<em>This shows the zero-order (unadjusted) relationship between the predictor and outcome variables, calculated on %s:</em>", dataset_desc),
      "",
      sprintf("<strong>Point-biserial correlation (Pearson's r):</strong> %.4f, 95%% CI [%.4f, %.4f], p %s", 
              pearson_r, pearson_ci[1], pearson_ci[2],
              ifelse(pearson_p < .001, "< .001", sprintf("= %.3f", pearson_p))),
      sprintf("<em>Sample size: %d cases (listwise deletion for these two variables)</em>", n),
      ""
    )
  } else {
    pearson_test <- cor.test(complete_data[[predictor_var]], complete_data[[outcome_var]], 
                             method = "pearson")
    pearson_r <- pearson_test$estimate
    pearson_p <- pearson_test$p.value
    pearson_ci <- pearson_test$conf.int
    
    has_ties <- any(duplicated(complete_data[[predictor_var]])) || any(duplicated(complete_data[[outcome_var]]))
    spearman_test <- cor.test(complete_data[[predictor_var]], complete_data[[outcome_var]], 
                              method = "spearman", exact = FALSE)
    spearman_rho <- spearman_test$estimate
    spearman_p <- spearman_test$p.value
    
    output <- c(
      "<br><strong>BIVARIATE CORRELATION: PREDICTOR AND OUTCOME</strong>",
      sprintf("<em>This shows the zero-order (unadjusted) relationship between the predictor and outcome variables, calculated on %s:</em>", dataset_desc),
      "",
      sprintf("<strong>Pearson's r:</strong> %.4f, 95%% CI [%.4f, %.4f], p %s", 
              pearson_r, pearson_ci[1], pearson_ci[2],
              ifelse(pearson_p < .001, "< .001", sprintf("= %.3f", pearson_p))),
      sprintf("<strong>Spearman's ρ:</strong> %.4f, p %s",
              spearman_rho,
              ifelse(spearman_p < .001, "< .001", sprintf("= %.3f", spearman_p))),
      if(has_ties) "<em>Note: Spearman p-value uses a standard approximation because repeated values (ties) were present.</em>",
      sprintf("<em>Sample size: %d cases (listwise deletion for these two variables)</em>", n),
      ""
    )
  }
  
  paste(output, collapse = "<br>")
}

# Helper function to create missing data breakdown
create_missing_data_breakdown <- function(data, settings) {
  all_vars <- c(settings$outcome_var, settings$predictor_var)
  if(!is.null(settings$mediator_vars)) all_vars <- c(all_vars, settings$mediator_vars)
  if(!is.null(settings$moderator_var)) all_vars <- c(all_vars, settings$moderator_var)
  if(!is.null(settings$moderator2_var)) all_vars <- c(all_vars, settings$moderator2_var)
  if(!is.null(settings$covariates)) all_vars <- c(all_vars, settings$covariates)
  
  missing_breakdown <- sapply(all_vars, function(var) {
    sum(is.na(data[[var]]))
  })
  
  missing_breakdown <- missing_breakdown[missing_breakdown > 0]
  
  if(length(missing_breakdown) > 0) {
    breakdown_lines <- sapply(names(missing_breakdown), function(var) {
      count <- missing_breakdown[var]
      sprintf("  %s: %d case%s missing", var, count, ifelse(count == 1, "", "s"))
    })
    paste(breakdown_lines, collapse = "<br>")
  } else {
    NULL
  }
}

# Format PROCESS output for display
create_formatted_output <- function(analysis_results) {
  if(is.null(analysis_results)) return("")
  
  settings <- analysis_results$settings
  process_output <- analysis_results$output
  
  
  # Model-role helpers for summary display (spec-driven with safe fallback)
  model_uses_w <- function(model_num) {
    if(!is.null(model_num) && !is.na(model_num) && exists("process_model_specs", inherits = TRUE)) {
      spec_tbl <- get("process_model_specs", inherits = TRUE)
      spec <- spec_tbl[spec_tbl$model == as.integer(model_num), , drop = FALSE]
      if(nrow(spec) == 1) {
        return(isTRUE(spec$requires_w_input))
      }
    }
    model_num %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74)
  }
  model_uses_z <- function(model_num) {
    if(!is.null(model_num) && !is.na(model_num) && exists("process_model_specs", inherits = TRUE)) {
      spec_tbl <- get("process_model_specs", inherits = TRUE)
      spec <- spec_tbl[spec_tbl$model == as.integer(model_num), , drop = FALSE]
      if(nrow(spec) == 1) {
        return(isTRUE(spec$requires_z_input))
      }
    }
    model_num %in% c(2, 3)
  }

  # Start with summary
  output_text <- c(
    "<strong>ANALYSIS SUMMARY</strong>",
    "",
    sprintf("Dataset name: %s", settings$dataset_name),
    sprintf("Dataset size: %d cases", settings$original_n),
    sprintf("PROCESS Model: %d", settings$model),
    if(settings$outliers_removed) {
      if(settings$outliers_method == "Cook's Distance") {
        sprintf("%d influential cases (Cook's D > %.4f) removed", 
                settings$outliers_count, settings$outliers_threshold)
      } else {
        sprintf("%d standardized residual outliers (|SR| > %.1f) removed", 
                settings$outliers_count, settings$outliers_threshold)
      }
    } else {
      "Original dataset with all cases"
    },
    "",
    sprintf("Predictor variable: %s", settings$predictor_var),
    sprintf("Outcome variable: %s", settings$outcome_var),
    if(!is.null(settings$mediator_vars)) {
      sprintf("Mediator variable(s): %s", paste(settings$mediator_vars, collapse = ", "))
    },
    # Only show moderator variables for models that use them
    summary_model_num <- if(!is.null(settings$model)) as.numeric(settings$model) else NA_real_,
    if(!is.null(settings$moderator_var) && settings$moderator_var != "" && 
       model_uses_w(summary_model_num)) {
      sprintf("Moderator variable: %s", settings$moderator_var)
    },
    if(!is.null(settings$moderator2_var) && settings$moderator2_var != "" &&
       model_uses_z(summary_model_num)) {
      sprintf("Second Moderator variable: %s", settings$moderator2_var)
    },
    if(!is.null(settings$covariates)) {
      sprintf("Covariate(s): %s", paste(settings$covariates, collapse = ", "))
    } else {
      "Covariate(s): none"
    },
    "",
    "<strong>ANALYSIS SETTINGS</strong>",
    paste("Centering:", {
      centering_val <- if(!is.null(settings$centering) && length(settings$centering) > 0) {
        as.character(settings$centering)[1]
      } else {
        "0"
      }
      switch(centering_val,
        "0" = "No centering",
        "1" = "All variables that define products",
        "2" = "Only continuous variables that define products"
      )
    }),
    "",
    "<strong>Bootstrap Settings:</strong>",
    if(!is.null(settings$use_bootstrap) && isTRUE(settings$use_bootstrap)) {
      bootstrap_info <- paste("Bootstrap samples:", if(!is.null(settings$boot_samples)) settings$boot_samples else "N/A")
      if(!is.null(settings$bootstrap_ci_method)) {
        ci_method_text <- if(settings$bootstrap_ci_method == "1") "Bias-corrected bootstrap" else "Percentile bootstrap"
        bootstrap_info <- paste(bootstrap_info, paste0("(", ci_method_text, ")"))
      }
      bootstrap_info
    } else {
      "Bootstrap: Not used"
    },
    if(!is.null(settings$seed)) {
      paste("Random seed:", settings$seed)
    },
    if(!is.null(settings$conf_level)) {
      paste("Confidence level:", settings$conf_level, "%")
    } else {
      "Confidence level: 95 %"
    },
    "",
    "<strong>Advanced Options:</strong>",
    {
      if(!is.null(settings$hc_method) && length(settings$hc_method) > 0) {
        hc_val <- as.character(settings$hc_method)[1]
        hc_display <- switch(hc_val,
          "none" = "OLS",
          "0" = "HC0 (Huber-White)",
          "1" = "HC1 (Hinkley)",
          "2" = "HC2",
          "3" = "HC3 (Davidson-MacKinnon)",
          "4" = "HC4 (Cribari-Neto)"
        )
        if(is.null(hc_display)) hc_display <- "OLS"  # fallback
        paste("Standard Errors:", hc_display)
      } else {
        "Standard Errors: OLS"
      }
    },
    if(!is.null(settings$stand) && isTRUE(settings$stand)) {
      "Coefficient type: Standardized coefficients"
    } else {
      "Coefficient type: Unstandardized coefficients"
    },
    if(!is.null(settings$stand) && isTRUE(settings$stand)) "Standardized coefficients: Yes",
    if(!is.null(settings$normal) && isTRUE(settings$normal)) "Normal theory tests: Yes",
    if(!is.null(settings$pairwise_contrasts) && isTRUE(settings$pairwise_contrasts)) "Pairwise contrasts of indirect effects: Yes",
    if(!is.null(settings$xmint) && isTRUE(settings$xmint)) "Allow X by M interaction: Yes",
    if(!is.null(settings$xmtest) && isTRUE(settings$xmtest)) "Test for X by M interaction: Yes",
    if(!is.null(settings$total) && isTRUE(settings$total)) "Total effect of X: Yes",
    "",
    if(!is.null(settings$probe_interactions) && isTRUE(settings$probe_interactions)) {
      c("<strong>Probing Moderation:</strong>",
        paste("Probe interactions: Yes"),
        if(!is.null(settings$probe_threshold)) paste("Probe threshold:", settings$probe_threshold),
        if(!is.null(settings$conditioning_values) && length(settings$conditioning_values) > 0) {
          cond_val <- as.character(settings$conditioning_values)[1]
          paste("Conditioning values:", switch(cond_val,
            "0" = "Mean ± 1 SD",
            "1" = "16th, 50th, 84th percentiles"
          ))
        },
        if(!is.null(settings$show_jn_regions) && isTRUE(settings$show_jn_regions)) "Show Johnson-Neyman significance regions: Yes"
      )
    },
    ""
  )
  
  # Add bivariate correlations
  bivariate_cor <- create_bivariate_correlations(
    analysis_results$original_data,
    settings$predictor_var,
    settings$outcome_var,
    settings
  )
  
  # Process PROCESS output - basic formatting
  # Filter out bootstrap progress lines only
  filtered_output <- process_output[
    !grepl("^Bootstrap|^Percentile bootstrap|^\\*+ BOOTSTRAP|^Level of confidence|^\\s*$|^\\*+$|^\\s*\\||^\\s*\\d+%|^\\s*>+\\s*$", 
           process_output, ignore.case = TRUE)
  ]
  
  # Basic formatting - output lines as-is
  processed_output <- character(0)
  in_covariance_matrix <- FALSE
  xmtest_explanation_added <- FALSE
  xmint_explanation_added <- FALSE
  in_jn_regions_section <- FALSE
  skip_jn_regions <- FALSE
  
  # Check if we should skip JN significance regions section
  if(!is.null(settings$show_jn_regions) && !isTRUE(settings$show_jn_regions)) {
    skip_jn_regions <- TRUE
  }
  
  for(i in seq_along(filtered_output)) {
    line <- filtered_output[i]
    
    # Track JN significance regions section
    if(grepl("Moderator value\\(s\\) defining Johnson-Neyman significance region\\(s\\):", line, ignore.case = TRUE)) {
      in_jn_regions_section <- TRUE
      if(skip_jn_regions) {
        # Skip this header line and continue to skip until next major section
        next
      } else {
        processed_output <- c(processed_output, line)
        next
      }
    }
    
    # If we're in JN regions section and should skip it, continue skipping until next major section
    if(in_jn_regions_section && skip_jn_regions) {
      # Check if we've reached the next major section
      # Look for section headers (lines that start with capital letters, contain colons, or are separators)
      if(grepl("^[A-Z].*:|^\\*+|^----------|^Conditional effect|^Data for visualizing|^Test\\(s\\) of|^Outcome Variable:", line, ignore.case = TRUE)) {
        # We've hit a new section - stop skipping
        in_jn_regions_section <- FALSE
        # Fall through to process this line normally
      } else {
        # Still in JN regions section, skip this line
        next
      }
    }
    
    # Bold important headers
    if(grepl("^Outcome Variable:", line, ignore.case = TRUE)) {
      processed_output <- c(processed_output, 
        paste0("<strong style='font-size: 1.1em;'>", line, "</strong>"))
      next
    }
    if(grepl("^Test\\(s\\) of highest order unconditional interaction\\(s\\):", line, ignore.case = TRUE)) {
      processed_output <- c(processed_output, 
        paste0("<strong style='font-size: 1.1em;'>", line, "</strong>"))
      next
    }
    
    # Track sections and add explanatory text
    if(grepl("^Test\\(s\\) of X by M interaction|^Likelihood ratio test\\(s\\) of X by M interaction", line, ignore.case = TRUE)) {
      if(!xmtest_explanation_added) {
        processed_output <- c(processed_output, 
          "<br><em><strong>Understanding 'Test for X by M interaction':</strong> This section presents statistical tests (F-tests or likelihood ratio tests) to determine whether X*M interaction terms are significant. A significant result indicates that the effect of the mediator(s) (M) on the outcome (Y) depends on the level of the independent variable (X). This test does not change the model structure - it evaluates whether interactions should be included.</em><br>")
        xmtest_explanation_added <- TRUE
      }
      processed_output <- c(processed_output, line)
      next
    }
    if(grepl("^COUNTERFACTUALLY DEFINED|^Direct, indirect, and total effects are counterfactually defined", line, ignore.case = TRUE)) {
      if(!xmint_explanation_added) {
        processed_output <- c(processed_output, 
          "<br><em><strong>Understanding 'Allow X by M interaction' (Model 4):</strong> When this option is enabled, Model 4 is converted to a counterfactual framework (Model 74 internally). This includes X*M interaction terms in the model, allowing the effect of mediators (M) on the outcome (Y) to vary depending on the level of X. Effects are now defined counterfactually (natural direct and indirect effects), which provides a different interpretation than standard mediation. This changes the model structure and enables estimation of controlled and natural direct effects.</em><br>")
        xmint_explanation_added <- TRUE
      }
      processed_output <- c(processed_output, line)
      next
    }
    if(grepl("^Covariance matrix", line, ignore.case = TRUE)) {
      in_covariance_matrix <- TRUE
      processed_output <- c(processed_output, line)
      next
    }
    if(grepl("^ANALYSIS NOTES|^\\*+$", line, ignore.case = TRUE)) {
      in_covariance_matrix <- FALSE
      processed_output <- c(processed_output, line)
      next
    }
    
    # Skip all covariance matrix content
    if(in_covariance_matrix) {
      processed_output <- c(processed_output, line)
      next
    }
    
    # Skip header lines (but NOT variable names like "constant" or "int_1" which are actual data rows)
    # Only skip lines that are clearly headers (column names, section titles, etc.)
    if(grepl("^\\s*(Model|Outcome|coeff|se\\(|t|p|LLCI|ULCI|R|R-sq|MSE|F\\(|df|Product|Variable|Mean|SD|Min|Max|Pearson|Spearman|effect|Effect|BootSE|BootLLCI|BootULCI|^\\s*$|Direct effect|Indirect effect|TOTAL|Completely standardized|Partially standardized|Specific indirect)", line, ignore.case = TRUE)) {
      processed_output <- c(processed_output, line)
      next
    }
    
    # Output all other lines as-is
    processed_output <- c(processed_output, line)
  }
  
  # Combine all output
  paste(
    "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
    paste(c(output_text, "", bivariate_cor, "", processed_output), collapse = "<br>"),
    "</div>"
  )
}

# Helper function to wrap results content in complete HTML document
# Used by download handler to create standalone HTML file
wrap_results_html <- function(content) {
  sprintf('
    <!DOCTYPE html>
    <html>
    <head>
      <style>
        body { font-family: Courier, monospace; white-space: pre-wrap; padding: 20px; }
        table { border-collapse: collapse; }
        th, td { border: 1px solid #dee2e6; padding: 8px; }
      </style>
    </head>
    <body>
      %s
    </body>
  </html>
  ', content)
}

# Display analysis results
output$analysis_output <- renderUI({
  dbg("DEBUG: analysis_output renderUI called")
  
  # If analysis results are NULL (e.g., after model switch), show empty state
  if(is.null(rv$analysis_results)) {
    dbg("DEBUG: rv$analysis_results is NULL, showing empty state")
    return(HTML("<div style='padding: 20px; text-align: center; color: #666;'>Run an analysis to see results here.</div>"))
  }
  
  # Check for validation errors first
  if(!is.null(rv$validation_error)) {
    dbg(paste("DEBUG: Validation error found:", rv$validation_error))
    return(HTML(paste0(
      "<div style='color: red; font-weight: bold; padding: 15px; border: 2px solid red; background-color: #ffe6e6; margin: 10px 0; border-radius: 5px;'>",
      "<strong>ERROR:</strong><br><br>",
      rv$validation_error,
      "</div>"
    )))
  }
  
  tryCatch({
    results <- analysis_results()
    dbg(paste("DEBUG: analysis_results() returned NULL?", is.null(results)))
    if(is.null(results)) {
      return(HTML("<p>No analysis results available. Please run an analysis.</p>"))
    }
    dbg("DEBUG: Creating formatted output...")
    HTML(create_formatted_output(results))
  }, error = function(e) {
    # Catch other errors and display them in the output
    error_msg <- conditionMessage(e)
    dbg(paste("DEBUG: Error caught in analysis_output:", error_msg))
    HTML(paste0(
      "<div style='color: red; font-weight: bold; padding: 15px; border: 2px solid red; background-color: #ffe6e6; margin: 10px 0; border-radius: 5px;'>",
      "<strong>ERROR:</strong><br><br>",
      error_msg,
      "</div>"
    ))
  })
})

# Download handlers
output$download_results <- downloadHandler(
  filename = function() {
    model_txt <- if(!is.null(input$process_model) && input$process_model != "") input$process_model else "unknown"
    paste0("model_", model_txt, "_process_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
  },
  content = function(file) {
    req(analysis_results())
    output_content <- create_formatted_output(analysis_results())
    writeLines(wrap_results_html(output_content), file)
  },
  contentType = "text/html"
)

# Disable download buttons until analysis is run and results match current model
observe({
  tryCatch({
    results <- analysis_results()
    current_model_num <- if(!is.null(input$process_model) && input$process_model != "") {
      as.numeric(input$process_model)
    } else {
      NULL
    }
    
    # Check if results exist and match current model
    has_results <- !is.null(results)
    if(has_results && !is.null(current_model_num) && !is.null(results$settings) && !is.null(results$settings$model)) {
      has_results <- results$settings$model == current_model_num
      if(!has_results) {
        dbg(paste("DEBUG: Download button disabled - results model", results$settings$model, "doesn't match current", current_model_num))
      }
    }
    
    shinyjs::toggleState("download_results", condition = has_results)
  }, error = function(e) {
    # If analysis_results() fails (e.g., inputs not initialized), disable all buttons
    shinyjs::disable("download_results")
  })
})

output$download_filtered_data <- downloadHandler(
  filename = function() {
    ext <- if(input$filtered_data_format == "sav") "sav" else "csv"
    model_txt <- if(!is.null(input$process_model) && input$process_model != "") input$process_model else "unknown"
    paste0("model_", model_txt, "_filtered_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
  },
  content = function(file) {
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    
    outliers <- identify_outliers()
    
    if(is.null(outliers) || outliers$count == 0) {
      stop("No outliers/influential cases found to remove")
    }
    
    filtered_data <- rv$original_dataset[-outliers$cases, ]
    
    if (input$filtered_data_format == "sav") {
      haven::write_sav(filtered_data, file)
    } else {
      write.csv(filtered_data, file, row.names = FALSE)
    }
  }
)

# Initially disable filtered dataset download button
shinyjs::disable("download_filtered_data")

# Enable/disable filtered dataset download button based on outlier count
observe({
  tryCatch({
    # Check if we have the required inputs
    has_required_inputs <- !is.null(rv$original_dataset) && 
                          !is.null(input$outcome_var) && input$outcome_var != "" &&
                          !is.null(input$predictor_var) && input$predictor_var != ""
    
    if(!has_required_inputs) {
      shinyjs::disable("download_filtered_data")
      return()
    }
    
    # Try to identify outliers
    outliers <- tryCatch({
      identify_outliers()
    }, error = function(e) {
      NULL
    })
    
    # Enable button only if outliers exist and count > 0
    has_outliers <- !is.null(outliers) && !is.null(outliers$count) && outliers$count > 0
    
    shinyjs::toggleState("download_filtered_data", condition = has_outliers)
  }, error = function(e) {
    # If anything fails, disable the button
    shinyjs::disable("download_filtered_data")
  })
})

