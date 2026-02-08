# ============================================================================
# ANALYSIS MODULE
# ============================================================================
# This module contains all analysis execution logic, including the shared
# run_process_analysis() function and the eventReactives for both original
# and outliers-removed analyses.
# Extracted from gbPROCESS.R as part of Stage 5 modularization.
#
# Contents:
# - run_process_analysis() function (shared by both analysis types)
# - original_analysis eventReactive
# - outliers_analysis eventReactive
# - Observers for analysis completion
# - analysis_results() reactive
# ============================================================================

# ============================================================================
# HELPER FUNCTION: Run PROCESS Analysis (Shared by both original and outliers-removed)
# ============================================================================
run_process_analysis <- function(analysis_dataset, remove_outliers = FALSE, outliers_info = NULL) {
  tryCatch({
  # Clear any previous validation errors at the start
  rv$validation_error <- NULL
  
  dbg(paste("DEBUG: ===== run_process_analysis STARTED (outliers_removed =", remove_outliers, ") ====="))

  # Extra diagnostics for vector-length issues in logical checks
  debug_input <- function(name, value) {
    value_len <- if(is.null(value)) 0 else length(value)
    value_type <- if(is.null(value)) "NULL" else class(value)[1]
    value_preview <- tryCatch({
      if(is.null(value)) "NULL" else paste(head(as.character(value), 3), collapse = ", ")
    }, error = function(e) "<unprintable>")
    dbg(paste("DEBUG: INPUT", name, "- type:", value_type, "len:", value_len, "value:", value_preview))
  }
  is_nonempty_scalar <- function(value) {
    !is.null(value) && length(value) == 1 && nzchar(as.character(value))
  }
  debug_input("input$process_model", input$process_model)
  debug_input("input$predictor_var", input$predictor_var)
  debug_input("input$outcome_var", input$outcome_var)
  debug_input("input$moderator_var", input$moderator_var)
  debug_input("input$moderator2_var", input$moderator2_var)
  debug_input("input$mediator_count", input$mediator_count)
  debug_input("input$covariates", input$covariates)
  debug_input("input$use_bootstrap", input$use_bootstrap)
  debug_input("input$bootstrap_ci_method", input$bootstrap_ci_method)
  debug_input("input$pairwise_contrasts", input$pairwise_contrasts)
  debug_input("input$xmint", input$xmint)
  debug_input("input$xmtest", input$xmtest)
  debug_input("input$probe_interactions", input$probe_interactions)
  debug_input("input$conditioning_values", input$conditioning_values)
  debug_input("input$probe_threshold", input$probe_threshold)
  debug_input("input$covmy", input$covmy)
  debug_input("input$covcoeff", input$covcoeff)
  debug_input("input$effsize", input$effsize)
  debug_input("input$stand", input$stand)
  debug_input("input$normal", input$normal)
  debug_input("input$matrices", input$matrices)
  debug_input("input$ssquares", input$ssquares)
  debug_input("input$modelres", input$modelres)
  debug_input("input$total", input$total)
  debug_input("input$plot", input$plot)
  debug_input("input$describe", input$describe)
  debug_input("input$listmiss", input$listmiss)
  debug_input("input$diagnose", input$diagnose)
  debug_input("input$seed", input$seed)
  debug_input("input$decimals", input$decimals)
  
  # Basic validation
  shiny::validate(
    need(!is.null(analysis_dataset), "Dataset not available"),
    need(input$outcome_var, "Outcome variable not selected"),
    need(input$predictor_var, "Predictor variable not selected"),
    need(input$process_model, "PROCESS model not selected"),
    need(input$outcome_var != input$predictor_var, 
         "Outcome and predictor must be different variables")
  )
  
  # Model-specific validation
  model_num <- as.numeric(input$process_model)
  
  # Moderation models require moderator
  if(model_num %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74, 83:92)) {
    shiny::validate(
      need(is_nonempty_scalar(input$moderator_var), 
           "Moderator variable (W) is required for this model")
    )
    
    # Models with second moderator require it to be selected
    if(model_num %in% models_with_second_moderator) {
      shiny::validate(
        need(is_nonempty_scalar(input$moderator2_var), 
             "Second moderator variable (Z) is required for this model")
      )
    }
  }
  
  # Models 4-92: All require at least one mediator
  if(model_num >= 4 && model_num <= 92) {
    mediator_vars_current <- mediator_vars_collected()
    shiny::validate(
      need(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0, 
           "At least one mediator variable is required for this model")
    )
    
    # Model 4 allows up to 10 mediators
    if(model_num == 4) {
      shiny::validate(
        need(length(mediator_vars_current) <= 10,
             "Model 4 allows up to 10 mediators")
      )
    }
    
    # Model 6 requires 2-6 mediators
    if(model_num == 6) {
      shiny::validate(
        need(length(mediator_vars_current) >= 2 && length(mediator_vars_current) <= 6,
             "Model 6 requires between 2 and 6 mediators")
      )
    }
    
    # Model 82 requires exactly 4 mediators
    if(model_num == 82) {
      mediator_count <- length(mediator_vars_current)
      if(mediator_count != 4) {
        if(mediator_count < 4) {
          error_msg <- paste0("Model 82 requires exactly 4 mediators. You have selected ", 
                             mediator_count, " mediator(s). Please select ", (4 - mediator_count), 
                             " more mediator(s).")
        } else {
          error_msg <- paste0("Model 82 requires exactly 4 mediators. You have selected ", 
                             mediator_count, " mediators. Please remove ", (mediator_count - 4), 
                             " mediator(s).")
        }
        showNotification(error_msg, type = "error", duration = 10)
        shiny::validate(need(FALSE, error_msg))
      }
    }
    
    # Models 83-92 require exactly 2 mediators
    if(model_num >= 83 && model_num <= 92) {
      mediator_count <- length(mediator_vars_current)
      if(mediator_count != 2) {
        if(mediator_count < 2) {
          error_msg <- paste0("Model ", model_num, " requires exactly 2 mediators. You have selected ", 
                             mediator_count, " mediator(s). Please select ", (2 - mediator_count), 
                             " more mediator(s).")
        } else {
          error_msg <- paste0("Model ", model_num, " requires exactly 2 mediators. You have selected ", 
                             mediator_count, " mediators. Please remove ", (mediator_count - 2), 
                             " mediator(s).")
        }
        showNotification(error_msg, type = "error", duration = 10)
        shiny::validate(need(FALSE, error_msg))
      }
    }
  }
  
  # Model 5: First and Second Stage Moderation
  if(model_num == 5) {
    shiny::validate(
      need(is_nonempty_scalar(input$moderator_var), 
           "Model 5 requires moderator variable W")
    )
  }
  
  # Check for validation errors (set by real-time validation observer)
  if(!is.null(rv$validation_error)) {
    showNotification(
      rv$validation_error,
      type = "error",
      duration = 10
    )
    return(NULL)
  }
  
  # Duplicate check: collect all selected variables and check for duplicates
  dbg("DEBUG: ===== Analysis duplicate check STARTED =====")
  models_with_moderator <- c(1, 5, 14, 15, 58, 59, 74, 83:92)
  models_with_second_moderator <- c(2, 3)
  models_with_moderators_disabled <- c(4, 6, 80:82)
  
  all_vars <- character(0)
  if(is_nonempty_scalar(input$predictor_var)) {
    all_vars <- c(all_vars, input$predictor_var)
  }
  if(is_nonempty_scalar(input$outcome_var)) {
    all_vars <- c(all_vars, input$outcome_var)
  }
  if((model_num %in% models_with_moderator || model_num %in% models_with_second_moderator) &&
     !(model_num %in% models_with_moderators_disabled)) {
    if(is_nonempty_scalar(input$moderator_var)) {
      all_vars <- c(all_vars, input$moderator_var)
    }
  }
  if(model_num %in% models_with_second_moderator) {
    if(is_nonempty_scalar(input$moderator2_var)) {
      all_vars <- c(all_vars, input$moderator2_var)
    }
  }
  if(model_num >= 4 && model_num <= 92) {
    current_mediators <- mediator_vars_collected()
    if(!is.null(current_mediators) && length(current_mediators) > 0) {
      all_vars <- c(all_vars, current_mediators)
    }
  }
  if(!is.null(input$covariates) && length(input$covariates) > 0) {
    all_vars <- c(all_vars, input$covariates)
  }
  
  # Final duplicate check (with exception for Model 74 where X and W can be the same)
  if(length(all_vars) > 0 && length(all_vars) != length(unique(all_vars))) {
    duplicate_vars <- all_vars[duplicated(all_vars)]
    
    if(model_num == 74) {
      if(length(duplicate_vars) == 1 && 
         is_nonempty_scalar(input$predictor_var) &&
         is_nonempty_scalar(input$moderator_var) &&
         input$predictor_var == input$moderator_var &&
         duplicate_vars[1] == input$predictor_var) {
        dbg("DEBUG: Model 74 - X=W is allowed, proceeding with analysis")
      } else {
        error_msg <- paste0("Error: The same variable cannot be used for multiple roles. Variable(s) '", 
                          paste(unique(duplicate_vars), collapse = "', '"), 
                          "' is/are used in more than one role.")
        showNotification(error_msg, type = "error", duration = 10)
        rv$validation_error <- error_msg
        return(NULL)
      }
    } else {
      error_msg <- paste0("Error: The same variable cannot be used for multiple roles. Variable(s) '", 
                        paste(unique(duplicate_vars), collapse = "', '"), 
                        "' is/are used in more than one role.")
      showNotification(error_msg, type = "error", duration = 10)
      rv$validation_error <- error_msg
      return(NULL)
    }
  }
  dbg("DEBUG: ===== Analysis duplicate check PASSED =====")
  
  req(analysis_dataset, input$outcome_var, input$predictor_var, input$process_model)
  
  withProgress(message = 'Running analysis...', value = 0, {
    # Set outliers_info
    if(remove_outliers) {
      rv$outliers_info <- outliers_info
    } else {
      rv$outliers_info <- NULL
    }
    
    # Build variable list for complete cases check
    all_vars_for_complete <- c(input$outcome_var, input$predictor_var)
    mediator_vars_current <- mediator_vars_collected()
    if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
      all_vars_for_complete <- c(all_vars_for_complete, mediator_vars_current)
    }
    if(is_nonempty_scalar(input$moderator_var)) {
      all_vars_for_complete <- c(all_vars_for_complete, input$moderator_var)
    }
    if(is_nonempty_scalar(input$moderator2_var)) {
      all_vars_for_complete <- c(all_vars_for_complete, input$moderator2_var)
    }
    if(!is.null(input$covariates) && length(input$covariates) > 0) {
      all_vars_for_complete <- c(all_vars_for_complete, input$covariates)
    }
    
    # Check complete cases
    complete_cases <- complete.cases(analysis_dataset[all_vars_for_complete])
    n_complete <- sum(complete_cases)
    
    if(n_complete < 3) {
      stop(sprintf("Only %d complete cases available. This is insufficient for analysis.", n_complete))
    }
    
    process_data <- analysis_dataset[complete_cases, ]
    
    # Store settings
    analysis_settings <- list(
      model = as.numeric(input$process_model),
      centering = input$centering,
      use_bootstrap = input$use_bootstrap,
      boot_samples = if(isTRUE(input$use_bootstrap)) input$boot_samples else NULL,
      hc_method = input$hc_method,
      conf_level = input$conf_level,
      dataset_name = tools::file_path_sans_ext(basename(input$data_file$name)),
      original_n = nrow(rv$original_dataset),
      outliers_removed = remove_outliers,
    outliers_count = if(isTRUE(remove_outliers) && !is.null(outliers_info)) outliers_info$count else 0,
    outliers_threshold = if(isTRUE(remove_outliers) && !is.null(outliers_info)) {
        outliers_info$threshold
      } else {
        if(is_binary_variable(rv$original_dataset, input$outcome_var)) {
          cooks_threshold_value()
        } else {
          input$residual_threshold
        }
      },
    outliers_method = if(isTRUE(remove_outliers) && !is.null(outliers_info)) {
        outliers_info$method
      } else {
        if(is_binary_variable(rv$original_dataset, input$outcome_var)) {
          "Cook's Distance"
        } else {
          "Standardized Residuals"
        }
      },
      predictor_var = input$predictor_var,
      outcome_var = input$outcome_var,
      mediator_vars = mediator_vars_collected(),
      moderator_var = input$moderator_var,
    moderator2_var = if(is_nonempty_scalar(input$moderator2_var)) input$moderator2_var else NULL,
      covariates = if(!is.null(input$covariates) && length(input$covariates) > 0) input$covariates else NULL,
      probe_interactions = if(!is.null(input$probe_interactions)) input$probe_interactions else FALSE,
      probe_threshold = if(!is.null(input$probe_threshold)) input$probe_threshold else "p < .10",
      conditioning_values = if(!is.null(input$conditioning_values)) input$conditioning_values else "1",
      show_jn_regions = if(!is.null(input$show_jn_regions)) input$show_jn_regions else TRUE
    )
    
    # Early validation check for W and Z same variable
    if(model_num %in% models_with_second_moderator) {
      if(is_nonempty_scalar(input$moderator_var) && 
         is_nonempty_scalar(input$moderator2_var) &&
         input$moderator_var == input$moderator2_var) {
        showNotification(
          "Error: W and Z moderators must be different variables. Please select different variables for the first and second moderators.",
          type = "error",
          duration = 10
        )
        rv$validation_error <- "W and Z moderators must be different variables."
        return(NULL)
      } else {
        rv$validation_error <- NULL
      }
    } else {
      rv$validation_error <- NULL
    }
    
    # Build PROCESS arguments
    # Handle centering - ensure we have a valid numeric value
    center_value <- if(!is.null(input$centering) && length(input$centering) > 0 && input$centering[1] != "") {
      as.numeric(input$centering[1])
    } else {
      0
    }
    # Ensure center_value is not NULL or length 0
    if(is.null(center_value) || length(center_value) == 0 || is.na(center_value)) {
      center_value <- 0
    }
    if(model_num == 4 && isTRUE(input$xmint)) {
      center_value <- 0
    }
    
    process_args <- list(
      data = process_data,
      y = input$outcome_var,
      x = input$predictor_var,
      model = model_num,
      center = center_value,
      conf = input$conf_level,
      modelbt = if(isTRUE(input$use_bootstrap)) 1 else 0,
      boot = if(isTRUE(input$use_bootstrap)) input$boot_samples else 0,
      bc = {
        if(isTRUE(input$use_bootstrap) && !is.null(input$bootstrap_ci_method) && 
           input$bootstrap_ci_method != "" && length(input$bootstrap_ci_method) > 0) {
          bc_val <- as.numeric(input$bootstrap_ci_method)
          if(is.null(bc_val) || length(bc_val) == 0 || is.na(bc_val)) 0 else bc_val
        } else {
          0
        }
      },
      hc = if(input$hc_method == "none") 5 else as.numeric(input$hc_method),
      covcoeff = if(isTRUE(input$covcoeff)) 1 else 0,
      cov = if(!is.null(input$covariates) && length(input$covariates) > 0 && 
                 !all(input$covariates == "")) {
        input$covariates
      } else {
        "xxxxx"
      }
    )
    
    # Add model-specific variables
    if(model_num >= 4 && model_num <= 92) {
      current_mediators <- mediator_vars_collected()
      if(!is.null(current_mediators) && length(current_mediators) > 0) {
        mediator_arg <- if(length(current_mediators) == 1) {
          current_mediators[1]
        } else {
          current_mediators
        }
        process_args$m <- mediator_arg
        
        if(model_num %in% c(4, 6) && isTRUE(input$pairwise_contrasts) && length(current_mediators) > 1) {
          process_args$contrast <- 1
        }
      }
    }
    
    # Add moderators
    if(model_num %in% c(1, 2, 3, 5, 14, 15, 16, 17, 18, 58, 59, 74, 83:92)) {
      if(is_nonempty_scalar(input$moderator_var)) {
        process_args$w <- input$moderator_var
        # Always set jn=1 for moderation models when probing is enabled to ensure JN plot data is generated
        # Check if probe_interactions is enabled (handle NULL from old JSON files)
        probe_interactions_enabled <- if(is.null(input$probe_interactions)) {
          TRUE  # Default to TRUE for moderation models
        } else {
          isTRUE(input$probe_interactions)
        }
        
        if(probe_interactions_enabled) {
          process_args$jn <- 1
          # Set moments based on conditioning_values if probe_interactions is enabled
          # Default to "1" (percentiles) if conditioning_values is NULL or empty
        cond_val <- if(!is.null(input$conditioning_values) && length(input$conditioning_values) > 0 && input$conditioning_values[1] != "") {
          input$conditioning_values[1]
          } else {
            "1"  # Default to percentiles
          }
          process_args$moments <- ifelse(cond_val == "0", 1, 0)
          dbg(paste("DEBUG: conditioning_values =", cond_val, "=> moments =", process_args$moments))
        }
      }
      
      if(model_num %in% models_with_second_moderator) {
        if(is_nonempty_scalar(input$moderator2_var)) {
          process_args$z <- input$moderator2_var
        }
      }
    }
    
    # Add other options
    if(isTRUE(input$effsize)) process_args$effsize <- 1
    if(isTRUE(input$stand)) process_args$stand <- 1
    if(isTRUE(input$normal)) process_args$normal <- 1
    if(isTRUE(input$matrices)) process_args$matrices <- 1
    if(isTRUE(input$covcoeff)) process_args$covcoeff <- 1
    if(isTRUE(input$covmy)) {
      if(model_num == 1) {
        process_args$cov <- "xxxxx"
      } else {
        if(!is.null(input$covariates) && length(input$covariates) > 0) {
          if(input$predictor_var %in% input$covariates) {
            showNotification(
              sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and the predictor (X). Please remove it from one of these roles.", input$predictor_var),
              type = "error",
              duration = 10
            )
            rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and the predictor (X).", input$predictor_var)
            return(NULL)
          }
          if(input$outcome_var %in% input$covariates) {
            showNotification(
              sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and the outcome (Y). Please remove it from one of these roles.", input$outcome_var),
              type = "error",
              duration = 10
            )
            rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and the outcome (Y).", input$outcome_var)
            return(NULL)
          }
          if(is_nonempty_scalar(input$moderator_var) && input$moderator_var %in% input$covariates) {
            showNotification(
              sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and a moderator (W). Please remove it from one of these roles.", input$moderator_var),
              type = "error",
              duration = 10
            )
            rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and a moderator (W).", input$moderator_var)
            return(NULL)
          }
          if(is_nonempty_scalar(input$moderator2_var) && input$moderator2_var %in% input$covariates) {
            showNotification(
              sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and a moderator (Z). Please remove it from one of these roles.", input$moderator2_var),
              type = "error",
              duration = 10
            )
            rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and a moderator (Z).", input$moderator2_var)
            return(NULL)
          }
          mediator_vars_current <- mediator_vars_collected()
          if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
            overlapping_mediators <- intersect(input$covariates, mediator_vars_current)
            if(length(overlapping_mediators) > 0) {
              showNotification(
                sprintf("Error: When 'Covariance matrix for Y' is checked, variable(s) '%s' cannot be used as both a covariate and a mediator. Please remove from one of these roles.", paste(overlapping_mediators, collapse="', '")),
                type = "error",
                duration = 10
              )
              rv$validation_error <- sprintf("Variable(s) '%s' cannot be used as both a covariate and a mediator.", paste(overlapping_mediators, collapse="', '"))
              return(NULL)
            }
          }
        }
      }
      process_args$covmy <- 1
    }
    if(isTRUE(input$describe)) process_args$describe <- 1
    if(isTRUE(input$listmiss)) process_args$listmiss <- 1
    if(isTRUE(input$diagnose)) process_args$diagnose <- 1
    if(isTRUE(input$ssquares)) process_args$ssquares <- 1
    if(isTRUE(input$modelres)) process_args$modelres <- 1
    
    # xmint handling
    if(model_num == 4 && isTRUE(input$xmint)) {
      if(isTRUE(input$xmtest)) {
        stop("Error: 'Allow X by M interaction' and 'Test for X by M interaction' cannot both be enabled. When 'Allow X by M interaction' is enabled, Model 4 is converted to Model 74, which makes the test option unavailable. Please disable one of these options.")
      }
      process_args$xmint <- 1
      x_is_dichotomous <- is_binary_variable(process_data, input$predictor_var)
      if(!x_is_dichotomous) {
        x_mean <- mean(process_data[[input$predictor_var]], na.rm = TRUE)
        process_args$xrefval <- x_mean
      }
    } else {
      process_args$xmint <- 0
    }
    
    if(isTRUE(input$xmtest)) {
      process_args$xmtest <- 1
    } else {
      process_args$xmtest <- 0
    }
    if(isTRUE(input$total)) process_args$total <- 1
    
    # Check if this is a moderation model
    is_mod_model <- model_num %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74)
    
    # For moderation models, set plot=2 and save=2 to get plot data
    if(is_mod_model) {
      process_args$plot <- 2
      process_args$save <- 2
    } else {
      if(isTRUE(input$use_bootstrap)) {
        process_args$save <- 1
      }
      if(isTRUE(input$plot)) {
        process_args$plot <- 1
        showNotification("Note: This will open R graphics device windows that cannot be easily saved or customized.", type = "warning", duration = 5)
      } else {
        process_args$plot <- 0
      }
    }
    
    # Handle probe_interactions - check if it's TRUE or if it's NULL (default to TRUE for moderation models)
    probe_interactions_enabled_for_intprobe <- if(is.null(input$probe_interactions)) {
      # If NULL (e.g., from old JSON), default to TRUE for moderation models
      is_mod_model
    } else {
      isTRUE(input$probe_interactions)
    }
    
    if(probe_interactions_enabled_for_intprobe) {
      probe_text <- if(is_nonempty_scalar(input$probe_threshold)) {
        input$probe_threshold
      } else {
        "p < .10"
      }
      probe_val <- tryCatch({
        num_match <- regmatches(probe_text, regexpr("0?\\.?\\d+", probe_text))
        if(length(num_match) > 0) {
          as.numeric(num_match[1])
        } else {
          0.1
        }
      }, error = function(e) 0.1)
      # Only set intprobe if we got a valid value
      if(!is.null(probe_val) && !is.na(probe_val) && length(probe_val) > 0) {
        process_args$intprobe <- probe_val
      }
    }
    seed_value <- if(!is.null(input$seed) && length(input$seed) > 0) input$seed[1] else NA
    if(!is.na(seed_value) && seed_value >= 1) process_args$seed <- seed_value
    if(!is.null(input$decimals) && length(input$decimals) > 0 && input$decimals != 4) {
      process_args$decimals <- paste0("9.", input$decimals)
    }
    
    # Debug: Print all process_args to identify any NULL or empty values
    dbg("DEBUG: ===== PROCESS ARGUMENTS BEFORE EXECUTION =====")
    for(arg_name in names(process_args)) {
      if(arg_name == "data") {
        dbg("DEBUG: process_args$data = <omitted>")
        next
      }
      arg_value <- process_args[[arg_name]]
      if(is.null(arg_value)) {
        dbg(paste("DEBUG: process_args$", arg_name, " = NULL", sep = ""))
      } else if(length(arg_value) == 0) {
        dbg(paste("DEBUG: process_args$", arg_name, " = (length 0)", sep = ""))
      } else if(is.character(arg_value) && length(arg_value) == 1 && arg_value == "") {
        dbg(paste("DEBUG: process_args$", arg_name, " = '' (empty string)", sep = ""))
      } else {
        dbg(paste("DEBUG: process_args$", arg_name, " = ", paste(arg_value, collapse = ", "), sep = ""))
      }
    }
    dbg("DEBUG: ===== END PROCESS ARGUMENTS =====")

    # Additional context for debugging analysis failures
    dbg(paste("DEBUG: complete_cases count =", n_complete, "of", nrow(analysis_dataset)))
    dbg(paste("DEBUG: all_vars_for_complete =", paste(all_vars_for_complete, collapse = ", ")))
    dbg(paste("DEBUG: mediator_vars_current =", {
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        paste(mediator_vars_current, collapse = ", ")
      } else {
        "NONE"
      }
    }))
    
    # Run PROCESS
    tryCatch({
      old_width <- options("width")
      options(width = 115)
      pdf(NULL)
      process_output <- capture.output({
        result <- withCallingHandlers(
          do.call(process, process_args),
          warning = function(w) {
            dbg(paste("DEBUG: WARNING during PROCESS execution:", conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        )
      })
      dev.off()
      options(width = old_width$width)
      
      # Store results including bootstrap data and plot data if available
      bootstrap_data <- NULL
      plot_data <- NULL

      # Debug: summarize result object
      dbg(paste("DEBUG: PROCESS result class =", paste(class(result), collapse = ", ")))
      dbg(paste("DEBUG: PROCESS result length =", if(is.null(result)) 0 else length(result)))
      if(is.list(result)) {
        dbg(paste("DEBUG: PROCESS result names =", paste(names(result), collapse = ", ")))
      }
      if(is.data.frame(result)) {
        dbg(paste("DEBUG: PROCESS result data.frame dims =", nrow(result), "x", ncol(result)))
      } else if(is.matrix(result)) {
        dbg(paste("DEBUG: PROCESS result matrix dims =", nrow(result), "x", ncol(result)))
      }
      dbg(paste("DEBUG: PROCESS output length =", length(process_output)))
      if(length(process_output) > 0) {
        dbg(paste("DEBUG: PROCESS output first line =", process_output[1]))
        dbg(paste("DEBUG: PROCESS output last line =", process_output[length(process_output)]))
      }
      
      if(is_mod_model && !is.null(result)) {
        if(is.list(result)) {
          rv$full_result <- result
        } else {
          rv$full_result <- list(result)
        }
        
        if(is.data.frame(result)) {
          plot_data <- result
        } else if(is.matrix(result)) {
          plot_data <- as.data.frame(result)
          plot_data <- plot_data[rowSums(is.na(plot_data) | plot_data == 99999) < ncol(plot_data), ]
        } else if(is.list(result) && length(result) > 0) {
          if(length(result) == 2) {
            if(is.data.frame(result[[2]])) {
              plot_data <- result[[2]]
            } else if(is.matrix(result[[2]])) {
              plot_data <- as.data.frame(result[[2]])
              plot_data <- plot_data[rowSums(is.na(plot_data) | plot_data == 99999) < ncol(plot_data), ]
            }
            if(is.data.frame(result[[1]])) {
              bootstrap_data <- result[[1]]
            } else if(is.matrix(result[[1]])) {
              bootstrap_data <- as.data.frame(result[[1]])
            }
          } else if(is.data.frame(result[[1]])) {
            plot_data <- result[[1]]
          } else if(is.matrix(result[[1]])) {
            plot_data <- as.data.frame(result[[1]])
            plot_data <- plot_data[rowSums(is.na(plot_data) | plot_data == 99999) < ncol(plot_data), ]
          }
        }
      } else if(isTRUE(input$use_bootstrap) && !is.null(result)) {
        if(is.data.frame(result)) {
          bootstrap_data <- result
        } else if(is.list(result) && length(result) > 0) {
          if(is.data.frame(result[[1]])) {
            bootstrap_data <- result[[1]]
          }
        }
      }
      
      # Clear validation error on successful run
      rv$validation_error <- NULL
      
      results_list <- list(
        output = process_output,
        data_used = process_data,
        original_data = rv$original_dataset,
        settings = analysis_settings,
        bootstrap_data = bootstrap_data,
        plot_data = plot_data,
        result = result
      )
      
      # Track which model these results are for
      if(!is.null(input$process_model) && input$process_model != "") {
        rv$results_model <- as.numeric(input$process_model)
      }
      
      return(results_list)
    }, error = function(e) {
      dbg(paste("DEBUG: ERROR in PROCESS execution:", e$message))
      dbg("DEBUG: Stack trace (sys.calls):")
      if (isTRUE(DEBUG_MODE)) {
        print(sys.calls())
      }
      showNotification(paste("Error running PROCESS analysis:", e$message), type = "error", duration = 10)
      rv$analysis_results <- NULL
      rv$validation_error <- conditionMessage(e)
      return(NULL)
    })
  })
  }, error = function(e) {
    dbg(paste("DEBUG: UNCAUGHT ERROR in run_process_analysis:", e$message))
    dbg("DEBUG: Stack trace (traceback):")
    if (isTRUE(DEBUG_MODE)) {
      traceback()
    }
    dbg("DEBUG: Stack trace (sys.calls):")
    if (isTRUE(DEBUG_MODE)) {
      print(sys.calls())
    }
    showNotification(paste("Error running PROCESS analysis:", e$message), type = "error", duration = 10)
    rv$analysis_results <- NULL
    rv$validation_error <- conditionMessage(e)
    return(NULL)
  })
}

# ============================================================================
# ANALYSIS EXECUTION - Original Dataset
# ============================================================================
original_analysis <- eventReactive(input$run_analysis, {
  req(rv$original_dataset)
  result <- run_process_analysis(rv$original_dataset, remove_outliers = FALSE, outliers_info = NULL)
  if(!is.null(result)) {
    rv$analysis_results <- result
  } else {
    dbg("DEBUG: original_analysis() returned NULL")
    if(!is.null(rv$validation_error)) {
      dbg(paste("DEBUG: rv$validation_error =", rv$validation_error))
    }
  }
  result
})

# Analysis with outliers removed
outliers_analysis <- eventReactive(input$run_analysis_no_outliers, {
  req(rv$original_dataset)
  
  # Identify outliers
  outliers <- identify_outliers()
  
  # Check if outliers were found
  if(is.null(outliers) || is.null(outliers$cases) || length(outliers$cases) == 0) {
    stop("No outliers found to remove. Please check your outlier detection settings.")
  }
  
  # Remove outliers
  reduced_data <- rv$original_dataset[-outliers$cases, ]
  
  # Run analysis with reduced dataset
  outliers_info <- list(
    count = outliers$count,
    threshold = outliers$threshold,
    method = outliers$method
  )
  
  result <- run_process_analysis(reduced_data, remove_outliers = TRUE, outliers_info = outliers_info)
  if(!is.null(result)) {
    rv$analysis_results <- result
  } else {
    dbg("DEBUG: outliers_analysis() returned NULL")
    if(!is.null(rv$validation_error)) {
      dbg(paste("DEBUG: rv$validation_error =", rv$validation_error))
    }
  }
  result
}, ignoreNULL = TRUE)

# Observer to trigger when original_analysis completes
observeEvent(original_analysis(), {
  # Only update if we're not in a clearing state (to prevent stale results from previous model)
  if(!isTRUE(rv$is_clearing)) {
    dbg("DEBUG: original_analysis() completed, checking if results match current model")
    result <- original_analysis()
    current_model_num <- if(!is.null(input$process_model) && input$process_model != "") {
      as.numeric(input$process_model)
    } else {
      NULL
    }
    
    # Verify results match current model before storing
    if(!is.null(result) && !is.null(result$settings) && !is.null(result$settings$model)) {
      if(is.null(current_model_num) || result$settings$model == current_model_num) {
        rv$analysis_results <- result
        rv$results_model <- result$settings$model
        dbg(paste("DEBUG: rv$analysis_results updated for model", rv$results_model))
      } else {
        dbg(paste("DEBUG: Skipping results update - results are for model", result$settings$model, "but current model is", current_model_num))
      }
    } else {
      dbg("DEBUG: Skipping results update - result doesn't have valid model information")
    }
  } else {
    dbg("DEBUG: Skipping analysis results update - in clearing state")
  }
}, ignoreNULL = TRUE)

# Observer to trigger when outliers_analysis completes
observeEvent(outliers_analysis(), {
  # Only update if we're not in a clearing state (to prevent stale results from previous model)
  if(!isTRUE(rv$is_clearing)) {
    dbg("DEBUG: outliers_analysis() completed, checking if results match current model")
    result <- outliers_analysis()
    current_model_num <- if(!is.null(input$process_model) && input$process_model != "") {
      as.numeric(input$process_model)
    } else {
      NULL
    }
    
    # Verify results match current model before storing
    if(!is.null(result) && !is.null(result$settings) && !is.null(result$settings$model)) {
      if(is.null(current_model_num) || result$settings$model == current_model_num) {
        rv$analysis_results <- result
        rv$results_model <- result$settings$model
        dbg(paste("DEBUG: rv$analysis_results updated for model", rv$results_model))
      } else {
        dbg(paste("DEBUG: Skipping results update - results are for model", result$settings$model, "but current model is", current_model_num))
      }
    } else {
      dbg("DEBUG: Skipping results update - result doesn't have valid model information")
    }
  } else {
    dbg("DEBUG: Skipping analysis results update - in clearing state")
  }
}, ignoreNULL = TRUE)

# Combined results reactive - use stored results from rv
analysis_results <- reactive({
  dbg(paste("DEBUG: analysis_results reactive called. rv$analysis_results is NULL?", is.null(rv$analysis_results)))
  
  # Get current model number
  current_model_num <- if(!is.null(input$process_model) && input$process_model != "") {
    as.numeric(input$process_model)
  } else {
    NULL
  }
  
  # Check if stored results match current model
  if(!is.null(rv$analysis_results) && !is.null(current_model_num)) {
    stored_model <- if(!is.null(rv$analysis_results$settings) && !is.null(rv$analysis_results$settings$model)) {
      rv$analysis_results$settings$model
    } else {
      NULL
    }
    if(!is.null(stored_model) && stored_model != current_model_num) {
      dbg(paste("DEBUG: Stored results are for model", stored_model, "but current model is", current_model_num, "- clearing results"))
      rv$analysis_results <- NULL
      rv$results_model <- NULL
      return(NULL)
    }
  }
  
  # If model has changed (tracked separately), don't try to recover old results
  if(!is.null(current_model_num) && !is.null(rv$results_model) && current_model_num != rv$results_model) {
    dbg(paste("DEBUG: Model changed from", rv$results_model, "to", current_model_num, "- not recovering old results"))
    return(NULL)
  }
  
  if(is.null(rv$analysis_results)) {
    # Try to get from eventReactive if rv is null
    # Check if inputs exist and have valid values before comparing
    run_analysis_val <- if(!is.null(input$run_analysis) && length(input$run_analysis) > 0) input$run_analysis else 0
    run_no_outliers_val <- if(!is.null(input$run_analysis_no_outliers) && length(input$run_analysis_no_outliers) > 0) input$run_analysis_no_outliers else 0
    
    if(run_analysis_val > 0 && (run_no_outliers_val == 0 || run_analysis_val >= run_no_outliers_val)) {
      dbg("DEBUG: rv$analysis_results is NULL, trying to get from original_analysis()")
      tryCatch({
        result <- original_analysis()
        # Verify the recovered results match the current model
        if(!is.null(result) && !is.null(result$settings) && !is.null(result$settings$model)) {
          if(is.null(current_model_num) || result$settings$model == current_model_num) {
            rv$analysis_results <- result
            rv$results_model <- if(!is.null(result$settings$model)) result$settings$model else current_model_num
            dbg(paste("DEBUG: Retrieved results from original_analysis() for model", rv$results_model))
          } else {
            dbg(paste("DEBUG: Recovered results are for model", result$settings$model, "but current model is", current_model_num, "- not using them"))
          }
        }
      }, error = function(e) {
        dbg(paste("DEBUG: Error getting from original_analysis():", e$message))
      })
    } else if(run_no_outliers_val > 0) {
      dbg("DEBUG: rv$analysis_results is NULL, trying to get from outliers_analysis()")
      tryCatch({
        result <- outliers_analysis()
        # Verify the recovered results match the current model
        if(!is.null(result) && !is.null(result$settings) && !is.null(result$settings$model)) {
          if(is.null(current_model_num) || result$settings$model == current_model_num) {
            rv$analysis_results <- result
            rv$results_model <- if(!is.null(result$settings$model)) result$settings$model else current_model_num
            dbg(paste("DEBUG: Retrieved results from outliers_analysis() for model", rv$results_model))
          } else {
            dbg(paste("DEBUG: Recovered results are for model", result$settings$model, "but current model is", current_model_num, "- not using them"))
          }
        }
      }, error = function(e) {
        dbg(paste("DEBUG: Error getting from outliers_analysis():", e$message))
      })
    }
  }
  
  # Final check: if we have results, verify they match current model
  if(!is.null(rv$analysis_results) && !is.null(current_model_num)) {
    stored_model <- if(!is.null(rv$analysis_results$settings) && !is.null(rv$analysis_results$settings$model)) {
      rv$analysis_results$settings$model
    } else {
      NULL
    }
    if(!is.null(stored_model) && stored_model != current_model_num) {
      dbg(paste("DEBUG: Final check - results model", stored_model, "doesn't match current", current_model_num, "- returning NULL"))
      return(NULL)
    }
  }
  
  rv$analysis_results
})

