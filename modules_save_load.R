# ============================================================================
# SAVE/LOAD ANALYSIS SETTINGS MODULE
# ============================================================================
# This module handles saving and loading analysis settings to/from JSON files.
# Source this file in gbPROCESS.R after defining the UI and server function.
#
# Usage in gbPROCESS.R:
#   source("modules_save_load.R", local = TRUE)
#
# Requirements:
#   - jsonlite package must be available (loaded via requireNamespace to avoid masking shiny::validate)
#   - Reactive values: rv$original_dataset, rv$is_clearing
#   - Session object: session
#   - All input objects (input$process_model, input$predictor_var, etc.)
# ============================================================================

# Check if jsonlite is available (without loading it to avoid masking shiny::validate)
if(!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required for save/load functionality. Please install it with: install.packages('jsonlite')")
}

# Save Analysis Settings Handler
output$save_settings <- downloadHandler(
  filename = function() {
    # Prevent execution if no dataset loaded
    req(rv$original_dataset)
    paste0("process_settings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
  },
  content = function(file) {
    # Prevent execution if no dataset loaded
    req(rv$original_dataset)
    # Collect all current input values
    settings <- list(
      # Metadata
      saved_date = as.character(Sys.time()),
      app_version = "1.0",
      
      # Model
      process_model = if(!is.null(input$process_model) && input$process_model != "") input$process_model else "",
      
      # Variables
      predictor_var = if(!is.null(input$predictor_var) && input$predictor_var != "") input$predictor_var else "",
      outcome_var = if(!is.null(input$outcome_var) && input$outcome_var != "") input$outcome_var else "",
      moderator_var = if(!is.null(input$moderator_var) && input$moderator_var != "") input$moderator_var else "",
      moderator2_var = if(!is.null(input$moderator2_var) && input$moderator2_var != "") input$moderator2_var else "",
      mediator_count = if(!is.null(input$mediator_count) && input$mediator_count != "") input$mediator_count else "",
      mediator_vars = sapply(1:10, function(i) {
        var_name <- paste0("mediator_m", i)
        if(!is.null(input[[var_name]]) && input[[var_name]] != "") {
          input[[var_name]]
        } else {
          ""
        }
      }),
      covariates = if(!is.null(input$covariates) && length(input$covariates) > 0) input$covariates else character(0),
      
      # Assumption Checks
      use_univariate_outlier_screen = if(!is.null(input$use_univariate_outlier_screen)) input$use_univariate_outlier_screen else TRUE,
      univariate_outlier_method = if(!is.null(input$univariate_outlier_method)) input$univariate_outlier_method else "iqr",
      univariate_iqr_multiplier = if(!is.null(input$univariate_iqr_multiplier)) input$univariate_iqr_multiplier else 1.5,
      univariate_mad_threshold = if(!is.null(input$univariate_mad_threshold)) input$univariate_mad_threshold else 3.5,
      residual_threshold = if(!is.null(input$residual_threshold)) input$residual_threshold else 2.0,
      cooks_threshold_type = if(!is.null(input$cooks_threshold_type)) input$cooks_threshold_type else "conservative",
      cooks_threshold_custom = if(!is.null(input$cooks_threshold_custom)) input$cooks_threshold_custom else 0.01,
      
      # PROCESS Options - Centering
      centering = if(!is.null(input$centering)) input$centering else "0",
      
      # PROCESS Options - Bootstrap
      use_bootstrap = if(!is.null(input$use_bootstrap)) input$use_bootstrap else TRUE,
      boot_samples = if(!is.null(input$boot_samples)) input$boot_samples else 5000,
      bootstrap_ci_method = if(!is.null(input$bootstrap_ci_method)) input$bootstrap_ci_method else "0",
      conf_level = if(!is.null(input$conf_level)) input$conf_level else 95,
      seed = if(!is.null(input$seed) && !is.na(input$seed)) input$seed else NA,
      
      # PROCESS Options - Advanced
      hc_method = if(!is.null(input$hc_method)) input$hc_method else "none",
      stand = if(!is.null(input$stand)) input$stand else FALSE,
      normal = if(!is.null(input$normal)) input$normal else FALSE,
      pairwise_contrasts = if(!is.null(input$pairwise_contrasts)) input$pairwise_contrasts else FALSE,
      
      # PROCESS Options - Output
      decimals = if(!is.null(input$decimals)) input$decimals else 4,
      describe = if(!is.null(input$describe)) input$describe else TRUE,
      covcoeff = if(!is.null(input$covcoeff)) input$covcoeff else FALSE,
      effsize = if(!is.null(input$effsize)) input$effsize else FALSE,
      listmiss = if(!is.null(input$listmiss)) input$listmiss else FALSE,
      ssquares = if(!is.null(input$ssquares)) input$ssquares else FALSE,
      modelres = if(!is.null(input$modelres)) input$modelres else FALSE,
      diagnose = if(!is.null(input$diagnose)) input$diagnose else FALSE,
      xmtest = if(!is.null(input$xmtest)) input$xmtest else FALSE,
      xmint = if(!is.null(input$xmint)) input$xmint else FALSE,
      total = if(!is.null(input$total)) input$total else FALSE,
      matrices = if(!is.null(input$matrices)) input$matrices else FALSE,
      covmy = if(!is.null(input$covmy)) input$covmy else FALSE,
      
      # PROCESS Options - Probing Moderation
      probe_interactions = if(!is.null(input$probe_interactions)) input$probe_interactions else TRUE,
      probe_threshold = if(!is.null(input$probe_threshold)) input$probe_threshold else "p < .10",
      conditioning_values = if(!is.null(input$conditioning_values)) input$conditioning_values else "1",
      show_jn_regions = if(!is.null(input$show_jn_regions)) input$show_jn_regions else TRUE,
      
      # Plot Options
      slopes_title = if(!is.null(input$slopes_title)) input$slopes_title else "Simple Slopes Plot",
      model3_plot_type = if(!is.null(input$model3_plot_type)) input$model3_plot_type else "conditional",
      use_color_lines = if(!is.null(input$use_color_lines)) input$use_color_lines else TRUE,
      custom_y_axis = if(!is.null(input$custom_y_axis)) input$custom_y_axis else FALSE,
      y_axis_min = if(!is.null(input$y_axis_min)) input$y_axis_min else 0,
      y_axis_max = if(!is.null(input$y_axis_max)) input$y_axis_max else 100,
      x_label = if(!is.null(input$x_label)) input$x_label else "",
      y_label = if(!is.null(input$y_label)) input$y_label else "",
      moderator_label = if(!is.null(input$moderator_label)) input$moderator_label else "",
      moderator2_label = if(!is.null(input$moderator2_label)) input$moderator2_label else "",
      decimal_places = if(!is.null(input$decimal_places)) input$decimal_places else 2,
      show_confidence_intervals = if(!is.null(input$show_confidence_intervals)) input$show_confidence_intervals else TRUE
    )
    
    # Convert to JSON and write
    jsonlite::write_json(settings, file, pretty = TRUE, auto_unbox = TRUE)
  },
  contentType = "application/json"
)

# Note: Save/load buttons are now conditionally rendered using conditionalPanel
# in modules_ui.R based on output$dataset_loaded reactive from modules_data_management.R
# This ensures buttons are completely hidden (not just disabled) when no dataset is loaded

# Load Analysis Settings Handler
observeEvent(input$load_settings_file, {
  # Prevent execution if no dataset loaded
  req(rv$original_dataset)
  req(input$load_settings_file)
  
  # Step 0: Validate file type
  ext <- tolower(tools::file_ext(input$load_settings_file$name))
  if(ext != "json") {
    showNotification("Please select a JSON file for loading settings.", type = "warning", duration = 5)
    return()
  }
  
  # Step 1: Validate dataset is loaded (redundant check since button is disabled, but good for safety)
  if(is.null(rv$original_dataset)) {
    showNotification("Please load a dataset first before loading settings.", type = "error", duration = 5)
    return()
  }
  
  # Step 2: Read and parse JSON
  tryCatch({
    settings <- jsonlite::fromJSON(input$load_settings_file$datapath)
    
    # Step 3: Validate variables exist in current dataset
    available_vars <- names(rv$original_dataset)
    vars_to_check <- c(
      settings$predictor_var,
      settings$outcome_var,
      if(settings$moderator_var != "") settings$moderator_var,
      if(settings$moderator2_var != "") settings$moderator2_var,
      if(length(settings$mediator_vars) > 0) settings$mediator_vars[settings$mediator_vars != ""],
      if(length(settings$covariates) > 0) settings$covariates
    )
    vars_to_check <- vars_to_check[vars_to_check != ""]
    
    missing_vars <- setdiff(vars_to_check, available_vars)
    if(length(missing_vars) > 0) {
      showNotification(
        paste0("Cannot load settings: The following variables are not in the current dataset: ", 
               paste(missing_vars, collapse = ", ")),
        type = "error",
        duration = 10
      )
      return()
    }
    
    # CRITICAL: Prevent stale outputs from prior runs when loading a new JSON.
    # Keep recovery suppressed until the user runs a new analysis.
    rv$suppress_results_recovery <- TRUE
    rv$analysis_results <- NULL
    rv$results_model <- NULL
    rv$validation_error <- NULL
    rv$full_result <- NULL
    rv$load_settings_pending <- FALSE
    rv$settings_to_load <- NULL
    rv$restore_mediators_pending <- FALSE
    rv$mediator_vars_to_restore <- NULL
    rv$expected_mediator_count <- NULL
    rv$mediator_restore_retry_count <- NULL
    rv$restore_labels_pending <- FALSE
    rv$labels_to_restore <- NULL
    rv$previous_predictor_var <- NULL
    rv$previous_outcome_var <- NULL
    rv$previous_moderator_var <- NULL
    rv$previous_moderator2_var <- NULL
    if(exists("reset_model_diagram_state", mode = "function", inherits = TRUE)) {
      get("reset_model_diagram_state", mode = "function", inherits = TRUE)()
    }
    dbg("DEBUG: JSON load started - hard-cleared analysis/diagram state and suppressed recovery")
    
    # Step 3.5: Ensure Variable Selection section is visible before restoration
    # This prevents hidden UI from delaying mediator_count/mediator inputs
    shinyjs::runjs("
      (function() {
        var details = document.getElementById('details_select_vars');
        if (details) { details.open = true; }
      })();
    ")
    
    # Step 4: Set model first (this triggers clearing)
    # Store settings first
    rv$settings_to_load <- settings
    
    if(!is.null(settings$process_model) && settings$process_model != "") {
      # CRITICAL: Set load_settings_pending BEFORE updating model
      # This allows the model change observer to skip clearing mediators
      rv$load_settings_pending <- TRUE
      
      # Set the model - this will trigger the model change observer which sets rv$is_clearing <- TRUE
      # The model change observer will see load_settings_pending and skip clearing mediators
      updateSelectInput(session, "process_model", selected = settings$process_model)
      
      # The restore observer will wait for is_clearing to become FALSE
      # The model change observer will set is_clearing to TRUE, then after 500ms it becomes FALSE
      dbg("DEBUG: Model set, settings marked as pending, waiting for clearing to complete")
    } else {
      # No model to set, restore immediately (no clearing needed)
      rv$load_settings_pending <- TRUE
    }
    
  }, error = function(e) {
    showNotification(paste0("Error loading settings file: ", e$message), type = "error", duration = 5)
  })
})

# Observer to restore settings after model clearing completes
# This observer waits for: 1) settings to be pending, 2) clearing to complete, 3) model to be set
observe({
  # CRITICAL: Skip if mediators are being restored - the mediator observer will handle that
  # This prevents the main observer from retriggering and clearing mediators
  if(isTRUE(rv$restore_mediators_pending)) {
    return()
  }
  
  # Only proceed if we have pending settings to load AND clearing is complete
  if(isTRUE(rv$load_settings_pending) && !isTRUE(rv$is_clearing) && !is.null(rv$settings_to_load)) {
    settings <- rv$settings_to_load
    
    # CRITICAL: If settings include a model, we MUST wait until:
    # 1. The model is actually set in the UI
    # 2. The model change observer has run (which sets is_clearing to TRUE, then FALSE after 500ms)
    # So if model is in settings, verify it's set AND that we've waited for clearing
    if(!is.null(settings$process_model) && settings$process_model != "") {
      current_model <- if(is.null(input$process_model) || input$process_model == "") NULL else input$process_model
      
      if(is.null(current_model) || current_model != settings$process_model) {
        # Model not set yet or doesn't match - wait
        # This prevents restoring variables before model change completes
        invalidateLater(100, session)
        return()
      }
      
      # Model is set, but we need to ensure clearing has completed
      # The is_clearing flag should be FALSE now, but add extra delay to be safe
      # This ensures variable_selectors renderUI has completed
      invalidateLater(400, session)
    } else {
      # No model to set, just add a small delay for UI updates
      invalidateLater(200, session)
    }
    
    isolate({
      # Double-check all conditions after delay
      if(isTRUE(rv$load_settings_pending) && !isTRUE(rv$is_clearing) && !is.null(rv$settings_to_load)) {
        settings <- rv$settings_to_load
        
        # Final check: if model was in settings, verify it's still set
        if(!is.null(settings$process_model) && settings$process_model != "") {
          current_model <- if(is.null(input$process_model) || input$process_model == "") NULL else input$process_model
          if(is.null(current_model) || current_model != settings$process_model) {
            # Model doesn't match, abort restoration
            dbg("DEBUG: Model mismatch, aborting restoration")
            return()
          }
        }
        settings <- rv$settings_to_load
        vars <- names(rv$original_dataset)
        
        dbg("DEBUG: Restoring variables from saved settings")
        dbg(paste("DEBUG: predictor_var to restore:", settings$predictor_var))
        dbg(paste("DEBUG: outcome_var to restore:", settings$outcome_var))
        dbg(paste("DEBUG: moderator_var to restore:", settings$moderator_var))
        dbg(paste("DEBUG: moderator2_var to restore:", settings$moderator2_var))
        
        # Hard-clear variable selections first so same-model JSON loads do not retain stale values.
        updateSelectInput(session, "predictor_var",
                         choices = c("Select variable" = "", vars),
                         selected = "")
        updateSelectInput(session, "outcome_var",
                         choices = c("Select variable" = "", vars),
                         selected = "")
        updateSelectInput(session, "moderator_var",
                         choices = c("Select variable" = "", vars),
                         selected = "")
        updateSelectInput(session, "moderator2_var",
                         choices = c("Select variable" = "", vars),
                         selected = "")
        updateSelectInput(session, "covariates", choices = vars, selected = character(0))
        updateSelectInput(session, "mediator_count", selected = "")
        for(i in 1:10) {
          updateSelectInput(session, paste0("mediator_m", i),
                           choices = c("Select variable" = "", vars),
                           selected = "")
        }

        # Restore variables (in correct order)
        if(!is.null(settings$predictor_var) && settings$predictor_var != "" && settings$predictor_var %in% vars) {
          updateSelectInput(session, "predictor_var", 
                           choices = c("Select variable" = "", vars), 
                           selected = settings$predictor_var)
          # CRITICAL: Set previous_predictor_var to the restored value so auto-label observer
          # doesn't think the variable changed and overwrite JSON labels
          rv$previous_predictor_var <- settings$predictor_var
          dbg(paste("DEBUG: Restored predictor_var:", settings$predictor_var))
        }
        
        if(!is.null(settings$outcome_var) && settings$outcome_var != "" && settings$outcome_var %in% vars) {
          updateSelectInput(session, "outcome_var", 
                           choices = c("Select variable" = "", vars), 
                           selected = settings$outcome_var)
          # CRITICAL: Set previous_outcome_var to the restored value
          rv$previous_outcome_var <- settings$outcome_var
          dbg(paste("DEBUG: Restored outcome_var:", settings$outcome_var))
        }
        
        if(!is.null(settings$moderator_var) && settings$moderator_var != "" && settings$moderator_var %in% vars) {
          updateSelectInput(session, "moderator_var", 
                           choices = c("Select variable" = "", vars), 
                           selected = settings$moderator_var)
          # CRITICAL: Set previous_moderator_var to the restored value
          rv$previous_moderator_var <- settings$moderator_var
          dbg(paste("DEBUG: Restored moderator_var:", settings$moderator_var))
        }
        
        if(!is.null(settings$moderator2_var) && settings$moderator2_var != "" && settings$moderator2_var %in% vars) {
          updateSelectInput(session, "moderator2_var", 
                           choices = c("Select variable" = "", vars), 
                           selected = settings$moderator2_var)
          # CRITICAL: Set previous_moderator2_var to the restored value
          rv$previous_moderator2_var <- settings$moderator2_var
          dbg(paste("DEBUG: Restored moderator2_var:", settings$moderator2_var))
        } else {
          # If no moderator2_var in settings, clear it
          rv$previous_moderator2_var <- NULL
        }
        
        # Restore mediator count FIRST, then individual mediators
        if(!is.null(settings$mediator_count) && settings$mediator_count != "" && 
           !is.null(settings$mediator_vars) && length(settings$mediator_vars) > 0) {
          # CRITICAL: Set restore flags BEFORE updating mediator_count
          # This allows the mediator_count observer to skip clearing if we're restoring
          rv$mediator_vars_to_restore <- settings$mediator_vars
          rv$restore_mediators_pending <- TRUE
          # Store the expected mediator count so restore observer knows what to expect
          # This also triggers mediator_list_ui to re-render with the correct selected value
          rv$expected_mediator_count <- if(!is.null(settings$mediator_count) && settings$mediator_count != "" && !is.na(as.numeric(settings$mediator_count))) {
            as.integer(settings$mediator_count)
          } else {
            sum(settings$mediator_vars != "" & !is.na(settings$mediator_vars))
          }
          dbg(paste("DEBUG: Set restore flags, about to restore mediator_count:", settings$mediator_count))
          dbg(paste("DEBUG: Expected mediator count:", rv$expected_mediator_count))
          dbg(paste("DEBUG: Mediator vars to restore:", paste(settings$mediator_vars[settings$mediator_vars != ""], collapse=", ")))
          
          # CRITICAL: Set cooldown BEFORE setting restore flags to prevent observer from clearing
          rv$mediator_restore_cooldown <- TRUE
          rv$mediator_restore_cooldown_time <- Sys.time()
          dbg("DEBUG: Cooldown set BEFORE setting restore flags to prevent observer from clearing")
          
          # Try to update mediator_count immediately (renderUI should also set it)
          # If it fails, the mediator restoration observer will retry
          tryCatch({
            updateSelectInput(session, "mediator_count", selected = as.character(settings$mediator_count))
            dbg(paste("DEBUG: Attempted to update mediator_count to:", settings$mediator_count))
          }, error = function(e) {
            dbg(paste("DEBUG: mediator_count update failed (will retry):", e$message))
            # The mediator restoration observer will retry after delay
          })
        }
        
        if(!is.null(settings$covariates) && length(settings$covariates) > 0) {
          # Filter covariates to only those that exist in current dataset
          valid_covariates <- settings$covariates[settings$covariates %in% vars]
          if(length(valid_covariates) > 0) {
            updateSelectInput(session, "covariates", choices = vars, selected = valid_covariates)
            dbg(paste("DEBUG: Restored covariates:", paste(valid_covariates, collapse=", ")))
          }
        } else {
          updateSelectInput(session, "covariates", choices = vars, selected = character(0))
        }
        
        # Restore Assumption Checks
    if(!is.null(settings$use_univariate_outlier_screen)) {
      updateCheckboxInput(session, "use_univariate_outlier_screen", value = settings$use_univariate_outlier_screen)
    }
    if(!is.null(settings$univariate_outlier_method)) {
      updateSelectInput(session, "univariate_outlier_method", selected = settings$univariate_outlier_method)
    }
    if(!is.null(settings$univariate_iqr_multiplier)) {
      updateNumericInput(session, "univariate_iqr_multiplier", value = settings$univariate_iqr_multiplier)
    }
    if(!is.null(settings$univariate_mad_threshold)) {
      updateNumericInput(session, "univariate_mad_threshold", value = settings$univariate_mad_threshold)
    }
    if(!is.null(settings$residual_threshold)) {
      updateNumericInput(session, "residual_threshold", value = settings$residual_threshold)
    }
    if(!is.null(settings$cooks_threshold_type)) {
      updateRadioButtons(session, "cooks_threshold_type", selected = settings$cooks_threshold_type)
    }
    if(!is.null(settings$cooks_threshold_custom)) {
      updateNumericInput(session, "cooks_threshold_custom", value = settings$cooks_threshold_custom)
    }
    
    # Restore PROCESS Options - Centering
    if(!is.null(settings$centering)) {
      updateRadioButtons(session, "centering", selected = settings$centering)
    }
    
    # Restore PROCESS Options - Bootstrap
    if(!is.null(settings$use_bootstrap)) {
      updateCheckboxInput(session, "use_bootstrap", value = settings$use_bootstrap)
    }
    if(!is.null(settings$boot_samples)) {
      updateNumericInput(session, "boot_samples", value = settings$boot_samples)
    }
    if(!is.null(settings$bootstrap_ci_method)) {
      updateRadioButtons(session, "bootstrap_ci_method", selected = settings$bootstrap_ci_method)
    }
    if(!is.null(settings$conf_level)) {
      updateNumericInput(session, "conf_level", value = settings$conf_level)
    }
    if(!is.null(settings$seed) && !is.na(settings$seed)) {
      updateNumericInput(session, "seed", value = settings$seed)
    } else {
      updateNumericInput(session, "seed", value = NA)
    }
    
    # Restore PROCESS Options - Advanced
    if(!is.null(settings$hc_method)) {
      updateSelectInput(session, "hc_method", selected = settings$hc_method)
    }
    if(!is.null(settings$stand)) {
      updateCheckboxInput(session, "stand", value = settings$stand)
    }
    if(!is.null(settings$normal)) {
      updateCheckboxInput(session, "normal", value = settings$normal)
    }
    if(!is.null(settings$pairwise_contrasts)) {
      updateCheckboxInput(session, "pairwise_contrasts", value = settings$pairwise_contrasts)
    }
    
    # Restore PROCESS Options - Output
    if(!is.null(settings$decimals)) {
      updateNumericInput(session, "decimals", value = settings$decimals)
    }
    if(!is.null(settings$describe)) {
      updateCheckboxInput(session, "describe", value = settings$describe)
    }
    if(!is.null(settings$covcoeff)) {
      updateCheckboxInput(session, "covcoeff", value = settings$covcoeff)
    }
    if(!is.null(settings$effsize)) {
      updateCheckboxInput(session, "effsize", value = settings$effsize)
    }
    if(!is.null(settings$listmiss)) {
      updateCheckboxInput(session, "listmiss", value = settings$listmiss)
    }
    if(!is.null(settings$ssquares)) {
      updateCheckboxInput(session, "ssquares", value = settings$ssquares)
    }
    if(!is.null(settings$modelres)) {
      updateCheckboxInput(session, "modelres", value = settings$modelres)
    }
    if(!is.null(settings$diagnose)) {
      updateCheckboxInput(session, "diagnose", value = settings$diagnose)
    }
    if(!is.null(settings$xmtest)) {
      updateCheckboxInput(session, "xmtest", value = settings$xmtest)
    }
    if(!is.null(settings$xmint)) {
      updateCheckboxInput(session, "xmint", value = settings$xmint)
    }
    if(!is.null(settings$total)) {
      updateCheckboxInput(session, "total", value = settings$total)
    }
    if(!is.null(settings$matrices)) {
      updateCheckboxInput(session, "matrices", value = settings$matrices)
    }
    if(!is.null(settings$covmy)) {
      updateCheckboxInput(session, "covmy", value = settings$covmy)
    }
    
    # Restore PROCESS Options - Probing Moderation
    if(!is.null(settings$probe_interactions)) {
      updateCheckboxInput(session, "probe_interactions", value = settings$probe_interactions)
    }
    if(!is.null(settings$probe_threshold)) {
      updateTextInput(session, "probe_threshold", value = settings$probe_threshold)
    }
    if(!is.null(settings$conditioning_values)) {
      updateRadioButtons(session, "conditioning_values", selected = settings$conditioning_values)
    }
    if(!is.null(settings$show_jn_regions)) {
      updateCheckboxInput(session, "show_jn_regions", value = settings$show_jn_regions)
    }
    # Legacy support: if old JSON has 'jn' instead of 'show_jn_regions', convert it
    if(is.null(settings$show_jn_regions) && !is.null(settings$jn)) {
      updateCheckboxInput(session, "show_jn_regions", value = settings$jn)
    }
    
    # Restore Plot Options
    if(!is.null(settings$slopes_title)) {
      updateTextInput(session, "slopes_title", value = settings$slopes_title)
    }
    if(!is.null(settings$model3_plot_type)) {
      updateRadioButtons(session, "model3_plot_type", selected = settings$model3_plot_type)
    }
    if(!is.null(settings$use_color_lines)) {
      updateCheckboxInput(session, "use_color_lines", value = settings$use_color_lines)
    }
    if(!is.null(settings$custom_y_axis)) {
      updateCheckboxInput(session, "custom_y_axis", value = settings$custom_y_axis)
    }
    if(!is.null(settings$y_axis_min)) {
      updateNumericInput(session, "y_axis_min", value = settings$y_axis_min)
    }
    if(!is.null(settings$y_axis_max)) {
      updateNumericInput(session, "y_axis_max", value = settings$y_axis_max)
    }
    if(!is.null(settings$decimal_places)) {
      updateNumericInput(session, "decimal_places", value = settings$decimal_places)
    }
    if(!is.null(settings$show_confidence_intervals)) {
      updateCheckboxInput(session, "show_confidence_intervals", value = settings$show_confidence_intervals)
    }
    
    # Store labels to restore later (after variables are set and auto-label observer has run)
    # We'll restore them in a separate observer with a delay
    dbg("DEBUG: ===== STORING LABELS FROM JSON FOR LATER RESTORATION =====")
    dbg(paste("DEBUG: JSON x_label:", if(!is.null(settings$x_label)) settings$x_label else "NULL"))
    dbg(paste("DEBUG: JSON y_label:", if(!is.null(settings$y_label)) settings$y_label else "NULL"))
    dbg(paste("DEBUG: JSON moderator_label:", if(!is.null(settings$moderator_label)) settings$moderator_label else "NULL"))
    dbg(paste("DEBUG: JSON moderator2_label:", if(!is.null(settings$moderator2_label)) settings$moderator2_label else "NULL"))
    
    rv$labels_to_restore <- list(
      x_label = if(!is.null(settings$x_label)) settings$x_label else NULL,
      y_label = if(!is.null(settings$y_label)) settings$y_label else NULL,
      moderator_label = if(!is.null(settings$moderator_label)) settings$moderator_label else NULL,
      moderator2_label = if(!is.null(settings$moderator2_label)) settings$moderator2_label else NULL
    )
    rv$restore_labels_pending <- TRUE
    dbg("DEBUG: Labels stored, restore_labels_pending set to TRUE")
      }
    })
  }
})

# Event-driven mediator restoration (restore only after mediator_count is set)
observeEvent(input$mediator_count, {
  if(!isTRUE(rv$restore_mediators_pending) ||
     is.null(rv$mediator_vars_to_restore) ||
     is.null(rv$original_dataset)) {
    return()
  }

  expected_count <- sum(rv$mediator_vars_to_restore != "" & !is.na(rv$mediator_vars_to_restore))
  current_count <- if(!is.null(input$mediator_count) && input$mediator_count != "" &&
                      !is.na(as.numeric(input$mediator_count))) {
    as.integer(input$mediator_count)
  } else {
    0
  }

  if(expected_count == 0 || current_count != expected_count) {
    return()
  }

  vars <- names(rv$original_dataset)
  restored_count <- 0

  for(i in 1:min(expected_count, length(rv$mediator_vars_to_restore))) {
    if(rv$mediator_vars_to_restore[i] != "" && rv$mediator_vars_to_restore[i] %in% vars) {
      var_name <- paste0("mediator_m", i)
      updateSelectInput(session, var_name,
                        choices = c("Select variable" = "", vars),
                        selected = rv$mediator_vars_to_restore[i])
      restored_count <- restored_count + 1
    }
  }

  if(restored_count == expected_count) {
    rv$restore_mediators_pending <- FALSE
    rv$mediator_vars_to_restore <- NULL
    rv$expected_mediator_count <- NULL
    rv$mediator_restore_retry_count <- NULL
    rv$load_settings_pending <- FALSE
    rv$settings_to_load <- NULL

    showNotification("Analysis settings loaded successfully!", type = "default", duration = 3)
    dbg("DEBUG: Settings restoration completed (including mediators and labels)")
  }
}, ignoreInit = TRUE)

# Observer to clear cooldown after the delay period
observe({
  if(isTRUE(rv$mediator_restore_cooldown) && !is.null(rv$mediator_restore_cooldown_time)) {
    # Check if 10 seconds have passed
    elapsed <- as.numeric(difftime(Sys.time(), rv$mediator_restore_cooldown_time, units = "secs"))
    if(elapsed >= 10) {
      rv$mediator_restore_cooldown <- FALSE
      rv$mediator_restore_cooldown_time <- NULL
      dbg("DEBUG: Cooldown period ended (10 seconds elapsed) - mediator_count observer can now clear if needed")
    } else {
      # Check again in 1 second
      invalidateLater(1000, session)
    }
  }
})

# Separate observer to restore plot labels after variables are restored
# This ensures labels are restored AFTER the auto-label observer has had a chance to run
# (but it will skip because rv$load_settings_pending is TRUE)
observe({
  if(isTRUE(rv$restore_labels_pending) && !is.null(rv$labels_to_restore)) {
    # Wait for variables to be set and auto-label observer to see load_settings_pending flag
    # Use a longer delay to ensure auto-label observer has run and been skipped
    invalidateLater(600, session)
    
    isolate({
      if(isTRUE(rv$restore_labels_pending) && !is.null(rv$labels_to_restore)) {
        labels <- rv$labels_to_restore
        
        dbg("DEBUG: ===== RESTORING PLOT LABELS FROM JSON =====")
        dbg(paste("DEBUG: JSON x_label value:", labels$x_label))
        dbg(paste("DEBUG: JSON y_label value:", labels$y_label))
        dbg(paste("DEBUG: JSON moderator_label value:", labels$moderator_label))
        dbg(paste("DEBUG: Current x_label before restore:", input$x_label))
        dbg(paste("DEBUG: Current y_label before restore:", input$y_label))
        dbg(paste("DEBUG: Current moderator_label before restore:", input$moderator_label))
        dbg(paste("DEBUG: load_settings_pending:", rv$load_settings_pending))
        dbg(paste("DEBUG: restore_labels_pending:", rv$restore_labels_pending))
        
        if(!is.null(labels$x_label)) {
          updateTextInput(session, "x_label", value = labels$x_label)
          dbg(paste("DEBUG: Restored x_label to:", labels$x_label))
        }
        if(!is.null(labels$y_label)) {
          updateTextInput(session, "y_label", value = labels$y_label)
          dbg(paste("DEBUG: Restored y_label to:", labels$y_label))
        }
        if(!is.null(labels$moderator_label)) {
          updateTextInput(session, "moderator_label", value = labels$moderator_label)
          dbg(paste("DEBUG: Restored moderator_label to:", labels$moderator_label))
        }
        if(!is.null(labels$moderator2_label)) {
          updateTextInput(session, "moderator2_label", value = labels$moderator2_label)
          dbg(paste("DEBUG: Restored moderator2_label to:", labels$moderator2_label))
        }
        
        # Clear restore_labels_pending flag first
        rv$restore_labels_pending <- FALSE
        rv$labels_to_restore <- NULL
        dbg("DEBUG: Labels restoration flags cleared")
        dbg(paste("DEBUG: Final x_label value:", input$x_label))
        dbg(paste("DEBUG: Final y_label value:", input$y_label))
        dbg(paste("DEBUG: Final moderator_label value:", input$moderator_label))
        
        # Wait a bit more before clearing load_settings_pending to ensure labels are fully set
        # and auto-label observer doesn't run immediately
        # At this point, rv$previous_*_var should be set to restored variables,
        # so when auto-label observer runs, it will see predictor_changed = FALSE
        invalidateLater(300, session)
        isolate({
          # CRITICAL: Don't clear load_settings_pending here if model requires mediators!
          # The mediator restoration observer will clear it after mediators are restored.
          # If model doesn't require mediators, clear it now.
          current_model <- if(!is.null(input$process_model) && input$process_model != "") {
            as.numeric(input$process_model)
          } else {
            NULL
          }
          
          # If mediator restoration is not pending, safe to clear now.
          if(!isTRUE(rv$restore_mediators_pending)) {
            rv$load_settings_pending <- FALSE
            rv$settings_to_load <- NULL
            dbg("DEBUG: load_settings_pending cleared - mediator restoration not pending")
            showNotification("Analysis settings loaded successfully!", type = "default", duration = 3)
            dbg("DEBUG: Settings restoration completed (including labels)")
          } else {
            # Mediators are still being restored - wait for mediator restoration observer.
            dbg("DEBUG: Waiting for mediators to restore before clearing load_settings_pending")
            # Don't show notification yet - mediator observer will show it when done
          }
        })
      }
    })
  }
})

