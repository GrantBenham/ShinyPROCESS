# ============================================================================
# DATA MANAGEMENT MODULE
# ============================================================================
# This module contains data upload, variable selection, and state management
# Extracted from gbPROCESS.R as part of Stage 3 modularization
#
# Contents:
# - File upload handling (CSV, SAV)
# - Variable selection observers
# - Model change detection and clearing logic
# - Mediator UI dynamic generation
# - Variable validation
# - UI conditional output reactives (is_moderation_model, is_plot_model, etc.)
# ============================================================================

  # Helper: fetch model spec row for a model number
  get_model_spec <- function(model_num) {
    if(is.null(model_num) || is.na(model_num)) {
      return(NULL)
    }
    if(!exists("process_model_specs", inherits = TRUE)) {
      return(NULL)
    }
    spec_tbl <- get("process_model_specs", inherits = TRUE)
    spec <- spec_tbl[spec_tbl$model == model_num, , drop = FALSE]
    if(nrow(spec) == 1) spec else NULL
  }

  # Helpers: model capability lookups (spec-driven with fallback behavior)
  model_has_m <- function(model_num) {
    spec <- get_model_spec(model_num)
    if(!is.null(spec)) return(isTRUE(spec$has_m))
    !is.null(model_num) && model_num >= 4 && model_num <= 92
  }

  model_requires_w <- function(model_num) {
    spec <- get_model_spec(model_num)
    if(!is.null(spec)) return(isTRUE(spec$requires_w_input))
    !is.null(model_num) && model_num %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74, 83:92)
  }

  model_requires_z <- function(model_num) {
    spec <- get_model_spec(model_num)
    if(!is.null(spec)) return(isTRUE(spec$requires_z_input))
    !is.null(model_num) && model_num %in% c(2, 3)
  }

  model_supports_plot <- function(model_num) {
    spec <- get_model_spec(model_num)
    if(!is.null(spec)) return(isTRUE(spec$supports_plot))
    !is.null(model_num) && model_num %in% c(1, 3)
  }

  model_max_mediators <- function(model_num) {
    spec <- get_model_spec(model_num)
    if(!is.null(spec) && !is.na(spec$max_mediators) && spec$max_mediators > 0) {
      return(as.integer(spec$max_mediators))
    }
    if(is.null(model_num)) return(0L)
    if(model_num == 4) return(10L)
    if(model_num == 6) return(6L)
    if(model_num == 82) return(4L)
    if(model_num >= 83 && model_num <= 92) return(2L)
    if(model_num >= 4 && model_num <= 92) return(10L)
    0L
  }

  model_min_mediators <- function(model_num) {
    spec <- get_model_spec(model_num)
    if(!is.null(spec) && !is.na(spec$min_mediators) && spec$min_mediators > 0) {
      return(as.integer(spec$min_mediators))
    }
    if(is.null(model_num)) return(0L)
    if(model_num >= 4 && model_num <= 92) return(1L)
    0L
  }

  # Update dataset handling
  observeEvent(input$data_file, {
    req(input$data_file)
    ext <- tolower(tools::file_ext(input$data_file$datapath))
    
    # Only process CSV or SAV files - ignore other file types
    if (!ext %in% c("csv", "sav")) {
      showNotification("Please select a CSV or SAV file for data upload.", type = "warning", duration = 5)
      return()
    }
    
    if (ext == "sav") {
      data <- read_sav(input$data_file$datapath)
    } else if (ext == "csv") {
      data <- read.csv(input$data_file$datapath)
    } else {
      showNotification("Invalid file type. Please upload a CSV or SAV file.", type = "error", duration = 5)
      return()
    }
    
    # Set clearing flag to prevent observers from repopulating and validation from running
    isolate({
      rv$is_clearing <- TRUE
      rv$mediator_order <- NULL
    })
    
    rv$original_dataset <- data
    rv$current_dataset <- data
    dbg(paste("DEBUG - Dataset loaded with", nrow(data), "rows"))
    rv$analysis_results <- NULL  # Reset analysis results when new file is loaded
    rv$validation_error <- NULL
    
    # Clear all variable selections
    updateSelectInput(session, "predictor_var", selected = "")
    updateSelectInput(session, "outcome_var", selected = "")
    updateSelectInput(session, "moderator_var", selected = "")
    updateSelectInput(session, "moderator2_var", selected = "")
    updateSelectInput(session, "mediator_vars", selected = character(0))
    updateSelectInput(session, "covariates", selected = NULL)
    
    # The is_clearing flag will be cleared by the separate observer after UI updates complete
    dbg("DEBUG - Cleared all variable selections after dataset upload")
  })
  
  # Clear analysis results and all variables when model changes
  observeEvent(input$process_model, {
    dbg("DEBUG: ===== MODEL CHANGE OBSERVER STARTED =====")
    dbg(paste("DEBUG: New model number:", input$process_model))
    dbg(paste("DEBUG: Previous model number:", rv$previous_model))
    
    # DEBUG: Print current values BEFORE clearing (removed; timing was misleading)
    
    # Set clearing flag to prevent observers from repopulating and validation from running
    # Clear analysis results immediately so old results don't show when model changes
    isolate({
      rv$is_clearing <- TRUE
      rv$mediator_order <- NULL
      rv$analysis_results <- NULL  # Clear analysis results immediately on model change
      rv$results_model <- NULL  # Clear the model number associated with results
      rv$validation_error <- NULL
      rv$previous_model <- input$process_model  # Store current model as previous for next change
    })

    # If settings are being loaded, skip clearing only for the expected JSON model-set event.
    # If user manually changes model while a load is pending, cancel restore and clear normally.
    is_json_model_set_event <- FALSE
    if(isTRUE(rv$load_settings_pending) && !is.null(rv$settings_to_load) &&
       !is.null(rv$settings_to_load$process_model) && rv$settings_to_load$process_model != "") {
      is_json_model_set_event <- identical(
        as.character(input$process_model),
        as.character(rv$settings_to_load$process_model)
      )
    }
    if(is_json_model_set_event) {
      dbg("DEBUG: Model change clearing skipped - JSON restore model-set event")
      return()
    }
    if(isTRUE(rv$load_settings_pending) && !is_json_model_set_event) {
      dbg("DEBUG: Manual model change detected while load pending - cancelling pending restore")
      rv$load_settings_pending <- FALSE
      rv$settings_to_load <- NULL
      rv$restore_mediators_pending <- FALSE
      rv$mediator_vars_to_restore <- NULL
      rv$expected_mediator_count <- NULL
      rv$mediator_restore_retry_count <- NULL
      rv$restore_labels_pending <- FALSE
      rv$labels_to_restore <- NULL
    }
    
    dbg("DEBUG - Model changed, resetting all variables to initial state")
    
    # Clear all reactive values first
    isolate({
      rv$mediator_order <- NULL
      rv$validation_error <- NULL
    })
    
    # CRITICAL: clear all UI selections, then repeat once after flush to catch late/dynamic inputs.
    if(!is.null(rv$original_dataset)) {
      vars <- names(rv$original_dataset)
      clear_model_inputs <- function() {
        updateSelectInput(session, "predictor_var", choices = c("Select variable" = "", vars), selected = "")
        updateSelectInput(session, "outcome_var", choices = c("Select variable" = "", vars), selected = "")
        updateSelectInput(session, "moderator_var", choices = c("Select variable" = "", vars), selected = "")
        updateSelectInput(session, "moderator2_var", choices = c("Select variable" = "", vars), selected = "")
        updateSelectInput(session, "covariates", choices = vars, selected = character(0))
        updateSelectInput(session, "mediator_count", selected = "")
        for(i in 1:10) {
          updateSelectInput(session, paste0("mediator_m", i), choices = c("Select variable" = "", vars), selected = "")
        }
      }
      clear_model_inputs()
      session$onFlushed(function() {
        clear_model_inputs()
        dbg("DEBUG: Re-applied model-change clear after flush")
      }, once = TRUE)
      
      # Clear plot labels when model changes (they will be auto-populated when new variables are selected)
      # Only clear if we're NOT loading settings (settings loading will restore labels after model is set)
      dbg(paste("DEBUG: load_settings_pending is:", rv$load_settings_pending))
      dbg(paste("DEBUG: Clearing plot labels - x_label was:", input$x_label))
      dbg(paste("DEBUG: Clearing plot labels - y_label was:", input$y_label))
      dbg(paste("DEBUG: Clearing plot labels - moderator_label was:", input$moderator_label))
      # Clear labels immediately
      updateTextInput(session, "x_label", value = "")
      updateTextInput(session, "y_label", value = "")
      updateTextInput(session, "moderator_label", value = "")
      updateTextInput(session, "moderator2_label", value = "")
      # Reset previous variable values so labels will be auto-populated when new variables are selected
      rv$previous_predictor_var <- NULL
      rv$previous_outcome_var <- NULL
      rv$previous_moderator_var <- NULL
      rv$previous_moderator2_var <- NULL
      dbg("DEBUG: Plot labels cleared (will be auto-populated when variables are selected)")
      
      dbg("DEBUG: All variable inputs cleared via updateSelectInput (all inputs exist in DOM)")
    }
    
    # Analysis results already cleared above in the first isolate block
    # This ensures old results don't persist when model changes
    
    dbg("DEBUG - All clearing methods attempted")
    dbg("DEBUG: ===== MODEL CHANGE OBSERVER COMPLETED =====")
  })
  
  # Note: Model 74 is not user-selectable - it is automatically created from Model 4
  # when "Allow X by M interaction" (xmint) is enabled. PROCESS handles W=X internally.
  
  # Separate observer to clear the is_clearing flag after UI updates complete
  # This runs independently and checks if we're still clearing
  # Wait for UI updates to complete, then clear the flag
  # NOTE: We don't wait for inputs to be empty - they will have values once user selects them
  # We just need to wait long enough for updateSelectInput calls to propagate
  observe({
    if(isTRUE(rv$is_clearing)) {
      # Wait for UI updates to complete, then clear the flag
      # updateSelectInput is asynchronous, so we need to wait for it to propagate
      invalidateLater(500, session)  # Wait 500ms for UI updates to complete
      isolate({
        rv$is_clearing <- FALSE
        dbg("DEBUG - Cleared is_clearing flag")
      })
    }
  }, priority = -1)  # Low priority to run after other observers
  
  # Real-time validation: Check for duplicate variables as user selects them
  # Use debounce to prevent rapid firing when user is typing/deleting quickly
  validation_trigger <- reactive({
    list(
      predictor = input$predictor_var,
      outcome = input$outcome_var,
      moderator = input$moderator_var,
      moderator2 = input$moderator2_var,
      mediators = mediator_vars_collected(),
      covariates = input$covariates
    )
  })
  
  validation_trigger_debounced <- debounce(validation_trigger, millis = 300)
  
  observeEvent(validation_trigger_debounced(), {
    # Skip validation if we're in the middle of clearing (model change)
    if(isTRUE(rv$is_clearing)) {
      dbg("DEBUG: Real-time validation skipped - is_clearing is TRUE")
      return()
    }
    
    # Get current model to determine which inputs are actually in use
    current_model <- if(!is.null(input$process_model) && input$process_model != "") {
      as.numeric(input$process_model)
    } else {
      NULL
    }
    
    # DEBUG: Print all current input values
    dbg("DEBUG: ===== Real-time validation check =====")
    dbg(paste("DEBUG: Current model:", if(is.null(current_model)) "NULL" else current_model))
    dbg(paste("DEBUG: predictor_var:", if(is.null(input$predictor_var) || input$predictor_var == "") "EMPTY" else input$predictor_var))
    dbg(paste("DEBUG: outcome_var:", if(is.null(input$outcome_var) || input$outcome_var == "") "EMPTY" else input$outcome_var))
    dbg(paste("DEBUG: moderator_var:", if(is.null(input$moderator_var) || input$moderator_var == "") "EMPTY" else input$moderator_var))
    dbg(paste("DEBUG: moderator2_var:", if(is.null(input$moderator2_var) || input$moderator2_var == "") "EMPTY" else input$moderator2_var))
    mediator_vars_current <- mediator_vars_collected()
    dbg(paste("DEBUG: mediator_count:", if(is.null(input$mediator_count) || input$mediator_count == "") "EMPTY" else input$mediator_count))
    dbg(paste("DEBUG: mediator_vars_collected:", if(is.null(mediator_vars_current) || length(mediator_vars_current) == 0) "EMPTY" else paste(mediator_vars_current, collapse=", ")))
    dbg(paste("DEBUG: covariates:", if(is.null(input$covariates) || length(input$covariates) == 0) "EMPTY" else paste(input$covariates, collapse=", ")))
    
    # Collect all selected variables - but only check enabled inputs
    # Disabled inputs can't be changed by user, so we only validate enabled ones
    
    all_vars <- character(0)
    
    # Always include predictor and outcome (all models use these)
    if(!is.null(input$predictor_var) && input$predictor_var != "") {
      all_vars <- c(all_vars, input$predictor_var)
    }
    if(!is.null(input$outcome_var) && input$outcome_var != "") {
      all_vars <- c(all_vars, input$outcome_var)
    }
    
    # Only include moderator_var if current model requires W
    if(!is.null(current_model) && model_requires_w(current_model)) {
      if(!is.null(input$moderator_var) && input$moderator_var != "") {
        all_vars <- c(all_vars, input$moderator_var)
      }
    }
    
    # Only include moderator2_var if current model requires Z
    if(!is.null(current_model) && model_requires_z(current_model)) {
      if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
        all_vars <- c(all_vars, input$moderator2_var)
      }
    }
    
    # Only include mediators if current model has mediators
    if(!is.null(current_model) && model_has_m(current_model)) {
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        all_vars <- c(all_vars, mediator_vars_current)
      }
    }
    
    # Always include covariates (all models can use them)
    if(!is.null(input$covariates) && length(input$covariates) > 0) {
      all_vars <- c(all_vars, input$covariates)
    }
    
    dbg(paste("DEBUG: All collected variables (only enabled inputs):", paste(all_vars, collapse=", ")))
    dbg(paste("DEBUG: Unique variables:", paste(unique(all_vars), collapse=", ")))
    dbg(paste("DEBUG: Length all_vars:", length(all_vars), "Length unique:", length(unique(all_vars))))
    
    # Check for duplicates (with exception for Model 74 where X and W can be the same)
    if(length(all_vars) > 0 && length(all_vars) != length(unique(all_vars))) {
      duplicate_vars <- all_vars[duplicated(all_vars)]
      
      # Exception: For Model 74, allow predictor_var and moderator_var to be the same
      if(!is.null(current_model) && current_model == 74) {
        # Remove the X=W duplicate from the check if it's the only duplicate
        if(length(duplicate_vars) == 1 && 
           !is.null(input$predictor_var) && input$predictor_var != "" &&
           !is.null(input$moderator_var) && input$moderator_var != "" &&
           input$predictor_var == input$moderator_var &&
           duplicate_vars[1] == input$predictor_var) {
          # This is the expected X=W for Model 74, so clear any error
          dbg("DEBUG: Model 74 - X=W is allowed, clearing duplicate error")
          rv$validation_error <- NULL
        } else {
          # There are other duplicates beyond X=W
          dbg(paste("DEBUG: DUPLICATES FOUND:", paste(unique(duplicate_vars), collapse=", ")))
          rv$validation_error <- paste0("Error: The same variable cannot be used for multiple roles. Variable(s) '", 
                                        paste(unique(duplicate_vars), collapse = "', '"), 
                                        "' is/are used in more than one role.")
          showNotification(
            rv$validation_error,
            type = "error",
            duration = 10
          )
        }
      } else {
        # Not Model 74, so duplicates are not allowed
        dbg(paste("DEBUG: DUPLICATES FOUND:", paste(unique(duplicate_vars), collapse=", ")))
        rv$validation_error <- paste0("Error: The same variable cannot be used for multiple roles. Variable(s) '", 
                                      paste(unique(duplicate_vars), collapse = "', '"), 
                                      "' is/are used in more than one role.")
        showNotification(
          rv$validation_error,
          type = "error",
          duration = 10
        )
      }
    } else {
      # Clear validation error if no duplicates
      dbg("DEBUG: No duplicates found - clearing validation error")
      rv$validation_error <- NULL
    }
    dbg("DEBUG: ===== End real-time validation check =====")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Clear analysis results when key variables change (to prevent stale data)
  observeEvent(c(input$outcome_var, input$predictor_var, input$moderator_var, input$moderator2_var), {
    # Only clear if we have existing results (to avoid clearing on initial load)
    if(!is.null(rv$analysis_results)) {
      rv$analysis_results <- NULL
      dbg("DEBUG - Key variables changed, clearing analysis results")
    }
  })
  
  # Model description removed - users should refer to Hayes' book for model diagrams
  
  # Determine if model is moderation or mediation type
  output$is_moderation_model <- reactive({
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    model_requires_w(model_num) || model_requires_z(model_num)
  })
  outputOptions(output, "is_moderation_model", suspendWhenHidden = FALSE)
  
  output$is_model3 <- reactive({
    req(input$process_model)
    as.numeric(input$process_model) == 3
  })
  outputOptions(output, "is_model3", suspendWhenHidden = FALSE)
  
  # Output to track if model has second moderator (Z)
  output$has_second_moderator <- reactive({
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    model_requires_z(model_num)
  })
  outputOptions(output, "has_second_moderator", suspendWhenHidden = FALSE)
  
  # Output to track if model supports plots
  output$is_plot_model <- reactive({
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    model_supports_plot(model_num)
  })
  outputOptions(output, "is_plot_model", suspendWhenHidden = FALSE)
  
  output$is_mediation_model <- reactive({
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    model_has_m(model_num)
  })
  outputOptions(output, "is_mediation_model", suspendWhenHidden = FALSE)
  
  # Count of mediators for conditional display (now based on mediator_count input)
  output$mediator_count <- reactive({
    if(is.null(input$mediator_count) || input$mediator_count == "" || is.na(as.numeric(input$mediator_count))) {
      0
    } else {
      as.integer(input$mediator_count)
    }
  })
  outputOptions(output, "mediator_count", suspendWhenHidden = FALSE)
  
  # NEW APPROACH: Collect mediators from individual M1, M2, M3... selects
  # This reactive collects all mediator inputs into a vector (replaces input$mediator_vars)
  mediator_vars_collected <- reactive({
    # Check if dataset and model are available
    if(is.null(rv$original_dataset) || is.null(input$process_model) || input$process_model == "") {
      return(NULL)
    }
    
    model_num <- as.numeric(input$process_model)
    if(!model_has_m(model_num)) {
      return(NULL)
    }
    
    # Get the number of mediators user wants
    # mediator_count is now a character string from selectInput
    mediator_count <- if(!is.null(input$mediator_count) && input$mediator_count != "" && !is.na(as.numeric(input$mediator_count)) && as.numeric(input$mediator_count) > 0) {
      as.integer(input$mediator_count)
    } else {
      0
    }
    
    if(mediator_count == 0) {
      return(NULL)
    }
    
    # Collect M1, M2, M3... in order (only non-empty values)
    mediators <- character(0)
    for(i in 1:mediator_count) {
      input_name <- paste0("mediator_m", i)
      if(!is.null(input[[input_name]]) && input[[input_name]] != "") {
        mediators <- c(mediators, input[[input_name]])
      }
    }
    
    if(length(mediators) > 0) {
      return(mediators)
    } else {
      return(NULL)
    }
  })
  
  # Mediator list UI with dropdown for count + individual M1, M2, M3... selects
  # Using selectInput (dropdown) instead of numericInput to avoid infinite loop issues
  output$mediator_list_ui <- renderUI({
    # Make this reactive to restore flags so it re-renders when restore starts
    # Access restore flags to make them dependencies (even if we don't use the variables)
    restore_pending <- rv$restore_mediators_pending
    expected_count <- rv$expected_mediator_count
    
    # Check if dataset and model are available
    if(is.null(rv$original_dataset) || is.null(input$process_model) || input$process_model == "") {
      return(NULL)
    }
    
    vars <- names(rv$original_dataset)
    model_num <- as.numeric(input$process_model)
    mediator_enabled <- model_has_m(model_num)
    
    if(!mediator_enabled) {
      return(NULL)
    }
    
    # Determine max mediators based on canonical model specs
    max_mediators <- model_max_mediators(model_num)
    
    min_mediators <- model_min_mediators(model_num)
    if(min_mediators < 1) min_mediators <- 1L
    if(max_mediators < min_mediators) max_mediators <- min_mediators
    allowed_counts <- as.character(min_mediators:max_mediators)
    
    # Create choices for dropdown from allowed range
    count_choices <- c("Select number..." = "", allowed_counts)
    names(count_choices)[2:length(count_choices)] <- allowed_counts
    
    # Get current count for determining how many M1, M2, M3... selects to show
    # During restore, use expected_mediator_count if available, otherwise use input$mediator_count
    current_count <- 0
    if(isTRUE(rv$restore_mediators_pending) && !is.null(rv$expected_mediator_count) && rv$expected_mediator_count > 0) {
      restore_count <- as.integer(rv$expected_mediator_count)
      if(!is.na(restore_count) && restore_count >= min_mediators && restore_count <= max_mediators) {
        current_count <- restore_count
      }
    } else if(!is.null(input$mediator_count) && input$mediator_count != "" && !is.na(as.numeric(input$mediator_count)) && as.numeric(input$mediator_count) > 0) {
      input_count <- as.integer(input$mediator_count)
      if(!is.na(input_count) && input_count >= min_mediators && input_count <= max_mediators) {
        current_count <- input_count
      }
    }
    
    # Determine selected value for mediator_count dropdown
    # During restore, use expected_mediator_count, otherwise use input$mediator_count
    selected_count <- if(current_count > 0) as.character(current_count) else ""
    
    dbg(paste("DEBUG: mediator_list_ui renderUI - restore_pending:", restore_pending, "expected_count:", expected_count, "selected_count:", selected_count))
    
    tagList(
      # Number of mediators dropdown
      selectInput("mediator_count", 
                  "Number of Mediators:", 
                  choices = count_choices,
                  selected = selected_count),
      
      # Dynamic M1, M2, M3... selects
      if(current_count > 0) {
        lapply(1:current_count, function(i) {
          # Get current value for this mediator slot (if any)
          current_val <- if(!is.null(input[[paste0("mediator_m", i)]]) && input[[paste0("mediator_m", i)]] != "") {
            input[[paste0("mediator_m", i)]]
          } else {
            ""
          }
          
          selectInput(paste0("mediator_m", i),
                     paste0("M", i, ":"),
                     choices = c("Select variable" = "", vars),
                     selected = current_val)
        })
      }
    )
  })
  outputOptions(output, "mediator_list_ui", suspendWhenHidden = FALSE)
  
  # Output reactive to indicate if dataset is loaded (for UI conditional rendering)
  output$dataset_loaded <- reactive({
    !is.null(rv$original_dataset)
  })
  outputOptions(output, "dataset_loaded", suspendWhenHidden = FALSE)
  
  # Observer to clear all mediator inputs when mediator_count changes
  # This ensures a clean slate when user changes the number of mediators
  observeEvent(input$mediator_count, {
    # CRITICAL: Check cooldown FIRST before any other checks
    # This prevents clearing mediators immediately after they're restored
    if(isTRUE(rv$mediator_restore_cooldown)) {
      dbg("DEBUG: Mediator count changed during cooldown period - skipping clear to prevent loop")
      return()
    }
    
    # Skip if we're in the middle of clearing (model change)
    if(isTRUE(rv$is_clearing)) {
      return()
    }
    
    # Skip if we're restoring mediators from saved settings
    # The restore observer will handle setting the mediator values after UI regenerates
    if(isTRUE(rv$restore_mediators_pending)) {
      dbg("DEBUG: Mediator count changed during restore - skipping clear, restore observer will handle values")
      return()
    }
    
    # Skip if dataset not available
    if(is.null(rv$original_dataset)) {
      return()
    }
    
    vars <- names(rv$original_dataset)
    
    # Get the new count
    new_count <- if(!is.null(input$mediator_count) && input$mediator_count != "" && !is.na(as.numeric(input$mediator_count))) {
      as.integer(input$mediator_count)
    } else {
      0
    }
    
    # Clear all mediator inputs (up to max of 10)
    # This ensures that if user changes from 3 to 2, M3 gets cleared
    # And if they change from 2 to 3, M3 starts fresh
    for(i in 1:10) {
      updateSelectInput(session, paste0("mediator_m", i), 
                        choices = c("Select variable" = "", vars),
                        selected = "")
    }
    
    dbg(paste("DEBUG: Mediator count changed to", new_count, "- all mediator inputs cleared"))
  }, ignoreInit = TRUE)  # ignoreInit = TRUE prevents clearing on initial load
  
  # Dynamically generate variable selectors based on model
  output$variable_selectors <- renderUI({
    # Debug output
    dbg(paste("DEBUG: variable_selectors renderUI called"))
    dbg(paste("DEBUG: rv$original_dataset is NULL?", is.null(rv$original_dataset)))
    dbg(paste("DEBUG: input$process_model:", input$process_model))
    
    # Check if dataset and model are available
    if(is.null(rv$original_dataset) || is.null(input$process_model) || input$process_model == "") {
      dbg("DEBUG: variable_selectors - dataset or model not available, returning NULL")
      return(tags$p("Please load a dataset and select a model number first."))
    }
    
    vars <- names(rv$original_dataset)
    model_num <- as.numeric(input$process_model)
    dbg(paste("DEBUG: variable_selectors - rendering for model", model_num))
    
    # Determine which inputs should be enabled/disabled for this model
    moderator_enabled <- model_requires_w(model_num)
    moderator2_enabled <- model_requires_z(model_num)
    mediator_enabled <- model_has_m(model_num)
    
    # ALWAYS render ALL inputs, but disable the ones not relevant to current model
    # This ensures updateSelectInput always works because all inputs exist in DOM
    selectors <- tagList(
      selectInput("predictor_var", "Predictor Variable (X)", 
                 choices = c("Select variable" = "", vars), 
                 selected = ""),
      selectInput("outcome_var", "Outcome Variable (Y)", 
                 choices = c("Select variable" = "", vars), 
                 selected = ""),
      div(id = "moderator_var_wrapper", style = if(!moderator_enabled) "opacity: 0.6;" else "",
        selectInput("moderator_var", "Moderator Variable (W)", 
                   choices = c("Select variable" = "", vars), 
                   selected = "")
      ),
      div(id = "moderator2_var_wrapper", style = if(!moderator2_enabled) "opacity: 0.6;" else "",
        selectInput("moderator2_var", "Second Moderator Variable (Z)", 
                   choices = c("Select variable" = "", vars), 
                   selected = "")
      ),
      uiOutput("mediator_list_ui"),
      selectInput("covariates", "Covariates (optional)", vars, multiple = TRUE),
      # Use JavaScript to properly disable/enable inputs based on model
      tags$script(HTML(paste0("
        (function() {
          function updateInputStates() {
            ", if(!moderator_enabled) "
            var modVar = document.getElementById('moderator_var');
            if (modVar) {
              modVar.disabled = true;
              modVar.style.pointerEvents = 'none';
              modVar.style.cursor = 'not-allowed';
            }
            " else "
            var modVar = document.getElementById('moderator_var');
            if (modVar) {
              modVar.disabled = false;
              modVar.style.pointerEvents = 'auto';
              modVar.style.cursor = 'pointer';
            }
            ", "
            ", if(!moderator2_enabled) "
            var mod2Var = document.getElementById('moderator2_var');
            if (mod2Var) {
              mod2Var.disabled = true;
              mod2Var.style.pointerEvents = 'none';
              mod2Var.style.cursor = 'not-allowed';
            }
            " else "
            var mod2Var = document.getElementById('moderator2_var');
            if (mod2Var) {
              mod2Var.disabled = false;
              mod2Var.style.pointerEvents = 'auto';
              mod2Var.style.cursor = 'pointer';
            }
            ", "
          }
          // Run immediately
          updateInputStates();
          // Also run after delays to catch any re-rendering
          setTimeout(updateInputStates, 100);
          setTimeout(updateInputStates, 500);
          // Listen for Shiny events
          $(document).on('shiny:connected', updateInputStates);
          $(document).on('shiny:value', updateInputStates);
        })();
      ")))
    )
    
    # Add help text based on model
    if(!moderator_enabled && !mediator_enabled) {
      selectors <- tagList(selectors,
        p(em("Note: This model does not use moderators or mediators."))
      )
    } else if(!moderator_enabled) {
      selectors <- tagList(selectors,
        p(em("Note: Moderator inputs are disabled for this model."))
      )
    } else if(!mediator_enabled) {
      selectors <- tagList(selectors,
        p(em("Note: Mediator inputs are disabled for this model."))
      )
    }
    
    selectors
  })
  outputOptions(output, "variable_selectors", suspendWhenHidden = FALSE)
  
  # Output to track if outcome is continuous
  output$outcome_is_continuous <- reactive({
    req(rv$original_dataset, input$outcome_var)
    !is_binary_variable(rv$original_dataset, input$outcome_var)
  })
  outputOptions(output, "outcome_is_continuous", suspendWhenHidden = FALSE)
  
  # Output to track if outcome is selected
  output$outcome_is_selected <- reactive({
    !is.null(input$outcome_var) && input$outcome_var != ""
  })
  outputOptions(output, "outcome_is_selected", suspendWhenHidden = FALSE)
  
  # Output to track if all required variables are selected for assumption checks
  output$all_vars_selected_for_assumptions <- reactive({
    if(is.null(input$process_model) || input$process_model == "" || 
       is.null(rv$original_dataset) || is.null(input$outcome_var) || is.null(input$predictor_var)) {
      return(FALSE)
    }
    model_num <- as.numeric(input$process_model)
    mediator_vars_current <- mediator_vars_collected()
    validation <- check_required_vars_for_assumptions(
      model_num, input$predictor_var, input$outcome_var,
      input$moderator_var, input$moderator2_var, mediator_vars_current
    )
    validation$valid
  })
  outputOptions(output, "all_vars_selected_for_assumptions", suspendWhenHidden = FALSE)
  
  # Output to track if any continuous variables are selected
  output$has_continuous_selected <- reactive({
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    selected_vars <- c(input$outcome_var, input$predictor_var)
    mediator_vars_current <- mediator_vars_collected()
    if(!is.null(mediator_vars_current)) selected_vars <- c(selected_vars, mediator_vars_current)
    if(!is.null(input$moderator_var)) selected_vars <- c(selected_vars, input$moderator_var)
    if(!is.null(input$moderator2_var)) selected_vars <- c(selected_vars, input$moderator2_var)
    any(vapply(selected_vars, function(v) is_continuous_variable(rv$original_dataset, v), logical(1)))
  })
  outputOptions(output, "has_continuous_selected", suspendWhenHidden = FALSE)
  
  # Output to track if any continuous covariates are selected
  output$has_continuous_covariates <- reactive({
    req(rv$original_dataset)
    if(is.null(input$covariates) || length(input$covariates) == 0) return(FALSE)
    any(vapply(input$covariates, function(v) is_continuous_variable(rv$original_dataset, v), logical(1)))
  })
  outputOptions(output, "has_continuous_covariates", suspendWhenHidden = FALSE)
  
  # Output to track if analysis results exist
  output$analysis_ready <- reactive({
    result <- !is.null(rv$analysis_results)
    dbg(paste("DEBUG: analysis_ready reactive called. Result:", result))
    result
  })
  outputOptions(output, "analysis_ready", suspendWhenHidden = FALSE)
