# Integrated PROCESS V5 Shiny Application
# Combines moderation and mediation analyses using Hayes PROCESS V5
# Based on previous gbMod (app.R) and gbMed (gbMed.R) applications

# Load necessary libraries
library(shiny)
library(bslib)
library(ggplot2)
library(stringr)
library(dplyr)
library(shinyjs)
library(car)  # For VIF and ncvTest functions
library(haven)  # For SPSS file support
# Note: For sortable mediator lists, we'll use a custom implementation
# Note: jsonlite is used via fully qualified names (jsonlite::) in modules_save_load.R

# Increase file upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)

# Debug controls (set to TRUE to enable verbose console output)
DEBUG_MODE <- FALSE
dbg <- function(...) {
  if (isTRUE(DEBUG_MODE)) {
    print(...)
  }
}

# Check PROCESS file availability/version from the first N lines.
check_process_file <- function(path = "process.R", expected_version = "5.0", scan_lines = 80L) {
  if(!file.exists(path)) {
    return(list(
      ready = FALSE,
      status = "missing",
      detected_version = NULL,
      message = "PROCESS file not found."
    ))
  }
  
  header_lines <- tryCatch({
    readLines(path, n = scan_lines, warn = FALSE)
  }, error = function(e) {
    character(0)
  })
  
  if(length(header_lines) == 0) {
    return(list(
      ready = FALSE,
      status = "unreadable",
      detected_version = NULL,
      message = "PROCESS file exists but could not be read."
    ))
  }
  
  # Look for lines like "PROCESS for R version 5.0"
  version_line_idx <- grep("PROCESS\\s+for\\s+R\\s+version", header_lines, ignore.case = TRUE)
  detected_version <- NULL
  if(length(version_line_idx) > 0) {
    first_match <- header_lines[version_line_idx[1]]
    version_match <- regmatches(first_match, regexpr("[0-9]+\\.[0-9]+", first_match))
    if(length(version_match) > 0 && !is.na(version_match[1])) {
      detected_version <- version_match[1]
    }
  }
  
  if(is.null(detected_version)) {
    return(list(
      ready = FALSE,
      status = "unknown_version",
      detected_version = NULL,
      message = "Could not confirm PROCESS for R version from the file header."
    ))
  }
  
  if(!identical(detected_version, expected_version)) {
    return(list(
      ready = FALSE,
      status = "wrong_version",
      detected_version = detected_version,
      message = paste0("Detected PROCESS for R version ", detected_version, ", expected version ", expected_version, ".")
    ))
  }
  
  list(
    ready = TRUE,
    status = "ok",
    detected_version = detected_version,
    message = paste0("PROCESS for R version ", detected_version, " detected.")
  )
}

# Runtime mode detection via runtime.txt in repo root.
# Defaults to "rshiny" if missing/unreadable/invalid.
get_runtime <- function(path = "runtime.txt") {
  runtime_value <- tryCatch({
    if(!file.exists(path)) {
      return("rshiny")
    }
    lines <- readLines(path, n = 1L, warn = FALSE)
    if(length(lines) == 0) {
      return("rshiny")
    }
    normalized <- tolower(trimws(lines[[1]]))
    sub("^\ufeff", "", normalized)
  }, error = function(e) {
    "rshiny"
  })
  
  if(!runtime_value %in% c("rshiny", "shinylive")) {
    return("rshiny")
  }
  runtime_value
}

# Source assumption checks module (helper functions)
source("modules_assumptions.R", local = TRUE)

# Source canonical model specs (available to UI and server modules)
source("model_specs.R", local = TRUE)

# Source UI module
source("modules_ui.R", local = TRUE)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  server_env <- environment()
  
  # Reactive values for data management
  rv <- reactiveValues(
    original_dataset = NULL,
    current_dataset = NULL,
    outliers_info = NULL,
    analysis_results = NULL,
    results_model = NULL,  # Track which model number was used to generate current results
    current_model = NULL,
    is_clearing = FALSE,  # Flag to prevent observers from repopulating during clearing
    # mediator_order removed - using mediator_vars_collected() reactive instead
    validation_error = NULL,
    previous_model = NULL,  # Track previous model to detect model changes
    mediator_ui_initialized = FALSE,  # Track if mediator selectInput has been initialized
    mediator_count_last_model = NULL,  # Track last model to detect when to reset mediator_count
    # Add these for load settings:
    load_settings_pending = FALSE,
    settings_to_load = NULL,
    restore_mediators_pending = FALSE,
    mediator_vars_to_restore = NULL,
    expected_mediator_count = NULL,  # Store expected count during restore
    mediator_restore_retry_count = NULL,  # Track retry count to prevent infinite loops
    restore_labels_pending = FALSE,
    labels_to_restore = NULL,
    # Track previous variable values to detect when variables change
    previous_predictor_var = NULL,
    previous_outcome_var = NULL,
    previous_moderator_var = NULL,
    previous_moderator2_var = NULL,
    # Cooldown period after mediator restoration to prevent observer from clearing
    mediator_restore_cooldown = FALSE,
    mediator_restore_cooldown_time = NULL,
    process_ready = FALSE,
    process_status = NULL,
    process_detected_version = NULL,
    process_source = NULL,
    app_runtime = "rshiny",
    # When TRUE, analysis_results() must not recover cached eventReactive output.
    # This is used during JSON settings loads to prevent stale results from reappearing.
    suppress_results_recovery = FALSE
  )
  
  # Detect runtime mode from runtime.txt
  app_runtime <- get_runtime(path = "runtime.txt")
  rv$app_runtime <- app_runtime
  
  # Validate and load PROCESS function (runtime-aware).
  # - rshiny: keep existing root-file auto-load behavior
  # - shinylive: rely on session upload fallback
  if(identical(app_runtime, "rshiny")) {
    process_check <- check_process_file(path = "process.R", expected_version = "5.0", scan_lines = 80L)
    process_ready_flag <- isTRUE(process_check$ready)
    rv$process_ready <- process_ready_flag
    rv$process_status <- process_check$status
    rv$process_detected_version <- process_check$detected_version
    rv$process_source <- if(process_ready_flag) "root" else NULL
    
    if(process_ready_flag) {
      source("process.R", local = server_env)
    } else {
      dbg(paste("DEBUG: PROCESS check failed:", process_check$message))
    }
  } else {
    rv$process_ready <- FALSE
    rv$process_status <- "missing"
    rv$process_detected_version <- NULL
    rv$process_source <- NULL
    dbg("DEBUG: Runtime is shinylive; waiting for session upload of process.R")
  }

  # Load save/load settings module
  source("modules_save_load.R", local = TRUE)
  
  # Load data management module
  source("modules_data_management.R", local = TRUE)

  # Load assumption outputs module
  source("modules_assumption_outputs.R", local = TRUE)
  
  # Load analysis module
  source("modules_analysis.R", local = TRUE)
  
  # Load results module
  source("modules_results.R", local = TRUE)
  
  # Load model diagrams parser scaffold module (no UI wiring yet)
  source("modules_model_diagrams.R", local = TRUE)

  # Helper for Model-Has-Z checks used by plot/label observers in this file.
  # Uses canonical specs when available, with a minimal fallback.
  has_second_moderator_model <- function(model_num) {
    if(is.null(model_num) || is.na(model_num)) {
      return(FALSE)
    }
    if(exists("model_requires_z", inherits = TRUE)) {
      return(isTRUE(model_requires_z(model_num)))
    }
    if(exists("process_model_specs", inherits = TRUE)) {
      spec_tbl <- get("process_model_specs", inherits = TRUE)
      spec <- spec_tbl[spec_tbl$model == model_num, , drop = FALSE]
      if(nrow(spec) == 1) {
        return(isTRUE(spec$requires_z_input))
      }
    }
    model_num %in% c(2, 3)
  }

  # Helper for plot-support checks in this file (spec-driven with fallback).
  supports_plot_model <- function(model_num) {
    if(is.null(model_num) || is.na(model_num)) {
      return(FALSE)
    }
    if(exists("model_supports_plot", inherits = TRUE)) {
      return(isTRUE(model_supports_plot(model_num)))
    }
    if(exists("process_model_specs", inherits = TRUE)) {
      spec_tbl <- get("process_model_specs", inherits = TRUE)
      spec <- spec_tbl[spec_tbl$model == as.integer(model_num), , drop = FALSE]
      if(nrow(spec) == 1) {
        return(isTRUE(spec$supports_plot))
      }
    }
    model_num %in% c(1, 3)
  }
  
  # DEBUG: Observer to track button clicks
  observeEvent(input$run_analysis, {
    dbg("DEBUG: Run Analysis button clicked!")
    dbg(paste("DEBUG: Button click count:", input$run_analysis))
  })

  # Expose PROCESS readiness to UI and show warning content when needed
  output$process_ready <- reactive({
    isTRUE(rv$process_ready)
  })
  outputOptions(output, "process_ready", suspendWhenHidden = FALSE)
  
  output$process_status_line <- renderUI({
    runtime_label <- if(identical(rv$app_runtime, "shinylive")) "Shinylive" else "R Shiny"
    readiness_label <- if(isTRUE(rv$process_ready)) "Ready" else "Not ready"
    source_label <- if(isTRUE(rv$process_ready) && !is.null(rv$process_source)) {
      paste0("Loaded from: ", rv$process_source)
    } else {
      "Loaded from: not loaded"
    }
    version_label <- if(!is.null(rv$process_detected_version)) {
      paste0("Detected version: ", rv$process_detected_version)
    } else {
      "Detected version: unknown"
    }
    
    div(
      style = "margin-top: 6px; margin-bottom: 12px; padding: 8px; background: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; font-size: 12px; color: #333;",
      HTML(paste0(
        "<strong>PROCESS status</strong><br/>",
        "Runtime: ", runtime_label, " | ",
        "State: ", readiness_label, "<br/>",
        source_label, "<br/>",
        version_label
      ))
    )
  })
  
  output$process_warning <- renderUI({
    if(isTRUE(rv$process_ready)) {
      return(NULL)
    }
    
    runtime_instruction <- if(identical(rv$app_runtime, "shinylive")) {
      HTML(paste0(
        "<strong>Shinylive runtime:</strong> Upload <code>process.R</code> each time you launch the app. ",
        "Placing it in the app root is not applicable in browser runtime."
      ))
    } else {
      HTML(paste0(
        "<strong>R Shiny runtime:</strong> Permanent fix: place <code>process.R</code> in the app folder ",
        "(same folder as <code>gbPROCESS.R</code>)."
      ))
    }
    
    div(
      style = "padding:16px; border:2px solid #b71c1c; background:#ffebee; color:#7f0000; border-radius:8px; margin-bottom:12px;",
      h4(style = "margin-top:0;", "PROCESS for R v5.0 Required"),
      p(
        style = "margin-bottom:8px;",
        HTML("ShinyPROCESS requires <strong>process.R (PROCESS for R version 5.0)</strong>. "),
        if(!is.null(rv$process_detected_version)) {
          paste0("Detected version: ", rv$process_detected_version, ". ")
        } else {
          NULL
        },
        HTML("Version is read from the header text in <code>process.R</code>. You may still browse inputs, but analysis execution is blocked until a valid file is available.")
      ),
      p(style = "margin-bottom:8px;", runtime_instruction),
      p(
        style = "margin-bottom:4px;",
        "Download PROCESS for R from: ",
        tags$a(
          href = "https://haskayne.ucalgary.ca/CCRAM/resource-hub",
          target = "_blank",
          "https://haskayne.ucalgary.ca/CCRAM/resource-hub"
        )
      ),
      p(
        style = "margin-bottom:8px;",
        "More information: ",
        tags$a(
          href = "https://processmacro.org/index.html",
          target = "_blank",
          "https://processmacro.org/index.html"
        )
      ),
      fileInput(
        "process_upload",
        "Upload process.R (session only)",
        accept = c(".R")
      )
    )
  })
  
  observeEvent(input$process_upload, {
    upload_info <- input$process_upload
    if(is.null(upload_info) || is.null(upload_info$datapath) || !nzchar(upload_info$datapath)) {
      return(invisible(NULL))
    }
    
    process_check <- check_process_file(path = upload_info$datapath, expected_version = "5.0", scan_lines = 80L)
    
    if(isTRUE(process_check$ready)) {
      source(upload_info$datapath, local = server_env)
      rv$process_ready <- TRUE
      rv$process_status <- "ok"
      rv$process_detected_version <- process_check$detected_version
      rv$process_source <- "upload"
      showNotification("PROCESS for R v5.0 loaded from uploaded file for this session.", type = "message", duration = 7)
    } else {
      rv$process_ready <- FALSE
      rv$process_status <- process_check$status
      rv$process_detected_version <- process_check$detected_version
      rv$process_source <- NULL
      showNotification(
        paste0(
          "Uploaded file is not a valid PROCESS for R v5.0 file. ",
          if(!is.null(process_check$message)) process_check$message else ""
        ),
        type = "error",
        duration = 10
      )
    }
  })
  
  # Disable run controls unless PROCESS v5.0 is ready
  observe({
    if(isTRUE(rv$process_ready)) {
      shinyjs::enable("run_analysis")
      shinyjs::enable("run_analysis_no_outliers")
    } else {
      shinyjs::disable("run_analysis")
      shinyjs::disable("run_analysis_no_outliers")
    }
  })
  
  observeEvent(input$run_analysis_no_outliers, {
    dbg("DEBUG: Run Analysis (No Outliers) button clicked!")
    dbg(paste("DEBUG: Button click count:", input$run_analysis_no_outliers))
  })
  
  # ============================================================================
  # PLOTS MODULE - Moderation Visualizations
  # ============================================================================
  
  # ============================================================================
  # PLOT GENERATION HELPER FUNCTIONS (Stage 1 Refactoring)
  # ============================================================================
  
  # ============================================================================
  # PLOTS MODULE - Moderation Visualizations
  # ============================================================================
  
  # ============================================================================
  # PLOT GENERATION HELPER FUNCTIONS (Stage 1 Refactoring)
  # ============================================================================
  
  # Helper function to validate plot requirements (Stage 5 Refactoring)
  # Consolidates validation logic used across all plot generation functions
  validate_plot_requirements <- function(results, input, required_model_num, check_moderator2 = FALSE) {
    # Check if results exist
    if(is.null(results) || is.null(results$settings)) {
      return(FALSE)
    }
    
    # Check model number
    model_num <- as.numeric(input$process_model)
    if(model_num != required_model_num) {
      return(FALSE)
    }
    
    # Check if model matches
    if(results$settings$model != model_num) {
      return(FALSE)
    }
    
    # Check if variables match
    if(results$settings$predictor_var != input$predictor_var ||
       results$settings$outcome_var != input$outcome_var ||
       results$settings$moderator_var != input$moderator_var) {
      return(FALSE)
    }
    
    # Check second moderator if required (for Model 3)
    if(check_moderator2) {
      if(!is.null(results$settings$moderator2_var) && results$settings$moderator2_var != input$moderator2_var) {
        return(FALSE)
      }
    }
    
    return(TRUE)
  }
  
  generate_jn_plot <- function(results, input, jn_available_func) {
    # Validation checks
    if(!validate_plot_requirements(results, input, required_model_num = 1)) {
      return(NULL)
    }
    
    if(!jn_available_func()) {
      return(NULL)
    }
    
    tryCatch({
      process_output <- results$output
      start_idx <- which(grepl("Conditional effect of focal predictor", process_output))
      
      if(length(start_idx) == 0) {
        return(NULL)
      }
      
      # Find the data section - skip header line
      data_start <- start_idx + 2  # Skip "Conditional effect..." and header line
      
      # Find the end of the data section (blank line or next section)
      end_idx <- which(grepl("^\\s*$|^Data for visualizing|^----------", process_output[data_start:length(process_output)]))[1]
      if(!is.na(end_idx)) {
        end_idx <- end_idx + data_start - 1
      } else {
        # If no clear end found, look for next major section
        end_idx <- which(grepl("^-----------|^\\*+", process_output[data_start:length(process_output)]))[1]
        if(!is.na(end_idx)) {
          end_idx <- end_idx + data_start - 1
        } else {
          end_idx <- min(data_start + 50, length(process_output))  # Safety limit
        }
      }
      
      if(is.na(end_idx) || end_idx <= data_start) {
        return(NULL)
      }
      
      # Extract data lines and filter out header/non-data lines
      data_lines <- process_output[data_start:end_idx]
      
      # Filter lines to only include those that look like data rows (start with number or negative number)
      data_lines <- data_lines[grepl("^\\s*-?\\d", data_lines)]
      
      if(length(data_lines) == 0) {
        return(NULL)
      }
      
      # Parse the data with better error handling
      jn_data <- tryCatch({
        # Use fill=TRUE to handle lines with missing columns
        parsed <- read.table(text = paste(data_lines, collapse = "\n"),
                            col.names = c("Moderator", "Effect", "se", "t", "p", "LLCI", "ULCI"),
                            stringsAsFactors = FALSE,
                            fill = TRUE,
                            blank.lines.skip = TRUE)
        
        # Filter to only rows with exactly 7 columns (all numeric)
        valid_rows <- apply(parsed, 1, function(row) {
          # Check if all 7 columns are numeric (or can be coerced to numeric)
          all(!is.na(suppressWarnings(as.numeric(row[1:7]))))
        })
        
        if(sum(valid_rows) == 0) {
          stop("No valid data rows with 7 numeric columns found")
        }
        
        parsed[valid_rows, 1:7]
      }, error = function(e) {
        print(paste("Error parsing JN data:", e$message))
        # Try alternative parsing method
        tryCatch({
          # Split each line and extract numeric values
          parsed_lines <- lapply(data_lines, function(line) {
            parts <- strsplit(trimws(line), "\\s+")[[1]]
            # Extract first 7 numeric values
            nums <- suppressWarnings(as.numeric(parts))
            nums <- nums[!is.na(nums)]
            if(length(nums) >= 7) {
              return(nums[1:7])
            }
            return(NULL)
          })
          
          # Filter out NULLs and convert to data frame
          valid_data <- do.call(rbind, Filter(Negate(is.null), parsed_lines))
          if(is.null(valid_data) || nrow(valid_data) == 0) {
            stop("Could not parse any valid data rows")
          }
          
          colnames(valid_data) <- c("Moderator", "Effect", "se", "t", "p", "LLCI", "ULCI")
          as.data.frame(valid_data)
        }, error = function(e2) {
          print(paste("Alternative parsing also failed:", e2$message))
          stop(e2)
        })
      })
      
      jn_data$significant <- jn_data$p < 0.05
      
      # Check if there's an actual transition (both significant and non-significant regions exist)
      has_transition <- any(jn_data$significant) && any(!jn_data$significant)
      
      # Only calculate transition point if there's an actual transition
      transition_point <- NULL
      if(has_transition) {
        transition_point <- jn_data$Moderator[which.min(abs(jn_data$p - 0.05))]
      }
      
      x_label_text <- if(input$moderator_label != "") input$moderator_label else input$moderator_var
      y_label_text <- if(input$x_label != "") paste("Effect of", input$x_label) else paste("Effect of", input$predictor_var)
      
      # Extract probe threshold value for title
      probe_threshold_text <- "p < .10"  # default
      if(!is.null(input$probe_threshold) && input$probe_threshold != "") {
        probe_threshold_text <- input$probe_threshold
      }
      
      # Ensure data is sorted by Moderator for proper ribbon rendering
      # This prevents overlapping ribbons that might appear as the same color
      jn_data <- jn_data[order(jn_data$Moderator), ]
      
      p <- ggplot(jn_data, aes(x = Moderator, y = Effect)) +
        geom_ribbon(aes(ymin = LLCI, ymax = ULCI, fill = !significant), alpha = 0.4) +
        scale_fill_manual(values = if(input$use_color_lines) 
                          c(`TRUE` = "pink", `FALSE` = "lightblue") else 
                          c(`TRUE` = "grey70", `FALSE` = "grey50"),
                        labels = c(`TRUE` = "n.s.", `FALSE` = "p < .05"),
                        name = "",
                        drop = FALSE) +
        geom_line(linewidth = 1, 
                 color = if(input$use_color_lines) "blue" else "black") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        theme_minimal() +
        labs(title = "Johnson–Neyman Plot",
             x = x_label_text,
             y = y_label_text) +
        theme(
          text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5)
        )
      
      # Only add transition line if there's an actual transition
      if(has_transition && !is.null(transition_point)) {
        p <- p + geom_vline(xintercept = transition_point,
                            linetype = "dashed", 
                            color = if(input$use_color_lines) "cyan" else "grey40")
      }
      
      return(p)
    }, error = function(e) {
      print(paste("Error in generate_jn_plot:", e$message))
      return(NULL)
    })
  }
  
  # Helper function to generate conditional effect plot (Model 3)
  # Returns ggplot object or NULL if plot cannot be generated
  generate_conditional_effect_plot <- function(results, input) {
    # Validation checks
    if(!validate_plot_requirements(results, input, required_model_num = 3, check_moderator2 = TRUE)) {
      return(NULL)
    }
    
    tryCatch({
      plot_data_df <- results$plot_data
      
      if(is.null(plot_data_df) || !is.data.frame(plot_data_df) || nrow(plot_data_df) == 0) {
        return(NULL)
      }
      
      cond_effect_data <- extract_model3_conditional_effect(results$output)
      
      if(is.null(cond_effect_data) || nrow(cond_effect_data) == 0) {
        return(NULL)
      }
      
      plot_data <- cond_effect_data
      
      x_label_text <- if(!is.null(input$moderator2_label) && input$moderator2_label != "") input$moderator2_label else input$moderator2_var
      y_label_text <- paste("Conditional effect of", if(input$x_label != "") input$x_label else input$predictor_var, 
                           "*", if(input$moderator_label != "") input$moderator_label else input$moderator_var,
                           "on", if(input$y_label != "") input$y_label else input$outcome_var)
      plot_title <- "Conditional Effect Plot"
      
      z_range <- range(plot_data$Z, na.rm = TRUE)
      y_range <- range(c(plot_data$Effect, plot_data$LLCI, plot_data$ULCI), na.rm = TRUE)
      
      p <- ggplot(plot_data, aes(x = Z, y = Effect)) +
        {if(input$show_confidence_intervals && !all(is.na(plot_data$LLCI)) && !all(is.na(plot_data$ULCI))) 
          geom_ribbon(aes(ymin = LLCI, ymax = ULCI), alpha = 0.2, fill = if(input$use_color_lines) "red" else "grey50", show.legend = FALSE)} +
        geom_hline(yintercept = 0, linetype = "dashed", color = if(input$use_color_lines) "red" else "black", linewidth = 0.8) +
        geom_line(color = if(input$use_color_lines) "red" else "black", linewidth = 1.2) +
        {if(input$custom_y_axis) coord_cartesian(ylim = c(input$y_axis_min, input$y_axis_max))} +
        {if(!input$custom_y_axis) coord_cartesian(xlim = z_range, ylim = y_range)} +
        labs(
          title = plot_title,
          x = x_label_text,
          y = y_label_text
        ) +
        theme_minimal() +
        theme(
          text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5),
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5)
        )
      
      return(p)
    }, error = function(e) {
      print(paste("Error in generate_conditional_effect_plot:", e$message))
      return(NULL)
    })
  }
  
  # Helper function to generate simple slopes plot (Model 1)
  # Returns ggplot object or NULL if plot cannot be generated
  generate_simple_slopes_plot <- function(results, input) {
    # Validation checks
    if(!validate_plot_requirements(results, input, required_model_num = 1)) {
      return(NULL)
    }
    
    tryCatch({
      data_used <- results$data_used
      outcome_is_binary <- is_binary_variable(data_used, input$outcome_var)
      
      # Parse "Data for visualizing" table from text output
      process_output <- results$output
      viz_idx <- which(grepl("Data for visualizing the conditional effect of the focal predictor:", process_output, ignore.case = TRUE))
      
      if(length(viz_idx) == 0) {
        return(NULL)
      }
      
      data_start <- viz_idx[1] + 2
      data_end <- min(data_start + 15, length(process_output))
      potential_data_lines <- process_output[data_start:data_end]
      potential_data_lines <- potential_data_lines[grepl("^\\s*-?\\d", potential_data_lines)]
      
      if(length(potential_data_lines) < 4) {
        return(NULL)
      }
      
      # Parse as 6 columns: X, W, Y, se, LLCI, ULCI
      parsed_viz <- tryCatch({
        data_text <- paste(potential_data_lines, collapse = "\n")
        parsed_viz <- read.table(text = data_text,
                                col.names = c("X", "W", "Y", "se", "LLCI", "ULCI"),
                                stringsAsFactors = FALSE, 
                                fill = TRUE, 
                                blank.lines.skip = TRUE,
                                na.strings = c("NA", ""))
        
        for(col in 1:ncol(parsed_viz)) {
          parsed_viz[, col] <- as.numeric(parsed_viz[, col])
        }
        parsed_viz
      }, error = function(e) {
        return(NULL)
      })
      
      if(is.null(parsed_viz) || ncol(parsed_viz) != 6 || nrow(parsed_viz) < 4) {
        return(NULL)
      }
      
      plot_data <- data.frame(
        Predictor = parsed_viz$X,
        Moderator = parsed_viz$W,
        Outcome = parsed_viz$Y,
        LLCI = parsed_viz$LLCI,
        ULCI = parsed_viz$ULCI
      )
      
      plot_data <- plot_data[complete.cases(plot_data), ]
      if(nrow(plot_data) == 0) {
        return(NULL)
      }
      
      moderator_levels_raw <- sort(unique(plot_data$Moderator))
      if(length(moderator_levels_raw) == 0) {
        return(NULL)
      }
      if(length(unique(plot_data$Predictor)) < 2 || length(moderator_levels_raw) < 2) {
        return(NULL)
      }
      
      moderator_levels <- round(moderator_levels_raw, input$decimal_places)
      predictor_range <- range(plot_data$Predictor, na.rm = TRUE)
      
      y_label_text <- if(outcome_is_binary) {
        if(input$y_label != "") paste(input$y_label, "(Probability)") else "Predicted Probability"
      } else {
        if(input$y_label != "") input$y_label else input$outcome_var
      }
      
      x_label_text <- if(input$x_label != "") input$x_label else input$predictor_var
      mod_label_text <- if(input$moderator_label != "") input$moderator_label else paste0(input$moderator_var, " Levels")
      plot_title <- "Simple Slopes Plot"
      
      plot_data$Moderator_factor <- factor(plot_data$Moderator, levels = moderator_levels_raw, labels = format(moderator_levels, nsmall = input$decimal_places))
      
      p <- ggplot(plot_data, aes(
        x = Predictor, 
        y = Outcome,
        color = if(input$use_color_lines) Moderator_factor else NULL,
        linetype = if(!input$use_color_lines) Moderator_factor else NULL,
        fill = if(input$show_confidence_intervals && input$use_color_lines) Moderator_factor else NULL
      )) +
        {if(input$show_confidence_intervals && !all(is.na(plot_data$LLCI))) 
          geom_ribbon(aes(ymin = LLCI, ymax = ULCI), alpha = 0.2, color = NA)} +
        geom_line(linewidth = 1) +
        {if(input$custom_y_axis) coord_cartesian(ylim = c(input$y_axis_min, input$y_axis_max))} +
        {if(!input$custom_y_axis && outcome_is_binary) coord_cartesian(ylim = c(0, 1))} +
        {if(!input$custom_y_axis && !outcome_is_binary) coord_cartesian(xlim = predictor_range)} +
        labs(
          title = plot_title,
          x = x_label_text,
          y = y_label_text,
          color = if(input$use_color_lines) mod_label_text else NULL,
          linetype = if(!input$use_color_lines) mod_label_text else NULL,
          fill = if(input$show_confidence_intervals && input$use_color_lines) mod_label_text else NULL
        ) +
        theme_minimal() +
        theme(
          text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5)
        )
      
      if(!input$show_confidence_intervals || !input$use_color_lines) {
        p <- p + guides(fill = "none")
      }
      
      return(p)
    }, error = function(e) {
      print(paste("Error in generate_simple_slopes_plot:", e$message))
      return(NULL)
    })
  }
  
  # Helper function to generate stacked simple slopes plot (Model 3)
  # Returns grob (grid object) or NULL if plot cannot be generated
  generate_stacked_slopes_plot <- function(results, input) {
    # Validation checks
    if(!validate_plot_requirements(results, input, required_model_num = 3, check_moderator2 = TRUE)) {
      return(NULL)
    }
    
    tryCatch({
      process_output <- results$output
      viz_idx <- which(grepl("Data for visualizing the conditional effect of the focal predictor:", process_output, ignore.case = TRUE))
      
      if(length(viz_idx) == 0) {
        return(NULL)
      }
      
      data_start <- viz_idx[1] + 2
      data_end <- min(data_start + 30, length(process_output))
      potential_data_lines <- process_output[data_start:data_end]
      potential_data_lines <- potential_data_lines[grepl("^\\s*-?\\d", potential_data_lines)]
      
      if(length(potential_data_lines) == 0) {
        return(NULL)
      }
      
      # Parse as 7 columns: X, W, Z, Y, se, LLCI, ULCI
      parsed_viz <- tryCatch({
        data_text <- paste(potential_data_lines, collapse = "\n")
        parsed_viz <- read.table(text = data_text,
                                col.names = c("X", "W", "Z", "Y", "se", "LLCI", "ULCI"),
                                stringsAsFactors = FALSE, 
                                fill = TRUE, 
                                blank.lines.skip = TRUE,
                                row.names = NULL,
                                na.strings = c("NA", ""))
        
        for(col in 1:ncol(parsed_viz)) {
          parsed_viz[, col] <- as.numeric(parsed_viz[, col])
        }
        parsed_viz
      }, error = function(e) {
        return(NULL)
      })
      
      if(is.null(parsed_viz) || ncol(parsed_viz) != 7 || nrow(parsed_viz) == 0) {
        return(NULL)
      }
      
      stacked_data <- data.frame(
        Predictor = parsed_viz$X,
        Moderator = parsed_viz$W,
        Z = parsed_viz$Z,
        Outcome = parsed_viz$Y,
        LLCI = parsed_viz$LLCI,
        ULCI = parsed_viz$ULCI
      )
      stacked_data <- stacked_data[complete.cases(stacked_data), ]
      
      if(nrow(stacked_data) == 0) {
        return(NULL)
      }
      
      unique_z <- sort(unique(stacked_data$Z))
      if(length(unique_z) == 0) {
        return(NULL)
      }
      
      unique_w <- sort(unique(stacked_data$Moderator))
      moderator_levels_raw <- unique_w
      
      plot_list <- list()
      z_label_text <- if(!is.null(input$moderator2_label) && input$moderator2_label != "") input$moderator2_label else input$moderator2_var
      x_label_text <- if(input$x_label != "") input$x_label else input$predictor_var
      y_label_text <- if(input$y_label != "") input$y_label else input$outcome_var
      mod_label_text <- if(input$moderator_label != "") input$moderator_label else paste0(input$moderator_var, " Levels")
      
      for(i in 1:length(unique_z)) {
        z_val <- unique_z[i]
        z_subset <- stacked_data[stacked_data$Z == z_val, ]
        
        if(nrow(z_subset) == 0) next
        
        z_display <- round(z_val, input$decimal_places)
        z_subset$Moderator_factor <- factor(z_subset$Moderator, levels = moderator_levels_raw, 
                                           labels = format(round(moderator_levels_raw, input$decimal_places), nsmall = input$decimal_places))
        
        p_sub <- ggplot(z_subset, aes(
          x = Predictor,
          y = Outcome,
          color = if(input$use_color_lines) Moderator_factor else NULL,
          linetype = if(!input$use_color_lines) Moderator_factor else NULL,
          fill = if(input$show_confidence_intervals && input$use_color_lines) Moderator_factor else NULL
        )) +
          {if(input$show_confidence_intervals && !all(is.na(z_subset$LLCI))) 
            geom_ribbon(aes(ymin = LLCI, ymax = ULCI), alpha = 0.2, color = NA)} +
          geom_line(linewidth = 1) +
          {if(input$custom_y_axis) coord_cartesian(ylim = c(input$y_axis_min, input$y_axis_max))} +
          labs(
            title = paste0("Simple Slopes at ", z_label_text, " = ", z_display),
            x = x_label_text,
            y = y_label_text,
            color = if(input$use_color_lines) mod_label_text else NULL,
            linetype = if(!input$use_color_lines) mod_label_text else NULL,
            fill = if(input$show_confidence_intervals && input$use_color_lines) mod_label_text else NULL
          ) +
          theme_minimal() +
          theme(
            text = element_text(size = 12),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            plot.title = element_text(size = 14, hjust = 0.5),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
        
        if(!input$show_confidence_intervals || !input$use_color_lines) {
          p_sub <- p_sub + guides(fill = "none")
        }
        
        plot_list[[i]] <- p_sub
      }
      
      if(length(plot_list) > 0) {
        p <- do.call(gridExtra::arrangeGrob, c(plot_list, ncol = 1))
        return(p)
      } else {
        return(NULL)
      }
    }, error = function(e) {
      print(paste("Error in generate_stacked_slopes_plot:", e$message))
      return(NULL)
    })
  }
  
  # Helper function to extract coefficients from PROCESS output for moderation models
  extract_coefficients <- function(process_output, predictor_var, moderator_var) {
    coef_data <- numeric(4)
    names(coef_data) <- c("constant", "predictor", "moderator", "interaction")
    
    # Find the Model section
    coef_start <- which(grepl("^Model:", process_output))
    if(length(coef_start) == 0) return(NULL)
    
    # Find where the coefficient section ends
    coef_end <- coef_start + 2
    for(i in (coef_start + 3):min(length(process_output), coef_start + 20)) {
      if(i > length(process_output)) break
      line <- trimws(process_output[i])
      if(line == "" || grepl("^Product terms key:", line) || 
         grepl("^Covariance matrix", line) || grepl("^Test\\(s\\)", line)) {
        coef_end <- i - 1
        break
      }
    }
    
    coef_lines <- process_output[(coef_start + 3):coef_end]
    
    for(i in seq_along(coef_lines)) {
      parts <- strsplit(trimws(coef_lines[i]), "\\s+")[[1]]
      if(length(parts) >= 2) {
        var_name <- parts[1]
        coef_value <- tryCatch(as.numeric(parts[2]), error = function(e) NA)
        if(!is.na(coef_value)) {
          if(grepl("constant|Intercept", var_name, ignore.case = TRUE)) {
            coef_data["constant"] <- coef_value
          } else if(grepl(paste0("^", predictor_var, "$"), var_name)) {
            coef_data["predictor"] <- coef_value
          } else if(grepl(paste0("^", moderator_var, "$"), var_name)) {
            coef_data["moderator"] <- coef_value
          } else if(grepl("^int_1|^Int_1", var_name, ignore.case = TRUE)) {
            coef_data["interaction"] <- coef_value
          }
        }
      }
    }
    
    # Check if we got all coefficients
    if(any(is.na(coef_data)) || all(coef_data == 0)) {
      return(NULL)
    }
    
    return(coef_data)
  }
  
  # Reactive to track JN plot availability (only for continuous moderators)
  jn_available <- reactiveVal(TRUE)
  
  # Update JN availability based on moderator type
  observe({
    if(!is.null(input$moderator_var) && input$moderator_var != "" && 
       !is.null(rv$original_dataset)) {
      if(is_binary_variable(rv$original_dataset, input$moderator_var)) {
        jn_available(FALSE)
      } else {
        jn_available(TRUE)
      }
    }
  })
  
  extract_model3_conditional_effect <- function(process_output) {
    if(is.null(process_output) || length(process_output) == 0) {
      return(NULL)
    }
    
    start_idx <- which(grepl("Conditional effects of the focal predictor at values of the moderator\\(s\\):",
                             process_output, ignore.case = TRUE))
    if(length(start_idx) == 0) {
      return(NULL)
    }
    
    start_line <- start_idx[1]
    data_start <- start_line + 2
    search_end <- min(data_start + 100, length(process_output))
    search_subset <- process_output[data_start:search_end]
    end_candidates <- which(grepl("^\\s*$|^Data for visualizing|^----------|^\\*+", search_subset))
    
    if(length(end_candidates) > 0) {
      end_idx <- data_start + end_candidates[1] - 1
    } else {
      end_idx <- min(data_start + 100, length(process_output))
    }
    
    if(end_idx <= data_start) {
      return(NULL)
    }
    
    data_lines <- process_output[data_start:end_idx]
    data_lines <- data_lines[grepl("^\\s*-?\\d", data_lines)]
    if(length(data_lines) == 0) {
      return(NULL)
    }
    
    parsed_rows <- lapply(data_lines, function(line) {
      nums <- suppressWarnings(as.numeric(strsplit(trimws(line), "\\s+")[[1]]))
      nums <- nums[!is.na(nums)]
      if(length(nums) >= 7) nums else NULL
    })
    parsed_rows <- Filter(Negate(is.null), parsed_rows)
    if(length(parsed_rows) == 0) {
      return(NULL)
    }
    
    max_len <- max(vapply(parsed_rows, length, 0L))
    if(max_len >= 8) {
      df <- as.data.frame(do.call(rbind, lapply(parsed_rows, function(nums) nums[1:8])))
      names(df) <- c("W", "Z", "Effect", "se", "t", "p", "LLCI", "ULCI")
    } else {
      df <- as.data.frame(do.call(rbind, lapply(parsed_rows, function(nums) nums[1:7])))
      names(df) <- c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")
    }
    
    df <- df[complete.cases(df[, c("Z", "Effect")]), ]
    if(nrow(df) == 0) {
      return(NULL)
    }
    
    if("W" %in% names(df)) {
      unique_z <- sort(unique(df$Z))
      cond_effect_data <- data.frame(
        Z = numeric(length(unique_z)),
        Effect = numeric(length(unique_z)),
        se = numeric(length(unique_z)),
        t = numeric(length(unique_z)),
        p = numeric(length(unique_z)),
        LLCI = numeric(length(unique_z)),
        ULCI = numeric(length(unique_z))
      )
      
      for(i in 1:length(unique_z)) {
        z_subset <- df[df$Z == unique_z[i], ]
        median_w_idx <- which.min(abs(z_subset$W - median(z_subset$W, na.rm = TRUE)))
        if(length(median_w_idx) == 0) median_w_idx <- 1
        cond_effect_data[i, ] <- z_subset[median_w_idx, c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")]
      }
      cond_effect_data
    } else {
      df[, c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")]
    }
  }
  
  save_stacked_slopes_plot <- function(file, results) {
    plot_data_df <- results$plot_data
    if(is.null(plot_data_df) || !is.data.frame(plot_data_df) || nrow(plot_data_df) == 0) {
      stop("Plot data not available. Please run the analysis first.")
    }
    
    stacked_data <- NULL
    
    tryCatch({
      process_output <- results$output
      viz_idx <- which(grepl("Data for visualizing the conditional effect of the focal predictor:", process_output, ignore.case = TRUE))
      
      if(length(viz_idx) > 0) {
        data_start <- viz_idx[1] + 2
        data_end <- min(data_start + 30, length(process_output))
        potential_data_lines <- process_output[data_start:data_end]
        potential_data_lines <- potential_data_lines[grepl("^\\s*-?\\d", potential_data_lines)]
        
        if(length(potential_data_lines) > 0) {
          parsed_viz <- tryCatch({
            data_text <- paste(potential_data_lines, collapse = "\n")
            parsed_viz <- read.table(text = data_text,
                                    col.names = c("X", "W", "Z", "Y", "se", "LLCI", "ULCI"),
                                    stringsAsFactors = FALSE, 
                                    fill = TRUE, 
                                    blank.lines.skip = TRUE,
                                    row.names = NULL,
                                    na.strings = c("NA", ""))
            
            for(col in 1:ncol(parsed_viz)) {
              parsed_viz[, col] <- as.numeric(parsed_viz[, col])
            }
            parsed_viz
          }, error = function(e) {
            dbg(paste("DEBUG: Error parsing stacked plot data:", e$message))
            NULL
          })
          
          if(!is.null(parsed_viz) && ncol(parsed_viz) == 7 && nrow(parsed_viz) > 0) {
            stacked_data <- data.frame(
              Predictor = parsed_viz$X,
              Moderator = parsed_viz$W,
              Z = parsed_viz$Z,
              Outcome = parsed_viz$Y,
              LLCI = parsed_viz$LLCI,
              ULCI = parsed_viz$ULCI
            )
            stacked_data <- stacked_data[complete.cases(stacked_data), ]
          }
        }
      }
    }, error = function(e) {
      dbg(paste("DEBUG: Error parsing stacked plot data:", e$message))
    })
    
    if(is.null(stacked_data) || nrow(stacked_data) == 0) {
      stop("Could not parse visualization data for stacked plot.")
    }
    
    unique_z <- sort(unique(stacked_data$Z))
    if(length(unique_z) == 0) {
      stop("No Z values found in visualization data.")
    }
    
    unique_w <- sort(unique(stacked_data$Moderator))
    moderator_levels_raw <- unique_w
    
    plot_list <- list()
    z_label_text <- if(!is.null(input$moderator2_label) && input$moderator2_label != "") input$moderator2_label else input$moderator2_var
    x_label_text <- if(input$x_label != "") input$x_label else input$predictor_var
    y_label_text <- if(input$y_label != "") input$y_label else input$outcome_var
    mod_label_text <- if(input$moderator_label != "") input$moderator_label else paste0(input$moderator_var, " Levels")
    
    for(i in 1:length(unique_z)) {
      z_val <- unique_z[i]
      z_subset <- stacked_data[stacked_data$Z == z_val, ]
      
      if(nrow(z_subset) == 0) next
      
      z_display <- round(z_val, input$decimal_places)
      
      z_subset$Moderator_factor <- factor(z_subset$Moderator, levels = moderator_levels_raw, 
                                         labels = format(round(moderator_levels_raw, input$decimal_places), nsmall = input$decimal_places))
      
      p_sub <- ggplot(z_subset, aes(
        x = Predictor,
        y = Outcome,
        color = if(input$use_color_lines) Moderator_factor else NULL,
        linetype = if(!input$use_color_lines) Moderator_factor else NULL,
        fill = if(input$show_confidence_intervals && input$use_color_lines) Moderator_factor else NULL
      )) +
        {if(input$show_confidence_intervals && !all(is.na(z_subset$LLCI))) 
          geom_ribbon(aes(ymin = LLCI, ymax = ULCI), alpha = 0.2, color = NA)} +
        geom_line(linewidth = 1) +
        {if(input$custom_y_axis) coord_cartesian(ylim = c(input$y_axis_min, input$y_axis_max))} +
        labs(
          title = paste0("Simple Slopes at ", z_label_text, " = ", z_display),
          x = x_label_text,
          y = y_label_text,
          color = if(input$use_color_lines) mod_label_text else NULL,
          linetype = if(!input$use_color_lines) mod_label_text else NULL,
          fill = if(input$show_confidence_intervals && input$use_color_lines) mod_label_text else NULL
        ) +
        theme_minimal() +
        theme(
          text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5)
        )
      
      if(!input$show_confidence_intervals || !input$use_color_lines) {
        p_sub <- p_sub + guides(fill = "none")
      }
      
      plot_list[[i]] <- p_sub
    }
    
    if(length(plot_list) > 0) {
      p <- do.call(gridExtra::arrangeGrob, c(plot_list, ncol = 1))
      grDevices::jpeg(file, width = 10, height = 8 * length(plot_list), units = "in", res = 600, bg = "white")
      on.exit(grDevices::dev.off(), add = TRUE)
      grid::grid.draw(p)
    } else {
      stop("No plots generated for stacked view.")
    }
  }
  
  # Simple Slopes Plot
  build_slopes_plot <- function(plot_type_override = NULL, model3_only = FALSE) {
    # Only allow plots for Models 1 and 3
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    if(!supports_plot_model(model_num)) {
      plot.new()
      text(0.5, 0.5, "Plots are only available for Models 1 and 3.", cex = 1.2)
      return()
    }
    
    # Check if we have valid analysis results for the current model/variables
    if(is.null(analysis_results())) {
      return()
    }
    
    req(input$moderator_var)
    req(input$predictor_var)
    
    # Check for validation errors
    if(!is.null(rv$validation_error)) {
      return()
    }
    
    # Verify that the analysis results match the current model AND variables
    results <- analysis_results()
    if(is.null(results) || is.null(results$settings)) {
      return()
    }
    
    # Check if model matches
    current_model <- as.numeric(input$process_model)
    if(results$settings$model != current_model) {
      return()
    }
    
    # CRITICAL: Check if variables match - if they don't, don't render plot
    # This prevents showing plots with wrong labels when variables change but analysis hasn't been rerun
    if(results$settings$predictor_var != input$predictor_var ||
       results$settings$outcome_var != input$outcome_var ||
       results$settings$moderator_var != input$moderator_var) {
      plot.new()
      text(0.5, 0.5, "Variables have changed.\nPlease run the analysis again.", cex = 1.2)
      return()
    }
    
    # For models with second moderator, also check moderator2_var
    if(has_second_moderator_model(current_model)) {
      result_mod2 <- if(is.null(results$settings$moderator2_var)) "" else results$settings$moderator2_var
      current_mod2 <- if(is.null(input$moderator2_var) || input$moderator2_var == "") "" else input$moderator2_var
      if(result_mod2 != current_mod2) {
        plot.new()
        text(0.5, 0.5, "Variables have changed.\nPlease run the analysis again.", cex = 1.2)
        return()
      }
    }
    
    data_used <- results$data_used
    outcome_is_binary <- is_binary_variable(data_used, input$outcome_var)
    
    # Determine model type
    model_num <- as.numeric(input$process_model)
    has_second_mod <- has_second_moderator_model(model_num)

    if(model3_only && !has_second_mod) {
      return()
    }
    
    # Use plot data from PROCESS directly (plot=2, save=2) - always uses percentiles
    plot_data_df <- results$plot_data
    
    if(is.null(plot_data_df) || !is.data.frame(plot_data_df) || nrow(plot_data_df) == 0) {
      return()
    }
    
    # PROCESS returns: X, W, [Z if dual], predicted, se, LLCI, ULCI
    # Column order: X, W, Z (if dual), Y, se, LLCI, ULCI
    if(has_second_mod && !is.null(input$moderator2_var) && input$moderator2_var != "") {
      # For Model 3, check if conditional effect data is in the result object
      # PROCESS generates this data when plot=2 is set, and it's stored in the result object
      
      # Initialize plot_data to NULL so it's always defined
      plot_data <- NULL
      active_moderator <- NULL
      cond_effect_data <- NULL  # Initialize outside tryCatch
      
      tryCatch({
        result_obj <- results$result
        
        # Debug: COMPREHENSIVE inspection of result object structure
        print("========================================")
        dbg("DEBUG: COMPREHENSIVE RESULT OBJECT INSPECTION FOR MODEL 3")
        print("========================================")
        print(paste("Result class:", paste(class(result_obj), collapse=", ")))
        print(paste("Result type:", typeof(result_obj)))
        
        # Deep dive into result object structure
        if(is.list(result_obj)) {
          print(paste("Result is list with", length(result_obj), "elements"))
          if(!is.null(names(result_obj))) {
            print("Named elements:")
            print(names(result_obj))
            # Check each named element
            for(name in names(result_obj)) {
              elem <- result_obj[[name]]
              print(paste("  Element '", name, "': class =", paste(class(elem), collapse=", "), 
                         ", type =", typeof(elem)))
              if(is.data.frame(elem)) {
                print(paste("    Data frame: ", nrow(elem), "rows,", ncol(elem), "cols"))
                print(paste("    Column names:", paste(names(elem), collapse=", ")))
                if(nrow(elem) > 0 && ncol(elem) > 0) {
                  print("    First few rows:")
                  print(head(elem, 3))
                }
              } else if(is.matrix(elem)) {
                print(paste("    Matrix: ", nrow(elem), "rows,", ncol(elem), "cols"))
                if(nrow(elem) > 0 && ncol(elem) > 0) {
                  print("    First few rows:")
                  print(head(elem, 3))
                }
              } else if(is.list(elem)) {
                print(paste("    Nested list with", length(elem), "elements"))
                if(!is.null(names(elem))) {
                  print(paste("    Nested names:", paste(names(elem), collapse=", ")))
                }
              }
            }
          } else {
            print("Unnamed list - checking elements by index:")
            for(i in 1:min(10, length(result_obj))) {
              elem <- result_obj[[i]]
              print(paste("  Element [[", i, "]]: class =", paste(class(elem), collapse=", ")))
              if(is.data.frame(elem)) {
                print(paste("    Data frame: ", nrow(elem), "rows,", ncol(elem), "cols"))
                if(nrow(elem) > 0 && ncol(elem) > 0) {
                  print("    Column names:", paste(names(elem), collapse=", "))
                  print("    First few rows:")
                  print(head(elem, 3))
                }
              } else if(is.matrix(elem)) {
                print(paste("    Matrix: ", nrow(elem), "rows,", ncol(elem), "cols"))
                if(nrow(elem) > 0 && ncol(elem) > 0) {
                  print("    First few rows:")
                  print(head(elem, 3))
                }
              }
            }
          }
        } else if(is.matrix(result_obj) || is.array(result_obj)) {
          print(paste("Result is matrix/array with", nrow(result_obj), "rows and", ncol(result_obj), "cols"))
          print("First few rows:")
          print(head(result_obj, 5))
          if(!is.null(colnames(result_obj))) {
            print("Column names:")
            print(colnames(result_obj))
          }
        } else if(is.data.frame(result_obj)) {
          print(paste("Result is data.frame with", nrow(result_obj), "rows and", ncol(result_obj), "cols"))
          print("Column names:")
          print(names(result_obj))
          print("First few rows:")
          print(head(result_obj, 5))
        }
        
        print("========================================")
        dbg("DEBUG: Checking results$plot_data structure")
        print("========================================")
        plot_data_df <- results$plot_data
        if(!is.null(plot_data_df)) {
          print(paste("plot_data class:", paste(class(plot_data_df), collapse=", ")))
          print(paste("plot_data dimensions:", nrow(plot_data_df), "x", ncol(plot_data_df)))
          print("plot_data column names/structure:")
          if(is.data.frame(plot_data_df)) {
            print(names(plot_data_df))
          } else if(is.matrix(plot_data_df)) {
            print(paste("Matrix with", ncol(plot_data_df), "columns"))
          }
          print("First 10 rows of plot_data:")
          print(head(plot_data_df, 10))
          print("Last 10 rows of plot_data:")
          print(tail(plot_data_df, 10))
        } else {
          print("plot_data is NULL")
        }
        
        # KEY INSIGHT: For Model 3, PROCESS appends jnvals (conditional effect data) to resultm
        # The resultm matrix contains multiple sections:
        # 1. Simple slopes plot data (X, W, Z, predicted Y, SE, LLCI, ULCI) - 8 columns
        # 2. Conditional effect data (Z, Effect, SE, t, p, LLCI, ULCI) - 7 columns, appended later
        # The conditional effect data should be at the END of the matrix, with Z values in column 1
        
        # Try to extract conditional effect data from result object
        cond_effect_data <- NULL
        
        # Check if plot_data_df contains the conditional effect data at the end
        if(!is.null(plot_data_df) && (is.data.frame(plot_data_df) || is.matrix(plot_data_df))) {
          # Convert to matrix for easier manipulation
          plot_matrix <- as.matrix(plot_data_df)
          
          # Look for rows that have 7 columns (conditional effect data) vs 8 columns (simple slopes)
          # The conditional effect data should be at the end, with Z values in column 1
          # and should have fewer unique values in column 1 (Z) than the simple slopes data
          
          dbg("DEBUG: Searching for conditional effect data in plot_data matrix...")
          print(paste("Matrix has", nrow(plot_matrix), "rows and", ncol(plot_matrix), "columns"))
          
          # Try to identify the conditional effect section
          # It should have Z values in first column, and Effect in second column
          # The Z values should be a continuous range (not the full X*W*Z grid)
          
          # Check if there's a section with 7 columns (conditional effect) vs 8 (simple slopes)
          # Look for rows where column 1 has values that look like Z (moderator) values
          # and column 2 has effect values (could be negative)
          
          # Strategy: Look for a section at the end of the matrix where:
          # - Column 1 contains Z values (should match moderator2_var range)
          # - Column 2 contains effect values
          # - There are 7 columns total
          
          # Get the range of Z values from the data
          if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
            z_var_data <- results$data_used[[input$moderator2_var]]
            z_range <- range(z_var_data, na.rm = TRUE)
            dbg(paste("DEBUG: Z variable range:", z_range[1], "to", z_range[2]))
            
            # Look for rows where column 1 values are within Z range and look like Z values
            # These should be the conditional effect rows
            potential_z_col <- plot_matrix[, 1]
            z_matches <- which(potential_z_col >= z_range[1] & potential_z_col <= z_range[2])
            
            if(length(z_matches) > 0) {
              dbg(paste("DEBUG: Found", length(z_matches), "rows with Z values in column 1"))
              
              # Check if these rows form a continuous sequence (conditional effect data)
              # vs scattered (simple slopes data)
              z_values <- unique(sort(potential_z_col[z_matches]))
              dbg(paste("DEBUG: Unique Z values found:", length(z_values)))
              print("First few Z values:")
              print(head(z_values, 10))
              
              # If we have a reasonable number of unique Z values (20-25 is typical for JN/conditional effect)
              # and they form a sequence, this is likely the conditional effect data
              if(length(z_values) >= 15 && length(z_values) <= 30) {
                # Extract rows where column 1 is in this Z range
                # But we need to be careful - simple slopes also has Z values
                # The key difference: conditional effect has Z in column 1, simple slopes has X in column 1
                
                # Better approach: Look for a section where column 1 values are sequential
                # and form a smooth progression (conditional effect) vs grid (simple slopes)
                z_sorted <- sort(potential_z_col[z_matches])
                z_diff <- diff(z_sorted)
                
                # If differences are relatively uniform, it's likely conditional effect data
                if(length(z_diff) > 10 && sd(z_diff) / mean(z_diff) < 0.5) {
                  dbg("DEBUG: Column 1 values appear to form a sequence (conditional effect data)")
                  
                  # Find the rows that correspond to this Z sequence
                  # They should be consecutive or near-consecutive in the matrix
                  cond_rows <- which(potential_z_col %in% z_values)
                  
                  if(length(cond_rows) >= 15) {
                    # Extract these rows - they should have 7 columns
                    cond_section <- plot_matrix[cond_rows, , drop = FALSE]
                    
                    # Verify it has the right structure: Z, Effect, SE, t, p, LLCI, ULCI
                    if(ncol(cond_section) >= 7) {
                      # Check if column 2 looks like effects (can be negative)
                      effects <- as.numeric(cond_section[, 2])
                      if(!all(is.na(effects)) && length(unique(effects)) > 1) {
                        dbg("DEBUG: Found conditional effect data section!")
                        cond_effect_data <- as.data.frame(cond_section[, 1:7])
                        colnames(cond_effect_data) <- c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")
                        dbg("DEBUG: Conditional effect data extracted:")
                        print(head(cond_effect_data, 10))
                      }
                    }
                  }
                }
              }
            }
          }
        }
        
        if(is.list(result_obj)) {
          print(paste("Result is list with", length(result_obj), "elements"))
          print("Result names/indices:")
          if(!is.null(names(result_obj))) {
            print(names(result_obj))
          } else {
            print(paste("Unnamed list, checking elements 1-", min(5, length(result_obj))))
          }
          
          # Check each element
          for(i in 1:min(5, length(result_obj))) {
            print(paste("Element", i, "class:", paste(class(result_obj[[i]]), collapse=", ")))
            if(is.data.frame(result_obj[[i]])) {
              print(paste("  Rows:", nrow(result_obj[[i]]), "Cols:", ncol(result_obj[[i]])))
              print(paste("  Column names:", paste(names(result_obj[[i]]), collapse=", ")))
            } else if(is.matrix(result_obj[[i]])) {
              print(paste("  Rows:", nrow(result_obj[[i]]), "Cols:", ncol(result_obj[[i]])))
            } else if(is.list(result_obj[[i]])) {
              print(paste("  Nested list with", length(result_obj[[i]]), "elements"))
            }
          }
        } else if(is.matrix(result_obj) || is.array(result_obj)) {
          print(paste("Result is matrix/array with", nrow(result_obj), "rows and", ncol(result_obj), "cols"))
          dbg("DEBUG: This matrix is the plot_data (X, W, Z, predicted Y, SE, LLCI, ULCI), not conditional effect data")
          dbg("DEBUG: For Model 3, conditional effect data must be found elsewhere (text output or calculated from coefficients)")
          # The result matrix contains simple slopes plot data, NOT conditional effect data
          # Conditional effect data shows how X*W changes across Z, which is different
          # We need to find it in text output or calculate from coefficients
        }
        
        if(!is.null(plot_data_df) && is.data.frame(plot_data_df) && ncol(plot_data_df) >= 7) {
          # For Model 3, plot_data contains: X, W, Z, predicted Y, SE, LLCI, ULCI
          # But we need the conditional effect of X*W across Z
          # This might require calculating from the coefficients or finding it in result object
          
          # Check if result object has conditional effect data (if not already found from matrix check above)
          if(is.null(cond_effect_data) && is.list(result_obj)) {
            # First check for named elements that might contain conditional effect data
            if(!is.null(names(result_obj))) {
              dbg("DEBUG: Result object has named elements:")
              print(names(result_obj))
              
              # Check for common names that might contain conditional effect data
              possible_names <- c("cond_effect", "conditional_effect", "probeplt", "probe", "effect", "modvals", "modvals3")
              for(name in possible_names) {
                if(name %in% names(result_obj)) {
                  elem <- result_obj[[name]]
                  dbg(paste("DEBUG: Found element named '", name, "'"))
                  if(is.data.frame(elem) || is.matrix(elem)) {
                    elem_df <- as.data.frame(elem)
                    print(paste("  Rows:", nrow(elem_df), "Cols:", ncol(elem_df)))
                    if(ncol(elem_df) >= 7) {
                      # Check if this looks like conditional effect data
                      col_check <- tryCatch({
                        z_vals <- as.numeric(elem_df[, 1])
                        effects <- as.numeric(elem_df[, 2])
                        if(!all(is.na(z_vals)) && !all(is.na(effects)) && 
                           length(unique(z_vals)) > 1 && length(unique(effects)) > 1) {
                          TRUE
                        } else {
                          FALSE
                        }
                      }, error = function(e) FALSE)
                      
                      if(col_check) {
                        dbg(paste("DEBUG: Found conditional effect data in result$", name))
                        cond_effect_data <- elem_df
                        colnames(cond_effect_data) <- c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")[1:ncol(cond_effect_data)]
                        break
                      }
                    }
                  }
                }
              }
            }
            
            # If not found by name, check all elements
            if(is.null(cond_effect_data) && length(result_obj) > 1) {
              dbg("DEBUG: Checking all result object elements for conditional effect data...")
              for(i in 1:length(result_obj)) {
                elem <- result_obj[[i]]
                if(is.data.frame(elem) && ncol(elem) >= 7) {
                  # Check if this looks like conditional effect data (Z, Effect, SE, t, p, LLCI, ULCI)
                  col_check <- tryCatch({
                    # Check if first column looks like Z values (moderator values)
                    # and second column looks like effects (could be negative)
                    z_vals <- as.numeric(elem[, 1])
                    effects <- as.numeric(elem[, 2])
                    if(!all(is.na(z_vals)) && !all(is.na(effects)) && 
                       length(unique(z_vals)) > 1 && length(unique(effects)) > 1) {
                      TRUE
                    } else {
                      FALSE
                    }
                  }, error = function(e) FALSE)
                  
                  if(col_check) {
                    dbg(paste("DEBUG: Found potential conditional effect data in result[[", i, "]]"))
                    cond_effect_data <- elem
                    colnames(cond_effect_data) <- c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")[1:ncol(cond_effect_data)]
                    break
                  }
                } else if(is.matrix(elem) && ncol(elem) >= 7) {
                  # Same check for matrices
                  col_check <- tryCatch({
                    z_vals <- as.numeric(elem[, 1])
                    effects <- as.numeric(elem[, 2])
                    if(!all(is.na(z_vals)) && !all(is.na(effects)) && 
                       length(unique(z_vals)) > 1 && length(unique(effects)) > 1) {
                      TRUE
                    } else {
                      FALSE
                    }
                  }, error = function(e) FALSE)
                  
                  if(col_check) {
                    dbg(paste("DEBUG: Found potential conditional effect data in result[[", i, "]] (matrix)"))
                    cond_effect_data <- as.data.frame(elem)
                    colnames(cond_effect_data) <- c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")[1:ncol(cond_effect_data)]
                    break
                  }
                }
              }
            }
          }
          
          # If not found in result object, try parsing text output as fallback
          if(is.null(cond_effect_data)) {
            process_output <- results$output
            
            # Debug: search for all possible section names
            dbg("DEBUG: Searching PROCESS output for conditional effect section...")
            all_effect_lines <- grep("effect|Effect|conditional|Conditional", process_output, ignore.case = TRUE, value = FALSE)
            if(length(all_effect_lines) > 0) {
              print(paste("Found", length(all_effect_lines), "lines containing 'effect' or 'conditional':"))
              print("Sample lines:")
              for(i in 1:min(10, length(all_effect_lines))) {
                print(paste("  Line", all_effect_lines[i], ":", process_output[all_effect_lines[i]]))
              }
            }
            
            # For Model 3, look specifically for "Conditional X*W interaction at values of the moderator Z:"
            # This is the section that contains the data we need
            start_idx <- which(grepl("Conditional X\\*W interaction at values of the moderator Z:", process_output, ignore.case = TRUE))
            
            # Fallback to other patterns if not found
            if(length(start_idx) == 0) {
              start_idx <- which(grepl("Conditional effect.*X.*W.*interaction.*values.*moderator.*Z", process_output, ignore.case = TRUE))
            }
            
            if(length(start_idx) == 0) {
              start_idx <- which(grepl("Conditional effect.*focal predictor|Conditional effects.*focal predictor", process_output, ignore.case = TRUE))
            }
            
            if(length(start_idx) > 0) {
              # Use the first match
              start_line <- start_idx[1]
              dbg(paste("DEBUG: Found conditional effect section at line", start_line))
              print(paste("Section header:", process_output[start_line]))
              
              # Find the data section - it should start 2 lines after the header
              data_start <- start_line + 2
              
              # Find where the data section ends
              # Look for blank line, "Data for visualizing", or separator line
              search_start <- data_start
              search_end <- min(data_start + 50, length(process_output))  # Limit search to next 50 lines
              
              # Search in the subset of process_output
              search_subset <- process_output[search_start:search_end]
              end_candidates <- which(grepl("^\\s*$|^Data for visualizing|^----------|^\\*+", search_subset))
              
              if(length(end_candidates) > 0) {
                # end_candidates are relative to search_subset, so add search_start - 1 to get absolute position
                end_idx <- search_start + end_candidates[1] - 1
              } else {
                # Try alternative search
                alt_search <- process_output[data_start:min(data_start + 100, length(process_output))]
                alt_end <- which(grepl("^-----------|^\\*+", alt_search))[1]
                if(!is.na(alt_end)) {
                  end_idx <- data_start + alt_end - 1
                } else {
                  end_idx <- min(data_start + 100, length(process_output))
                }
              }
              
              dbg(paste("DEBUG: Data section from line", data_start, "to", end_idx))
              
              if(end_idx > data_start) {
                data_lines <- process_output[data_start:end_idx]
                data_lines <- data_lines[grepl("^\\s*-?\\d", data_lines)]
                
                if(length(data_lines) > 0) {
                  dbg(paste("DEBUG: Found", length(data_lines), "potential data lines"))
                  print("First few data lines:")
                  print(head(data_lines, 5))
                  
                  # Parse the data - for Model 3, format is: W, Z, Effect, se, t, p, LLCI, ULCI (8 columns)
                  parsed <- tryCatch({
                    # Use read.table with proper column names
                    data_text <- paste(data_lines, collapse = "\n")
                    parsed <- read.table(text = data_text,
                                        col.names = c("W", "Z", "Effect", "se", "t", "p", "LLCI", "ULCI"),
                                        stringsAsFactors = FALSE, 
                                        fill = TRUE, 
                                        blank.lines.skip = TRUE,
                                        row.names = NULL,
                                        na.strings = c("NA", ""))
                    
                    # Convert to numeric
                    for(col in names(parsed)) {
                      parsed[[col]] <- as.numeric(parsed[[col]])
                    }
                    
                    parsed
                  }, error = function(e) {
                    dbg(paste("DEBUG: Error parsing table:", e$message))
                    # Try alternative parsing
                    tryCatch({
                      # Split by whitespace and parse manually
                      lines_split <- strsplit(data_lines, "\\s+")
                      # Filter to lines with at least 8 elements
                      valid_lines <- sapply(lines_split, function(x) length(x) >= 8)
                      if(sum(valid_lines) > 0) {
                        lines_split <- lines_split[valid_lines]
                        # Convert to matrix
                        data_matrix <- do.call(rbind, lapply(lines_split, function(x) {
                          as.numeric(x[1:8])
                        }))
                        as.data.frame(data_matrix, col.names = c("W", "Z", "Effect", "se", "t", "p", "LLCI", "ULCI"))
                      } else {
                        NULL
                      }
                    }, error = function(e2) {
                      dbg(paste("DEBUG: Alternative parsing also failed:", e2$message))
                      NULL
                    })
                  })
                  
                  if(!is.null(parsed) && nrow(parsed) > 0) {
                    dbg(paste("DEBUG: Parsed", nrow(parsed), "rows"))
                    print("First few rows of parsed data:")
                    print(head(parsed, 10))
                    
                    # Validate rows - all 8 columns should be numeric
                    valid_rows <- apply(parsed, 1, function(row) {
                      !any(is.na(row[1:8])) && is.numeric(row[1]) && is.numeric(row[2]) && is.numeric(row[3])
                    })
                    
                    if(sum(valid_rows) > 0) {
                      dbg(paste("DEBUG: Found", sum(valid_rows), "valid rows"))
                      # For conditional effect plot, we need to aggregate by Z
                      # If there are multiple W values per Z, we'll use the median W value's effect
                      parsed_valid <- parsed[valid_rows, ]
                      
                      # Aggregate by Z: for each unique Z, take the effect at median W
                      unique_z <- sort(unique(parsed_valid$Z))
                      cond_effect_data <- data.frame(
                        Z = numeric(length(unique_z)),
                        Effect = numeric(length(unique_z)),
                        se = numeric(length(unique_z)),
                        t = numeric(length(unique_z)),
                        p = numeric(length(unique_z)),
                        LLCI = numeric(length(unique_z)),
                        ULCI = numeric(length(unique_z))
                      )
                      
                      for(i in 1:length(unique_z)) {
                        z_subset <- parsed_valid[parsed_valid$Z == unique_z[i], ]
                        # Use median W value's effect (or mean if multiple)
                        median_w_idx <- which.min(abs(z_subset$W - median(z_subset$W, na.rm = TRUE)))
                        if(length(median_w_idx) == 0) median_w_idx <- 1
                        cond_effect_data[i, ] <- z_subset[median_w_idx, c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")]
                      }
                      
                      dbg("DEBUG: Conditional effect data aggregated by Z:")
                      print(cond_effect_data)
                      # Try to get ranges, but don't fail if it errors
                      tryCatch({
                        z_range_debug <- range(cond_effect_data$Z, na.rm = TRUE)
                        effect_range_debug <- range(cond_effect_data$Effect, na.rm = TRUE)
                        dbg(paste("DEBUG: Z range:", paste(z_range_debug, collapse=" to ")))
                        dbg(paste("DEBUG: Effect range:", paste(effect_range_debug, collapse=" to ")))
                      }, error = function(e) {
                        dbg("DEBUG: Could not calculate ranges (non-fatal)")
                      })
                    } else {
                      dbg("DEBUG: No valid rows found after parsing")
                    }
                  } else {
                    dbg("DEBUG: Parsing returned NULL or empty")
                  }
                } else {
                  dbg("DEBUG: No data lines found matching pattern")
                }
              }
            }
          }
        }  # Close if(is.null(cond_effect_data)) block
          
      }, error = function(e) {
        dbg(paste("DEBUG: Error in conditional effect extraction:", e$message))
        # Don't return here - let the code continue to check if cond_effect_data was set
      })
      
      # If still not found, try "Data for visualizing the conditional effect of the focal predictor:"
      if(is.null(cond_effect_data)) {
        # Ensure process_output is available
        if(!exists("process_output") || is.null(process_output)) {
          process_output <- results$output
        }
        viz_idx <- which(grepl("Data for visualizing the conditional effect of the focal predictor:", process_output, ignore.case = TRUE))
        if(length(viz_idx) > 0) {
          # The data section should start 2 lines after this header (header line + column names line)
          data_start <- viz_idx[1] + 2
          data_end <- min(data_start + 100, length(process_output))  # Increase range to get all data
          potential_data_lines <- process_output[data_start:data_end]
          # Filter to lines that start with numbers (data rows)
          potential_data_lines <- potential_data_lines[grepl("^\\s*-?\\d", potential_data_lines)]
          
          if(length(potential_data_lines) > 0) {
            # Parse as 7 columns: X, W, Z, Y, se, LLCI, ULCI
            parsed_viz <- tryCatch({
              data_text <- paste(potential_data_lines, collapse = "\n")
              parsed_viz <- read.table(text = data_text,
                                      col.names = c("X", "W", "Z", "Y", "se", "LLCI", "ULCI"),
                                      stringsAsFactors = FALSE, 
                                      fill = TRUE, 
                                      blank.lines.skip = TRUE,
                                      na.strings = c("NA", ""))
              
              # Convert to numeric
              for(col in 1:ncol(parsed_viz)) {
                parsed_viz[, col] <- as.numeric(parsed_viz[, col])
              }
              
              parsed_viz
            }, error = function(e) {
              dbg(paste("DEBUG: Error parsing visualization data:", e$message))
              NULL
            })
            
            if(!is.null(parsed_viz) && ncol(parsed_viz) == 7 && nrow(parsed_viz) > 0) {
              dbg(paste("DEBUG: Successfully parsed visualization data with", nrow(parsed_viz), "rows"))
              dbg("DEBUG: First few rows:")
              print(head(parsed_viz, 5))
              
              # For Model 3 conditional effect plot, we need to show the effect of X*W across Z
              # This means we need to hold X and W constant at their median values
              # and plot the predicted Y (or conditional effect) across Z
              
              # Get unique X and W values
              unique_x <- unique(parsed_viz$X)
              unique_w <- unique(parsed_viz$W)
              unique_x <- unique_x[!is.na(unique_x)]
              unique_w <- unique_w[!is.na(unique_w)]
              
              if(length(unique_x) > 0 && length(unique_w) > 0) {
                # Use median X and median W
                x_median <- median(unique_x, na.rm = TRUE)
                w_median <- median(unique_w, na.rm = TRUE)
                
                # Find rows where X and W are closest to their medians
                x_distances <- abs(parsed_viz$X - x_median)
                w_distances <- abs(parsed_viz$W - w_median)
                min_x_dist <- min(x_distances, na.rm = TRUE)
                min_w_dist <- min(w_distances, na.rm = TRUE)
                tolerance_x <- max(0.01, min_x_dist * 1.5)
                tolerance_w <- max(0.01, min_w_dist * 1.5)
                
                rows_at_median <- x_distances <= tolerance_x & w_distances <= tolerance_w
                
                if(sum(rows_at_median) > 0) {
                  # Extract Z, Y, se, LLCI, ULCI for rows at median X and W
                  cond_effect_data <- parsed_viz[rows_at_median, c("Z", "Y", "se", "LLCI", "ULCI")]
                  # Rename Y to Effect for consistency with plot code
                  colnames(cond_effect_data) <- c("Z", "Effect", "se", "LLCI", "ULCI")
                  
                  # Add placeholder columns for t and p (not needed for plotting)
                  cond_effect_data$t <- NA
                  cond_effect_data$p <- NA
                  
                  # Remove any duplicate Z values (keep first occurrence)
                  cond_effect_data <- cond_effect_data[!duplicated(cond_effect_data$Z), ]
                  
                  # Sort by Z to ensure smooth line
                  cond_effect_data <- cond_effect_data[order(cond_effect_data$Z), ]
                  
                  # Validate that we have valid data
                  valid_rows <- !is.na(cond_effect_data$Z) & !is.na(cond_effect_data$Effect) & 
                                is.finite(cond_effect_data$Z) & is.finite(cond_effect_data$Effect)
                  if(sum(valid_rows) >= 3) {
                    cond_effect_data <- cond_effect_data[valid_rows, ]
                    dbg(paste("DEBUG: Extracted conditional effect data from visualization section"))
                    dbg(paste("DEBUG: Using X =", x_median, "and W =", w_median))
                    dbg(paste("DEBUG: Found", nrow(cond_effect_data), "unique Z values"))
                    z_range <- range(cond_effect_data$Z, na.rm = TRUE)
                    effect_range <- range(cond_effect_data$Effect, na.rm = TRUE)
                    dbg(paste("DEBUG: Z range:", paste(z_range, collapse=" to ")))
                    dbg(paste("DEBUG: Effect range:", paste(effect_range, collapse=" to ")))
                  } else {
                    dbg("DEBUG: Not enough valid rows after filtering")
                    cond_effect_data <- NULL
                  }
                } else {
                  dbg("DEBUG: No rows found at median X and W")
                  cond_effect_data <- NULL
                }
              } else {
                dbg("DEBUG: Could not determine unique X or W values")
                cond_effect_data <- NULL
              }
            } else {
              dbg("DEBUG: Failed to parse visualization data as 7 columns")
              cond_effect_data <- NULL
            }
          } else {
            dbg("DEBUG: No data lines found in visualization section")
            cond_effect_data <- NULL
          }
        }
      }
      
      # After tryCatch, check if we have conditional effect data
      if(is.null(cond_effect_data)) {
        return()
      }
      
      # Use the conditional effect data
      plot_data <- cond_effect_data
      active_moderator <- input$moderator2_var
      dbg("DEBUG: Successfully assigned cond_effect_data to plot_data")
      dbg(paste("DEBUG: plot_data has", nrow(plot_data), "rows and", ncol(plot_data), "columns"))
      dbg(paste("DEBUG: plot_data column names:", paste(names(plot_data), collapse=", ")))
    } else {
      # Single moderation: Parse "Data for visualizing" table from text output
      # This table contains exactly 9 rows (3 moderator levels × 3 X values) with 6 columns
      active_moderator <- input$moderator_var
      plot_data <- NULL
      
      tryCatch({
        process_output <- results$output
        viz_idx <- which(grepl("Data for visualizing the conditional effect of the focal predictor:", process_output, ignore.case = TRUE))
        
        if(length(viz_idx) > 0) {
          # The data section should start 2 lines after this header (header line + column names line)
          data_start <- viz_idx[1] + 2
          data_end <- min(data_start + 15, length(process_output))  # Should be exactly 9 rows
          potential_data_lines <- process_output[data_start:data_end]
          
          # Filter to lines that start with numbers (data rows)
          potential_data_lines <- potential_data_lines[grepl("^\\s*-?\\d", potential_data_lines)]
          
          if(length(potential_data_lines) >= 9) {
            # Parse as 6 columns: X, W, Y, se, LLCI, ULCI
            parsed_viz <- tryCatch({
              data_text <- paste(potential_data_lines, collapse = "\n")
              parsed_viz <- read.table(text = data_text,
                                      col.names = c("X", "W", "Y", "se", "LLCI", "ULCI"),
                                      stringsAsFactors = FALSE, 
                                      fill = TRUE, 
                                      blank.lines.skip = TRUE,
                                      na.strings = c("NA", ""))
              
              # Convert to numeric
              for(col in 1:ncol(parsed_viz)) {
                parsed_viz[, col] <- as.numeric(parsed_viz[, col])
              }
              
              parsed_viz
            }, error = function(e) {
              dbg(paste("DEBUG: Error parsing visualization data from text:", e$message))
              NULL
            })
            
            if(!is.null(parsed_viz) && ncol(parsed_viz) == 6 && nrow(parsed_viz) >= 9) {
              # Take first 9 rows (in case there are more)
              parsed_viz <- parsed_viz[1:9, ]
              
              dbg(paste("DEBUG: Successfully parsed visualization data from text with", nrow(parsed_viz), "rows"))
              
              plot_data <- data.frame(
                Predictor = parsed_viz$X,
                Moderator = parsed_viz$W,
                Outcome = parsed_viz$Y,
                LLCI = parsed_viz$LLCI,
                ULCI = parsed_viz$ULCI
              )
              
              # Remove any rows with NA values
              plot_data <- plot_data[complete.cases(plot_data), ]
              
              if(nrow(plot_data) == 0) {
                plot.new()
                text(0.5, 0.5, "Parsed visualization data contains invalid values.", cex = 1.2)
                return()
              }
              
              # Extract moderator levels directly from parsed data (should be exactly 3 unique values)
              moderator_levels_raw <- sort(unique(plot_data$Moderator))
              
              if(length(moderator_levels_raw) == 0) {
                plot.new()
                text(0.5, 0.5, "Could not extract moderator levels from visualization data.", cex = 1.2)
                return()
              }
              
            } else {
              dbg(paste("DEBUG: Parsed visualization data has wrong dimensions. Expected 9 rows × 6 columns, found", 
                         ifelse(is.null(parsed_viz), "NULL", nrow(parsed_viz)), "rows ×", 
                         ifelse(is.null(parsed_viz), "NULL", ncol(parsed_viz)), "columns."))
            }
          } else {
            dbg(paste("DEBUG: Found only", length(potential_data_lines), "data lines, expected at least 9"))
          }
        } else {
          dbg("DEBUG: Could not find 'Data for visualizing' section in PROCESS output")
        }
      }, error = function(e) {
        dbg(paste("DEBUG: Error trying to parse visualization data from text:", e$message))
      })
      
      if(is.null(plot_data) || nrow(plot_data) == 0) {
        plot.new()
        text(0.5, 0.5, "Could not parse visualization data from PROCESS text output.\nPlease check that the analysis completed successfully.", cex = 1.2)
        return()
      }
      
      # Round for display
      moderator_levels <- round(moderator_levels_raw, input$decimal_places)
    }
    
    # Verify plot_data exists before using it
    if(is.null(plot_data) || !is.data.frame(plot_data) || nrow(plot_data) == 0) {
      return()
    }
    
    # Create plot labels and determine plot type
    if(has_second_mod && !is.null(input$moderator2_var) && input$moderator2_var != "") {
      # For Model 3: Determine which plot type to render
      plot_type <- if(!is.null(plot_type_override)) plot_type_override else "stacked"
      
      if(plot_type == "stacked") {
        # Stacked Simple Slopes Plot: Parse "Data for visualizing" table and create 3 plots (one per Z level)
        stacked_data <- NULL
        
        tryCatch({
          process_output <- results$output
          viz_idx <- which(grepl("Data for visualizing the conditional effect of the focal predictor:", process_output, ignore.case = TRUE))
          
          if(length(viz_idx) > 0) {
            data_start <- viz_idx[1] + 2
            data_end <- min(data_start + 30, length(process_output))  # Model 3 has more rows (18 for 3 X × 3 W × 2 Z)
            potential_data_lines <- process_output[data_start:data_end]
            potential_data_lines <- potential_data_lines[grepl("^\\s*-?\\d", potential_data_lines)]
            
            if(length(potential_data_lines) > 0) {
              # Parse as 7 columns: X, W, Z, Y, se, LLCI, ULCI
              parsed_viz <- tryCatch({
                data_text <- paste(potential_data_lines, collapse = "\n")
                parsed_viz <- read.table(text = data_text,
                                        col.names = c("X", "W", "Z", "Y", "se", "LLCI", "ULCI"),
                                        stringsAsFactors = FALSE, 
                                        fill = TRUE, 
                                        blank.lines.skip = TRUE,
                                        na.strings = c("NA", ""))
                
                for(col in 1:ncol(parsed_viz)) {
                  parsed_viz[, col] <- as.numeric(parsed_viz[, col])
                }
                parsed_viz
              }, error = function(e) {
                dbg(paste("DEBUG: Error parsing stacked plot data:", e$message))
                NULL
              })
              
              if(!is.null(parsed_viz) && ncol(parsed_viz) == 7 && nrow(parsed_viz) > 0) {
                stacked_data <- data.frame(
                  Predictor = parsed_viz$X,
                  Moderator = parsed_viz$W,
                  Z = parsed_viz$Z,
                  Outcome = parsed_viz$Y,
                  LLCI = parsed_viz$LLCI,
                  ULCI = parsed_viz$ULCI
                )
                stacked_data <- stacked_data[complete.cases(stacked_data), ]
              }
            }
          }
        }, error = function(e) {
          dbg(paste("DEBUG: Error parsing stacked plot data:", e$message))
        })
        
        if(is.null(stacked_data) || nrow(stacked_data) == 0) {
          plot.new()
          text(0.5, 0.5, "Could not parse visualization data for stacked plot.", cex = 1.2)
          return()
        }
        
        # Get unique Z values
        unique_z <- sort(unique(stacked_data$Z))
        if(length(unique_z) == 0) {
          plot.new()
          text(0.5, 0.5, "No Z values found in visualization data.", cex = 1.2)
          return()
        }
        
        # Get unique W values for each Z
        unique_w <- sort(unique(stacked_data$Moderator))
        moderator_levels_raw <- unique_w
        
        # Create plots for each Z level
        plot_list <- list()
        z_label_text <- if(!is.null(input$moderator2_label) && input$moderator2_label != "") input$moderator2_label else input$moderator2_var
        x_label_text <- if(input$x_label != "") input$x_label else input$predictor_var
        y_label_text <- if(input$y_label != "") input$y_label else input$outcome_var
        mod_label_text <- if(input$moderator_label != "") input$moderator_label else paste0(input$moderator_var, " Levels")
        
        for(i in 1:length(unique_z)) {
          z_val <- unique_z[i]
          z_subset <- stacked_data[stacked_data$Z == z_val, ]
          
          if(nrow(z_subset) == 0) next
          
          # Round Z value for display
          z_display <- round(z_val, input$decimal_places)
          
          # Map moderator values to factors
          z_subset$Moderator_factor <- factor(z_subset$Moderator, levels = moderator_levels_raw, 
                                             labels = format(round(moderator_levels_raw, input$decimal_places), nsmall = input$decimal_places))
          
          p_sub <- ggplot(z_subset, aes(
            x = Predictor,
            y = Outcome,
            color = if(input$use_color_lines) Moderator_factor else NULL,
            linetype = if(!input$use_color_lines) Moderator_factor else NULL,
            fill = if(input$show_confidence_intervals && input$use_color_lines) Moderator_factor else NULL
          )) +
            {if(input$show_confidence_intervals && !all(is.na(z_subset$LLCI))) 
              geom_ribbon(aes(ymin = LLCI, ymax = ULCI), alpha = 0.2, color = NA)} +
            geom_line(linewidth = 1) +
            {if(input$custom_y_axis) coord_cartesian(ylim = c(input$y_axis_min, input$y_axis_max))} +
            labs(
              title = paste0("Simple Slopes at ", z_label_text, " = ", z_display),
              x = x_label_text,
              y = y_label_text,
              color = if(input$use_color_lines) mod_label_text else NULL,
              linetype = if(!input$use_color_lines) mod_label_text else NULL,
              fill = if(input$show_confidence_intervals && input$use_color_lines) mod_label_text else NULL
            ) +
            theme_minimal() +
            theme(
              text = element_text(size = 12),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              plot.title = element_text(size = 14, hjust = 0.5),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 12),
              axis.line = element_line(color = "black", linewidth = 0.5),
              axis.ticks = element_line(color = "black", linewidth = 0.5)
            )
          
          if(!input$show_confidence_intervals || !input$use_color_lines) {
            p_sub <- p_sub + guides(fill = "none")
          }
          
          plot_list[[i]] <- p_sub
        }
        
        # Arrange plots vertically using grid.arrange
        if(length(plot_list) > 0) {
          library(gridExtra)
          p <- do.call(grid.arrange, c(plot_list, ncol = 1))
        } else {
          plot.new()
          text(0.5, 0.5, "No plots generated for stacked view.", cex = 1.2)
          return()
        }
        
      } else {
        # Conditional effect plot (default)
        # Verify we have the required columns
        if(!"Z" %in% names(plot_data) || !"Effect" %in% names(plot_data)) {
          return()
        }
        
        # For Model 3, X-axis should be Z (second moderator), not W (first moderator)
        # Use moderator2_label if provided, otherwise use moderator2_var
        x_label_text <- if(!is.null(input$moderator2_label) && input$moderator2_label != "") input$moderator2_label else input$moderator2_var
        y_label_text <- paste("Conditional effect of", if(input$x_label != "") input$x_label else input$predictor_var, 
                             "*", if(input$moderator_label != "") input$moderator_label else input$moderator_var,
                             "on", if(input$y_label != "") input$y_label else input$outcome_var)
        plot_title <- "Conditional Effect Plot"
        
        # Limit Z range to actual data range
        z_range <- range(plot_data$Z, na.rm = TRUE)
        
        # Create the conditional effect plot
        # Show confidence intervals if checkbox is checked
        p <- ggplot(plot_data, aes(x = Z, y = Effect)) +
          {if(input$show_confidence_intervals && !all(is.na(plot_data$LLCI)) && !all(is.na(plot_data$ULCI))) 
            geom_ribbon(aes(ymin = LLCI, ymax = ULCI), alpha = 0.2, fill = if(input$use_color_lines) "red" else "grey50", show.legend = FALSE)} +
          geom_hline(yintercept = 0, linetype = "dashed", color = if(input$use_color_lines) "red" else "black", linewidth = 0.8) +
          geom_line(color = if(input$use_color_lines) "red" else "black", linewidth = 1.2) +
          {if(input$custom_y_axis) coord_cartesian(ylim = c(input$y_axis_min, input$y_axis_max))} +
          {if(!input$custom_y_axis) coord_cartesian(xlim = z_range)} +
          labs(
            title = plot_title,
            x = x_label_text,
            y = y_label_text
          ) +
          theme_minimal() +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
      }
    } else {
      # Single moderation: Simple slopes plot
      # Limit predictor range to actual data range in plot_data (PROCESS uses percentiles)
      predictor_range <- range(plot_data$Predictor, na.rm = TRUE)
      
      y_label_text <- if(outcome_is_binary) {
        if(input$y_label != "") paste(input$y_label, "(Probability)") else "Predicted Probability"
      } else {
        if(input$y_label != "") input$y_label else input$outcome_var
      }
      
      x_label_text <- if(input$x_label != "") input$x_label else input$predictor_var
      mod_label_text <- if(input$moderator_label != "") input$moderator_label else paste0(active_moderator, " Levels")
      
      plot_title <- "Simple Slopes Plot"
      
      # Single moderation: single plot with 3 lines
      # Map moderator values to factors using the raw values, but display rounded labels
      plot_data$Moderator_factor <- factor(plot_data$Moderator, levels = moderator_levels_raw, labels = format(moderator_levels, nsmall = input$decimal_places))
      
      p <- ggplot(plot_data, aes(
        x = Predictor, 
        y = Outcome,
        color = if(input$use_color_lines) Moderator_factor else NULL,
        linetype = if(!input$use_color_lines) Moderator_factor else NULL,
        fill = if(input$show_confidence_intervals && input$use_color_lines) Moderator_factor else NULL
      )) +
        {if(input$show_confidence_intervals && !all(is.na(plot_data$LLCI))) 
          geom_ribbon(aes(ymin = LLCI, ymax = ULCI), alpha = 0.2, color = NA)} +
        geom_line(linewidth = 1) +
        {if(input$custom_y_axis) coord_cartesian(ylim = c(input$y_axis_min, input$y_axis_max))} +
        {if(!input$custom_y_axis && outcome_is_binary) coord_cartesian(ylim = c(0, 1))} +
        {if(!input$custom_y_axis && !outcome_is_binary) coord_cartesian(xlim = predictor_range)} +
        labs(
          title = plot_title,
          x = x_label_text,
          y = y_label_text,
          color = if(input$use_color_lines) mod_label_text else NULL,
          linetype = if(!input$use_color_lines) mod_label_text else NULL,
          fill = if(input$show_confidence_intervals && input$use_color_lines) mod_label_text else NULL
        ) +
        theme_minimal() +
        theme(
          text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          axis.line = element_line(color = "black", linewidth = 0.5),
          axis.ticks = element_line(color = "black", linewidth = 0.5)
        )
      
      # Suppress fill legend if not using color lines or not showing CIs
      if(!input$show_confidence_intervals || !input$use_color_lines) {
        p <- p + guides(fill = "none")
      }
    }
    
    print(p)
  }
  
  output$slopes_plot <- renderPlot({
    # Only allow plots for Models 1 and 3
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    if(!supports_plot_model(model_num)) {
      plot.new()
      text(0.5, 0.5, "Plots are only available for Models 1 and 3.", cex = 1.2)
      return()
    }
    
    # Check if we have valid analysis results
    if(is.null(analysis_results())) {
      return()
    }
    
    req(input$moderator_var)
    req(input$predictor_var)
    
    results <- analysis_results()
    
    # For Model 1, use simple slopes plot
    if(model_num == 1) {
      p <- generate_simple_slopes_plot(results, input)
      if(is.null(p)) {
        plot.new()
        text(0.5, 0.5, "Could not generate simple slopes plot.\nPlease run the analysis again.", cex = 1.2)
        return()
      }
      print(p)
    } else if(model_num == 3) {
      # For Model 3, use stacked slopes plot
      p <- generate_stacked_slopes_plot(results, input)
      if(is.null(p)) {
        plot.new()
        text(0.5, 0.5, "Could not generate stacked slopes plot.\nPlease run the analysis again.", cex = 1.2)
        return()
      }
      grid::grid.draw(p)
    }
  })
  
  output$stacked_slopes_plot <- renderPlot({
    # Only allow plots for Model 3
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    if(model_num != 3) {
      return()
    }
    
    # Check if we have valid analysis results
    if(is.null(analysis_results())) {
      return()
    }
    
    req(input$moderator_var)
    req(input$predictor_var)
    
    results <- analysis_results()
    p <- generate_stacked_slopes_plot(results, input)
    
    if(is.null(p)) {
      plot.new()
      text(0.5, 0.5, "Could not generate stacked slopes plot.\nPlease run the analysis again.", cex = 1.2)
      return()
    }
    
    grid::grid.draw(p)
  })
  
  output$conditional_effect_plot <- renderPlot({
    # Only allow plots for Models 1 and 3
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    if(!supports_plot_model(model_num)) {
      plot.new()
      text(0.5, 0.5, "Plots are only available for Models 1 and 3.", cex = 1.2)
      return()
    }
    
    # Check if we have valid analysis results
    if(is.null(analysis_results())) {
      return()
    }
    
    req(input$moderator_var)
    req(input$predictor_var)
    
    # Generate plot using helper function
    results <- analysis_results()
    p <- generate_conditional_effect_plot(results, input)
    
    if(is.null(p)) {
      # Show appropriate message if plot couldn't be generated
      if(is.null(results) || is.null(results$settings)) {
        return()
      }
      model_num <- as.numeric(input$process_model)
      if(model_num != 3) {
        return()
      }
      if(results$settings$model != model_num) {
        return()
      }
      if(results$settings$predictor_var != input$predictor_var ||
         results$settings$outcome_var != input$outcome_var ||
         results$settings$moderator_var != input$moderator_var) {
        plot.new()
        text(0.5, 0.5, "Variables have changed.\nPlease run the analysis again.", cex = 1.2)
        return()
      }
      return()
    }
    
    print(p)
  })
  
  # Johnson-Neyman Plot
  output$jn_plot <- renderPlot({
    # Only allow plots for Models 1 and 3
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    if(!supports_plot_model(model_num)) {
      plot.new()
      text(0.5, 0.5, "Plots are only available for Models 1 and 3.", cex = 1.2)
      return()
    }
    
    # Check if we have valid analysis results for the current model/variables
    if(is.null(analysis_results())) {
      plot.new()
      text(0.5, 0.5, "Please run the analysis first.", cex = 1.2)
      return()
    }
    
    req(input$moderator_var)
    
    # Generate plot using helper function
    results <- analysis_results()
    p <- generate_jn_plot(results, input, jn_available)
    
    if(is.null(p)) {
      # Show appropriate message based on why plot couldn't be generated
      if(is.null(results) || is.null(results$settings)) {
        return()
      }
      model_num <- as.numeric(input$process_model)
      if(model_num != 1) {
        return()
      }
      if(!jn_available()) {
        return()
      }
      if(results$settings$model != model_num) {
        return()
      }
      if(results$settings$predictor_var != input$predictor_var ||
         results$settings$outcome_var != input$outcome_var ||
         results$settings$moderator_var != input$moderator_var) {
        plot.new()
        text(0.5, 0.5, "Variables have changed.\nPlease run the analysis again.", cex = 1.2)
        return()
      }
      return()
    }
    
    print(p)
  })
  
  # Observe JN availability to enable/disable download button
  observe({
    shinyjs::toggleState("download_jn", condition = jn_available() && !is.null(analysis_results()))
  })
  
  # Collapse menu sections when Plots tab is selected
  observeEvent(input$tabset_panel, {
    if(!is.null(input$tabset_panel) && input$tabset_panel == "Plots") {
      # Collapse all sections above Plot Options
      shinyjs::runjs("
        var sections = ['details_select_vars', 'details_assumption_checks', 
                        'details_basic_options', 'details_bootstrap_options',
                        'details_advanced_options', 'details_output_options', 
                        'details_probing_moderation'];
        sections.forEach(function(id) {
          var elem = document.getElementById(id);
          if(elem) elem.removeAttribute('open');
        });
      ")
    }
  })
  
  # Download handlers for plots
  output$download_jn <- downloadHandler(
    filename = function() {
      model_txt <- if(!is.null(input$process_model) && input$process_model != "") input$process_model else "unknown"
      paste0("model_", model_txt, "_jn_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
    },
    contentType = "image/jpeg",
    content = function(file) {
      tryCatch({
        # Only allow downloads for Models 1 and 3
        req(input$process_model)
        model_num <- as.numeric(input$process_model)
        if(!supports_plot_model(model_num)) {
          stop("Plots are only available for Models 1 and 3.")
        }
        
        req(analysis_results())
        req(input$moderator_var)
        
        # Generate plot using helper function
        results <- analysis_results()
        p <- generate_jn_plot(results, input, jn_available)
        
        if(is.null(p)) {
          stop("Could not generate JN plot. Please ensure analysis has been run and JN technique is enabled.")
        }
        
        ggsave(file, plot = p, device = "jpeg", width = 10, height = 8, dpi = 600, units = "in", bg = "white")
      }, error = function(e) {
        print(paste("Error saving JN plot:", e$message))
        # Create an error message file instead
        writeLines(paste("Error generating JN plot:", e$message), file)
      })
    }
  )
  
  output$download_slopes <- downloadHandler(
    filename = function() {
      # Determine if this is a conditional effect plot or simple slopes plot
      model_num <- tryCatch(as.numeric(input$process_model), error = function(e) NULL)
      model_txt <- if(!is.null(input$process_model) && input$process_model != "") input$process_model else "unknown"
      has_second_mod <- !is.null(model_num) && has_second_moderator_model(model_num)
      if(has_second_mod && !is.null(input$moderator2_var) && input$moderator2_var != "") {
        paste0("model_", model_txt, "_stacked_simple_slopes_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
      } else {
        paste0("model_", model_txt, "_simple_slopes_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
      }
    },
    contentType = "image/jpeg",
    content = function(file) {
      tryCatch({
        # Only allow downloads for Models 1 and 3
        req(input$process_model)
        model_num <- as.numeric(input$process_model)
        if(!supports_plot_model(model_num)) {
          stop("Plots are only available for Models 1 and 3.")
        }
        
        req(analysis_results())
        req(input$moderator_var)
        req(input$predictor_var)
        
        results <- analysis_results()
        
        # Determine model type
        has_second_mod <- has_second_moderator_model(model_num)
        
        # For Model 3, use stacked slopes plot helper
        if(has_second_mod && !is.null(input$moderator2_var) && input$moderator2_var != "") {
          p <- generate_stacked_slopes_plot(results, input)
          
          if(is.null(p)) {
            stop("Could not generate stacked slopes plot. Please ensure analysis has been run for Model 3.")
          }
          
          # Calculate height based on number of Z levels
          process_output <- results$output
          viz_idx <- which(grepl("Data for visualizing the conditional effect of the focal predictor:", process_output, ignore.case = TRUE))
          plot_height <- 8
          if(length(viz_idx) > 0) {
            data_start <- viz_idx[1] + 2
            data_end <- min(data_start + 30, length(process_output))
            potential_data_lines <- process_output[data_start:data_end]
            potential_data_lines <- potential_data_lines[grepl("^\\s*-?\\d", potential_data_lines)]
            if(length(potential_data_lines) > 0) {
              parsed_viz <- tryCatch({
                data_text <- paste(potential_data_lines, collapse = "\n")
                parsed_viz <- read.table(text = data_text,
                                        col.names = c("X", "W", "Z", "Y", "se", "LLCI", "ULCI"),
                                        stringsAsFactors = FALSE, fill = TRUE, blank.lines.skip = TRUE,
                                        row.names = NULL, na.strings = c("NA", ""))
                for(col in 1:ncol(parsed_viz)) parsed_viz[, col] <- as.numeric(parsed_viz[, col])
                parsed_viz
              }, error = function(e) NULL)
              if(!is.null(parsed_viz) && ncol(parsed_viz) == 7 && nrow(parsed_viz) > 0) {
                unique_z <- sort(unique(parsed_viz$Z))
                plot_height <- 8 * length(unique_z)
              }
            }
          }
          
          grDevices::jpeg(file, width = 10, height = plot_height, units = "in", res = 600, bg = "white")
          on.exit(grDevices::dev.off(), add = TRUE)
          grid::grid.draw(p)
        } else {
          # Single moderation (Model 1): Use helper function
          p <- generate_simple_slopes_plot(results, input)
          
          if(is.null(p)) {
            stop("Could not generate simple slopes plot. Please ensure analysis has been run for Model 1.")
          }
          
          ggsave(file, plot = p, device = "jpeg", width = 10, height = 8, dpi = 600, units = "in", bg = "white")
        }
      }, error = function(e) {
        print(paste("Error saving slopes plot:", e$message))
        # Create an error message file instead
        writeLines(paste("Error generating plot:", e$message), file)
      })
    }
  )
  
  output$download_stacked_slopes <- downloadHandler(
    filename = function() {
      model_txt <- if(!is.null(input$process_model) && input$process_model != "") input$process_model else "unknown"
      paste0("model_", model_txt, "_stacked_simple_slopes_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
    },
    contentType = "image/jpeg",
    content = function(file) {
      tryCatch({
        req(input$process_model)
        model_num <- as.numeric(input$process_model)
        if(model_num != 3) {
          stop("Stacked simple slopes plot is only available for Model 3.")
        }
        
        req(analysis_results())
        req(input$moderator_var)
        req(input$predictor_var)
        
        results <- analysis_results()
        p <- generate_stacked_slopes_plot(results, input)
        
        if(is.null(p)) {
          stop("Could not generate stacked slopes plot. Please ensure analysis has been run for Model 3.")
        }
        
        # Calculate height based on number of Z levels
        process_output <- results$output
        viz_idx <- which(grepl("Data for visualizing the conditional effect of the focal predictor:", process_output, ignore.case = TRUE))
        if(length(viz_idx) > 0) {
          data_start <- viz_idx[1] + 2
          data_end <- min(data_start + 30, length(process_output))
          potential_data_lines <- process_output[data_start:data_end]
          potential_data_lines <- potential_data_lines[grepl("^\\s*-?\\d", potential_data_lines)]
          if(length(potential_data_lines) > 0) {
            parsed_viz <- tryCatch({
              data_text <- paste(potential_data_lines, collapse = "\n")
              parsed_viz <- read.table(text = data_text,
                                      col.names = c("X", "W", "Z", "Y", "se", "LLCI", "ULCI"),
                                      stringsAsFactors = FALSE, fill = TRUE, blank.lines.skip = TRUE,
                                      row.names = NULL, na.strings = c("NA", ""))
              for(col in 1:ncol(parsed_viz)) parsed_viz[, col] <- as.numeric(parsed_viz[, col])
              parsed_viz
            }, error = function(e) NULL)
            if(!is.null(parsed_viz) && ncol(parsed_viz) == 7 && nrow(parsed_viz) > 0) {
              unique_z <- sort(unique(parsed_viz$Z))
              plot_height <- 8 * length(unique_z)
            } else {
              plot_height <- 8
            }
          } else {
            plot_height <- 8
          }
        } else {
          plot_height <- 8
        }
        
        grDevices::jpeg(file, width = 10, height = plot_height, units = "in", res = 600, bg = "white")
        on.exit(grDevices::dev.off(), add = TRUE)
        grid::grid.draw(p)
      }, error = function(e) {
        print(paste("Error saving stacked slopes plot:", e$message))
        writeLines(paste("Error generating plot:", e$message), file)
      })
    }
  )
  
  output$download_conditional_effect <- downloadHandler(
    filename = function() {
      model_txt <- if(!is.null(input$process_model) && input$process_model != "") input$process_model else "unknown"
      paste0("model_", model_txt, "_conditional_effect_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
    },
    contentType = "image/jpeg",
    content = function(file) {
      tryCatch({
        req(input$process_model)
        model_num <- as.numeric(input$process_model)
        if(model_num != 3) {
          stop("Conditional effect plot is only available for Model 3.")
        }
        
        req(analysis_results())
        req(input$moderator_var)
        req(input$predictor_var)
        
        # Generate plot using helper function
        results <- analysis_results()
        p <- generate_conditional_effect_plot(results, input)
        
        if(is.null(p)) {
          stop("Could not generate conditional effect plot. Please ensure analysis has been run for Model 3.")
        }
        
        ggsave(file, plot = p, device = "jpeg", width = 10, height = 8, dpi = 600, units = "in", bg = "white")
      }, error = function(e) {
        print(paste("Error saving conditional effect plot:", e$message))
        writeLines(paste("Error generating plot:", e$message), file)
      })
    }
  )
  
  # Disable plot download buttons until analysis is run
  observe({
    tryCatch({
      has_results <- !is.null(analysis_results())
      shinyjs::toggleState("download_slopes", condition = has_results)
      shinyjs::toggleState("download_stacked_slopes", condition = has_results)
      shinyjs::toggleState("download_conditional_effect", condition = has_results)
      shinyjs::toggleState("download_assumptions", condition = !is.null(input$outcome_var) && input$outcome_var != "")
    }, error = function(e) {
      shinyjs::disable("download_slopes")
      shinyjs::disable("download_stacked_slopes")
      shinyjs::disable("download_conditional_effect")
      shinyjs::disable("download_assumptions")
    })
  })
  
  # Auto-populate plot labels from selected variables (only for moderation models)
  # This automatically updates labels when variables are selected, matching the old app behavior
  # Users can then edit these labels if they want clearer/more descriptive text
  # SKIP this observer when loading settings to prevent overwriting saved labels
  observe({
    # Don't auto-update labels when loading settings or during clearing
    if(isTRUE(rv$load_settings_pending) || isTRUE(rv$is_clearing) || isTRUE(rv$restore_labels_pending)) {
      dbg("DEBUG: Auto-label observer SKIPPED - load_settings_pending, is_clearing, or restore_labels_pending is TRUE")
      return()
    }
    
    if(!is.null(input$process_model) && input$process_model %in% c("1", "2", "3", "5", "14", "15", "58", "59", "74")) {
      # CRITICAL: Don't update labels if variables are empty (e.g., during model change clearing)
      # This prevents labels from being updated with old variable values before they're cleared
      if(is.null(input$predictor_var) || input$predictor_var == "" ||
         is.null(input$outcome_var) || input$outcome_var == "" ||
         is.null(input$moderator_var) || input$moderator_var == "") {
        dbg("DEBUG: Auto-label observer SKIPPED - variables are empty (likely during model change)")
        return()
      }
      
      # Only update if all required variables are selected (matching old app req() behavior)
      if(!is.null(input$predictor_var) && input$predictor_var != "" &&
         !is.null(input$outcome_var) && input$outcome_var != "" &&
         !is.null(input$moderator_var) && input$moderator_var != "") {
        
        # Check if variables have changed from their previous values
        predictor_changed <- is.null(rv$previous_predictor_var) || rv$previous_predictor_var != input$predictor_var
        outcome_changed <- is.null(rv$previous_outcome_var) || rv$previous_outcome_var != input$outcome_var
        moderator_changed <- is.null(rv$previous_moderator_var) || rv$previous_moderator_var != input$moderator_var
        
        # Check if labels are empty
        x_label_empty <- is.null(input$x_label) || input$x_label == ""
        y_label_empty <- is.null(input$y_label) || input$y_label == ""
        mod_label_empty <- is.null(input$moderator_label) || input$moderator_label == ""
        
        # Check if labels match current variables
        x_label_matches_current <- !x_label_empty && input$x_label == input$predictor_var
        y_label_matches_current <- !y_label_empty && input$y_label == input$outcome_var
        mod_label_matches_current <- !mod_label_empty && input$moderator_label == input$moderator_var
        
        # CRITICAL: Only update labels if:
        # 1. Label is empty (needs to be populated), OR
        # 2. Variable changed (user selected new variable from dropdown - update label to match)
        # If variable didn't change, DON'T update label (let customized labels stick)
        should_update_x <- x_label_empty || predictor_changed
        should_update_y <- y_label_empty || outcome_changed
        should_update_mod <- mod_label_empty || moderator_changed
        
        # Debug output
        dbg(paste("DEBUG: Auto-label observer - predictor_changed:", predictor_changed, 
                    "x_label:", input$x_label, "predictor_var:", input$predictor_var,
                    "should_update_x:", should_update_x))
        dbg(paste("DEBUG: Auto-label observer - outcome_changed:", outcome_changed,
                    "y_label:", input$y_label, "outcome_var:", input$outcome_var,
                    "should_update_y:", should_update_y))
        dbg(paste("DEBUG: Auto-label observer - moderator_changed:", moderator_changed,
                    "moderator_label:", input$moderator_label, "moderator_var:", input$moderator_var,
                    "should_update_mod:", should_update_mod))
        
        # Update labels that need updating
        if(should_update_x) {
          dbg(paste("DEBUG: Updating x_label from", input$x_label, "to", input$predictor_var))
          updateTextInput(session, "x_label", value = input$predictor_var)
        }
        if(should_update_y) {
          dbg(paste("DEBUG: Updating y_label from", input$y_label, "to", input$outcome_var))
          updateTextInput(session, "y_label", value = input$outcome_var)
        }
        if(should_update_mod) {
          dbg(paste("DEBUG: Updating moderator_label from", input$moderator_label, "to", input$moderator_var))
          updateTextInput(session, "moderator_label", value = input$moderator_var)
        }
        
        # Update previous variable values to track changes
        rv$previous_predictor_var <- input$predictor_var
        rv$previous_outcome_var <- input$outcome_var
        rv$previous_moderator_var <- input$moderator_var
        
        # For models with second moderator, also auto-populate moderator2_label
        model_num <- as.numeric(input$process_model)
        if(has_second_moderator_model(model_num) && 
           !is.null(input$moderator2_var) && input$moderator2_var != "") {
          mod2_changed <- is.null(rv$previous_moderator2_var) || rv$previous_moderator2_var != input$moderator2_var
          mod2_label_empty <- is.null(input$moderator2_label) || input$moderator2_label == ""
          mod2_label_matches_current <- !mod2_label_empty && input$moderator2_label == input$moderator2_var
          # If moderator2 changed, always update the label
          should_update_mod2 <- mod2_label_empty || mod2_changed || !mod2_label_matches_current
          
          if(should_update_mod2) {
            dbg(paste("DEBUG: Updating moderator2_label from", input$moderator2_label, "to", input$moderator2_var))
            updateTextInput(session, "moderator2_label", value = input$moderator2_var)
          }
          
          rv$previous_moderator2_var <- input$moderator2_var
        } else {
          rv$previous_moderator2_var <- NULL
        }
      }
    }
  })
  
  # Make xmint and xmtest mutually exclusive for Model 4
  observeEvent(input$xmint, {
    if(!is.null(input$process_model) && input$process_model == "4" && input$xmint) {
      # If xmint is checked, uncheck xmtest
      updateCheckboxInput(session, "xmtest", value = FALSE)
    }
  })
  
  observeEvent(input$xmtest, {
    if(!is.null(input$process_model) && input$process_model == "4" && input$xmtest) {
      # If xmtest is checked, uncheck xmint
      updateCheckboxInput(session, "xmint", value = FALSE)
    }
  })
  
  # Button state observers
  observe({
    input$residual_threshold
    input$cooks_threshold_type
    input$cooks_threshold_custom
    input$covariates
    input$outcome_var
    input$predictor_var
    mediator_vars_collected()
    input$moderator_var
    input$moderator2_var
    
    if (is.null(rv$original_dataset) || 
        is.null(input$outcome_var) || input$outcome_var == "" ||
        is.null(input$predictor_var) || input$predictor_var == "" ||
        is.null(input$process_model) || input$process_model == "") {
      shinyjs::disable("run_analysis")
      shinyjs::disable("run_analysis_no_outliers")
    } else {
      shinyjs::enable("run_analysis")
      tryCatch({
        outliers <- identify_outliers()
        if(!is.null(outliers) && outliers$count > 0) {
          reduced_data <- rv$original_dataset[-outliers$cases, ]
          all_vars <- c(input$outcome_var, input$predictor_var)
          mediator_vars_current <- mediator_vars_collected()
          if(!is.null(mediator_vars_current)) all_vars <- c(all_vars, mediator_vars_current)
          if(!is.null(input$moderator_var)) all_vars <- c(all_vars, input$moderator_var)
          if(!is.null(input$covariates) && length(input$covariates) > 0) {
            all_vars <- c(all_vars, input$covariates)
          }
          complete_cases <- complete.cases(reduced_data[all_vars])
          n_complete <- sum(complete_cases)
          
          if(n_complete >= 3) {
            shinyjs::enable("run_analysis_no_outliers")
          } else {
            shinyjs::disable("run_analysis_no_outliers")
          }
        } else {
          shinyjs::disable("run_analysis_no_outliers")
        }
      }, error = function(e) {
        shinyjs::disable("run_analysis_no_outliers")
      })
    }
  })
}

# ============================================================================
# NOTE: Model-specific visualizations (JN plots, simple slopes) can be added
# as a future enhancement by extracting data from PROCESS output or running
# additional analyses. The core functionality is complete and operational.
# ============================================================================

# Run the app
shinyApp(ui, server)

