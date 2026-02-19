# ============================================================================
# MODEL DIAGRAMS MODULE (PARSER SCAFFOLD)
# ============================================================================
# Purpose:
# - Parse PROCESS output into a diagram-friendly path table schema.
# - Provide metadata for downstream rendering and fallback notes.
# - This is a non-UI scaffold used for upcoming Model Diagram tab work.
# ============================================================================

strip_html_tags <- function(x) {
  gsub("<[^>]+>", "", x)
}

normalize_output_line <- function(x) {
  out <- strip_html_tags(x)
  out <- gsub("&nbsp;", " ", out, fixed = TRUE)
  trimws(out)
}

stars_from_p <- function(p) {
  if(is.na(p)) return("")
  if(p < 0.001) return("***")
  if(p < 0.01) return("**")
  if(p < 0.05) return("*")
  ""
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

empty_path_table <- function() {
  data.frame(
    model = integer(0),
    outcome_block = character(0),
    from = character(0),
    to = character(0),
    path_kind = character(0),
    estimate_raw = numeric(0),
    estimate_std = numeric(0),
    se = numeric(0),
    p = numeric(0),
    llci = numeric(0),
    ulci = numeric(0),
    stars = character(0),
    label_raw = character(0),
    label_std = character(0),
    is_available_raw = logical(0),
    is_available_std = logical(0),
    parse_source = character(0),
    note = character(0),
    stringsAsFactors = FALSE
  )
}

build_path_row <- function(model, outcome_block, from, to, path_kind,
                           estimate_raw = NA_real_, estimate_std = NA_real_,
                           se = NA_real_, p = NA_real_, llci = NA_real_, ulci = NA_real_,
                           parse_source = "model_table", note = "") {
  st <- stars_from_p(p)
  label_raw <- if(!is.na(estimate_raw)) {
    paste0("b = ", sprintf("%.3f", estimate_raw), st)
  } else {
    ""
  }
  label_std <- if(!is.na(estimate_std)) {
    paste0("beta = ", sprintf("%.3f", estimate_std), st)
  } else {
    ""
  }
  
  data.frame(
    model = as.integer(model),
    outcome_block = as.character(outcome_block),
    from = as.character(from),
    to = as.character(to),
    path_kind = as.character(path_kind),
    estimate_raw = estimate_raw,
    estimate_std = estimate_std,
    se = se,
    p = p,
    llci = llci,
    ulci = ulci,
    stars = st,
    label_raw = label_raw,
    label_std = label_std,
    is_available_raw = !is.na(estimate_raw),
    is_available_std = !is.na(estimate_std),
    parse_source = as.character(parse_source),
    note = as.character(note),
    stringsAsFactors = FALSE
  )
}

infer_path_kind <- function(var_name, settings) {
  if(identical(var_name, settings$predictor_var)) return("direct")
  if(!is.null(settings$mediator_vars) && var_name %in% settings$mediator_vars) return("mediator")
  if(!is.null(settings$moderator_var) && nzchar(settings$moderator_var) && identical(var_name, settings$moderator_var)) return("moderator")
  if(!is.null(settings$moderator2_var) && nzchar(settings$moderator2_var) && identical(var_name, settings$moderator2_var)) return("moderator")
  if(!is.null(settings$covariates) && var_name %in% settings$covariates) return("covariate")
  if(grepl("^int_[0-9]+$", var_name)) return("interaction")
  "unknown"
}

extract_model_table_rows <- function(lines, start_idx) {
  # Expects:
  # Model:
  #   coeff se t p LLCI ULCI
  #   var   ...
  # Stops at separators or section headers.
  rows <- list()
  
  if(start_idx + 2 > length(lines)) return(rows)
  
  i <- start_idx + 2
  while(i <= length(lines)) {
    line <- normalize_output_line(lines[[i]])
    if(!nzchar(line)) {
      i <- i + 1
      next
    }
    if(grepl("^[-*]{5,}", line) ||
       grepl("^Some regression diagnostics", line, ignore.case = TRUE) ||
       grepl("^Product terms key", line, ignore.case = TRUE) ||
       grepl("^Scale-free and standardized measures of association", line, ignore.case = TRUE) ||
       grepl("^Likelihood ratio test", line, ignore.case = TRUE) ||
       grepl("^Test\\(s\\) of highest order", line, ignore.case = TRUE) ||
       grepl("^Focal predictor:", line, ignore.case = TRUE)) {
      break
    }
    
    tokens <- strsplit(line, "\\s+")[[1]]
    if(length(tokens) >= 7) {
      var_name <- tokens[[1]]
      nums <- safe_numeric(tokens[2:7])
      if(!all(is.na(nums))) {
        rows[[length(rows) + 1]] <- list(
          var = var_name,
          coeff = nums[1],
          se = nums[2],
          stat = nums[3],
          p = nums[4],
          llci = nums[5],
          ulci = nums[6]
        )
      }
    }
    
    i <- i + 1
  }
  
  rows
}

extract_standyx_rows <- function(lines, outcome_start_idx) {
  # Parses optional "Scale-free and standardized measures of association:" block
  # within an outcome block and returns named standYX by variable.
  out <- numeric(0)
  
  upper_bound <- min(length(lines), outcome_start_idx + 220)
  idx <- NA_integer_
  for(i in outcome_start_idx:upper_bound) {
    line <- normalize_output_line(lines[[i]])
    if(grepl("^Scale-free and standardized measures of association:", line, ignore.case = TRUE)) {
      idx <- i
      break
    }
    if(i > outcome_start_idx && grepl("^Outcome Variable:", line, ignore.case = TRUE)) break
  }
  if(is.na(idx)) return(out)
  
  i <- idx + 2
  while(i <= upper_bound) {
    line <- normalize_output_line(lines[[i]])
    if(!nzchar(line)) {
      i <- i + 1
      next
    }
    if(grepl("^[-*]{5,}", line) ||
       grepl("^Product terms key", line, ignore.case = TRUE) ||
       grepl("^Test\\(s\\) of highest order", line, ignore.case = TRUE) ||
       grepl("^Focal predictor:", line, ignore.case = TRUE) ||
       grepl("^Outcome Variable:", line, ignore.case = TRUE)) {
      break
    }
    tokens <- strsplit(line, "\\s+")[[1]]
    # expected: var r sr pr standYX standY standX
    if(length(tokens) >= 7 && !grepl("^eta-sq$", tokens[[2]], ignore.case = TRUE)) {
      var_name <- tokens[[1]]
      standyx <- safe_numeric(tokens[[5]])
      if(!is.na(standyx)) {
        out[var_name] <- standyx
      }
    }
    i <- i + 1
  }
  
  out
}

parse_process_for_model_diagram <- function(analysis_results_obj) {
  if(is.null(analysis_results_obj) || is.null(analysis_results_obj$output) || is.null(analysis_results_obj$settings)) {
    return(list(
      paths = empty_path_table(),
      metadata = list(
        selected_model = NA_integer_,
        stand_requested = FALSE,
        label_mode_used = "raw",
        centering = NA_character_,
        has_covariates = FALSE,
        warnings = "No analysis results available."
      )
    ))
  }
  
  settings <- analysis_results_obj$settings
  lines <- unname(as.character(analysis_results_obj$output))
  
  model_num <- suppressWarnings(as.integer(settings$model))
  stand_requested <- isTRUE(settings$stand)
  centering_value <- if(!is.null(settings$centering)) as.character(settings$centering)[1] else NA_character_
  has_covariates <- !is.null(settings$covariates) && length(settings$covariates) > 0
  
  warnings <- character(0)
  path_rows <- empty_path_table()
  
  outcome_idx <- grep("^\\s*Outcome Variable:", sapply(lines, normalize_output_line), ignore.case = TRUE)
  if(length(outcome_idx) == 0) {
    warnings <- c(warnings, "No 'Outcome Variable' blocks found in PROCESS output.")
  }
  
  for(idx in outcome_idx) {
    outcome_line <- normalize_output_line(lines[[idx]])
    outcome_name <- sub("^\\s*Outcome Variable:\\s*", "", outcome_line, ignore.case = TRUE)
    if(!nzchar(outcome_name)) next
    
    # locate the nearest "Model:" line after outcome heading
    model_line_idx <- NA_integer_
    for(i in (idx + 1):min(length(lines), idx + 80)) {
      line <- normalize_output_line(lines[[i]])
      if(grepl("^Model:\\s*$", line, ignore.case = TRUE)) {
        model_line_idx <- i
        break
      }
      if(grepl("^Outcome Variable:", line, ignore.case = TRUE)) break
    }
    if(is.na(model_line_idx)) next
    
    coeff_rows <- extract_model_table_rows(lines, model_line_idx)
    standyx_map <- extract_standyx_rows(lines, idx)
    
    for(r in coeff_rows) {
      if(identical(r$var, "constant")) next
      
      from_var <- r$var
      to_var <- outcome_name
      kind <- infer_path_kind(r$var, settings)
      std_val <- if(length(standyx_map) > 0 && r$var %in% names(standyx_map)) standyx_map[[r$var]] else NA_real_
      
      path_rows <- rbind(
        path_rows,
        build_path_row(
          model = model_num,
          outcome_block = outcome_name,
          from = from_var,
          to = to_var,
          path_kind = kind,
          estimate_raw = r$coeff,
          estimate_std = std_val,
          se = r$se,
          p = r$p,
          llci = r$llci,
          ulci = r$ulci,
          parse_source = "model_table",
          note = ""
        )
      )
    }
  }
  
  if(nrow(path_rows) == 0) {
    warnings <- c(warnings, "No coefficient rows were parsed from model tables.")
  }
  
  label_mode_used <- if(stand_requested && any(path_rows$is_available_std)) {
    "std"
  } else if(stand_requested && nrow(path_rows) > 0) {
    warnings <- c(warnings, "Standardized labels requested, but standardized path values were not available for one or more paths.")
    "fallback_raw"
  } else {
    "raw"
  }
  
  list(
    paths = path_rows,
    metadata = list(
      selected_model = model_num,
      stand_requested = stand_requested,
      label_mode_used = label_mode_used,
      centering = centering_value,
      has_covariates = has_covariates,
      warnings = warnings
    )
  )
}

# Reactive wrapper available to future diagram UI/server outputs.
diagram_parse_results <- reactive({
  req(analysis_results())
  parse_process_for_model_diagram(analysis_results())
})

