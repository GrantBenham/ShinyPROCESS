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

extract_product_term_aliases <- function(lines) {
  # Parse "Product terms key:" section into a named vector like:
  # c(int_1 = "XW", int_2 = "XZ")
  aliases <- character(0)
  norm_lines <- sapply(lines, normalize_output_line)
  key_idx <- grep("^Product terms key:", norm_lines, ignore.case = TRUE)
  if(length(key_idx) == 0) return(aliases)

  i <- key_idx[[1]] + 1L
  while(i <= length(norm_lines)) {
    line <- norm_lines[[i]]
    if(!nzchar(line)) {
      i <- i + 1L
      next
    }
    if(grepl("^[-*]{5,}", line) ||
       grepl("^Test\\(s\\) of highest order", line, ignore.case = TRUE) ||
       grepl("^Focal predictor:", line, ignore.case = TRUE) ||
       grepl("^Outcome Variable:", line, ignore.case = TRUE)) {
      break
    }

    # Typical forms:
    # int_1 : X x W
    # Int_1 : X*W
    m <- regexec("^([Ii]nt_[0-9]+)\\s*:\\s*(.+)$", line)
    g <- regmatches(line, m)[[1]]
    if(length(g) == 3) {
      int_name <- tolower(trimws(g[[2]]))
      rhs <- trimws(g[[3]])
      parts <- unlist(strsplit(rhs, "\\s*\\*\\s*|\\s+[xX]\\s+"))
      parts <- toupper(trimws(parts))
      parts <- parts[nzchar(parts)]
      if(length(parts) >= 2) {
        aliases[[int_name]] <- paste(parts, collapse = " x ")
      } else if(length(parts) == 1) {
        aliases[[int_name]] <- parts[[1]]
      }
    }
    i <- i + 1L
  }

  aliases
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
  product_aliases <- extract_product_term_aliases(lines)
  
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
      product_aliases = product_aliases,
      warnings = warnings
    )
  )
}

# Reactive wrapper available to future diagram UI/server outputs.
diagram_parse_results <- reactive({
  req(analysis_results())
  parse_process_for_model_diagram(analysis_results())
})

SUPPORTED_DIAGRAM_MODELS <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 14L)

compute_diagram_eligibility <- function(settings) {
  model_num <- suppressWarnings(as.integer(settings$model))
  mediators <- if(!is.null(settings$mediator_vars)) settings$mediator_vars else character(0)
  n_m <- length(mediators)

  if(is.na(model_num) || !(model_num %in% SUPPORTED_DIAGRAM_MODELS)) {
    return(list(
      eligible = FALSE,
      reason = paste0(
        "Model diagrams are currently supported for Models ",
        paste(SUPPORTED_DIAGRAM_MODELS, collapse = ", "),
        " only."
      )
    ))
  }

  if(n_m > 2) {
    return(list(
      eligible = FALSE,
      reason = paste0(
        "Diagram generation is limited to a maximum of 2 mediators for supported models. ",
        "Current analysis has ", n_m, " mediators."
      )
    ))
  }

  list(eligible = TRUE, reason = "")
}

message_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 5.2) +
    ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

build_default_label_map <- function(settings) {
  out <- character(0)
  if(!is.null(settings$predictor_var) && nzchar(settings$predictor_var)) out[[settings$predictor_var]] <- settings$predictor_var
  if(!is.null(settings$outcome_var) && nzchar(settings$outcome_var)) out[[settings$outcome_var]] <- settings$outcome_var
  if(!is.null(settings$moderator_var) && nzchar(settings$moderator_var)) out[[settings$moderator_var]] <- settings$moderator_var
  if(!is.null(settings$moderator2_var) && nzchar(settings$moderator2_var)) out[[settings$moderator2_var]] <- settings$moderator2_var
  if(!is.null(settings$mediator_vars) && length(settings$mediator_vars) > 0) {
    for(m in settings$mediator_vars) {
      if(!is.null(m) && nzchar(m)) out[[m]] <- m
    }
  }
  out
}

sanitize_label_text <- function(x, fallback) {
  val <- trimws(as.character(x))
  if(!nzchar(val)) fallback else val
}

map_interaction_label <- function(raw_label, label_map) {
  lbl <- trimws(as.character(raw_label))
  if(!nzchar(lbl) || is.null(label_map) || length(label_map) == 0) return(lbl)
  parts <- unlist(strsplit(lbl, "\\s+[xX]\\s+"))
  if(length(parts) <= 1) return(lbl)
  mapped <- vapply(parts, function(p) {
    key <- names(label_map)[toupper(names(label_map)) == toupper(trimws(p))]
    if(length(key) > 0) sanitize_label_text(label_map[[key[[1]]]], key[[1]]) else trimws(p)
  }, character(1))
  paste(mapped, collapse = " x ")
}

compose_path_label <- function(edge_row, label_mode = "auto",
                               include_ci = FALSE, include_p = FALSE, include_stars = TRUE) {
  use_mode <- label_mode
  if(identical(use_mode, "auto")) {
    if(isTRUE(edge_row$is_available_std)) {
      use_mode <- "std"
    } else {
      use_mode <- "raw"
    }
  }
  
  est <- if(identical(use_mode, "std")) edge_row$estimate_std else edge_row$estimate_raw
  prefix <- if(identical(use_mode, "std")) "beta" else "b"
  
  if(is.na(est)) return("")
  
  label <- paste0(prefix, " = ", sprintf("%.3f", est))
  if(isTRUE(include_stars) && nzchar(edge_row$stars)) {
    label <- paste0(label, edge_row$stars)
  }
  if(isTRUE(include_ci) && !is.na(edge_row$llci) && !is.na(edge_row$ulci)) {
    label <- paste0(label, "\n[", sprintf("%.3f", edge_row$llci), ", ", sprintf("%.3f", edge_row$ulci), "]")
  }
  if(isTRUE(include_p) && !is.na(edge_row$p)) {
    p_txt <- if(edge_row$p < 0.001) "p < .001" else paste0("p = ", sprintf("%.3f", edge_row$p))
    label <- paste0(label, "\n", p_txt)
  }
  label
}

build_model_diagram_nodes <- function(settings, edges) {
  x_var <- settings$predictor_var
  y_var <- settings$outcome_var
  mediators <- if(!is.null(settings$mediator_vars)) settings$mediator_vars else character(0)
  w_var <- if(!is.null(settings$moderator_var) && nzchar(settings$moderator_var)) settings$moderator_var else character(0)
  z_var <- if(!is.null(settings$moderator2_var) && nzchar(settings$moderator2_var)) settings$moderator2_var else character(0)
  covars <- character(0)
  
  n_m <- length(mediators)
  vars <- unique(c(x_var, y_var, mediators, w_var, z_var, covars, edges$from, edges$to))
  vars <- vars[!grepl("^int_[0-9]+$", vars)]
  vars <- vars[nzchar(vars)]
  
  nodes <- data.frame(
    name = vars,
    role = "other",
    x = 0,
    y = 0,
    stringsAsFactors = FALSE
  )
  
  nodes$role[nodes$name == x_var] <- "x"
  nodes$role[nodes$name == y_var] <- "y"
  if(n_m > 0) nodes$role[nodes$name %in% mediators] <- "m"
  if(length(w_var) > 0) nodes$role[nodes$name == w_var] <- "w"
  if(length(z_var) > 0) nodes$role[nodes$name == z_var] <- "z"
  if(length(covars) > 0) nodes$role[nodes$name %in% covars] <- "cov"
  
  # Template families:
  # A) moderation-only (no mediators)
  # B) mediation / moderated mediation (with mediators)
  nodes$x[nodes$role == "x"] <- -0.8
  nodes$y[nodes$role == "x"] <- 0.0
  nodes$x[nodes$role == "y"] <- 0.8
  nodes$y[nodes$role == "y"] <- 0.0
  
  if(n_m == 0) {
    if(length(w_var) > 0) {
      nodes$x[nodes$role == "w"] <- -0.8
      nodes$y[nodes$role == "w"] <- 0.35
    }
    if(length(z_var) > 0) {
      nodes$x[nodes$role == "z"] <- -0.8
      nodes$y[nodes$role == "z"] <- 0.12
    }
    if(length(covars) > 0) {
      cov_x <- if(length(covars) == 1) 0.60 else seq(0.45, 0.80, length.out = length(covars))
      for(i in seq_along(covars)) {
        nodes$x[nodes$name == covars[[i]]] <- cov_x[[i]]
        nodes$y[nodes$name == covars[[i]]] <- -0.62
      }
    }
  } else {
    if(n_m == 1) {
      nodes$x[nodes$name == mediators[[1]]] <- 0
      nodes$y[nodes$name == mediators[[1]]] <- 0.42
    } else if(n_m == 2) {
      # Parallel 2-mediator layout: stack vertically around the X->Y line.
      nodes$x[nodes$name == mediators[[1]]] <- 0
      nodes$y[nodes$name == mediators[[1]]] <- 0.46
      nodes$x[nodes$name == mediators[[2]]] <- 0
      nodes$y[nodes$name == mediators[[2]]] <- -0.20
    } else {
      # 3+ mediators: stagger vertically for readability.
      med_y <- seq(0.52, -0.18, length.out = n_m)
      med_x <- rep(0, n_m)
      for(i in seq_along(mediators)) {
        nodes$x[nodes$name == mediators[[i]]] <- med_x[[i]]
        nodes$y[nodes$name == mediators[[i]]] <- med_y[[i]]
      }
    }
    if(length(w_var) > 0) {
      nodes$x[nodes$role == "w"] <- -0.35
      nodes$y[nodes$role == "w"] <- -0.38
    }
    if(length(z_var) > 0) {
      nodes$x[nodes$role == "z"] <- 0.35
      nodes$y[nodes$role == "z"] <- -0.38
    }
    if(length(covars) > 0) {
      cov_x <- if(length(covars) == 1) 0 else seq(-0.45, 0.45, length.out = length(covars))
      for(i in seq_along(covars)) {
        nodes$x[nodes$name == covars[[i]]] <- cov_x[[i]]
        nodes$y[nodes$name == covars[[i]]] <- -0.75
      }
    }
  }
  
  nodes
}

resolve_interaction_label <- function(var_name, alias_map) {
  key <- tolower(trimws(var_name))
  if(key %in% names(alias_map)) {
    out <- trimws(as.character(alias_map[[key]]))
    if(nzchar(out)) return(out)
  }
  trimws(as.character(var_name))
}

format_interaction_label <- function(raw_label, settings) {
  lbl <- trimws(as.character(raw_label))
  if(!nzchar(lbl)) return(lbl)
  lbl_upper <- toupper(lbl)
  parts <- unlist(strsplit(lbl_upper, "\\s*[xX*]\\s*"))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if(length(parts) >= 2) {
    return(paste(unique(parts), collapse = " x "))
  }
  # Fallback for concatenated PROCESS names where separators are missing.
  known_vars <- toupper(trimws(c(
    if(!is.null(settings$predictor_var)) settings$predictor_var else character(0),
    if(!is.null(settings$moderator_var)) settings$moderator_var else character(0),
    if(!is.null(settings$moderator2_var)) settings$moderator2_var else character(0),
    if(!is.null(settings$mediator_vars)) settings$mediator_vars else character(0)
  )))
  known_vars <- unique(known_vars[nzchar(known_vars)])
  hits <- known_vars[vapply(known_vars, function(v) grepl(v, lbl_upper, fixed = TRUE), logical(1))]
  if(length(hits) >= 2) {
    return(paste(hits, collapse = " x "))
  }
  lbl_upper
}

build_template_diagram <- function(parsed, settings, diagram_type = c("conceptual", "statistical"),
                                    label_mode = "auto", show_interactions = TRUE, show_moderator_main_effects = TRUE,
                                    include_ci = FALSE, include_p = FALSE, include_stars = TRUE,
                                    label_map = NULL) {
  diagram_type <- match.arg(diagram_type)
  edges <- parsed$paths
  model_num <- suppressWarnings(as.integer(settings$model))
  alias_map <- parsed$metadata$product_aliases
  if(is.null(alias_map)) alias_map <- character(0)
  elig <- compute_diagram_eligibility(settings)
  if(!isTRUE(elig$eligible)) {
    return(message_plot(elig$reason))
  }

  if(nrow(edges) == 0) {
    return(message_plot("No diagram data available for current analysis."))
  }
  
  # Drop covariates entirely from diagram logic.
  edges <- edges[edges$path_kind != "covariate", , drop = FALSE]
  if(!isTRUE(show_interactions)) {
    edges <- edges[!(edges$path_kind == "interaction" | grepl("^int_[0-9]+$", edges$from)), , drop = FALSE]
  }
  if(!identical(diagram_type, "conceptual") && !isTRUE(show_moderator_main_effects)) {
    # Diagram-display option: hide moderator main-effect paths while keeping interactions.
    edges <- edges[edges$path_kind != "moderator", , drop = FALSE]
  }

  x_var <- settings$predictor_var
  y_var <- settings$outcome_var
  mediators <- if(!is.null(settings$mediator_vars)) settings$mediator_vars else character(0)
  w_var <- if(!is.null(settings$moderator_var) && nzchar(settings$moderator_var)) settings$moderator_var else character(0)
  z_var <- if(!is.null(settings$moderator2_var) && nzchar(settings$moderator2_var)) settings$moderator2_var else character(0)
  if(!identical(diagram_type, "conceptual") && !isTRUE(show_moderator_main_effects)) {
    w_var <- character(0)
    z_var <- character(0)
  }
  int_edges <- edges[edges$path_kind == "interaction" | grepl("^int_[0-9]+$", edges$from), , drop = FALSE]
  
  # Model 14 conceptual fallback:
  # if analysis settings have fewer mediators than currently selected in UI, prefer UI selection
  # so M2 is not dropped from conceptual rendering when selections changed.
  if(model_num == 14L && identical(diagram_type, "conceptual") && length(mediators) < 2 && exists("input", inherits = TRUE)) {
    live_mediators <- character(0)
    m_count <- suppressWarnings(as.integer(input$mediator_count))
    if(!is.na(m_count) && m_count > 0) {
      upper <- min(2L, m_count)
      for(i in seq_len(upper)) {
        m_id <- paste0("mediator_m", i)
        m_val <- input[[m_id]]
        if(!is.null(m_val) && nzchar(m_val)) live_mediators <- c(live_mediators, m_val)
      }
    }
    if(length(live_mediators) > length(mediators)) {
      mediators <- unique(c(mediators, live_mediators))
      if(length(mediators) > 2) mediators <- mediators[1:2]
    }
  }

  if(identical(diagram_type, "conceptual")) {
    # Conceptual diagrams show structural paths only.
    concept_edges <- edges[edges$path_kind %in% c("direct", "mediator"), , drop = FALSE]
    concept_edges <- unique(concept_edges)
    if(nrow(concept_edges) == 0 && nzchar(x_var) && nzchar(y_var)) {
      # Safe fallback for simple moderation models.
      if(nrow(edges) > 0) {
        row <- edges[1, , drop = FALSE]
      } else {
        row <- data.frame(
          model = model_num, outcome_block = y_var, from = x_var, to = y_var, path_kind = "direct",
          estimate_raw = NA_real_, estimate_std = NA_real_, se = NA_real_, p = NA_real_,
          llci = NA_real_, ulci = NA_real_, stars = "", label_raw = "", label_std = "",
          is_available_raw = FALSE, is_available_std = FALSE, parse_source = "fallback", note = "",
          stringsAsFactors = FALSE
        )
      }
      row$from <- x_var
      row$to <- y_var
      row$path_kind <- "direct"
      row$estimate_raw <- NA_real_
      row$estimate_std <- NA_real_
      row$se <- NA_real_
      row$p <- NA_real_
      row$llci <- NA_real_
      row$ulci <- NA_real_
      row$stars <- ""
      row$is_available_raw <- FALSE
      row$is_available_std <- FALSE
      concept_edges <- row
    }
    edges <- concept_edges
    int_edges <- edges[0, , drop = FALSE]
  } else {
    # Statistical diagrams: map int_* aliases to readable interaction labels.
    if(nrow(int_edges) > 0) {
      for(i in seq_len(nrow(int_edges))) {
        int_lbl <- resolve_interaction_label(int_edges$from[[i]], alias_map)
        int_lbl <- format_interaction_label(int_lbl, settings)
        edges$from[edges$from == int_edges$from[[i]]] <- int_lbl
      }
      int_edges <- edges[
        edges$path_kind == "interaction" |
          edges$from %in% unname(alias_map) |
          grepl("^.*\\sx\\s.*$", edges$from, ignore.case = TRUE),
        ,
        drop = FALSE
      ]
      if(nrow(int_edges) > 0) {
        int_edges <- unique(int_edges[, c("from", "to", "path_kind"), drop = FALSE])
      }
    }
  }

  if(nrow(edges) == 0) {
    return(message_plot("No paths available for diagram with current display options."))
  }

  # ---------- Nodes by template ----------
  nodes <- data.frame(name = character(0), x = numeric(0), y = numeric(0), role = character(0), stringsAsFactors = FALSE)
  add_node <- function(nm, x, y, role) {
    if(is.null(nm) || !nzchar(nm)) return()
    nodes <<- rbind(nodes, data.frame(name = nm, x = x, y = y, role = role, stringsAsFactors = FALSE))
  }

  if(identical(diagram_type, "conceptual")) {
    add_node(x_var, -0.75, 0.00, "x")
    add_node(y_var, 0.75, 0.00, "y")
    if(model_num == 1L) {
      if(length(w_var) > 0) add_node(w_var, 0.00, 0.45, "mod")
    } else if(model_num == 2L) {
      # Model 2 conceptual: W and Z align over 1/3 and 2/3 of the X->Y path.
      if(length(w_var) > 0) add_node(w_var, -0.25, 0.45, "mod")
      if(length(z_var) > 0) add_node(z_var, 0.25, 0.45, "mod")
    } else if(model_num == 3L) {
      # Model 3 conceptual: W centered above X->Y; Z feeds horizontally into W's vertical cue.
      if(length(w_var) > 0) add_node(w_var, 0.00, 0.45, "mod")
      if(length(z_var) > 0) add_node(z_var, -0.45, 0.24, "mod")
    }
    if(model_num %in% c(4L, 5L, 6L, 7L, 8L, 14L)) {
      n_m <- length(mediators)
      if(model_num == 4L && n_m > 0) {
        # Model 4 (parallel mediation):
        # mediator node centers align on the X->Y midpoint vertical and are symmetric about X->Y.
        med_x <- 0.00
        med_offset <- 0.52
        if(n_m == 1) {
          add_node(mediators[[1]], med_x, med_offset, "m")
        } else if(n_m >= 2) {
          add_node(mediators[[1]], med_x, med_offset, "m")
          add_node(mediators[[2]], med_x, -med_offset, "m")
        }
      } else if(model_num == 8L && n_m > 0) {
        # Cleaner layout rule for Model 8:
        # M1 above X->Y; M2+ below X->Y.
        add_node(mediators[[1]], 0.10, 0.52, "m")
        if(n_m >= 2) {
          lower_y <- if(n_m == 2) -0.52 else seq(-0.52, -0.74, length.out = n_m - 1)
          for(i in 2:n_m) {
            add_node(mediators[[i]], 0.10, lower_y[[i - 1]], "m")
          }
        }
      } else if(model_num == 6L && n_m > 0) {
        if(n_m == 1) {
          add_node(mediators[[1]], 0.08, 0.52, "m")
        } else {
          # Model 6 (2 mediators): symmetric top-mediator layout (Hayes-style).
          add_node(mediators[[1]], -0.14, 0.52, "m")
          add_node(mediators[[2]], 0.26, 0.52, "m")
        }
      } else if(model_num == 14L) {
        if(n_m == 1) {
          add_node(mediators[[1]], 0.00, 0.50, "m")
        } else if(n_m >= 2) {
          add_node(mediators[[1]], 0.00, 0.52, "m")
          add_node(mediators[[2]], 0.00, -0.52, "m")
        }
      } else {
        if(n_m == 1) {
          add_node(mediators[[1]], 0.00, 0.45, "m")
        } else if(n_m == 2) {
          add_node(mediators[[1]], 0.00, 0.52, "m")
          add_node(mediators[[2]], 0.00, -0.52, "m")
        } else if(n_m > 2) {
          med_y <- seq(0.55, -0.30, length.out = n_m)
          for(i in seq_along(mediators)) add_node(mediators[[i]], 0.00, med_y[[i]], "m")
        }
      }
      if(length(w_var) > 0 && model_num %in% c(5L, 7L, 8L)) add_node(w_var, -0.45, 0.55, "mod")
      if(length(w_var) > 0 && model_num == 14L) add_node(w_var, 0.45, 0.52, "mod")
      if(length(z_var) > 0) add_node(z_var, -0.55, 0.22, "mod")
    }
  } else {
    # Statistical templates
    if(model_num == 1L) {
      add_node(x_var, -0.75, 0.00, "x")
      if(length(w_var) > 0) add_node(w_var, -0.75, 0.28, "mod")
      int_terms <- unique(int_edges$from)
      if(length(int_terms) > 0) {
        for(i in seq_along(int_terms)) {
          int_lbl <- resolve_interaction_label(int_terms[[i]], alias_map)
          int_lbl <- format_interaction_label(int_lbl, settings)
          add_node(int_lbl, -0.75, -0.48 - 0.26 * (i - 1), "int")
        }
      }
      add_node(y_var, 0.75, 0.00, "y")
    } else if(model_num == 2L) {
      add_node(x_var, -0.75, 0.00, "x")
      if(length(w_var) > 0) add_node(w_var, -0.75, 0.20, "mod")
      if(length(z_var) > 0) add_node(z_var, -0.75, -0.20, "mod")
      if(nrow(int_edges) > 0) {
        int_terms <- unique(int_edges$from)
        y_pos <- seq(-0.44, -0.89, length.out = length(int_terms))
        for(i in seq_along(int_terms)) {
          int_lbl <- resolve_interaction_label(int_terms[[i]], alias_map)
          int_lbl <- format_interaction_label(int_lbl, settings)
          y_here <- y_pos[[i]]
          # Model 2 request: move X x W interaction node down a little.
          int_parts <- toupper(trimws(unlist(strsplit(int_lbl, "\\s+[xX]\\s+"))))
          if(length(int_parts) >= 2 &&
             toupper(x_var) %in% int_parts &&
             length(w_var) > 0 &&
             toupper(w_var) %in% int_parts) {
            y_here <- y_here - 0.08
          }
          add_node(int_lbl, -0.75, y_here, "int")
        }
      }
      add_node(y_var, 0.75, 0.00, "y")
    } else if(model_num == 3L) {
      add_node(x_var, -0.75, 0.00, "x")
      if(length(w_var) > 0) add_node(w_var, -0.75, 0.20, "mod")
      if(length(z_var) > 0) add_node(z_var, -0.75, -0.20, "mod")
      if(nrow(int_edges) > 0) {
        int_terms <- unique(int_edges$from)
        y_pos <- seq(-0.46, -1.02, length.out = length(int_terms))
        for(i in seq_along(int_terms)) {
          int_lbl <- resolve_interaction_label(int_terms[[i]], alias_map)
          int_lbl <- format_interaction_label(int_lbl, settings)
          add_node(int_lbl, -0.75, y_pos[[i]], "int")
        }
      }
      add_node(y_var, 0.75, 0.00, "y")
    } else if(model_num == 4L) {
      add_node(x_var, -0.75, 0.00, "x")
      add_node(y_var, 0.75, 0.00, "y")
      n_m <- length(mediators)
      med_x <- 0.00
      med_offset <- 0.52
      if(n_m == 1) {
        add_node(mediators[[1]], med_x, med_offset, "m")
      } else if(n_m == 2) {
        add_node(mediators[[1]], med_x, med_offset, "m")
        add_node(mediators[[2]], med_x, -med_offset, "m")
      } else if(n_m > 2) {
        med_y <- seq(0.58, -0.35, length.out = n_m)
        for(i in seq_along(mediators)) add_node(mediators[[i]], 0.00, med_y[[i]], "m")
      }
      if(nrow(int_edges) > 0) {
        for(i in seq_len(nrow(int_edges))) add_node(resolve_interaction_label(int_edges$from[[i]], alias_map), 0.25, -0.70 - 0.20 * (i - 1), "int")
      }
    } else if(model_num == 8L) {
      add_node(x_var, -0.78, 0.00, "x")
      if(length(w_var) > 0) add_node(w_var, -0.92, -0.34, "mod")
      int_terms <- unique(int_edges$from)
      if(length(int_terms) > 0) {
        int_x <- if(length(mediators) >= 2) -0.56 else -0.30
        int_y_base <- if(length(mediators) >= 2) -0.66 else -0.92
        for(i in seq_along(int_terms)) {
          int_lbl <- resolve_interaction_label(int_terms[[i]], alias_map)
          int_lbl <- format_interaction_label(int_lbl, settings)
          add_node(int_lbl, int_x, int_y_base - 0.18 * (i - 1), "int")
        }
      }
      n_m <- length(mediators)
      if(n_m == 1) {
        add_node(mediators[[1]], 0.06, 0.60, "m")
      } else if(n_m >= 2) {
        # Model 8 statistical rule:
        # M1 above X->Y; M2+ below X->Y.
        add_node(mediators[[1]], 0.06, 0.62, "m")
        lower_y <- if(n_m == 2) -0.62 else seq(-0.62, -0.82, length.out = n_m - 1)
        for(i in 2:n_m) {
          add_node(mediators[[i]], 0.10, lower_y[[i - 1]], "m")
        }
      }
      add_node(y_var, 0.80, 0.00, "y")
    } else if(model_num == 6L) {
      # Match statistical layout to conceptual serial/mediator-only structure.
      add_node(x_var, -0.78, 0.00, "x")
      add_node(y_var, 0.80, 0.00, "y")
      n_m <- length(mediators)
      if(n_m == 1) {
        add_node(mediators[[1]], 0.04, 0.58, "m")
      } else if(n_m >= 2) {
        add_node(mediators[[1]], -0.12, 0.58, "m")
        add_node(mediators[[2]], 0.24, 0.58, "m")
      }
    } else if(model_num == 5L) {
      # Model 5 statistical: moderated direct path with parallel mediators.
      add_node(x_var, -0.78, 0.00, "x")
      add_node(y_var, 0.80, 0.00, "y")
      n_m <- length(mediators)
      if(n_m == 1) {
        add_node(mediators[[1]], 0.02, 0.58, "m")
      } else if(n_m >= 2) {
        # Keep two mediators vertically centered on the X->Y midpoint vertical.
        add_node(mediators[[1]], 0.02, 0.56, "m")
        add_node(mediators[[2]], 0.02, -0.56, "m")
      }
      # Requested cleaner routing: move moderator to the lower-right lane for Model 5.
      if(length(w_var) > 0) {
        if(n_m >= 2) {
          add_node(w_var, 0.30, -0.52, "mod")
        } else {
          add_node(w_var, 0.24, -0.46, "mod")
        }
      }
      if(length(z_var) > 0) add_node(z_var, -0.92, 0.24, "mod")
      if(nrow(int_edges) > 0) {
        int_terms <- unique(int_edges$from)
        int_base_x <- if(n_m >= 2) 0.58 else 0.48
        int_base_y <- if(n_m >= 2) -0.78 else -0.74
        for(i in seq_along(int_terms)) {
          int_lbl <- resolve_interaction_label(int_terms[[i]], alias_map)
          int_lbl <- format_interaction_label(int_lbl, settings)
          # Requested cleaner routing: move interaction term box toward lower-right lane.
          add_node(int_lbl, int_base_x + 0.12 * (i - 1), int_base_y - 0.12 * (i - 1), "int")
        }
      }
    } else if(model_num %in% c(7L, 14L)) {
      add_node(x_var, -0.78, 0.00, "x")
      add_node(y_var, 0.80, 0.00, "y")
      n_m <- length(mediators)
      if(n_m == 1) {
        add_node(mediators[[1]], 0.04, 0.58, "m")
      } else if(n_m >= 2) {
        add_node(mediators[[1]], 0.04, 0.60, "m")
        add_node(mediators[[2]], 0.04, -0.40, "m")
      }
      if(length(w_var) > 0) add_node(w_var, -0.92, -0.30, "mod")
      if(length(z_var) > 0) add_node(z_var, -0.92, 0.24, "mod")
      if(nrow(int_edges) > 0) {
        int_terms <- unique(int_edges$from)
        for(i in seq_along(int_terms)) {
          int_lbl <- resolve_interaction_label(int_terms[[i]], alias_map)
          int_lbl <- format_interaction_label(int_lbl, settings)
          if(model_num == 14L && length(int_terms) >= 2) {
            # Move interaction terms rightward and stagger horizontally for clarity.
            add_node(int_lbl, 0.12 + 0.24 * (i - 1), -0.84 - 0.08 * (i - 1), "int")
          } else {
            add_node(int_lbl, -0.45, -0.84 - 0.20 * (i - 1), "int")
          }
        }
      }
    }
  }

  nodes <- unique(nodes)
  if(!is.null(label_map) && length(label_map) > 0) {
    node_labels <- vapply(nodes$name, function(nm) {
      if(nm %in% names(label_map)) {
        sanitize_label_text(label_map[[nm]], nm)
      } else if(grepl("\\s+[xX]\\s+", nm)) {
        map_interaction_label(nm, label_map)
      } else {
        nm
      }
    }, character(1))
    nodes$label <- node_labels
  } else {
    nodes$label <- nodes$name
  }
  if(nrow(nodes) == 0) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = "No template nodes available", size = 6) + ggplot2::theme_void())
  }

  from_xy <- nodes[, c("name", "x", "y")]
  names(from_xy) <- c("from", "x_from", "y_from")
  to_xy <- nodes[, c("name", "x", "y")]
  names(to_xy) <- c("to", "x_to", "y_to")
  
  edge_plot <- merge(edges, from_xy, by = "from", all.x = TRUE)
  edge_plot <- merge(edge_plot, to_xy, by = "to", all.x = TRUE)
  edge_plot <- edge_plot[!is.na(edge_plot$x_from) & !is.na(edge_plot$x_to), , drop = FALSE]
  role_map <- nodes[, c("name", "role"), drop = FALSE]
  names(role_map) <- c("from", "from_role")
  edge_plot <- merge(edge_plot, role_map, by = "from", all.x = TRUE)
  role_map2 <- nodes[, c("name", "role"), drop = FALSE]
  names(role_map2) <- c("to", "to_role")
  edge_plot <- merge(edge_plot, role_map2, by = "to", all.x = TRUE)

  clip_segment_to_box <- function(x0, y0, x1, y1, hw = 0.085, hh = 0.055) {
    dx <- x1 - x0
    dy <- y1 - y0
    seg_len <- sqrt(dx * dx + dy * dy)
    if(seg_len <= 1e-9) {
      return(c(x0, y0, x1, y1))
    }
    t0x <- if(abs(dx) < 1e-9) Inf else hw / abs(dx)
    t0y <- if(abs(dy) < 1e-9) Inf else hh / abs(dy)
    t0 <- min(t0x, t0y)
    xs <- x0 + dx * t0
    ys <- y0 + dy * t0

    t1x <- if(abs(dx) < 1e-9) Inf else hw / abs(dx)
    t1y <- if(abs(dy) < 1e-9) Inf else hh / abs(dy)
    t1 <- min(t1x, t1y)
    xe <- x1 - dx * t1
    ye <- y1 - dy * t1
    c(xs, ys, xe, ye)
  }
  
  if(identical(diagram_type, "conceptual")) {
    edge_plot$label <- ""
  } else {
    edge_plot$label <- apply(edge_plot, 1, function(r) {
      row_df <- as.list(r)
      row_df$is_available_std <- isTRUE(as.logical(row_df$is_available_std))
      row_df$is_available_raw <- isTRUE(as.logical(row_df$is_available_raw))
      row_df$estimate_std <- safe_numeric(row_df$estimate_std)
      row_df$estimate_raw <- safe_numeric(row_df$estimate_raw)
      row_df$llci <- safe_numeric(row_df$llci)
      row_df$ulci <- safe_numeric(row_df$ulci)
      row_df$p <- safe_numeric(row_df$p)
      row_df$stars <- as.character(row_df$stars)
      compose_path_label(
        row_df,
        label_mode = label_mode,
        include_ci = include_ci,
        include_p = include_p,
        include_stars = include_stars
      )
    })
  }
  
  clipped <- t(mapply(
    clip_segment_to_box,
    edge_plot$x_from, edge_plot$y_from, edge_plot$x_to, edge_plot$y_to
  ))
  edge_plot$x_from_draw <- clipped[, 1]
  edge_plot$y_from_draw <- clipped[, 2]
  edge_plot$x_to_draw <- clipped[, 3]
  edge_plot$y_to_draw <- clipped[, 4]
  edge_plot$dx <- edge_plot$x_to_draw - edge_plot$x_from_draw
  edge_plot$dy <- edge_plot$y_to_draw - edge_plot$y_from_draw
  edge_plot$seg_len <- pmax(sqrt(edge_plot$dx^2 + edge_plot$dy^2), 1e-6)
  # Statistical label placement rule:
  # keep coefficient-label centers on their own line by moving only along path length.
  edge_plot$t_label <- 0.50
  if(!identical(diagram_type, "conceptual")) {
    set_t <- function(idx, value) {
      edge_plot$t_label[idx] <<- value
    }
    spread_t <- function(idx, lo, hi, order_mode = c("from_y", "to_y", "from_x")) {
      order_mode <- match.arg(order_mode)
      ids <- which(idx)
      if(length(ids) == 0) return()
      if(length(ids) == 1) {
        edge_plot$t_label[ids] <<- (lo + hi) / 2
        return()
      }
      if(identical(order_mode, "to_y")) {
        ord <- ids[order(-edge_plot$y_to[ids], edge_plot$x_to[ids], edge_plot$to[ids], edge_plot$from[ids])]
      } else if(identical(order_mode, "from_x")) {
        ord <- ids[order(edge_plot$x_from[ids], -edge_plot$y_from[ids], edge_plot$from[ids], edge_plot$to[ids])]
      } else {
        ord <- ids[order(-edge_plot$y_from[ids], edge_plot$x_from[ids], edge_plot$from[ids], edge_plot$to[ids])]
      }
      edge_plot$t_label[ord] <<- seq(lo, hi, length.out = length(ord))
    }

    # Baseline lanes by role with midpoint default preserved whenever feasible.
    set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.50)
    spread_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.28, 0.34, "from_y")
    spread_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.62, 0.80, "from_y")
    spread_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.84, 0.94, "from_x")
    spread_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.36, 0.64, "to_y")
    spread_t(edge_plot$to_role == "m" & edge_plot$from_role == "mod", 0.24, 0.44, "to_y")
    spread_t(edge_plot$to_role == "m" & edge_plot$from_role == "int", 0.66, 0.86, "to_y")
    set_t(edge_plot$to_role == "m" & edge_plot$from_role == "m", 0.50)

    if(model_num == 1L) {
      # Model 1 experiment: align all Y-path labels to true center-to-center midpoints.
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.50)
    }
    if(model_num == 2L) {
      # Model 2 pass: same midpoint rule as Model 1 for Y-target paths.
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.50)
    }
    if(model_num == 3L) {
      # Model 3 baseline pass: keep Y-target coefficients at line midpoints.
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.50)
    }

    if(model_num == 5L) {
      # Model 5: keep Y-target labels separated but on-line; tune for 1 vs 2 mediators.
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.52)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.74)
      if(length(mediators) == 1) {
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.70)
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.46)
      } else if(length(mediators) >= 2) {
        m1_name <- mediators[[1]]
        m2_name <- mediators[[2]]
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "x", 0.40)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "x", 0.66)
        set_t(edge_plot$to == y_var & edge_plot$from == m1_name, 0.62)
        set_t(edge_plot$to == y_var & edge_plot$from == m2_name, 0.82)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "mod", 0.54)
      }
    }

    if(model_num == 4L) {
      # Keep simple mediation coefficients centered by default.
      set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.50)
    }

    if(model_num == 6L && length(mediators) >= 2) {
      m1_name <- mediators[[1]]
      m2_name <- mediators[[2]]
      set_t(edge_plot$to == m1_name & edge_plot$from_role == "x", 0.42)
      set_t(edge_plot$to == m2_name & edge_plot$from_role == "x", 0.60)
      set_t(edge_plot$from == m1_name & edge_plot$to == m2_name, 0.50)
      set_t(edge_plot$from == m1_name & edge_plot$to == y_var, 0.62)
      set_t(edge_plot$from == m2_name & edge_plot$to == y_var, 0.74)
    }

    if(model_num == 7L) {
      # First-stage moderation lanes for two-mediator layouts.
      spread_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.42, 0.70, "to_y")
      spread_t(edge_plot$to_role == "m" & edge_plot$from_role == "mod", 0.26, 0.50, "to_y")
      spread_t(edge_plot$to_role == "m" & edge_plot$from_role == "int", 0.74, 0.88, "to_y")
      if(length(mediators) >= 2) {
        m1_name <- mediators[[1]]
        m2_name <- mediators[[2]]
        set_t(edge_plot$to == y_var & edge_plot$from == m1_name, 0.64)
        set_t(edge_plot$to == y_var & edge_plot$from == m2_name, 0.80)
      }
    }

    if(model_num == 8L) {
      if(length(mediators) == 1) {
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.40)
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "mod", 0.22)
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "int", 0.78)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.30)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.68)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.86)
      } else if(length(mediators) >= 2) {
        m1_name <- mediators[[1]]
        m2_name <- mediators[[2]]
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "x", 0.36)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "x", 0.74)
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "mod", 0.24)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "mod", 0.54)
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "int", 0.56)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "int", 0.82)
        set_t(edge_plot$to == y_var & edge_plot$from == m1_name, 0.64)
        set_t(edge_plot$to == y_var & edge_plot$from == m2_name, 0.82)
        set_t(edge_plot$to == y_var & edge_plot$from_role == "int", 0.93)
      }
    }

    if(model_num == 14L) {
      if(length(mediators) == 1) {
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.44)
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "mod", 0.30)
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "int", 0.78)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.30)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.70)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.88)
      } else if(length(mediators) >= 2) {
        m1_name <- mediators[[1]]
        m2_name <- mediators[[2]]
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "x", 0.42)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "x", 0.70)
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "mod", 0.28)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "mod", 0.58)
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "int", 0.74)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "int", 0.88)
        set_t(edge_plot$to == y_var & edge_plot$from == m1_name, 0.64)
        set_t(edge_plot$to == y_var & edge_plot$from == m2_name, 0.80)
        idx_int_y <- edge_plot$to == y_var & edge_plot$from_role == "int"
        if(sum(idx_int_y) > 1) {
          ids <- which(idx_int_y)
          ord <- ids[order(edge_plot$x_from[ids], edge_plot$from[ids])]
          edge_plot$t_label[ord] <- seq(0.86, 0.95, length.out = length(ord))
        } else {
          set_t(idx_int_y, 0.90)
        }
      }
    }
  }
  if(!identical(diagram_type, "conceptual") && model_num %in% c(1L, 2L, 3L, 4L, 5L)) {
    # For Models 1-5, place labels by center-to-center geometry (not clipped box edges).
    edge_plot$x_label <- edge_plot$x_from + edge_plot$t_label * (edge_plot$x_to - edge_plot$x_from)
    edge_plot$y_label <- edge_plot$y_from + edge_plot$t_label * (edge_plot$y_to - edge_plot$y_from)
  } else {
    edge_plot$x_label <- edge_plot$x_from_draw + edge_plot$t_label * edge_plot$dx
    edge_plot$y_label <- edge_plot$y_from_draw + edge_plot$t_label * edge_plot$dy
  }

  # Conceptual moderation cue arrows (to path midpoint only; no coefficient label)
  mod_cue <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0))
  if(identical(diagram_type, "conceptual") && model_num %in% c(1L, 2L, 3L, 5L, 7L, 8L, 14L)) {
    x_node <- nodes[nodes$name == x_var, , drop = FALSE]
    y_node <- nodes[nodes$name == y_var, , drop = FALSE]
    if(nrow(x_node) == 1 && nrow(y_node) == 1) {
      midx <- (x_node$x + y_node$x) / 2
      midy <- (x_node$y + y_node$y) / 2
      xy_point <- function(t_pos) {
        c(x_node$x + t_pos * (y_node$x - x_node$x), x_node$y + t_pos * (y_node$y - x_node$y))
      }
      if(length(w_var) > 0 && any(nodes$name == w_var)) {
        w_node <- nodes[nodes$name == w_var, , drop = FALSE]
        if(model_num == 2L) {
          # Model 2 conceptual: W points vertically to ~1/3 of X->Y path.
          w_target <- xy_point(1/3)
          mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = w_target[[1]], yend = w_target[[2]]))
        } else if(model_num == 7L && length(mediators) > 0 && any(nodes$name == mediators[[1]])) {
          # Model 7 conceptual: moderator targets the first-stage X->M1 path.
          m_node <- nodes[nodes$name == mediators[[1]], , drop = FALSE]
          xm_midx <- x_node$x + 0.66 * (m_node$x - x_node$x)
          xm_midy <- x_node$y + 0.66 * (m_node$y - x_node$y)
          mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = xm_midx, yend = xm_midy))
        } else {
          # Default moderation cue: to X->Y midpoint.
          mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = midx, yend = midy))
        }
        if(model_num == 8L && length(mediators) > 0) {
          cue_t <- if(length(mediators) == 1) 0.44 else seq(0.30, 0.68, length.out = length(mediators))
          for(m in mediators) {
            if(any(nodes$name == m)) {
              m_node <- nodes[nodes$name == m, , drop = FALSE]
              m_idx <- which(mediators == m)[1]
              t_pos <- cue_t[[m_idx]]
              xm_midx <- x_node$x + t_pos * (m_node$x - x_node$x)
              xm_midy <- x_node$y + t_pos * (m_node$y - x_node$y)
              mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = xm_midx, yend = xm_midy))
            }
          }
        }
        if(model_num == 14L && length(mediators) > 0 && any(nodes$name == mediators[[1]])) {
          m_node <- nodes[nodes$name == mediators[[1]], , drop = FALSE]
          my_midx <- (m_node$x + y_node$x) / 2
          my_midy <- (m_node$y + y_node$y) / 2
          mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = my_midx, yend = my_midy))
        }
      }
      if(length(z_var) > 0 && any(nodes$name == z_var)) {
        z_node <- nodes[nodes$name == z_var, , drop = FALSE]
        if(model_num == 2L) {
          # Model 2 conceptual: Z points vertically to ~2/3 of X->Y path.
          z_target <- xy_point(2/3)
          mod_cue <- rbind(mod_cue, data.frame(x = z_node$x, y = z_node$y, xend = z_target[[1]], yend = z_target[[2]]))
        } else if(model_num == 3L && length(w_var) > 0 && any(nodes$name == w_var)) {
          # Model 3 conceptual: Z feeds horizontally into W's vertical moderation line.
          w_node <- nodes[nodes$name == w_var, , drop = FALSE]
          mod_cue <- rbind(mod_cue, data.frame(x = z_node$x, y = z_node$y, xend = w_node$x, yend = z_node$y))
        } else {
          mod_cue <- rbind(mod_cue, data.frame(x = z_node$x, y = z_node$y, xend = midx, yend = midy))
        }
      }
    }
  }
  if(nrow(mod_cue) > 0) {
    cue_clip <- t(mapply(function(x0, y0, x1, y1) {
      # Clip only cue start to originating box; end stays exactly on target path.
      clipped <- clip_segment_to_box(x0, y0, x1, y1)
      c(clipped[[1]], clipped[[2]], x1, y1)
    }, mod_cue$x, mod_cue$y, mod_cue$xend, mod_cue$yend))
    mod_cue$x <- cue_clip[, 1]
    mod_cue$y <- cue_clip[, 2]
  }

  title_txt <- paste0("Model ", settings$model, " ", if(identical(diagram_type, "conceptual")) "Conceptual" else "Statistical", " Diagram")
  subtitle_txt <- if(identical(diagram_type, "conceptual")) {
    NULL
  } else {
    if(identical(label_mode, "std")) "Standardized coefficients" else "Unstandardized coefficients"
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_plot,
      ggplot2::aes(x = x_from_draw, y = y_from_draw, xend = x_to_draw, yend = y_to_draw, linetype = path_kind, linewidth = path_kind),
      arrow = grid::arrow(length = grid::unit(0.15, "cm")),
      color = "black",
      lineend = "round"
    ) +
    ggplot2::geom_label(
      data = nodes,
      ggplot2::aes(x = x, y = y, label = label),
      size = if(identical(diagram_type, "conceptual")) 5.4 else 4.8,
      label.size = 0.45,
      fill = "white",
      color = "black",
      fontface = "bold"
    ) +
    ggplot2::geom_segment(
      data = mod_cue,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      linetype = "solid",
      linewidth = 0.45,
      color = "black",
      arrow = grid::arrow(length = grid::unit(0.12, "cm"))
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        direct = "solid",
        mediator = "solid",
        moderator = "solid",
        interaction = "solid",
        covariate = "dotted",
        unknown = "solid"
      ),
      guide = "none"
    ) +
    ggplot2::scale_linewidth_manual(
      values = c(
        direct = 0.80,
        mediator = 0.80,
        moderator = 0.55,
        interaction = 0.55,
        covariate = 0.45,
        unknown = 0.65
      ),
      guide = "none"
    ) +
    ggplot2::coord_fixed(
      xlim = if(identical(diagram_type, "conceptual")) c(-0.85, 0.85) else c(-1.0, 1.0),
      ylim = c(-1.05, 0.85),
      ratio = 1.0,
      clip = "off"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0.5, color = "black"),
      plot.subtitle = ggplot2::element_text(size = 10.5, hjust = 0.5, color = "black")
    ) +
    ggplot2::labs(title = title_txt, subtitle = subtitle_txt)

  if(!identical(diagram_type, "conceptual")) {
    label_df <- edge_plot[nzchar(edge_plot$label), , drop = FALSE]
    p <- p + ggplot2::geom_label(
      data = label_df,
      ggplot2::aes(x = x_label, y = y_label, label = label),
      size = 3.3,
      label.size = 0.2,
      fill = "white",
      color = "black"
    )
  }

  p
}

diagram_settings_key <- function(settings) {
  paste(
    as.character(settings$model),
    as.character(settings$predictor_var),
    as.character(settings$outcome_var),
    paste(as.character(settings$mediator_vars), collapse = "|"),
    as.character(settings$moderator_var),
    as.character(settings$moderator2_var),
    sep = "||"
  )
}

diagram_label_map <- reactiveVal(character(0))
diagram_key <- reactiveVal("")

reset_model_diagram_state <- function() {
  diagram_label_map(character(0))
  diagram_key("")
}

observeEvent(analysis_results(), {
  settings <- analysis_results()$settings
  key <- diagram_settings_key(settings)
  if(!identical(key, diagram_key())) {
    diagram_label_map(build_default_label_map(settings))
    diagram_key(key)
  }
}, ignoreInit = FALSE)

diagram_eligibility <- reactive({
  req(analysis_results())
  compute_diagram_eligibility(analysis_results()$settings)
})

output$diagram_eligibility_msg <- renderUI({
  req(analysis_results())
  elig <- diagram_eligibility()
  if(isTRUE(elig$eligible)) return(NULL)
  HTML(
    paste0(
      "<div style='margin:8px 0 14px 0; padding:10px; border:1px solid #f0ad4e; background:#fcf8e3; color:#8a6d3b;'>",
      "<strong>Diagram unavailable:</strong> ",
      elig$reason,
      "</div>"
    )
  )
})

output$diagram_label_editor <- renderUI({
  req(analysis_results())
  settings <- analysis_results()$settings
  current_map <- diagram_label_map()

  fields <- list()
  if(!is.null(settings$predictor_var) && nzchar(settings$predictor_var)) {
    fields <- c(fields, list(list(id = "diagram_lbl_x", title = "X label", var = settings$predictor_var)))
  }
  if(!is.null(settings$outcome_var) && nzchar(settings$outcome_var)) {
    fields <- c(fields, list(list(id = "diagram_lbl_y", title = "Y label", var = settings$outcome_var)))
  }
  if(!is.null(settings$moderator_var) && nzchar(settings$moderator_var)) {
    fields <- c(fields, list(list(id = "diagram_lbl_w", title = "W label", var = settings$moderator_var)))
  }
  if(!is.null(settings$moderator2_var) && nzchar(settings$moderator2_var)) {
    fields <- c(fields, list(list(id = "diagram_lbl_z", title = "Z label", var = settings$moderator2_var)))
  }
  mediators <- if(!is.null(settings$mediator_vars)) settings$mediator_vars else character(0)
  if(length(mediators) >= 1) {
    fields <- c(fields, list(list(id = "diagram_lbl_m1", title = "M1 label", var = mediators[[1]])))
  }
  if(length(mediators) >= 2) {
    fields <- c(fields, list(list(id = "diagram_lbl_m2", title = "M2 label", var = mediators[[2]])))
  }

  if(length(fields) == 0) return(NULL)

  input_controls <- lapply(fields, function(f) {
    textInput(
      inputId = f$id,
      label = f$title,
      value = if(f$var %in% names(current_map)) current_map[[f$var]] else f$var,
      width = "34%"
    )
  })

  tags$div(
    style = "margin: 10px 0 6px 0; padding: 10px; border: 1px solid #ddd; border-radius: 4px; background: #fafafa;",
    tags$strong("Diagram variable labels"),
    tags$div(style = "font-size: 12px; color: #555; margin-bottom: 8px;",
             "Edit labels, then click Regenerate Diagrams to apply."),
    do.call(tagList, input_controls)
  )
})

observeEvent(input$diagram_regenerate, {
  req(analysis_results())
  settings <- analysis_results()$settings
  current <- diagram_label_map()
  if(length(current) == 0) current <- build_default_label_map(settings)

  apply_if_present <- function(var_nm, input_id) {
    if(!is.null(var_nm) && nzchar(var_nm) && !is.null(input[[input_id]])) {
      current[[var_nm]] <<- sanitize_label_text(input[[input_id]], var_nm)
    }
  }

  apply_if_present(settings$predictor_var, "diagram_lbl_x")
  apply_if_present(settings$outcome_var, "diagram_lbl_y")
  apply_if_present(settings$moderator_var, "diagram_lbl_w")
  apply_if_present(settings$moderator2_var, "diagram_lbl_z")
  mediators <- if(!is.null(settings$mediator_vars)) settings$mediator_vars else character(0)
  if(length(mediators) >= 1) apply_if_present(mediators[[1]], "diagram_lbl_m1")
  if(length(mediators) >= 2) apply_if_present(mediators[[2]], "diagram_lbl_m2")

  diagram_label_map(current)
}, ignoreInit = TRUE)

conceptual_diagram_plot_obj <- reactive({
  req(analysis_results())
  parsed <- diagram_parse_results()
  settings <- analysis_results()$settings
  mode_input <- input$diagram_coef_mode
  if(is.null(mode_input) || mode_input == "") mode_input <- "raw"

  build_template_diagram(
    parsed = parsed,
    settings = settings,
    diagram_type = "conceptual",
    label_mode = mode_input,
    show_interactions = FALSE,
    include_ci = FALSE,
    include_p = FALSE,
    include_stars = FALSE,
    label_map = diagram_label_map()
  )
})

statistical_diagram_plot_obj <- reactive({
  req(analysis_results())
  parsed <- diagram_parse_results()
  settings <- analysis_results()$settings
  mode_input <- input$diagram_coef_mode
  if(is.null(mode_input) || mode_input == "") mode_input <- "raw"
  show_int <- if(is.null(input$diagram_show_interactions)) TRUE else isTRUE(input$diagram_show_interactions)
  show_mod_main <- if(is.null(input$diagram_show_mod_main_effects)) TRUE else isTRUE(input$diagram_show_mod_main_effects)
  include_ci_opt <- if(is.null(input$diagram_include_ci)) FALSE else isTRUE(input$diagram_include_ci)
  include_p_opt <- if(is.null(input$diagram_include_p)) FALSE else isTRUE(input$diagram_include_p)
  include_stars_opt <- if(is.null(input$diagram_include_stars)) TRUE else isTRUE(input$diagram_include_stars)

  build_template_diagram(
    parsed = parsed,
    settings = settings,
    diagram_type = "statistical",
    label_mode = mode_input,
    show_interactions = show_int,
    show_moderator_main_effects = show_mod_main,
    include_ci = include_ci_opt,
    include_p = include_p_opt,
    include_stars = include_stars_opt,
    label_map = diagram_label_map()
  )
})

observe({
  results <- analysis_results()
  if(is.null(results)) {
    updateSelectInput(
      session,
      "diagram_coef_mode",
      choices = c("Unstandardized coefficients" = "raw"),
      selected = "raw"
    )
    return()
  }
  parsed <- diagram_parse_results()
  has_std <- isTRUE(results$settings$stand) &&
    nrow(parsed$paths) > 0 &&
    any(parsed$paths$is_available_std %in% TRUE)
  choices <- if(has_std) {
    c(
      "Unstandardized coefficients" = "raw",
      "Standardized coefficients" = "std"
    )
  } else {
    c("Unstandardized coefficients" = "raw")
  }
  selected <- isolate(input$diagram_coef_mode)
  if(is.null(selected) || !selected %in% unname(choices)) {
    selected <- if(has_std) "std" else "raw"
  }
  updateSelectInput(session, "diagram_coef_mode", choices = choices, selected = selected)
})

output$conceptual_diagram_plot <- renderPlot({
  if(is.null(analysis_results())) {
    plot.new()
    text(0.5, 0.5, "Run an analysis to generate the conceptual diagram.", col = "#666666")
    return(invisible(NULL))
  }
  print(conceptual_diagram_plot_obj())
}, bg = "white")

output$statistical_diagram_plot <- renderPlot({
  if(is.null(analysis_results())) {
    plot.new()
    text(0.5, 0.5, "Run an analysis to generate the statistical diagram.", col = "#666666")
    return(invisible(NULL))
  }
  print(statistical_diagram_plot_obj())
}, bg = "white")

output$model_diagram_notes <- renderUI({
  if(is.null(analysis_results())) return(NULL)
  parsed <- diagram_parse_results()
  warn <- parsed$metadata$warnings
  if(is.null(warn) || length(warn) == 0) return(NULL)
  HTML(paste0("<div style='margin-top:8px; color:#555;'><strong>Notes:</strong><br>", paste(unique(warn), collapse = "<br>"), "</div>"))
})

output$download_conceptual_diagram <- downloadHandler(
  filename = function() {
    model_txt <- if(!is.null(input$process_model) && nzchar(input$process_model)) input$process_model else "unknown"
    paste0("model_", model_txt, "_conceptual_diagram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
  },
  contentType = "image/jpeg",
  content = function(file) {
    req(analysis_results())
    p <- conceptual_diagram_plot_obj()
    ggplot2::ggsave(file, plot = p, device = "jpeg", width = 10, height = 6.2, dpi = 600, units = "in", bg = "white")
  }
)

output$download_statistical_diagram <- downloadHandler(
  filename = function() {
    model_txt <- if(!is.null(input$process_model) && nzchar(input$process_model)) input$process_model else "unknown"
    paste0("model_", model_txt, "_statistical_diagram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
  },
  contentType = "image/jpeg",
  content = function(file) {
    req(analysis_results())
    p <- statistical_diagram_plot_obj()
    ggplot2::ggsave(file, plot = p, device = "jpeg", width = 12, height = 8, dpi = 600, units = "in", bg = "white")
  }
)

observe({
  has_results <- !is.null(analysis_results())
  elig <- if(has_results) diagram_eligibility() else list(eligible = FALSE)
  can_download <- has_results && isTRUE(elig$eligible)
  shinyjs::toggleState("download_conceptual_diagram", condition = can_download)
  shinyjs::toggleState("download_statistical_diagram", condition = can_download)
})
