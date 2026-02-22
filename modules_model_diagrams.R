# ============================================================================
# MODEL DIAGRAMS MODULE (PARSER SCAFFOLD)
# ============================================================================
# Purpose:
# - Parse PROCESS output into a diagram-friendly path table schema.
# - Provide metadata for downstream rendering and fallback notes.
# - This is a non-UI scaffold used for upcoming Model Diagram tab work.
# ============================================================================

if(FALSE) {
  # Keep DiagrammeR discoverable for shinylive static package scanning.
  library(DiagrammeR)
}

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

  if(model_num == 6L && n_m != 2) {
    return(list(
      eligible = FALSE,
      reason = paste0(
        "Model 6 diagram generation requires exactly 2 mediators. ",
        "Current analysis has ", n_m, " mediator(s)."
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
                               include_ci = FALSE, include_p = FALSE, include_stars = TRUE,
                               coef_digits = 3) {
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

  digits <- suppressWarnings(as.integer(coef_digits))
  if(is.na(digits) || !(digits %in% c(2L, 3L))) digits <- 3L
  fmt <- paste0("%.", digits, "f")

  label <- paste0(prefix, " = ", sprintf(fmt, est))
  if(isTRUE(include_stars) && nzchar(edge_row$stars)) {
    label <- paste0(label, edge_row$stars)
  }
  if(isTRUE(include_ci) && !is.na(edge_row$llci) && !is.na(edge_row$ulci)) {
    label <- paste0(label, "\n[", sprintf(fmt, edge_row$llci), ", ", sprintf(fmt, edge_row$ulci), "]")
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
                                    label_map = NULL, coef_digits = 3,
                                    coef_label_size = 3.3, coef_label_orientation = "line",
                                    fill_variable_boxes_blue = FALSE,
                                    plot_width_px = NULL, plot_height_px = NULL) {
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
  # Keep pixel-to-data conversion aligned with the coord_fixed ranges used below.
  x_span <- if(identical(diagram_type, "conceptual")) 1.70 else 2.00
  y_span <- 1.90
  pixel_gap_to_data <- function(gap_px, fallback_data) {
    gp <- suppressWarnings(as.numeric(gap_px))
    w_px <- suppressWarnings(as.numeric(plot_width_px))
    h_px <- suppressWarnings(as.numeric(plot_height_px))
    if(is.na(gp) || gp <= 0) return(fallback_data)
    if(is.na(w_px) || is.na(h_px) || w_px <= 0 || h_px <= 0) return(fallback_data)
    gp * max(x_span / w_px, y_span / h_px)
  }
  
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
      } else if(model_num == 6L && n_m >= 2) {
        # Model 6 (2 mediators): serial top-row layout.
        # Place mediator centers at 25% and 75% of the X->Y center-to-center span.
        x0 <- -0.75
        x1 <- 0.75
        add_node(mediators[[1]], x0 + 0.25 * (x1 - x0), 0.52, "m")
        add_node(mediators[[2]], x0 + 0.75 * (x1 - x0), 0.52, "m")
      } else if(model_num == 7L) {
        if(n_m == 1) {
          add_node(mediators[[1]], 0.06, 0.54, "m")
        } else if(n_m >= 2) {
          # Model 7 conceptual: keep mediators balanced around X->Y like Model 4.
          add_node(mediators[[1]], 0.06, 0.54, "m")
          add_node(mediators[[2]], 0.06, -0.54, "m")
        }
      } else if(model_num == 14L) {
        if(n_m == 1) {
          add_node(mediators[[1]], -0.06, 0.54, "m")
        } else if(n_m >= 2) {
          add_node(mediators[[1]], -0.06, 0.54, "m")
          add_node(mediators[[2]], -0.06, -0.54, "m")
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
      if(length(w_var) > 0 && model_num == 7L) add_node(w_var, -0.74, 0.74, "mod")
      if(length(w_var) > 0 && model_num %in% c(5L, 8L)) add_node(w_var, -0.45, 0.55, "mod")
      if(length(w_var) > 0 && model_num == 14L) {
        if(n_m == 1) {
          add_node(w_var, 0.74, 0.54, "mod")
        } else {
          add_node(w_var, 0.74, 0.74, "mod")
        }
      }
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
        int_y_base <- if(length(mediators) >= 2) -0.74 else -0.84
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
      # Match statistical layout to conceptual serial structure with two mediators.
      add_node(x_var, -0.75, 0.00, "x")
      add_node(y_var, 0.75, 0.00, "y")
      n_m <- length(mediators)
      if(n_m >= 2) {
        x0 <- -0.75
        x1 <- 0.75
        add_node(mediators[[1]], x0 + 0.25 * (x1 - x0), 0.52, "m")
        add_node(mediators[[2]], x0 + 0.75 * (x1 - x0), 0.52, "m")
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
      # Place moderator/int nodes so turning off moderator main effects leaves interaction near Y.
      if(length(w_var) > 0) {
        if(n_m >= 2) {
          # Preferred 2-mediator layout: W directly below Y (aligned with M2 vertical level).
          add_node(w_var, 0.80, -0.56, "mod")
        } else {
          # 1-mediator layout: switch W and interaction relative positions.
          add_node(w_var, 0.48, -0.74, "mod")
        }
      }
      if(length(z_var) > 0) add_node(z_var, -0.92, 0.24, "mod")
      if(nrow(int_edges) > 0) {
        int_terms <- unique(int_edges$from)
        int_base_x <- if(n_m >= 2) 0.80 else 0.20
        int_base_y <- if(n_m >= 2) 0.56 else -0.38
        for(i in seq_along(int_terms)) {
          int_lbl <- resolve_interaction_label(int_terms[[i]], alias_map)
          int_lbl <- format_interaction_label(int_lbl, settings)
          if(n_m >= 2) {
            # Preferred 2-mediator layout: interaction above Y, near M1 height.
            add_node(int_lbl, int_base_x + 0.10 * (i - 1), int_base_y - 0.08 * (i - 1), "int")
          } else {
            add_node(int_lbl, int_base_x + 0.12 * (i - 1), int_base_y - 0.12 * (i - 1), "int")
          }
        }
      }
    } else if(model_num == 7L) {
      add_node(x_var, -0.82, 0.00, "x")
      add_node(y_var, 0.82, 0.00, "y")
      n_m <- length(mediators)
      if(n_m == 1) {
        add_node(mediators[[1]], 0.06, 0.56, "m")
      } else if(n_m >= 2) {
        # Model 7: keep mediator geometry balanced around X->Y like Model 4.
        add_node(mediators[[1]], 0.00, 0.56, "m")
        add_node(mediators[[2]], 0.00, -0.56, "m")
      }
      if(length(w_var) > 0) {
        if(n_m == 1) {
          # 1-mediator Model 7: keep W midway between INT and X levels.
          add_node(w_var, -0.84, 0.28, "mod")
        } else {
          # 2-mediator Model 7: keep W right edge aligned with X; INT sits further right.
          add_node(w_var, -0.82, -0.50, "mod")
        }
      }
      if(length(z_var) > 0) add_node(z_var, -0.92, 0.24, "mod")
      if(nrow(int_edges) > 0) {
        int_terms <- unique(int_edges$from)
        for(i in seq_along(int_terms)) {
          int_lbl <- resolve_interaction_label(int_terms[[i]], alias_map)
          int_lbl <- format_interaction_label(int_lbl, settings)
          if(n_m == 1) {
            # 1-mediator Model 7: interaction term horizontally aligned with M1.
            add_node(int_lbl, -0.84 + 0.10 * (i - 1), 0.56 + 0.10 * (i - 1), "int")
          } else {
            # 2-mediator Model 7: shift interaction right of W to separate W->M1 vs INT->M1 lanes.
            add_node(int_lbl, -0.36 + 0.14 * (i - 1), -0.92 - 0.10 * (i - 1), "int")
          }
        }
      }
    } else if(model_num == 14L) {
      add_node(x_var, -0.82, 0.00, "x")
      add_node(y_var, 0.82, 0.00, "y")
      n_m <- length(mediators)
      if(n_m == 1) {
        add_node(mediators[[1]], -0.06, 0.56, "m")
      } else if(n_m >= 2) {
        # Mirror of Model 7 two-mediator geometry around the Y-axis.
        add_node(mediators[[1]], 0.00, 0.56, "m")
        add_node(mediators[[2]], 0.00, -0.56, "m")
      }
      if(length(w_var) > 0) {
        if(n_m == 1) {
          # Model 14 (1 mediator): W vertically aligned under Y.
          add_node(w_var, 0.82, -0.56, "mod")
        } else {
          # Model 14 (2 mediators): W above Y (same centerline), left of INT lanes.
          add_node(w_var, 0.82, 0.56, "mod")
        }
      }
      if(length(z_var) > 0) add_node(z_var, -0.92, 0.24, "mod")
      if(nrow(int_edges) > 0) {
        int_terms <- unique(int_edges$from)
        for(i in seq_along(int_terms)) {
          int_lbl <- resolve_interaction_label(int_terms[[i]], alias_map)
          int_lbl <- format_interaction_label(int_lbl, settings)
          if(n_m == 1) {
            # Model 14 (1 mediator): INT vertically aligned above Y.
            add_node(int_lbl, 0.82, 0.56 + 0.10 * (i - 1), "int")
          } else {
            # Model 14 (2 mediators): interaction terms to the right of W, aligned to mediator lanes.
            y_lane <- if(i == 1) 0.56 else if(i == 2) -0.56 else (0.56 - 0.28 * (i - 1))
            x_lane <- if(i <= 2) 1.16 else (1.16 + 0.08 * (i - 2))
            add_node(int_lbl, x_lane, y_lane, "int")
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

  estimate_node_half_box <- function(label_txt) {
    txt <- as.character(label_txt)
    if(is.na(txt) || !nzchar(txt)) {
      return(c(hw = 0.085, hh = 0.055))
    }
    lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
    if(length(lines) == 0) lines <- ""
    max_chars <- max(nchar(lines, type = "width"), na.rm = TRUE)
    n_lines <- length(lines)
    # Calibrated for ggplot::geom_label defaults used in this module.
    # Conceptual diagrams use larger label text and need more conservative clipping.
    if(identical(diagram_type, "conceptual")) {
      hw <- 0.082 + 0.013 * max_chars
      hh <- 0.052 + 0.024 * n_lines
    } else {
      hw <- 0.055 + 0.010 * max_chars
      hh <- 0.040 + 0.020 * n_lines
    }
    c(hw = hw, hh = hh)
  }

  measure_node_half_box_precise <- function(labels, font_mm, x_span, y_span, width_px, height_px) {
    if(length(labels) == 0) return(matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("hw", "hh"))))
    w_px <- suppressWarnings(as.numeric(width_px))
    h_px <- suppressWarnings(as.numeric(height_px))
    if(is.na(w_px) || is.na(h_px) || w_px <= 0 || h_px <= 0) {
      return(NULL)
    }

    tf <- tempfile(fileext = ".png")
    opened <- FALSE
    on.exit({
      if(opened) grDevices::dev.off()
      if(file.exists(tf)) unlink(tf)
    }, add = TRUE)

    tryCatch({
      grDevices::png(
        filename = tf,
        width = as.integer(max(400, round(w_px))),
        height = as.integer(max(300, round(h_px))),
        units = "px",
        res = 96,
        bg = "white"
      )
      opened <<- TRUE
    }, error = function(e) {
      opened <<- FALSE
    })
    if(!opened) return(NULL)

    fontsize_pt <- font_mm * ggplot2::.pt
    pad_in <- (0.25 * 1.2 * fontsize_pt) / 72.27
    stroke_in <- 0.45 / 25.4

    widths_in <- vapply(labels, function(lbl) {
      tg <- grid::textGrob(lbl, gp = grid::gpar(fontsize = fontsize_pt, fontface = "bold"))
      grid::convertWidth(grid::grobWidth(tg), "in", valueOnly = TRUE)
    }, numeric(1))
    heights_in <- vapply(labels, function(lbl) {
      tg <- grid::textGrob(lbl, gp = grid::gpar(fontsize = fontsize_pt, fontface = "bold"))
      grid::convertHeight(grid::grobHeight(tg), "in", valueOnly = TRUE)
    }, numeric(1))

    box_w_in <- widths_in + 2 * pad_in + stroke_in
    box_h_in <- heights_in + 2 * pad_in + stroke_in

    panel_w_in <- max(1e-6, w_px / 96)
    panel_h_in <- max(1e-6, h_px / 96)

    hw <- ((box_w_in / panel_w_in) * x_span / 2) * 1.04
    hh <- ((box_h_in / panel_h_in) * y_span / 2) * 1.04
    cbind(hw = pmax(hw, 0.055), hh = pmax(hh, 0.040))
  }

  node_dims <- data.frame(
    name = nodes$name,
    hw = rep(0.085, nrow(nodes)),
    hh = rep(0.055, nrow(nodes)),
    stringsAsFactors = FALSE
  )
  if(nrow(nodes) > 0) {
    est_heur <- t(vapply(nodes$label, estimate_node_half_box, numeric(2)))
    est_precise <- measure_node_half_box_precise(
      labels = nodes$label,
      font_mm = if(identical(diagram_type, "conceptual")) 5.4 else 4.8,
      x_span = x_span,
      y_span = y_span,
      width_px = plot_width_px,
      height_px = plot_height_px
    )
    if(!is.null(est_precise) && nrow(est_precise) == nrow(nodes)) {
      # Use measured bounds, with a conservative safety multiplier, and never smaller
      # than heuristic fallback. This keeps clipping robust when labels become longer.
      safety_mult <- if(identical(diagram_type, "conceptual")) 1.10 else 1.07
      node_dims$hw <- pmax(est_precise[, "hw"] * safety_mult, est_heur[, "hw"])
      node_dims$hh <- pmax(est_precise[, "hh"] * safety_mult, est_heur[, "hh"])
    } else {
      node_dims$hw <- est_heur[, "hw"]
      node_dims$hh <- est_heur[, "hh"]
    }
  }
  if(model_num %in% c(7L, 14L) && length(mediators) == 1 && nrow(nodes) > 0) {
    # Conservative floor for the Model 7/14 (1 mediator) rect/text renderer:
    # ensure label boxes expand robustly as names get longer.
    est_conservative <- t(vapply(nodes$label, function(lbl) {
      txt <- as.character(lbl)
      if(is.na(txt) || !nzchar(txt)) return(c(hw = 0.10, hh = 0.06))
      lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
      if(length(lines) == 0) lines <- ""
      max_chars <- max(nchar(lines, type = "width"), na.rm = TRUE)
      n_lines <- length(lines)
      if(identical(diagram_type, "conceptual")) {
        c(hw = 0.102 + 0.016 * max_chars, hh = 0.064 + 0.027 * n_lines)
      } else {
        c(hw = 0.070 + 0.0125 * max_chars, hh = 0.046 + 0.022 * n_lines)
      }
    }, numeric(2)))
    node_dims$hw <- pmax(node_dims$hw, est_conservative[, "hw"])
    node_dims$hh <- pmax(node_dims$hh, est_conservative[, "hh"])
  }
  if(model_num %in% c(7L, 14L) && length(mediators) == 1 && nrow(nodes) > 0) {
    m1_name <- mediators[[1]]
    get_hw <- function(node_name) {
      j <- match(node_name, node_dims$name)
      if(is.na(j)) return(0.085)
      node_dims$hw[[j]]
    }
    pin_right <- function(node_name, x_right_ref) {
      idx <- which(nodes$name == node_name)
      if(length(idx) == 0) return()
      hw <- get_hw(node_name)
      nodes$x[idx[[1]]] <<- x_right_ref - hw
    }
    pin_left <- function(node_name, x_left_ref) {
      idx <- which(nodes$name == node_name)
      if(length(idx) == 0) return()
      hw <- get_hw(node_name)
      nodes$x[idx[[1]]] <<- x_left_ref + hw
    }

    # Anchor-sensitive layout for Model 7/14 (1 mediator):
    # keep key attachment sides fixed as labels grow/shrink.
    x_right_ref <- if(identical(diagram_type, "conceptual")) -0.72 else -0.78
    y_left_ref <- if(identical(diagram_type, "conceptual")) 0.72 else 0.78
    pin_right(x_var, x_right_ref)
    pin_left(y_var, y_left_ref)
    if(length(w_var) > 0) {
      if(model_num == 7L) {
        pin_right(w_var, x_right_ref)
      } else {
        idx_y <- which(nodes$name == y_var)
        idx_w <- which(nodes$name == w_var)
        if(length(idx_y) > 0 && length(idx_w) > 0) {
          # Model 14 (1 mediator): keep W centerline vertically aligned with Y.
          nodes$x[idx_w[[1]]] <- nodes$x[idx_y[[1]]]
        }
      }
    }

    idx_m1 <- which(nodes$name == m1_name)
    if(length(idx_m1) > 0) nodes$x[idx_m1[[1]]] <- 0.00

    idx_int <- which(nodes$role == "int")
    if(length(idx_int) > 0) {
      ord <- idx_int[order(-nodes$y[idx_int], nodes$name[idx_int])]
      for(k in seq_along(ord)) {
        i <- ord[[k]]
        nm <- nodes$name[[i]]
        hw <- get_hw(nm)
        if(model_num == 7L) {
          right_ref <- x_right_ref + 0.08 * (k - 1)
          nodes$x[[i]] <- right_ref - hw
        } else {
          idx_y <- which(nodes$name == y_var)
          if(length(idx_y) > 0) {
            # Model 14 (1 mediator): INT centerline vertically aligned with Y.
            nodes$x[[i]] <- nodes$x[idx_y[[1]]]
          } else {
            left_ref <- y_left_ref + 0.08 * (k - 1)
            nodes$x[[i]] <- left_ref + hw
          }
        }
      }
      if(length(w_var) > 0) {
        idx_w <- which(nodes$name == w_var)
        if(model_num == 7L) {
          idx_x <- which(nodes$name == x_var)
          if(length(idx_w) > 0 && length(idx_x) > 0) {
            nodes$y[idx_w[[1]]] <- (nodes$y[idx_x[[1]]] + nodes$y[ord[[1]]]) / 2
          }
        } else {
          idx_y <- which(nodes$name == y_var)
          if(length(idx_w) > 0 && length(idx_y) > 0 && length(ord) > 0) {
            # Place W directly below Y by mirroring INT's vertical distance.
            delta <- abs(nodes$y[ord[[1]]] - nodes$y[idx_y[[1]]])
            if(delta < 1e-6) delta <- 0.56
            nodes$y[idx_w[[1]]] <- nodes$y[idx_y[[1]]] - delta
          }
        }
      }
    }
  }
  if(!identical(diagram_type, "conceptual") && model_num == 1L && nrow(nodes) > 0) {
    # Model 1 statistical: pin left cluster by right edge and Y by left edge
    # so endpoint geometry stays stable under long label text.
    get_hw <- function(node_name) {
      j <- match(node_name, node_dims$name)
      if(is.na(j)) return(0.085)
      node_dims$hw[[j]]
    }
    pin_right <- function(node_name, x_right_ref) {
      idx <- which(nodes$name == node_name)
      if(length(idx) == 0) return()
      hw <- get_hw(node_name)
      nodes$x[idx[[1]]] <<- x_right_ref - hw
    }
    pin_left <- function(node_name, x_left_ref) {
      idx <- which(nodes$name == node_name)
      if(length(idx) == 0) return()
      hw <- get_hw(node_name)
      nodes$x[idx[[1]]] <<- x_left_ref + hw
    }

    x_right_ref <- -0.66
    y_left_ref <- 0.66
    pin_right(x_var, x_right_ref)
    if(length(w_var) > 0) pin_right(w_var, x_right_ref)

    idx_int <- which(nodes$role == "int")
    if(length(idx_int) > 0) {
      for(i in idx_int) {
        nm <- nodes$name[[i]]
        nodes$x[[i]] <- x_right_ref - get_hw(nm)
      }
    }
    pin_left(y_var, y_left_ref)
  }
  if(!identical(diagram_type, "conceptual") && model_num == 2L && nrow(nodes) > 0) {
    # Model 2 statistical: stabilize left cluster and Y anchor sides so
    # start/end attachment points stay consistent as labels expand.
    get_hw <- function(node_name) {
      j <- match(node_name, node_dims$name)
      if(is.na(j)) return(0.085)
      node_dims$hw[[j]]
    }
    pin_right <- function(node_name, x_right_ref) {
      idx <- which(nodes$name == node_name)
      if(length(idx) == 0) return()
      hw <- get_hw(node_name)
      nodes$x[idx[[1]]] <<- x_right_ref - hw
    }
    pin_left <- function(node_name, x_left_ref) {
      idx <- which(nodes$name == node_name)
      if(length(idx) == 0) return()
      hw <- get_hw(node_name)
      nodes$x[idx[[1]]] <<- x_left_ref + hw
    }

    x_right_ref <- -0.66
    y_left_ref <- 0.66
    pin_right(x_var, x_right_ref)
    if(length(w_var) > 0) pin_right(w_var, x_right_ref)
    if(length(z_var) > 0) pin_right(z_var, x_right_ref)

    idx_int <- which(nodes$role == "int")
    if(length(idx_int) > 0) {
      for(i in idx_int) {
        nm <- nodes$name[[i]]
        nodes$x[[i]] <- x_right_ref - get_hw(nm)
      }
    }
    pin_left(y_var, y_left_ref)
  }
  if(!identical(diagram_type, "conceptual") && model_num == 3L && nrow(nodes) > 0) {
    # Model 3 statistical: side-pin the left cluster and Y anchor side for
    # label-length robustness in dense multi-line convergence.
    get_hw <- function(node_name) {
      j <- match(node_name, node_dims$name)
      if(is.na(j)) return(0.085)
      node_dims$hw[[j]]
    }
    pin_right <- function(node_name, x_right_ref) {
      idx <- which(nodes$name == node_name)
      if(length(idx) == 0) return()
      hw <- get_hw(node_name)
      nodes$x[idx[[1]]] <<- x_right_ref - hw
    }
    pin_left <- function(node_name, x_left_ref) {
      idx <- which(nodes$name == node_name)
      if(length(idx) == 0) return()
      hw <- get_hw(node_name)
      nodes$x[idx[[1]]] <<- x_left_ref + hw
    }

    x_right_ref <- -0.66
    y_left_ref <- 0.66
    pin_right(x_var, x_right_ref)
    if(length(w_var) > 0) pin_right(w_var, x_right_ref)
    if(length(z_var) > 0) pin_right(z_var, x_right_ref)

    idx_int <- which(nodes$role == "int")
    if(length(idx_int) > 0) {
      for(i in idx_int) {
        nm <- nodes$name[[i]]
        nodes$x[[i]] <- x_right_ref - get_hw(nm)
      }
    }
    pin_left(y_var, y_left_ref)
  }
  if(!identical(diagram_type, "conceptual") && model_num == 4L && nrow(nodes) > 0) {
    # Model 4 statistical (1M/2M): pin X/Y attachment sides while keeping
    # mediator centers fixed so long labels do not shift the overall geometry.
    get_hw <- function(node_name) {
      j <- match(node_name, node_dims$name)
      if(is.na(j)) return(0.085)
      node_dims$hw[[j]]
    }
    pin_right <- function(nodes_df, node_name, x_right_ref) {
      idx <- which(nodes_df$name == node_name)
      if(length(idx) == 0) return(nodes_df)
      hw <- get_hw(node_name)
      nodes_df$x[idx[[1]]] <- x_right_ref - hw
      nodes_df
    }
    pin_left <- function(nodes_df, node_name, x_left_ref) {
      idx <- which(nodes_df$name == node_name)
      if(length(idx) == 0) return(nodes_df)
      hw <- get_hw(node_name)
      nodes_df$x[idx[[1]]] <- x_left_ref + hw
      nodes_df
    }
    x_right_ref <- -0.72
    y_left_ref <- 0.72
    nodes <- pin_right(nodes, x_var, x_right_ref)
    nodes <- pin_left(nodes, y_var, y_left_ref)
    for(m_name in mediators[mediators %in% nodes$name]) {
      idx <- which(nodes$name == m_name)
      if(length(idx) > 0) nodes$x[idx[[1]]] <- 0.00
    }
  }
  if(!identical(diagram_type, "conceptual") && model_num == 5L && nrow(nodes) > 0) {
    # Model 5 statistical (1M/2M): stabilize X/Y side anchors; preserve mediator centers;
    # for 2M, keep W/INT vertically aligned to Y for second-stage paths.
    get_hw <- function(node_name) {
      j <- match(node_name, node_dims$name)
      if(is.na(j)) return(0.085)
      node_dims$hw[[j]]
    }
    pin_right <- function(nodes_df, node_name, x_right_ref) {
      idx <- which(nodes_df$name == node_name)
      if(length(idx) == 0) return(nodes_df)
      hw <- get_hw(node_name)
      nodes_df$x[idx[[1]]] <- x_right_ref - hw
      nodes_df
    }
    pin_left <- function(nodes_df, node_name, x_left_ref) {
      idx <- which(nodes_df$name == node_name)
      if(length(idx) == 0) return(nodes_df)
      hw <- get_hw(node_name)
      nodes_df$x[idx[[1]]] <- x_left_ref + hw
      nodes_df
    }
    x_right_ref <- -0.74
    y_left_ref <- 0.74
    nodes <- pin_right(nodes, x_var, x_right_ref)
    nodes <- pin_left(nodes, y_var, y_left_ref)

    # Keep mediator center column stable in the intended template lane.
    for(m_name in mediators[mediators %in% nodes$name]) {
      idx <- which(nodes$name == m_name)
      if(length(idx) > 0) nodes$x[idx[[1]]] <- 0.02
    }

    if(length(mediators) >= 2) {
      idx_y <- which(nodes$name == y_var)
      if(length(idx_y) > 0) {
        y_cx <- nodes$x[idx_y[[1]]]
        # Align moderator and interaction centers with Y so their paths are vertical.
        for(i in which(nodes$role %in% c("mod", "int"))) {
          nodes$x[i] <- y_cx
        }
      }
    }
  }
  if(!identical(diagram_type, "conceptual") && model_num == 6L && nrow(nodes) > 0) {
    # Model 6 statistical (2M serial): preserve the established top-row serial layout
    # while pinning X/Y attachment sides so long labels do not shift the diagram geometry.
    get_hw <- function(node_name) {
      j <- match(node_name, node_dims$name)
      if(is.na(j)) return(0.085)
      node_dims$hw[[j]]
    }
    pin_right <- function(nodes_df, node_name, x_right_ref) {
      idx <- which(nodes_df$name == node_name)
      if(length(idx) == 0) return(nodes_df)
      hw <- get_hw(node_name)
      nodes_df$x[idx[[1]]] <- x_right_ref - hw
      nodes_df
    }
    pin_left <- function(nodes_df, node_name, x_left_ref) {
      idx <- which(nodes_df$name == node_name)
      if(length(idx) == 0) return(nodes_df)
      hw <- get_hw(node_name)
      nodes_df$x[idx[[1]]] <- x_left_ref + hw
      nodes_df
    }
    x_right_ref <- -0.72
    y_left_ref <- 0.72
    nodes <- pin_right(nodes, x_var, x_right_ref)
    nodes <- pin_left(nodes, y_var, y_left_ref)
    if(length(mediators) >= 2) {
      x0 <- -0.75
      x1 <- 0.75
      m1_idx <- which(nodes$name == mediators[[1]])
      m2_idx <- which(nodes$name == mediators[[2]])
      if(length(m1_idx) > 0) nodes$x[m1_idx[[1]]] <- x0 + 0.25 * (x1 - x0)
      if(length(m2_idx) > 0) nodes$x[m2_idx[[1]]] <- x0 + 0.75 * (x1 - x0)
    }
  }
  if(!identical(diagram_type, "conceptual") && model_num == 8L && nrow(nodes) > 0) {
    # Model 8 statistical (1M/2M): stabilize X/W/INT on the left by right edge, pin Y by left edge,
    # and keep mediator centers in fixed lanes so label width changes do not distort the layout.
    get_hw <- function(node_name) {
      j <- match(node_name, node_dims$name)
      if(is.na(j)) return(0.085)
      node_dims$hw[[j]]
    }
    pin_right <- function(nodes_df, node_name, x_right_ref) {
      idx <- which(nodes_df$name == node_name)
      if(length(idx) == 0) return(nodes_df)
      hw <- get_hw(node_name)
      nodes_df$x[idx[[1]]] <- x_right_ref - hw
      nodes_df
    }
    pin_left <- function(nodes_df, node_name, x_left_ref) {
      idx <- which(nodes_df$name == node_name)
      if(length(idx) == 0) return(nodes_df)
      hw <- get_hw(node_name)
      nodes_df$x[idx[[1]]] <- x_left_ref + hw
      nodes_df
    }
    x_right_ref <- -0.72
    w_right_ref <- -0.72
    y_left_ref <- 0.72
    nodes <- pin_right(nodes, x_var, x_right_ref)
    if(length(w_var) > 0) nodes <- pin_right(nodes, w_var, w_right_ref)
    nodes <- pin_left(nodes, y_var, y_left_ref)

    # Keep interaction terms in a stable left-lower lane.
    idx_int <- which(nodes$role == "int")
    if(length(idx_int) > 0) {
      int_right_ref <- if(length(mediators) >= 2) -0.38 else -0.30
      for(i in idx_int) {
        nm <- nodes$name[[i]]
        nodes$x[[i]] <- int_right_ref - get_hw(nm)
      }
    }

    # Keep mediator centers fixed in template lanes.
    n_m <- length(mediators)
    if(n_m >= 1) {
      m1_idx <- which(nodes$name == mediators[[1]])
      if(length(m1_idx) > 0) nodes$x[m1_idx[[1]]] <- 0.06
    }
    if(n_m >= 2) {
      for(i in 2:n_m) {
        mi_idx <- which(nodes$name == mediators[[i]])
        if(length(mi_idx) > 0) nodes$x[mi_idx[[1]]] <- 0.10
      }
    }
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

  from_dims <- node_dims
  names(from_dims) <- c("from", "hw_from", "hh_from")
  to_dims <- node_dims
  names(to_dims) <- c("to", "hw_to", "hh_to")
  edge_plot <- merge(edge_plot, from_dims, by = "from", all.x = TRUE)
  edge_plot <- merge(edge_plot, to_dims, by = "to", all.x = TRUE)
  edge_plot$hw_from[is.na(edge_plot$hw_from)] <- 0.085
  edge_plot$hh_from[is.na(edge_plot$hh_from)] <- 0.055
  edge_plot$hw_to[is.na(edge_plot$hw_to)] <- 0.085
  edge_plot$hh_to[is.na(edge_plot$hh_to)] <- 0.055

  clip_segment_to_box <- function(x0, y0, x1, y1,
                                  hw0 = 0.085, hh0 = 0.055,
                                  hw1 = 0.085, hh1 = 0.055) {
    dx <- x1 - x0
    dy <- y1 - y0
    seg_len <- sqrt(dx * dx + dy * dy)
    if(seg_len <= 1e-9) {
      return(c(x0, y0, x1, y1))
    }
    t0x <- if(abs(dx) < 1e-9) Inf else hw0 / abs(dx)
    t0y <- if(abs(dy) < 1e-9) Inf else hh0 / abs(dy)
    t0 <- min(t0x, t0y)
    xs <- x0 + dx * t0
    ys <- y0 + dy * t0

    t1x <- if(abs(dx) < 1e-9) Inf else hw1 / abs(dx)
    t1y <- if(abs(dy) < 1e-9) Inf else hh1 / abs(dy)
    t1 <- min(t1x, t1y)
    xe <- x1 - dx * t1
    ye <- y1 - dy * t1
    c(xs, ys, xe, ye)
  }

  inset_segment <- function(x0, y0, x1, y1, pad = 0) {
    if(is.na(pad) || pad <= 0) return(c(x0, y0, x1, y1))
    dx <- x1 - x0
    dy <- y1 - y0
    seg_len <- sqrt(dx * dx + dy * dy)
    if(seg_len <= 1e-9) return(c(x0, y0, x1, y1))
    # Keep a valid segment even for short paths.
    step <- min(pad, seg_len * 0.45)
    ux <- dx / seg_len
    uy <- dy / seg_len
    c(
      x0 + ux * step,
      y0 + uy * step,
      x1 - ux * step,
      y1 - uy * step
    )
  }
  inset_segment_asymmetric <- function(x0, y0, x1, y1, pad_start = 0, pad_end = 0) {
    ps <- suppressWarnings(as.numeric(pad_start))
    pe <- suppressWarnings(as.numeric(pad_end))
    if(is.na(ps) || ps < 0) ps <- 0
    if(is.na(pe) || pe < 0) pe <- 0
    if(ps <= 0 && pe <= 0) return(c(x0, y0, x1, y1))
    dx <- x1 - x0
    dy <- y1 - y0
    seg_len <- sqrt(dx * dx + dy * dy)
    if(seg_len <= 1e-9) return(c(x0, y0, x1, y1))
    step_start <- min(ps, seg_len * 0.90)
    step_end <- min(pe, seg_len * 0.90)
    total <- step_start + step_end
    max_total <- seg_len * 0.90
    if(total > max_total && total > 0) {
      scale <- max_total / total
      step_start <- step_start * scale
      step_end <- step_end * scale
    }
    ux <- dx / seg_len
    uy <- dy / seg_len
    c(
      x0 + ux * step_start,
      y0 + uy * step_start,
      x1 - ux * step_end,
      y1 - uy * step_end
    )
  }

  retract_endpoint <- function(x0, y0, x1, y1, pad = 0) {
    if(is.na(pad) || pad <= 0) return(c(x0, y0, x1, y1))
    dx <- x1 - x0
    dy <- y1 - y0
    seg_len <- sqrt(dx * dx + dy * dy)
    if(seg_len <= 1e-9) return(c(x0, y0, x1, y1))
    step <- min(pad, seg_len * 0.45)
    ux <- dx / seg_len
    uy <- dy / seg_len
    c(x0, y0, x1 - ux * step, y1 - uy * step)
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
        include_stars = include_stars,
        coef_digits = coef_digits
      )
    })
  }
  
  clipped <- t(mapply(
    clip_segment_to_box,
    edge_plot$x_from, edge_plot$y_from, edge_plot$x_to, edge_plot$y_to,
    edge_plot$hw_from, edge_plot$hh_from, edge_plot$hw_to, edge_plot$hh_to
  ))
  if(!identical(diagram_type, "conceptual") && model_num %in% c(1L, 2L, 3L, 7L)) {
    idx_to_y <- which(edge_plot$to == y_var)
    if(length(idx_to_y) > 0) {
      # Requested for Models 1-3 (extended to 7): all paths to Y leave source nodes from right-middle.
      clipped[idx_to_y, 1] <- edge_plot$x_from[idx_to_y] + edge_plot$hw_from[idx_to_y]
      clipped[idx_to_y, 2] <- edge_plot$y_from[idx_to_y]
      # Paths terminate on Y's left edge.
      clipped[idx_to_y, 3] <- edge_plot$x_to[idx_to_y] - edge_plot$hw_to[idx_to_y]

      if(model_num == 1L) {
        # Model 1: explicit top/middle/bottom terminations on Y (W, X, INT).
        idx_mod <- idx_to_y[edge_plot$from_role[idx_to_y] == "mod"]
        idx_x <- idx_to_y[edge_plot$from_role[idx_to_y] == "x"]
        idx_int <- idx_to_y[edge_plot$from_role[idx_to_y] == "int"]
        if(length(idx_mod) > 0) {
          clipped[idx_mod, 4] <- edge_plot$y_to[idx_mod] + 0.72 * edge_plot$hh_to[idx_mod]
        }
        if(length(idx_x) > 0) {
          clipped[idx_x, 4] <- edge_plot$y_to[idx_x]
        }
        if(length(idx_int) > 0) {
          clipped[idx_int, 4] <- edge_plot$y_to[idx_int] - 0.72 * edge_plot$hh_to[idx_int]
        }
        idx_other <- setdiff(idx_to_y, c(idx_mod, idx_x, idx_int))
        if(length(idx_other) > 0) {
          ord_other <- idx_other[order(-edge_plot$y_from[idx_other], edge_plot$from[idx_other])]
          y_mid <- edge_plot$y_to[ord_other][1]
          h <- edge_plot$hh_to[ord_other][1]
          y_vals <- if(length(ord_other) == 1) y_mid else seq(y_mid + 0.72 * h, y_mid - 0.72 * h, length.out = length(ord_other))
          clipped[ord_other, 4] <- y_vals
        }
      } else if(model_num == 2L) {
        # Model 2: keep X->Y horizontal at center; distribute all other Y-bound
        # terminations from top to bottom to avoid endpoint stacking.
        idx_x <- idx_to_y[edge_plot$from_role[idx_to_y] == "x"]
        if(length(idx_x) > 0) {
          clipped[idx_x, 4] <- edge_plot$y_to[idx_x]
        }
        idx_other <- setdiff(idx_to_y, idx_x)
        if(length(idx_other) > 0) {
          x_src_y <- if(length(idx_x) > 0) edge_plot$y_from[idx_x[[1]]] else 0
          y_mid <- edge_plot$y_to[idx_other][1]
          h <- edge_plot$hh_to[idx_other][1]

          idx_top <- idx_other[edge_plot$y_from[idx_other] >= x_src_y]
          idx_bot <- idx_other[edge_plot$y_from[idx_other] < x_src_y]

          if(length(idx_top) > 0) {
            ord_top <- idx_top[order(-edge_plot$y_from[idx_top], edge_plot$from[idx_top])]
            y_top <- if(length(ord_top) == 1) {
              # Single top-side path (typically W->Y): keep it above X->Y
              # but closer to the center lane for better visual grouping.
              y_mid + 0.30 * h
            } else {
              seq(y_mid + 0.72 * h, y_mid + 0.24 * h, length.out = length(ord_top))
            }
            clipped[ord_top, 4] <- y_top
          }

          if(length(idx_bot) > 0) {
            # Keep the source closest to X just below the X->Y center lane
            # (e.g., Z main effect), with lower interaction paths stacked below.
            ord_bot <- idx_bot[order(-edge_plot$y_from[idx_bot], edge_plot$from[idx_bot])]
            y_bot <- if(length(ord_bot) == 1) {
              y_mid - 0.60 * h
            } else {
              seq(y_mid - 0.24 * h, y_mid - 0.72 * h, length.out = length(ord_bot))
            }
            clipped[ord_bot, 4] <- y_bot
          }
        }
      } else if(model_num == 3L) {
        # Model 3: same side-preserving Y-lane logic as Model 2, but allow a
        # slightly deeper lower stack because there are typically more interaction paths.
        idx_x <- idx_to_y[edge_plot$from_role[idx_to_y] == "x"]
        if(length(idx_x) > 0) {
          clipped[idx_x, 4] <- edge_plot$y_to[idx_x]
        }
        idx_other <- setdiff(idx_to_y, idx_x)
        if(length(idx_other) > 0) {
          x_src_y <- if(length(idx_x) > 0) edge_plot$y_from[idx_x[[1]]] else 0
          y_mid <- edge_plot$y_to[idx_other][1]
          h <- edge_plot$hh_to[idx_other][1]

          idx_top <- idx_other[edge_plot$y_from[idx_other] >= x_src_y]
          idx_bot <- idx_other[edge_plot$y_from[idx_other] < x_src_y]

          if(length(idx_top) > 0) {
            ord_top <- idx_top[order(-edge_plot$y_from[idx_top], edge_plot$from[idx_top])]
            y_top <- if(length(ord_top) == 1) {
              # Single top-side path (typically W->Y): bring it closer to X->Y
              # without collapsing into the center lane.
              y_mid + 0.22 * h
            } else {
              seq(y_mid + 0.72 * h, y_mid + 0.22 * h, length.out = length(ord_top))
            }
            clipped[ord_top, 4] <- y_top
          }

          if(length(idx_bot) > 0) {
            ord_bot <- idx_bot[order(-edge_plot$y_from[idx_bot], edge_plot$from[idx_bot])]
            y_bot <- if(length(ord_bot) == 1) {
              y_mid - 0.60 * h
            } else {
              seq(y_mid - 0.22 * h, y_mid - 0.86 * h, length.out = length(ord_bot))
            }
            clipped[ord_bot, 4] <- y_bot
          }
        }
      } else {
        # Models 2-7 fallback: equally space all Y-bound terminations from top to bottom.
        ord <- idx_to_y[order(-edge_plot$y_from[idx_to_y], edge_plot$from[idx_to_y])]
        y_mid <- edge_plot$y_to[ord][1]
        h <- edge_plot$hh_to[ord][1]
        y_vals <- if(length(ord) == 1) y_mid else seq(y_mid + 0.74 * h, y_mid - 0.74 * h, length.out = length(ord))
        clipped[ord, 4] <- y_vals
      }
    }
  }
  if(model_num == 7L && length(mediators) == 1) {
    m1_name <- mediators[[1]]
    # Bottom-edge split anchors for first-stage X and second-stage M1->Y.
    # `anchor_frac` is measured from node center toward side edge (0=center, 1=edge).
    # Kept as a single scalar so M2 top-edge mirrors can be added with the same rule.
    anchor_frac <- 0.75
    x_names <- nodes$name[nodes$role == "x"]
    mod_names <- nodes$name[nodes$role == "mod"]
    int_names <- nodes$name[nodes$role == "int"]
    idx_x_y <- edge_plot$from %in% x_names & edge_plot$to == y_var
    idx_x_m1 <- edge_plot$from %in% x_names & edge_plot$to == m1_name
    idx_w_m1 <- edge_plot$from %in% mod_names & edge_plot$to == m1_name
    idx_int_m1 <- edge_plot$from %in% int_names & edge_plot$to == m1_name
    idx_m1_y <- edge_plot$from == m1_name & edge_plot$to == y_var

    # Keep direct path anchored on fixed box sides regardless label width.
    if(any(idx_x_y)) {
      clipped[idx_x_y, 1] <- edge_plot$x_from[idx_x_y] + edge_plot$hw_from[idx_x_y]
      clipped[idx_x_y, 2] <- edge_plot$y_from[idx_x_y]
      clipped[idx_x_y, 3] <- edge_plot$x_to[idx_x_y] - edge_plot$hw_to[idx_x_y]
      clipped[idx_x_y, 4] <- edge_plot$y_to[idx_x_y]
    }
    # First-stage paths use explicit lane anchors on M1's left edge.
    if(any(idx_x_m1)) {
      clipped[idx_x_m1, 1] <- edge_plot$x_from[idx_x_m1] + edge_plot$hw_from[idx_x_m1]
      clipped[idx_x_m1, 2] <- edge_plot$y_from[idx_x_m1] + 0.52 * edge_plot$hh_from[idx_x_m1]
      if(!identical(diagram_type, "conceptual")) {
        # X -> M1: land on M1 bottom edge, left of center.
        clipped[idx_x_m1, 3] <- edge_plot$x_to[idx_x_m1] - anchor_frac * edge_plot$hw_to[idx_x_m1]
        clipped[idx_x_m1, 4] <- edge_plot$y_to[idx_x_m1] - edge_plot$hh_to[idx_x_m1]
      } else {
        clipped[idx_x_m1, 3] <- edge_plot$x_to[idx_x_m1] - edge_plot$hw_to[idx_x_m1]
        clipped[idx_x_m1, 4] <- edge_plot$y_to[idx_x_m1] - 0.34 * edge_plot$hh_to[idx_x_m1]
      }
    }
    if(any(idx_w_m1)) {
      clipped[idx_w_m1, 1] <- edge_plot$x_from[idx_w_m1] + edge_plot$hw_from[idx_w_m1]
      clipped[idx_w_m1, 2] <- edge_plot$y_from[idx_w_m1]
      clipped[idx_w_m1, 3] <- edge_plot$x_to[idx_w_m1] - edge_plot$hw_to[idx_w_m1]
      clipped[idx_w_m1, 4] <- edge_plot$y_to[idx_w_m1] - 0.62 * edge_plot$hh_to[idx_w_m1]
    }
    if(any(idx_int_m1)) {
      clipped[idx_int_m1, 1] <- edge_plot$x_from[idx_int_m1] + edge_plot$hw_from[idx_int_m1]
      clipped[idx_int_m1, 2] <- edge_plot$y_from[idx_int_m1]
      clipped[idx_int_m1, 3] <- edge_plot$x_to[idx_int_m1] - edge_plot$hw_to[idx_int_m1]
      clipped[idx_int_m1, 4] <- edge_plot$y_to[idx_int_m1]
    }
    if(any(idx_m1_y)) {
      if(identical(diagram_type, "conceptual")) {
        clipped[idx_m1_y, 1] <- edge_plot$x_from[idx_m1_y] + edge_plot$hw_from[idx_m1_y]
        clipped[idx_m1_y, 2] <- edge_plot$y_from[idx_m1_y]
      } else {
        # M1 -> Y: leave from M1 bottom edge, right of center (mirror of X->M1 landing).
        clipped[idx_m1_y, 1] <- edge_plot$x_from[idx_m1_y] + anchor_frac * edge_plot$hw_from[idx_m1_y]
        clipped[idx_m1_y, 2] <- edge_plot$y_from[idx_m1_y] - edge_plot$hh_from[idx_m1_y]
      }
      clipped[idx_m1_y, 3] <- edge_plot$x_to[idx_m1_y] - edge_plot$hw_to[idx_m1_y]
      clipped[idx_m1_y, 4] <- edge_plot$y_to[idx_m1_y]
    }
  } else if(!identical(diagram_type, "conceptual") && model_num == 4L) {
    # Keep Model 4 direct effect on fixed horizontal side anchors regardless of label width.
    idx_x_y <- edge_plot$from_role == "x" & edge_plot$to == y_var
    if(any(idx_x_y)) {
      clipped[idx_x_y, 1] <- edge_plot$x_from[idx_x_y] + edge_plot$hw_from[idx_x_y]
      clipped[idx_x_y, 2] <- edge_plot$y_from[idx_x_y]
      clipped[idx_x_y, 3] <- edge_plot$x_to[idx_x_y] - edge_plot$hw_to[idx_x_y]
      clipped[idx_x_y, 4] <- edge_plot$y_to[idx_x_y]
    }
  } else if(!identical(diagram_type, "conceptual") && model_num == 5L) {
    # Keep Model 5 direct effect on fixed horizontal side anchors regardless of label width.
    idx_x_y <- edge_plot$from_role == "x" & edge_plot$to == y_var
    if(any(idx_x_y)) {
      clipped[idx_x_y, 1] <- edge_plot$x_from[idx_x_y] + edge_plot$hw_from[idx_x_y]
      clipped[idx_x_y, 2] <- edge_plot$y_from[idx_x_y]
      clipped[idx_x_y, 3] <- edge_plot$x_to[idx_x_y] - edge_plot$hw_to[idx_x_y]
      clipped[idx_x_y, 4] <- edge_plot$y_to[idx_x_y]
    }
    if(length(mediators) >= 2) {
      # Explicit vertical second-stage moderation paths to Y (top for INT, bottom for W).
      idx_mod_y <- edge_plot$from_role == "mod" & edge_plot$to == y_var
      idx_int_y <- edge_plot$from_role == "int" & edge_plot$to == y_var
      if(any(idx_mod_y)) {
        clipped[idx_mod_y, 1] <- edge_plot$x_from[idx_mod_y]
        clipped[idx_mod_y, 2] <- edge_plot$y_from[idx_mod_y] + edge_plot$hh_from[idx_mod_y]
        clipped[idx_mod_y, 3] <- edge_plot$x_to[idx_mod_y]
        clipped[idx_mod_y, 4] <- edge_plot$y_to[idx_mod_y] - edge_plot$hh_to[idx_mod_y]
      }
      if(any(idx_int_y)) {
        clipped[idx_int_y, 1] <- edge_plot$x_from[idx_int_y]
        clipped[idx_int_y, 2] <- edge_plot$y_from[idx_int_y] - edge_plot$hh_from[idx_int_y]
        clipped[idx_int_y, 3] <- edge_plot$x_to[idx_int_y]
        clipped[idx_int_y, 4] <- edge_plot$y_to[idx_int_y] + edge_plot$hh_to[idx_int_y]
      }
    }
  } else if(!identical(diagram_type, "conceptual") && model_num == 6L) {
    # Keep Model 6 direct effect on fixed horizontal side anchors regardless of label width.
    idx_x_y <- edge_plot$from_role == "x" & edge_plot$to == y_var
    if(any(idx_x_y)) {
      clipped[idx_x_y, 1] <- edge_plot$x_from[idx_x_y] + edge_plot$hw_from[idx_x_y]
      clipped[idx_x_y, 2] <- edge_plot$y_from[idx_x_y]
      clipped[idx_x_y, 3] <- edge_plot$x_to[idx_x_y] - edge_plot$hw_to[idx_x_y]
      clipped[idx_x_y, 4] <- edge_plot$y_to[idx_x_y]
    }
  } else if(!identical(diagram_type, "conceptual") && model_num == 8L) {
    # Model 8 statistical: preserve a clean X->Y horizontal lane and distribute
    # mediator/Y endpoints to reduce crowding from W/INT paths.
    idx_x_y <- edge_plot$from_role == "x" & edge_plot$to == y_var
    if(any(idx_x_y)) {
      clipped[idx_x_y, 1] <- edge_plot$x_from[idx_x_y] + edge_plot$hw_from[idx_x_y]
      clipped[idx_x_y, 2] <- edge_plot$y_from[idx_x_y]
      clipped[idx_x_y, 3] <- edge_plot$x_to[idx_x_y] - edge_plot$hw_to[idx_x_y]
      clipped[idx_x_y, 4] <- edge_plot$y_to[idx_x_y]
    }

    # Keep X->M paths leaving from stable upper/lower right lanes on X.
    if(length(mediators) >= 1) {
      m1_name <- mediators[[1]]
      idx_x_m1 <- edge_plot$from_role == "x" & edge_plot$to == m1_name
      if(any(idx_x_m1)) {
        clipped[idx_x_m1, 1] <- edge_plot$x_from[idx_x_m1] + edge_plot$hw_from[idx_x_m1]
        clipped[idx_x_m1, 2] <- edge_plot$y_from[idx_x_m1] + 0.50 * edge_plot$hh_from[idx_x_m1]
      }
    }
    if(length(mediators) >= 2) {
      for(i in 2:length(mediators)) {
        mi_name <- mediators[[i]]
        idx_x_mi <- edge_plot$from_role == "x" & edge_plot$to == mi_name
        if(any(idx_x_mi)) {
          clipped[idx_x_mi, 1] <- edge_plot$x_from[idx_x_mi] + edge_plot$hw_from[idx_x_mi]
          clipped[idx_x_mi, 2] <- edge_plot$y_from[idx_x_mi] - 0.50 * edge_plot$hh_from[idx_x_mi]
        }
      }
    }

    # Distribute incoming lanes on each mediator (left edge) to avoid overlap among X/W/INT.
    for(m_name in mediators) {
      idx_to_m <- which(edge_plot$to == m_name)
      if(length(idx_to_m) == 0) next
      y_mid <- edge_plot$y_to[idx_to_m][1]
      h <- edge_plot$hh_to[idx_to_m][1]
      idx_mod <- idx_to_m[edge_plot$from_role[idx_to_m] == "mod"]
      idx_x <- idx_to_m[edge_plot$from_role[idx_to_m] == "x"]
      idx_int <- idx_to_m[edge_plot$from_role[idx_to_m] == "int"]
      # Default to left-edge arrivals, then apply mediator-specific lane intent.
      clipped[idx_to_m, 3] <- edge_plot$x_to[idx_to_m] - edge_plot$hw_to[idx_to_m]
      if(length(mediators) >= 1 && identical(m_name, mediators[[1]])) {
        # M1: X/W use left-edge lanes, INT lands middle-bottom.
        if(length(idx_x) > 0) clipped[idx_x, 4] <- y_mid + 0.18 * h
        if(length(idx_mod) > 0) clipped[idx_mod, 4] <- y_mid - 0.12 * h
        if(length(idx_int) > 0) {
          clipped[idx_int, 3] <- edge_plot$x_to[idx_int]
          clipped[idx_int, 4] <- edge_plot$y_to[idx_int] - edge_plot$hh_to[idx_int]
        }
      } else if(length(mediators) >= 2 && identical(m_name, mediators[[2]])) {
        # M2 requested lane order on left edge: X top, W middle, INT bottom.
        if(length(idx_x) > 0) {
          clipped[idx_x, 3] <- edge_plot$x_to[idx_x] - edge_plot$hw_to[idx_x]
          clipped[idx_x, 4] <- y_mid + 0.72 * h
        }
        if(length(idx_mod) > 0) clipped[idx_mod, 4] <- y_mid
        if(length(idx_int) > 0) {
          clipped[idx_int, 3] <- edge_plot$x_to[idx_int] - edge_plot$hw_to[idx_int]
          clipped[idx_int, 4] <- y_mid - 0.72 * h
        }
      } else {
        if(length(idx_mod) > 0) clipped[idx_mod, 4] <- y_mid - 0.28 * h
        if(length(idx_x) > 0) clipped[idx_x, 4] <- y_mid
        if(length(idx_int) > 0) clipped[idx_int, 4] <- y_mid + 0.42 * h
      }
      idx_other <- setdiff(idx_to_m, c(idx_mod, idx_x, idx_int))
      if(length(idx_other) > 0) {
        ord <- idx_other[order(-edge_plot$y_from[idx_other], edge_plot$from[idx_other])]
        vals <- if(length(ord) == 1) y_mid else seq(y_mid + 0.55 * h, y_mid - 0.55 * h, length.out = length(ord))
        clipped[ord, 4] <- vals
      }
    }

    # Improve mediator->Y departure lanes so they don't merge at the same point on the mediator box.
    if(length(mediators) >= 1) {
      m1_name <- mediators[[1]]
      idx_m1_y <- edge_plot$from == m1_name & edge_plot$to == y_var
      if(any(idx_m1_y)) {
        clipped[idx_m1_y, 1] <- edge_plot$x_from[idx_m1_y] + edge_plot$hw_from[idx_m1_y]
        clipped[idx_m1_y, 2] <- edge_plot$y_from[idx_m1_y] - 0.34 * edge_plot$hh_from[idx_m1_y]
      }
    }
    if(length(mediators) >= 2) {
      for(i in 2:length(mediators)) {
        mi_name <- mediators[[i]]
        idx_mi_y <- edge_plot$from == mi_name & edge_plot$to == y_var
        if(any(idx_mi_y)) {
          clipped[idx_mi_y, 1] <- edge_plot$x_from[idx_mi_y] + edge_plot$hw_from[idx_mi_y]
          clipped[idx_mi_y, 2] <- edge_plot$y_from[idx_mi_y] + 0.34 * edge_plot$hh_from[idx_mi_y]
        }
      }
    }

    # Y-side lane assignment: X->Y fixed at center, then preserve top/bottom ordering.
    idx_to_y <- which(edge_plot$to == y_var)
    if(length(idx_to_y) > 0) {
      clipped[idx_to_y, 3] <- edge_plot$x_to[idx_to_y] - edge_plot$hw_to[idx_to_y]
      idx_x <- idx_to_y[edge_plot$from_role[idx_to_y] == "x"]
      if(length(idx_x) > 0) {
        clipped[idx_x, 4] <- edge_plot$y_to[idx_x]
      }
      idx_other <- setdiff(idx_to_y, idx_x)
      if(length(idx_other) > 0) {
        x_src_y <- if(length(idx_x) > 0) edge_plot$y_from[idx_x[[1]]] else 0
        y_mid <- edge_plot$y_to[idx_other][1]
        h <- edge_plot$hh_to[idx_other][1]
        idx_top <- idx_other[edge_plot$y_from[idx_other] >= x_src_y]
        idx_bot <- idx_other[edge_plot$y_from[idx_other] < x_src_y]
        if(length(idx_top) > 0) {
          ord_top <- idx_top[order(-edge_plot$y_from[idx_top], edge_plot$from[idx_top])]
          y_top <- if(length(ord_top) == 1) {
            y_mid + 0.26 * h
          } else {
            seq(y_mid + 0.78 * h, y_mid + 0.22 * h, length.out = length(ord_top))
          }
          clipped[ord_top, 4] <- y_top
        }
        if(length(idx_bot) > 0) {
          ord_bot <- idx_bot[order(-edge_plot$y_from[idx_bot], edge_plot$from[idx_bot])]
          y_bot <- if(length(ord_bot) == 1) {
            y_mid - 0.60 * h
          } else {
            seq(y_mid - 0.20 * h, y_mid - 0.88 * h, length.out = length(ord_bot))
          }
          clipped[ord_bot, 4] <- y_bot
        }
      }

      # Model 8 (2M): keep lower Y-side convergence ordered as W (higher), INT (middle),
      # M2 (lower) to prevent the INT->Y lane from crossing the M2->Y lane near Y.
      if(length(mediators) >= 2) {
        m2_name <- mediators[[2]]
        idx_w_y <- idx_to_y[edge_plot$from_role[idx_to_y] == "mod"]
        idx_int_y <- idx_to_y[edge_plot$from_role[idx_to_y] == "int"]
        idx_m2_y <- idx_to_y[edge_plot$from[idx_to_y] == m2_name]
        if(length(idx_w_y) > 0 && length(idx_int_y) > 0 && length(idx_m2_y) > 0) {
          y_mid_all <- edge_plot$y_to[idx_to_y][1]
          h_all <- edge_plot$hh_to[idx_to_y][1]
          clipped[idx_w_y, 4] <- y_mid_all - 0.14 * h_all
          clipped[idx_int_y, 4] <- y_mid_all - 0.28 * h_all
          clipped[idx_m2_y, 4] <- y_mid_all - 0.42 * h_all
        }
      }
    }
  } else if(!identical(diagram_type, "conceptual") && model_num == 7L) {
    # Preserve existing 2-mediator statistical tuning.
    idx_x_y <- edge_plot$from_role == "x" & edge_plot$to == y_var
    if(any(idx_x_y)) {
      clipped[idx_x_y, 1] <- edge_plot$x_from[idx_x_y] + edge_plot$hw_from[idx_x_y]
      clipped[idx_x_y, 2] <- edge_plot$y_from[idx_x_y]
      clipped[idx_x_y, 3] <- edge_plot$x_to[idx_x_y] - edge_plot$hw_to[idx_x_y]
      clipped[idx_x_y, 4] <- edge_plot$y_to[idx_x_y]
    }
    if(length(mediators) >= 2) {
      m1_name <- mediators[[1]]
      m2_name <- mediators[[2]]
      idx_w_m2 <- edge_plot$from_role == "mod" & edge_plot$to == m2_name
      if(any(idx_w_m2)) {
        clipped[idx_w_m2, 1] <- edge_plot$x_from[idx_w_m2] + 0.95 * edge_plot$hw_from[idx_w_m2]
        clipped[idx_w_m2, 2] <- edge_plot$y_from[idx_w_m2]
      }
      idx_m1_y <- edge_plot$from == m1_name & edge_plot$to == y_var
      if(any(idx_m1_y)) {
        clipped[idx_m1_y, 1] <- edge_plot$x_from[idx_m1_y] + 0.95 * edge_plot$hw_from[idx_m1_y]
        clipped[idx_m1_y, 2] <- edge_plot$y_from[idx_m1_y] - 0.95 * edge_plot$hh_from[idx_m1_y]
      }
    }
  } else if(!identical(diagram_type, "conceptual") && model_num == 14L) {
    # Model 14 statistical: explicit second-stage anchoring to reduce Y-side crowding.
    idx_x_y <- edge_plot$from_role == "x" & edge_plot$to == y_var
    if(any(idx_x_y)) {
      clipped[idx_x_y, 1] <- edge_plot$x_from[idx_x_y] + edge_plot$hw_from[idx_x_y]
      clipped[idx_x_y, 2] <- edge_plot$y_from[idx_x_y]
      clipped[idx_x_y, 3] <- edge_plot$x_to[idx_x_y] - edge_plot$hw_to[idx_x_y]
      clipped[idx_x_y, 4] <- edge_plot$y_to[idx_x_y]
    }
    if(length(mediators) == 1) {
      m1_name <- mediators[[1]]
      anchor_frac <- 0.75
      idx_x_m1 <- edge_plot$from_role == "x" & edge_plot$to == m1_name
      idx_m1_y <- edge_plot$from == m1_name & edge_plot$to == y_var
      idx_w_y <- edge_plot$from_role == "mod" & edge_plot$to == y_var
      idx_int_y <- edge_plot$from_role == "int" & edge_plot$to == y_var
      if(any(idx_x_m1)) {
        # Avoid crossing at M1: X->M1 lands bottom-left.
        clipped[idx_x_m1, 1] <- edge_plot$x_from[idx_x_m1] + edge_plot$hw_from[idx_x_m1]
        clipped[idx_x_m1, 2] <- edge_plot$y_from[idx_x_m1] + 0.52 * edge_plot$hh_from[idx_x_m1]
        clipped[idx_x_m1, 3] <- edge_plot$x_to[idx_x_m1] - anchor_frac * edge_plot$hw_to[idx_x_m1]
        clipped[idx_x_m1, 4] <- edge_plot$y_to[idx_x_m1] - edge_plot$hh_to[idx_x_m1]
      }
      if(any(idx_m1_y)) {
        # Avoid crossing at M1: M1->Y leaves bottom-right.
        clipped[idx_m1_y, 1] <- edge_plot$x_from[idx_m1_y] + anchor_frac * edge_plot$hw_from[idx_m1_y]
        clipped[idx_m1_y, 2] <- edge_plot$y_from[idx_m1_y] - edge_plot$hh_from[idx_m1_y]
        clipped[idx_m1_y, 3] <- edge_plot$x_to[idx_m1_y] - edge_plot$hw_to[idx_m1_y]
        clipped[idx_m1_y, 4] <- edge_plot$y_to[idx_m1_y] + 0.62 * edge_plot$hh_to[idx_m1_y]
      }
      if(any(idx_w_y)) {
        # W below Y with vertical path to Y bottom-middle.
        clipped[idx_w_y, 1] <- edge_plot$x_from[idx_w_y]
        clipped[idx_w_y, 2] <- edge_plot$y_from[idx_w_y] + edge_plot$hh_from[idx_w_y]
        clipped[idx_w_y, 3] <- edge_plot$x_to[idx_w_y]
        clipped[idx_w_y, 4] <- edge_plot$y_to[idx_w_y] - edge_plot$hh_to[idx_w_y]
      }
      if(any(idx_int_y)) {
        # INT above Y with vertical path to Y top-middle.
        clipped[idx_int_y, 1] <- edge_plot$x_from[idx_int_y]
        clipped[idx_int_y, 2] <- edge_plot$y_from[idx_int_y] - edge_plot$hh_from[idx_int_y]
        clipped[idx_int_y, 3] <- edge_plot$x_to[idx_int_y]
        clipped[idx_int_y, 4] <- edge_plot$y_to[idx_int_y] + edge_plot$hh_to[idx_int_y]
      }
    } else if(length(mediators) >= 2) {
      m1_name <- mediators[[1]]
      m2_name <- mediators[[2]]
      anchor_frac <- 0.72
      idx_x_m1 <- edge_plot$from_role == "x" & edge_plot$to == m1_name
      idx_x_m2 <- edge_plot$from_role == "x" & edge_plot$to == m2_name
      idx_m1_y <- edge_plot$from == m1_name & edge_plot$to == y_var
      idx_m2_y <- edge_plot$from == m2_name & edge_plot$to == y_var
      idx_w_y <- edge_plot$from_role == "mod" & edge_plot$to == y_var
      idx_int_y <- edge_plot$from_role == "int" & edge_plot$to == y_var

      if(any(idx_x_m1)) {
        clipped[idx_x_m1, 1] <- edge_plot$x_from[idx_x_m1] + edge_plot$hw_from[idx_x_m1]
        clipped[idx_x_m1, 2] <- edge_plot$y_from[idx_x_m1] + 0.52 * edge_plot$hh_from[idx_x_m1]
        clipped[idx_x_m1, 3] <- edge_plot$x_to[idx_x_m1] - anchor_frac * edge_plot$hw_to[idx_x_m1]
        clipped[idx_x_m1, 4] <- edge_plot$y_to[idx_x_m1] - edge_plot$hh_to[idx_x_m1]
      }
      if(any(idx_x_m2)) {
        clipped[idx_x_m2, 1] <- edge_plot$x_from[idx_x_m2] + edge_plot$hw_from[idx_x_m2]
        clipped[idx_x_m2, 2] <- edge_plot$y_from[idx_x_m2] - 0.52 * edge_plot$hh_from[idx_x_m2]
        clipped[idx_x_m2, 3] <- edge_plot$x_to[idx_x_m2] - anchor_frac * edge_plot$hw_to[idx_x_m2]
        clipped[idx_x_m2, 4] <- edge_plot$y_to[idx_x_m2] + edge_plot$hh_to[idx_x_m2]
      }
      if(any(idx_m1_y)) {
        clipped[idx_m1_y, 1] <- edge_plot$x_from[idx_m1_y] + anchor_frac * edge_plot$hw_from[idx_m1_y]
        clipped[idx_m1_y, 2] <- edge_plot$y_from[idx_m1_y] - edge_plot$hh_from[idx_m1_y]
        clipped[idx_m1_y, 3] <- edge_plot$x_to[idx_m1_y] - edge_plot$hw_to[idx_m1_y]
        clipped[idx_m1_y, 4] <- edge_plot$y_to[idx_m1_y] + 0.62 * edge_plot$hh_to[idx_m1_y]
      }
      if(any(idx_m2_y)) {
        clipped[idx_m2_y, 1] <- edge_plot$x_from[idx_m2_y] + anchor_frac * edge_plot$hw_from[idx_m2_y]
        clipped[idx_m2_y, 2] <- edge_plot$y_from[idx_m2_y] + edge_plot$hh_from[idx_m2_y]
        clipped[idx_m2_y, 3] <- edge_plot$x_to[idx_m2_y] - edge_plot$hw_to[idx_m2_y]
        clipped[idx_m2_y, 4] <- edge_plot$y_to[idx_m2_y] - 0.62 * edge_plot$hh_to[idx_m2_y]
      }
      if(any(idx_w_y)) {
        # W above Y with vertical path to Y top-middle.
        clipped[idx_w_y, 1] <- edge_plot$x_from[idx_w_y]
        clipped[idx_w_y, 2] <- edge_plot$y_from[idx_w_y] - edge_plot$hh_from[idx_w_y]
        clipped[idx_w_y, 3] <- edge_plot$x_to[idx_w_y]
        clipped[idx_w_y, 4] <- edge_plot$y_to[idx_w_y] + edge_plot$hh_to[idx_w_y]
      }
      if(any(idx_int_y)) {
        ids <- which(idx_int_y)
        # Upper interaction: center-bottom -> Y upper-right corner.
        # Lower interaction: center-top -> Y lower-right corner.
        ord <- ids[order(-edge_plot$y_from[ids], edge_plot$from[ids])]
        for(k in seq_along(ord)) {
          i <- ord[[k]]
          upper <- (k == 1)
          clipped[i, 1] <- edge_plot$x_from[i]
          clipped[i, 2] <- if(upper) edge_plot$y_from[i] - edge_plot$hh_from[i] else edge_plot$y_from[i] + edge_plot$hh_from[i]
          clipped[i, 3] <- edge_plot$x_to[i] + edge_plot$hw_to[i]
          clipped[i, 4] <- if(upper) edge_plot$y_to[i] + 0.95 * edge_plot$hh_to[i] else edge_plot$y_to[i] - 0.95 * edge_plot$hh_to[i]
        }
      }
    }
  }
  # Models 1-7 and 14 spacing pass: enforce a consistent visible white-space gap at both node ends.
  # Use pixel-calibrated insets so the visible gap remains stable across app view and JPG export.
  edge_gap <- 0
  if(model_num %in% c(1:7, 14L)) {
    edge_gap <- if(identical(diagram_type, "conceptual")) {
      pixel_gap_to_data(gap_px = 6.0, fallback_data = 0.010)
    } else {
      pixel_gap_to_data(gap_px = 5.0, fallback_data = 0.0042)
    }
  }
  if(edge_gap > 0 && nrow(clipped) > 0) {
    if(identical(diagram_type, "conceptual")) {
      clipped <- t(mapply(
        function(xs, ys, xe, ye) inset_segment(xs, ys, xe, ye, pad = edge_gap),
        clipped[, 1], clipped[, 2], clipped[, 3], clipped[, 4]
      ))
    } else {
      # Statistical diagrams need extra target-end retraction so arrowheads don't erase
      # the visible inset at node borders.
      n_edges <- nrow(clipped)
      pad_start <- rep(edge_gap, n_edges)
      arrow_len_px <- (0.15 / 2.54) * 96
      arrow_pad <- pixel_gap_to_data(gap_px = arrow_len_px, fallback_data = 0.0095)
      pad_end <- rep(edge_gap + arrow_pad, n_edges)
      clipped <- t(mapply(
        function(xs, ys, xe, ye, ps, pe) inset_segment_asymmetric(xs, ys, xe, ye, pad_start = ps, pad_end = pe),
        clipped[, 1], clipped[, 2], clipped[, 3], clipped[, 4], pad_start, pad_end
      ))
    }
  }
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
      # Model 3: shift Y-target coefficients leftward (~1/3 along line) to use
      # wider spacing and reduce overlap when CI/p text is enabled.
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.34)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.34)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.34)
    }

    if(model_num == 5L) {
      # Model 5: keep Y-target labels separated but on-line; tune for 1 vs 2 mediators.
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.50)
      if(length(mediators) == 1) {
        # Keep coefficients nearer line midpoints for single-mediator model 5.
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.58)
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.50)
      } else if(length(mediators) >= 2) {
        m1_name <- mediators[[1]]
        m2_name <- mediators[[2]]
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "x", 0.46)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "x", 0.54)
        set_t(edge_plot$to == y_var & edge_plot$from == m1_name, 0.58)
        set_t(edge_plot$to == y_var & edge_plot$from == m2_name, 0.58)
      }
    }

    if(model_num == 4L) {
      # Keep simple mediation coefficients centered by default.
      set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.50)
    }

    if(model_num == 6L && length(mediators) >= 2) {
      # Model 6: keep all coefficient labels on true centerline midpoints.
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.50)
      set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.50)
      set_t(edge_plot$to_role == "m" & edge_plot$from_role == "m", 0.50)
      set_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.50)
    }

    if(model_num == 7L) {
      # Model 7: midpoint-first placement, then targeted nudges for overlap control.
      set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.50)
      set_t(edge_plot$to_role == "m" & edge_plot$from_role == "mod", 0.50)
      set_t(edge_plot$to_role == "m" & edge_plot$from_role == "int", 0.50)
      if(length(mediators) >= 2) {
        m1_name <- mediators[[1]]
        m2_name <- mediators[[2]]
        # Keep near-midpoint labels while separating converging lanes.
        # Target shape: W->M1 slightly higher than midpoint; INT->M1 just below that.
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "x", 0.44)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "x", 0.44)
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "mod", 0.00)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "mod", 0.46)
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "int", 0.56)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "int", 0.52)
        set_t(edge_plot$to == y_var & edge_plot$from == m1_name, 0.58)
        set_t(edge_plot$to == y_var & edge_plot$from == m2_name, 0.54)
      } else {
        # 1-mediator Model 7: midpoint labels on each path.
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.50)
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "mod", 0.50)
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "int", 0.50)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.50)
      }
    }

    if(model_num == 8L) {
      if(length(mediators) == 1) {
        # Pull first-stage moderation labels apart and keep second-stage/direct labels
        # farther from Y convergence.
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.34)
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "mod", 0.18)
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "int", 0.74)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "x", 0.46)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.46)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.56)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.32)
      } else if(length(mediators) >= 2) {
        m1_name <- mediators[[1]]
        m2_name <- mediators[[2]]
        # Dense 2-mediator layout: targeted nudges from visual review.
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "x", 0.55)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "x", 0.29)
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "mod", 0.50)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "mod", 0.26)
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "int", 0.70)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "int", 0.54)
        set_t(edge_plot$to == y_var & edge_plot$from == m1_name, 0.48)
        set_t(edge_plot$to == y_var & edge_plot$from == m2_name, 0.44)
        set_t(edge_plot$to == y_var & edge_plot$from_role == "mod", 0.42)
        set_t(edge_plot$to == y_var & edge_plot$from_role == "int", 0.52)
        set_t(edge_plot$to == y_var & edge_plot$from_role == "x", 0.56)
      }
    }

    if(model_num == 14L) {
      if(length(mediators) == 1) {
        # Model 14 (1 mediator): keep vertical second-stage labels away from Y center.
        set_t(edge_plot$to_role == "m" & edge_plot$from_role == "x", 0.50)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "m", 0.56)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "mod", 0.36)
        set_t(edge_plot$to_role == "y" & edge_plot$from_role == "int", 0.36)
      } else if(length(mediators) >= 2) {
        m1_name <- mediators[[1]]
        m2_name <- mediators[[2]]
        set_t(edge_plot$to == m1_name & edge_plot$from_role == "x", 0.44)
        set_t(edge_plot$to == m2_name & edge_plot$from_role == "x", 0.44)
        set_t(edge_plot$to == y_var & edge_plot$from == m1_name, 0.58)
        set_t(edge_plot$to == y_var & edge_plot$from == m2_name, 0.54)
        set_t(edge_plot$to == y_var & edge_plot$from_role == "mod", 0.42)
        set_t(edge_plot$to == y_var & edge_plot$from_role == "int", 0.30)
        idx_int_y <- edge_plot$to == y_var & edge_plot$from_role == "int"
        if(sum(idx_int_y) > 1) {
          ids <- which(idx_int_y)
          ord <- ids[order(-edge_plot$y_from[ids], edge_plot$from[ids])]
          edge_plot$t_label[ord] <- seq(0.26, 0.34, length.out = length(ord))
        } else {
          set_t(idx_int_y, 0.30)
        }
      }
    }
  }
  if(!identical(diagram_type, "conceptual") && model_num %in% c(4L, 5L, 6L)) {
    # For Models 4-6, place labels by center-to-center geometry.
    edge_plot$x_label <- edge_plot$x_from + edge_plot$t_label * (edge_plot$x_to - edge_plot$x_from)
    edge_plot$y_label <- edge_plot$y_from + edge_plot$t_label * (edge_plot$y_to - edge_plot$y_from)
  } else {
    # Default (and Models 1-3 after explicit endpoint anchoring): on rendered path segment.
    edge_plot$x_label <- edge_plot$x_from_draw + edge_plot$t_label * edge_plot$dx
    edge_plot$y_label <- edge_plot$y_from_draw + edge_plot$t_label * edge_plot$dy
  }
  edge_plot$label_angle <- 0
  if(!identical(diagram_type, "conceptual")) {
    orient <- tolower(trimws(as.character(coef_label_orientation)))
    if(!orient %in% c("line", "horizontal")) orient <- "line"
    if(identical(orient, "line")) {
      ang <- atan2(edge_plot$dy, edge_plot$dx) * 180 / pi
      ang <- ifelse(ang > 90, ang - 180, ifelse(ang < -90, ang + 180, ang))
      # Keep labels readable for steep paths.
      edge_plot$label_angle <- pmax(pmin(ang, 75), -75)
      if(model_num %in% c(5L, 8L, 14L)) {
        # For Models 5/14 second-stage paths, follow center-to-center orientation exactly
        # (including near-vertical paths) so W/INT->Y labels remain aligned after custom anchoring.
        idx_stage2 <- edge_plot$to == y_var & edge_plot$from_role %in% c("mod", "int")
        if(any(idx_stage2)) {
          # Use center-to-center vector (not clipped segment) for robust orientation
          # when endpoint anchoring is customized near the Y box.
          dx_stage2 <- edge_plot$x_to[idx_stage2] - edge_plot$x_from[idx_stage2]
          dy_stage2 <- edge_plot$y_to[idx_stage2] - edge_plot$y_from[idx_stage2]
          ang_stage2 <- atan2(dy_stage2, dx_stage2) * 180 / pi
          ang_stage2 <- ifelse(ang_stage2 > 90, ang_stage2 - 180, ifelse(ang_stage2 < -90, ang_stage2 + 180, ang_stage2))
          # Snap near-vertical paths to true vertical so W->Y / INT->Y labels track the line cleanly.
          # Use both angle- and dx-based checks because tiny residual x drift can otherwise leave
          # visibly "almost vertical" labels in 2-mediator layouts.
          near_vertical <- (abs(abs(ang_stage2) - 90) < 12) |
            (abs(dx_stage2) < 0.02 & abs(dy_stage2) > 0.08)
          ang_stage2[near_vertical] <- ifelse(ang_stage2[near_vertical] >= 0, 90, -90)
          edge_plot$label_angle[idx_stage2] <- ang_stage2
        }
      }
    } else {
      edge_plot$label_angle <- 0
    }
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
        cue_midpoint_from_rendered <- function(from_name, to_name, fallback_from = NULL, fallback_to = NULL) {
          idx <- which(edge_plot$from == from_name & edge_plot$to == to_name)
          if(length(idx) > 0) {
            j <- idx[[1]]
            return(c(
              edge_plot$x_from_draw[j] + 0.50 * edge_plot$dx[j],
              edge_plot$y_from_draw[j] + 0.50 * edge_plot$dy[j]
            ))
          }
          from_node <- if(is.null(fallback_from)) nodes[nodes$name == from_name, , drop = FALSE] else fallback_from
          to_node <- if(is.null(fallback_to)) nodes[nodes$name == to_name, , drop = FALSE] else fallback_to
          c(
            from_node$x + 0.50 * (to_node$x - from_node$x),
            from_node$y + 0.50 * (to_node$y - from_node$y)
          )
        }
        if(model_num == 2L) {
          # Model 2 conceptual: W points vertically to ~1/3 of X->Y path.
          w_target <- xy_point(1/3)
          mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = w_target[[1]], yend = w_target[[2]]))
        } else if(model_num == 7L && length(mediators) > 0 && any(nodes$name == mediators[[1]])) {
          # Model 7 conceptual:
          # target rendered X->M1 midpoint; when present with 2 mediators, also target X->M2 midpoint.
          cue_midpoint_for_m <- function(m_name) {
            idx_xm <- which(edge_plot$from_role == "x" & edge_plot$to == m_name)
            if(length(idx_xm) > 0) {
              j <- idx_xm[[1]]
              c(
                edge_plot$x_from_draw[j] + 0.50 * edge_plot$dx[j],
                edge_plot$y_from_draw[j] + 0.50 * edge_plot$dy[j]
              )
            } else {
              m_node <- nodes[nodes$name == m_name, , drop = FALSE]
              c(
                x_node$x + 0.50 * (m_node$x - x_node$x),
                x_node$y + 0.50 * (m_node$y - x_node$y)
              )
            }
          }
          m1_mid <- cue_midpoint_for_m(mediators[[1]])
          mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = m1_mid[[1]], yend = m1_mid[[2]]))
          if(length(mediators) >= 2 && any(nodes$name == mediators[[2]])) {
            m2_mid <- cue_midpoint_for_m(mediators[[2]])
            mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = m2_mid[[1]], yend = m2_mid[[2]]))
          }
        } else if(model_num == 14L && length(mediators) > 0 && any(nodes$name == mediators[[1]])) {
          # Model 14 conceptual (mirror of Model 7 moderation cues):
          # target rendered M->Y midpoint(s).
          m_targets <- mediators[mediators %in% nodes$name]
          if(length(m_targets) > 0) {
            for(m_name in m_targets) {
              my_mid <- cue_midpoint_from_rendered(m_name, y_var)
              mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = my_mid[[1]], yend = my_mid[[2]]))
            }
          } else {
            mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = midx, yend = midy))
          }
        } else {
          # Default moderation cue: to X->Y midpoint.
          mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = midx, yend = midy))
        }
        if(model_num == 8L && length(mediators) > 0) {
          for(m in mediators) {
            if(any(nodes$name == m)) {
              idx_xm <- which(edge_plot$from_role == "x" & edge_plot$to == m)
              if(length(idx_xm) > 0) {
                j <- idx_xm[[1]]
                xm_midx <- edge_plot$x_from_draw[j] + 0.50 * edge_plot$dx[j]
                xm_midy <- edge_plot$y_from_draw[j] + 0.50 * edge_plot$dy[j]
              } else {
                m_node <- nodes[nodes$name == m, , drop = FALSE]
                xm_midx <- x_node$x + 0.50 * (m_node$x - x_node$x)
                xm_midy <- x_node$y + 0.50 * (m_node$y - x_node$y)
              }
              mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = xm_midx, yend = xm_midy))
            }
          }
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
    cue_dims <- t(vapply(seq_len(nrow(mod_cue)), function(i) {
      d2 <- (nodes$x - mod_cue$x[[i]])^2 + (nodes$y - mod_cue$y[[i]])^2
      j <- which.min(d2)
      nm <- nodes$name[[j]]
      dim_row <- node_dims[node_dims$name == nm, , drop = FALSE]
      c(hw = dim_row$hw[[1]], hh = dim_row$hh[[1]])
    }, numeric(2)))
    cue_gap <- pixel_gap_to_data(gap_px = 10.5, fallback_data = 0.018)
    cue_clip <- t(mapply(function(x0, y0, x1, y1, hw, hh) {
      # Clip cue start to source box edge.
      clipped <- clip_segment_to_box(x0, y0, x1, y1, hw0 = hw, hh0 = hh, hw1 = 0, hh1 = 0)
      # Add a small visible gap before the target path intersection.
      retract_endpoint(clipped[[1]], clipped[[2]], clipped[[3]], clipped[[4]], pad = cue_gap)
    }, mod_cue$x, mod_cue$y, mod_cue$xend, mod_cue$yend, cue_dims[, 1], cue_dims[, 2]))
    mod_cue$x <- cue_clip[, 1]
    mod_cue$y <- cue_clip[, 2]
    mod_cue$xend <- cue_clip[, 3]
    mod_cue$yend <- cue_clip[, 4]
  }

  rect_one_mediator <- model_num %in% c(7L, 14L) && length(mediators) == 1
  node_fill_color <- if(isTRUE(fill_variable_boxes_blue)) "#DCEEFF" else "white"
  title_txt <- paste0(
    "Model ",
    settings$model,
    " ",
    if(identical(diagram_type, "conceptual")) "Conceptual" else "Statistical",
    " Diagram"
  )
  subtitle_txt <- if(identical(diagram_type, "conceptual")) {
    NULL
  } else {
    if(identical(label_mode, "std")) "Standardized coefficients" else "Unstandardized coefficients"
  }
  nodes_draw <- nodes
  nodes_draw$hw <- node_dims$hw[match(nodes_draw$name, node_dims$name)]
  nodes_draw$hh <- node_dims$hh[match(nodes_draw$name, node_dims$name)]
  nodes_draw$hw[is.na(nodes_draw$hw)] <- 0.085
  nodes_draw$hh[is.na(nodes_draw$hh)] <- 0.055
  if(rect_one_mediator) {
    node_boxes <- nodes_draw
    node_boxes$xmin <- node_boxes$x - node_boxes$hw
    node_boxes$xmax <- node_boxes$x + node_boxes$hw
    node_boxes$ymin <- node_boxes$y - node_boxes$hh
    node_boxes$ymax <- node_boxes$y + node_boxes$hh
  } else {
    node_boxes <- NULL
  }
  cue_linewidth <- 0.45
  cue_arrow_cm <- 0.12
  if(identical(diagram_type, "conceptual") && nrow(mod_cue) > 0) {
    # Match moderator cue style to all other path lines for conceptual diagrams
    # whenever cue arrows are present (not just model-specific branches).
    cue_linewidth <- 0.80
    cue_arrow_cm <- 0.15
  }
  node_layer <- if(rect_one_mediator) {
    list(
        ggplot2::geom_rect(
          data = node_boxes,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          fill = node_fill_color,
          color = "black",
          linewidth = 0.45,
          inherit.aes = FALSE
        ),
      ggplot2::geom_text(
        data = nodes_draw,
        ggplot2::aes(x = x, y = y, label = label),
        size = if(identical(diagram_type, "conceptual")) 5.4 else 4.8,
        color = "black",
        fontface = "bold"
      )
    )
  } else {
    list(
      ggplot2::geom_label(
        data = nodes,
        ggplot2::aes(x = x, y = y, label = label),
        size = if(identical(diagram_type, "conceptual")) 5.4 else 4.8,
        label.size = 0.45,
        fill = node_fill_color,
        color = "black",
        fontface = "bold"
      )
    )
  }
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_plot,
      ggplot2::aes(x = x_from_draw, y = y_from_draw, xend = x_to_draw, yend = y_to_draw, linetype = path_kind, linewidth = path_kind),
      arrow = grid::arrow(length = grid::unit(0.15, "cm")),
      color = "black",
      lineend = "butt"
    ) +
    node_layer +
    ggplot2::geom_segment(
      data = mod_cue,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      linetype = "solid",
      linewidth = cue_linewidth,
      color = "black",
      arrow = grid::arrow(length = grid::unit(cue_arrow_cm, "cm")),
      lineend = "butt"
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
        moderator = 0.80,
        interaction = 0.80,
        covariate = 0.80,
        unknown = 0.80
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
    coef_sz <- suppressWarnings(as.numeric(coef_label_size))
    if(is.na(coef_sz) || coef_sz < 2.0 || coef_sz > 5.0) coef_sz <- 3.3
    p <- p + ggplot2::geom_label(
      data = label_df,
      ggplot2::aes(x = x_label, y = y_label, label = label, angle = label_angle),
      size = coef_sz,
      label.size = 0.2,
      fill = "white",
      color = "black"
    )
  }

  p
}

dot_escape_label <- function(x) {
  out <- as.character(x)
  out <- gsub("\\\\", "\\\\\\\\", out)
  out <- gsub("\"", "\\\\\"", out, fixed = TRUE)
  out <- gsub("\n", "\\\\n", out, fixed = TRUE)
  out
}

dot_escape_html_label <- function(x) {
  out <- as.character(x)
  out <- gsub("&", "&amp;", out, fixed = TRUE)
  out <- gsub("<", "&lt;", out, fixed = TRUE)
  out <- gsub(">", "&gt;", out, fixed = TRUE)
  out <- gsub("\"", "&quot;", out, fixed = TRUE)
  out <- gsub("\n", "<BR/>", out, fixed = TRUE)
  out
}

build_graphviz_statistical_dot <- function(parsed, settings,
                                                   label_mode = "raw",
                                                   show_interactions = TRUE,
                                                   show_moderator_main_effects = TRUE,
                                                   include_ci = FALSE,
                                                   include_p = FALSE,
                                                   include_stars = TRUE,
                                                   label_map = NULL,
                                                   coef_digits = 3) {
  model_num <- suppressWarnings(as.integer(settings$model))
  if(!(model_num %in% c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 14L))) return(NULL)
  edges <- parsed$paths
  if(nrow(edges) == 0) return(NULL)
  edges <- edges[edges$path_kind != "covariate", , drop = FALSE]
  if(!isTRUE(show_interactions)) {
    edges <- edges[!(edges$path_kind == "interaction" | grepl("^int_[0-9]+$", edges$from)), , drop = FALSE]
  }
  if(!isTRUE(show_moderator_main_effects)) {
    edges <- edges[edges$path_kind != "moderator", , drop = FALSE]
  }
  if(nrow(edges) == 0) return(NULL)

  x_var <- settings$predictor_var
  y_var <- settings$outcome_var
  mediators <- if(!is.null(settings$mediator_vars)) settings$mediator_vars else character(0)
  w_var <- if(!is.null(settings$moderator_var) && nzchar(settings$moderator_var) && isTRUE(show_moderator_main_effects)) settings$moderator_var else character(0)
  z_var <- if(!is.null(settings$moderator2_var) && nzchar(settings$moderator2_var) && isTRUE(show_moderator_main_effects)) settings$moderator2_var else character(0)
  alias_map <- parsed$metadata$product_aliases
  if(is.null(alias_map)) alias_map <- character(0)

  int_edges <- edges[edges$path_kind == "interaction" | grepl("^int_[0-9]+$", edges$from), , drop = FALSE]
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
  }

  nodes <- data.frame(name = character(0), x = numeric(0), y = numeric(0), stringsAsFactors = FALSE)
  add_node <- function(nm, x, y) {
    if(is.null(nm) || !nzchar(nm)) return()
    nodes <<- rbind(nodes, data.frame(name = nm, x = x, y = y, stringsAsFactors = FALSE))
  }

  add_node(x_var, -0.84, 0.00)
  add_node(y_var, 0.86, 0.00)
  n_m <- length(mediators)
  if(model_num == 1L) {
    if(length(w_var) > 0) add_node(w_var, -0.84, 0.30)
    if(nrow(int_edges) > 0) {
      int_terms <- unique(int_edges$from)
      y_pos <- seq(-0.52, -0.82, length.out = length(int_terms))
      for(i in seq_along(int_terms)) {
        add_node(int_terms[[i]], -0.84, y_pos[[i]])
      }
    }
  } else if(model_num == 2L) {
    if(length(w_var) > 0) add_node(w_var, -0.84, 0.22)
    if(length(z_var) > 0) add_node(z_var, -0.84, -0.22)
    if(nrow(int_edges) > 0) {
      int_terms <- unique(int_edges$from)
      y_pos <- seq(-0.56, -0.86, length.out = length(int_terms))
      for(i in seq_along(int_terms)) {
        add_node(int_terms[[i]], -0.84, y_pos[[i]])
      }
    }
  } else if(model_num == 3L) {
    if(length(w_var) > 0) add_node(w_var, -0.84, 0.22)
    if(length(z_var) > 0) add_node(z_var, -0.84, -0.22)
    if(nrow(int_edges) > 0) {
      int_terms <- unique(int_edges$from)
      y_pos <- seq(-0.50, -1.02, length.out = length(int_terms))
      for(i in seq_along(int_terms)) {
        add_node(int_terms[[i]], -0.84, y_pos[[i]])
      }
    }
  } else if(model_num == 4L) {
    if(n_m == 1) {
      add_node(mediators[[1]], 0.00, 0.56)
    } else if(n_m >= 2) {
      add_node(mediators[[1]], 0.00, 0.56)
      add_node(mediators[[2]], 0.00, -0.56)
    }
  } else if(model_num == 5L) {
    if(n_m == 1) {
      add_node(mediators[[1]], 0.02, 0.58)
    } else if(n_m >= 2) {
      add_node(mediators[[1]], 0.02, 0.56)
      add_node(mediators[[2]], 0.02, -0.56)
    }
    if(length(w_var) > 0) {
      if(n_m >= 2) {
        add_node(w_var, 0.86, -0.56)
      } else {
        add_node(w_var, 0.48, -0.74)
      }
    }
    if(nrow(int_edges) > 0) {
      int_terms <- unique(int_edges$from)
      int_base_x <- if(n_m >= 2) 0.86 else 0.20
      int_base_y <- if(n_m >= 2) 0.56 else -0.38
      for(i in seq_along(int_terms)) {
        if(n_m >= 2) {
          add_node(int_terms[[i]], int_base_x + 0.00 * (i - 1), int_base_y - 0.08 * (i - 1))
        } else {
          add_node(int_terms[[i]], int_base_x + 0.12 * (i - 1), int_base_y - 0.12 * (i - 1))
        }
      }
    }
  } else if(model_num == 6L) {
    if(n_m >= 2) {
      x0 <- -0.75
      x1 <- 0.75
      add_node(mediators[[1]], x0 + 0.25 * (x1 - x0), 0.56)
      add_node(mediators[[2]], x0 + 0.75 * (x1 - x0), 0.56)
    }
  } else if(model_num == 8L) {
    if(length(w_var) > 0) add_node(w_var, -0.92, -0.34)
    if(nrow(int_edges) > 0) {
      int_terms <- unique(int_edges$from)
      int_x <- if(n_m >= 2) -0.38 else -0.20
      int_y_base <- if(n_m >= 2) -0.66 else -0.92
      for(i in seq_along(int_terms)) {
        add_node(int_terms[[i]], int_x, int_y_base - 0.16 * (i - 1))
      }
    }
    if(n_m == 1) {
      add_node(mediators[[1]], 0.06, 0.62)
    } else if(n_m >= 2) {
      add_node(mediators[[1]], 0.06, 0.62)
      lower_y <- if(n_m == 2) -0.62 else seq(-0.62, -0.82, length.out = n_m - 1)
      for(i in 2:n_m) {
        add_node(mediators[[i]], 0.10, lower_y[[i - 1]])
      }
    }
  } else if(model_num == 7L) {
    if(n_m == 1) {
      add_node(mediators[[1]], 0.06, 0.56)
    } else if(n_m >= 2) {
      add_node(mediators[[1]], 0.06, 0.56)
      add_node(mediators[[2]], 0.06, -0.56)
    }
    if(length(w_var) > 0) {
      if(n_m == 1) {
        add_node(w_var, -0.84, 0.28)
      } else {
        add_node(w_var, -0.84, -0.56)
      }
    }
  } else {
    # Model 14 mirror branch.
    if(n_m == 1) {
      add_node(mediators[[1]], -0.06, 0.56)
    } else if(n_m >= 2) {
      add_node(mediators[[1]], 0.00, 0.56)
      add_node(mediators[[2]], 0.00, -0.56)
    }
    if(length(w_var) > 0) {
      if(n_m == 1) {
        add_node(w_var, 0.86, -0.56)
      } else {
        add_node(w_var, 0.86, 0.56)
      }
    }
  }
  if(length(z_var) > 0 && !any(nodes$name == z_var)) add_node(z_var, -0.92, 0.24)
  if(nrow(int_edges) > 0 && model_num %in% c(7L, 14L)) {
    int_terms <- unique(int_edges$from)
    for(i in seq_along(int_terms)) {
      int_lbl <- int_terms[[i]]
      if(model_num == 7L) {
        if(n_m == 1) {
          add_node(int_lbl, -0.84 + 0.10 * (i - 1), 0.56 + 0.10 * (i - 1))
        } else {
          add_node(int_lbl, -0.48 + 0.12 * (i - 1), -0.92 - 0.12 * (i - 1))
        }
      } else {
        if(n_m == 1) {
          add_node(int_lbl, 0.86, 0.56 + 0.10 * (i - 1))
        } else {
          y_lane <- if(i == 1) 0.56 else if(i == 2) -0.56 else (0.56 - 0.28 * (i - 1))
          x_lane <- if(i <= 2) 1.22 else (1.22 + 0.08 * (i - 2))
          add_node(int_lbl, x_lane, y_lane)
        }
      }
    }
  }
  nodes <- unique(nodes)
  if(nrow(nodes) == 0) return(NULL)

  if(!is.null(label_map) && length(label_map) > 0) {
    nodes$label <- vapply(nodes$name, function(nm) {
      if(nm %in% names(label_map)) {
        sanitize_label_text(label_map[[nm]], nm)
      } else if(grepl("\\s+[xX]\\s+", nm)) {
        map_interaction_label(nm, label_map)
      } else {
        nm
      }
    }, character(1))
  } else {
    nodes$label <- nodes$name
  }

  edges <- edges[edges$from %in% nodes$name & edges$to %in% nodes$name, , drop = FALSE]
  if(nrow(edges) == 0) return(NULL)

  edges$label <- vapply(seq_len(nrow(edges)), function(i) {
    compose_path_label(
      edge_row = edges[i, , drop = FALSE],
      label_mode = label_mode,
      include_ci = include_ci,
      include_p = include_p,
      include_stars = include_stars,
      coef_digits = coef_digits
    )
  }, character(1))

  node_ids <- setNames(paste0("n", seq_len(nrow(nodes))), nodes$name)
  x_scale <- 7.0
  y_scale <- 5.6
  node_lines <- vapply(seq_len(nrow(nodes)), function(i) {
    nid <- node_ids[[nodes$name[[i]]]]
    lbl <- dot_escape_label(nodes$label[[i]])
    x <- nodes$x[[i]] * x_scale
    y <- nodes$y[[i]] * y_scale
    paste0("  ", nid, " [label=\"", lbl, "\", pos=\"", sprintf("%.2f,%.2f!", x, y), "\", pin=true];")
  }, character(1))

  mod_names <- c(w_var, z_var)
  mod_names <- mod_names[nzchar(mod_names)]
  int_names <- unique(int_edges$from)
  int_names <- int_names[nzchar(int_names)]
  m1_name <- if(length(mediators) >= 1) mediators[[1]] else ""
  get_node_xy <- function(nm) {
    j <- match(nm, nodes$name)
    if(is.na(j)) return(c(x = 0, y = 0))
    c(x = nodes$x[[j]], y = nodes$y[[j]])
  }
  label_idx <- which(nzchar(edges$label))
  label_node_lines <- if(length(label_idx) > 0) {
    vapply(label_idx, function(i) {
      p0 <- get_node_xy(edges$from[[i]])
      p1 <- get_node_xy(edges$to[[i]])
      t_pos <- 0.50
      if(model_num == 7L && nzchar(m1_name) && identical(edges$to[[i]], m1_name)) {
        if(identical(edges$from[[i]], x_var)) {
          t_pos <- if(length(mediators) == 1) 0.38 else 0.44
        } else if(edges$from[[i]] %in% mod_names) {
          # Keep W->M1 coefficient visibly above X->Y lane.
          t_pos <- if(length(mediators) == 1) 0.58 else 0.68
        } else if(edges$from[[i]] %in% int_names) {
          # Keep INT->M1 just below W->M1.
          t_pos <- if(length(mediators) == 1) 0.60 else 0.54
        }
      }
      if(model_num %in% c(1L, 2L, 3L) && identical(edges$to[[i]], y_var)) {
        from_nm <- edges$from[[i]]
        if(model_num == 3L) {
          # Crowded Model 3: move all labels leftward (~1/3 along path).
          t_pos <- 0.34
        } else {
          if(from_nm %in% mod_names) {
            t_pos <- 0.38
          } else if(from_nm %in% int_names) {
            t_pos <- 0.38
          } else if(identical(from_nm, x_var)) {
            t_pos <- 0.50
          }
        }
      }
      if(model_num == 4L && identical(edges$to[[i]], y_var)) {
        # Model 4 is simple, but slight left shift gives more room for CI labels.
        t_pos <- 0.42
      }
      if(model_num == 5L) {
        from_nm <- edges$from[[i]]
        to_nm <- edges$to[[i]]
        if(identical(to_nm, y_var) && from_nm %in% c(w_var, int_names)) {
          t_pos <- 0.50
        } else if(identical(to_nm, y_var)) {
          t_pos <- 0.46
        }
      }
      if(model_num == 8L) {
        from_nm <- edges$from[[i]]
        to_nm <- edges$to[[i]]
        if(length(mediators) == 1) {
          if(identical(to_nm, y_var) && (from_nm %in% c(w_var, int_names))) {
            t_pos <- 0.30
          } else if(identical(to_nm, y_var) && identical(from_nm, mediators[[1]])) {
            t_pos <- 0.52
          } else if(identical(to_nm, y_var) && identical(from_nm, x_var)) {
            t_pos <- 0.46
          } else if(identical(to_nm, mediators[[1]]) && from_nm %in% c(w_var, int_names)) {
            t_pos <- 0.22
          } else if(identical(to_nm, mediators[[1]]) && identical(from_nm, x_var)) {
            t_pos <- 0.34
          }
        } else if(length(mediators) >= 2) {
          m1_name <- mediators[[1]]
          m2_name <- mediators[[2]]
          if(identical(to_nm, y_var) && from_nm %in% c(w_var, int_names)) {
            t_pos <- 0.28
          } else if(identical(to_nm, y_var) && from_nm %in% mediators) {
            t_pos <- if(identical(from_nm, m1_name)) 0.46 else 0.42
          } else if(identical(to_nm, y_var) && identical(from_nm, x_var)) {
            t_pos <- 0.46
          } else if(identical(to_nm, m1_name) && from_nm %in% c(w_var, int_names)) {
            t_pos <- 0.22
          } else if(identical(to_nm, m2_name) && from_nm %in% c(w_var, int_names)) {
            t_pos <- 0.30
          } else if(to_nm %in% c(m1_name, m2_name) && identical(from_nm, x_var)) {
            t_pos <- 0.30
          }
        }
      }
      if(model_num == 14L && identical(edges$to[[i]], y_var)) {
        if(edges$from[[i]] %in% mediators) {
          if(length(mediators) == 1) {
            t_pos <- 0.56
          } else if(identical(edges$from[[i]], mediators[[1]])) {
            t_pos <- 0.58
          } else {
            t_pos <- 0.54
          }
        } else if(edges$from[[i]] %in% mod_names) {
          t_pos <- if(length(mediators) == 1) 0.36 else 0.42
        } else if(edges$from[[i]] %in% int_names) {
          t_pos <- if(length(mediators) == 1) 0.36 else 0.30
        }
      }
      if(model_num == 7L && length(mediators) == 1 && identical(edges$to[[i]], y_var) && identical(edges$from[[i]], m1_name)) {
        t_pos <- 0.56
      }
      lx <- p0[["x"]] + t_pos * (p1[["x"]] - p0[["x"]])
      ly <- p0[["y"]] + t_pos * (p1[["y"]] - p0[["y"]])
      lbl_html <- dot_escape_html_label(edges$label[[i]])
      n_lines <- length(strsplit(edges$label[[i]], "\n", fixed = TRUE)[[1]])
      lab_fs <- if(n_lines >= 2) 9.0 else 10.5
      paste0(
        "  lab", i,
        " [label=<<FONT POINT-SIZE=\"", sprintf("%.1f", lab_fs), "\">", lbl_html, "</FONT>>",
        ", shape=box, style=\"rounded,filled\", fillcolor=\"white\", color=\"#666666\",",
        " penwidth=0.7, fontname=\"Helvetica\"",
        ", pos=\"", sprintf("%.2f,%.2f!", lx * x_scale, ly * y_scale),
        "\", pin=true, width=0, height=0, margin=\"0.06,0.03\"];"
      )
    }, character(1))
  } else {
    character(0)
  }

  edge_lines <- vapply(seq_len(nrow(edges)), function(i) {
    fr <- node_ids[[edges$from[[i]]]]
    to <- node_ids[[edges$to[[i]]]]
    attrs <- c("color=\"black\"", "penwidth=1.6", "arrowsize=0.7")
    if(model_num %in% c(1L, 2L, 3L)) {
      from_nm <- edges$from[[i]]
      to_nm <- edges$to[[i]]
      if(identical(to_nm, y_var)) {
        attrs <- c(attrs, "tailport=e")
        if(model_num %in% c(2L, 3L)) {
          # Models 2-3 Graphviz: use a single left-mid Y target for cleaner,
          # predictable endpoints in dense convergence.
          attrs <- c(attrs, "headport=w")
        } else {
        y_from <- get_node_xy(from_nm)[["y"]]
        if(from_nm %in% mod_names && y_from >= 0) {
          attrs <- c(attrs, "headport=nw")
        } else if(from_nm %in% int_names || (from_nm %in% mod_names && y_from < 0)) {
          attrs <- c(attrs, "headport=sw")
        } else if(identical(from_nm, x_var)) {
          attrs <- c(attrs, "headport=w")
        }
        }
      }
    }
    if(model_num == 4L) {
      from_nm <- edges$from[[i]]
      to_nm <- edges$to[[i]]
      if(identical(from_nm, x_var) && identical(to_nm, y_var)) {
        attrs <- c(attrs, "tailport=e", "headport=w")
      }
      if(length(mediators) >= 1) {
        m1_name <- mediators[[1]]
        if(identical(from_nm, x_var) && identical(to_nm, m1_name)) {
          attrs <- c(attrs, "tailport=e", "headport=sw")
        }
        if(identical(from_nm, m1_name) && identical(to_nm, y_var)) {
          attrs <- c(attrs, "tailport=se", "headport=nw")
        }
      }
      if(length(mediators) >= 2) {
        m2_name <- mediators[[2]]
        if(identical(from_nm, x_var) && identical(to_nm, m2_name)) {
          attrs <- c(attrs, "tailport=e", "headport=nw")
        }
        if(identical(from_nm, m2_name) && identical(to_nm, y_var)) {
          attrs <- c(attrs, "tailport=ne", "headport=sw")
        }
      }
    }
    if(model_num == 5L) {
      from_nm <- edges$from[[i]]
      to_nm <- edges$to[[i]]
      if(identical(from_nm, x_var) && identical(to_nm, y_var)) {
        attrs <- c(attrs, "tailport=e", "headport=w")
      }
      if(length(mediators) >= 1) {
        m1_name <- mediators[[1]]
        if(identical(from_nm, x_var) && identical(to_nm, m1_name)) {
          attrs <- c(attrs, "tailport=e", "headport=sw")
        }
        if(identical(from_nm, m1_name) && identical(to_nm, y_var)) {
          attrs <- c(attrs, "tailport=se", "headport=nw")
        }
      }
      if(length(mediators) >= 2) {
        m2_name <- mediators[[2]]
        if(identical(from_nm, x_var) && identical(to_nm, m2_name)) {
          attrs <- c(attrs, "tailport=e", "headport=nw")
        }
        if(identical(from_nm, m2_name) && identical(to_nm, y_var)) {
          attrs <- c(attrs, "tailport=ne", "headport=sw")
        }
      }
      if(identical(to_nm, y_var) && from_nm %in% mod_names) {
        if(length(mediators) >= 2) {
          attrs <- c(attrs, "tailport=n", "headport=s")
        } else {
          attrs <- c(attrs, "headport=sw")
        }
      }
      if(identical(to_nm, y_var) && from_nm %in% int_names) {
        if(length(mediators) >= 2) {
          attrs <- c(attrs, "tailport=s", "headport=n")
        } else {
          attrs <- c(attrs, "headport=w")
        }
      }
    }
    if(model_num == 6L) {
      from_nm <- edges$from[[i]]
      to_nm <- edges$to[[i]]
      if(identical(from_nm, x_var) && identical(to_nm, y_var)) {
        attrs <- c(attrs, "tailport=e", "headport=w")
      }
      if(length(mediators) >= 1) {
        m1_name <- mediators[[1]]
        if(identical(from_nm, x_var) && identical(to_nm, m1_name)) {
          attrs <- c(attrs, "tailport=e", "headport=sw")
        }
        if(identical(from_nm, m1_name) && identical(to_nm, y_var)) {
          attrs <- c(attrs, "tailport=se", "headport=w")
        }
      }
      if(length(mediators) >= 2) {
        m2_name <- mediators[[2]]
        if(identical(from_nm, x_var) && identical(to_nm, m2_name)) {
          attrs <- c(attrs, "tailport=e", "headport=sw")
        }
        if(identical(from_nm, m1_name) && identical(to_nm, m2_name)) {
          attrs <- c(attrs, "tailport=e", "headport=w")
        }
        if(identical(from_nm, m2_name) && identical(to_nm, y_var)) {
          attrs <- c(attrs, "tailport=se", "headport=w")
        }
      }
      # Model 6 Graphviz: use a shared left-midpoint Y endpoint for all ->Y paths.
      # This keeps X->Y horizontal and produces cleaner convergence for the serial layout.
      if(identical(to_nm, y_var)) {
        attrs <- c(attrs[!grepl("^headport=", attrs)], "headport=w")
      }
    }
    if(model_num == 8L) {
      from_nm <- edges$from[[i]]
      to_nm <- edges$to[[i]]
      if(identical(from_nm, x_var) && identical(to_nm, y_var)) {
        attrs <- c(attrs, "tailport=e")
      }
      if(length(mediators) >= 1) {
        m1_name <- mediators[[1]]
        if(identical(from_nm, x_var) && identical(to_nm, m1_name)) {
          attrs <- c(attrs, "tailport=e", "headport=sw")
        }
        if(identical(from_nm, m1_name) && identical(to_nm, y_var)) {
          attrs <- c(attrs, "tailport=se")
        }
      }
      if(length(mediators) >= 2) {
        m2_name <- mediators[[2]]
        if(identical(from_nm, x_var) && identical(to_nm, m2_name)) {
          attrs <- c(attrs, "tailport=e", "headport=nw")
        }
        if(identical(from_nm, m2_name) && identical(to_nm, y_var)) {
          attrs <- c(attrs, "tailport=ne")
        }
      }
      if(identical(to_nm, y_var)) {
        # Dense Y convergence: shared left-midpoint target keeps endpoints readable and
        # preserves a horizontal direct X->Y path.
        attrs <- c(attrs[!grepl("^headport=", attrs)], "headport=w")
      }
    }
    if(model_num == 14L) {
      from_nm <- edges$from[[i]]
      to_nm <- edges$to[[i]]
      if(identical(to_nm, y_var) && from_nm %in% mod_names) {
        if(length(mediators) == 1) {
          attrs <- c(attrs, "tailport=n", "headport=s")
        } else {
          attrs <- c(attrs, "tailport=s", "headport=n")
        }
      }
      if(identical(to_nm, y_var) && from_nm %in% int_names) {
        y_from <- get_node_xy(from_nm)[["y"]]
        y_y <- get_node_xy(y_var)[["y"]]
        if(length(mediators) == 1) {
          attrs <- c(attrs, "tailport=s", "headport=n")
        } else {
          if(isTRUE(y_from >= y_y)) {
            attrs <- c(attrs, "tailport=s", "headport=ne")
          } else {
            attrs <- c(attrs, "tailport=n", "headport=se")
          }
        }
      }
      if(length(mediators) >= 1) {
        m1_name <- mediators[[1]]
        if(identical(from_nm, x_var) && identical(to_nm, m1_name)) {
          attrs <- c(attrs, "tailport=e", "headport=sw")
        }
        if(identical(from_nm, m1_name) && identical(to_nm, y_var)) {
          attrs <- c(attrs, "tailport=se", "headport=nw")
        }
      }
      if(length(mediators) >= 2) {
        m2_name <- mediators[[2]]
        if(identical(from_nm, x_var) && identical(to_nm, m2_name)) {
          attrs <- c(attrs, "tailport=e", "headport=nw")
        }
        if(identical(from_nm, m2_name) && identical(to_nm, y_var)) {
          attrs <- c(attrs, "tailport=ne", "headport=sw")
        }
      }
    }
    paste0("  ", fr, " -> ", to, " [", paste(attrs, collapse = ", "), "];")
  }, character(1))

  paste(
    "digraph G {",
    "  graph [layout=neato, bgcolor=\"white\", overlap=false, splines=true, outputorder=\"edgesfirst\", margin=0.04];",
    "  node [shape=box, style=\"rounded,filled\", color=\"black\", fillcolor=\"white\", fontname=\"Helvetica\", fontsize=22, penwidth=1.2];",
    "  edge [color=\"black\", arrowsize=0.7, penwidth=1.6, fontname=\"Helvetica\", fontsize=10];",
    paste(node_lines, collapse = "\n"),
    paste(label_node_lines, collapse = "\n"),
    paste(edge_lines, collapse = "\n"),
    "}",
    sep = "\n"
  )
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

output$diagram_show_mod_controls <- reactive({
  if(is.null(analysis_results())) return(TRUE)
  model_num <- suppressWarnings(as.integer(analysis_results()$settings$model))
  !(model_num %in% c(4L, 6L))
})
outputOptions(output, "diagram_show_mod_controls", suspendWhenHidden = FALSE)

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

get_plot_dims_px <- function(output_id, fallback_w, fallback_h) {
  w <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", output_id, "_width")]]))
  h <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", output_id, "_height")]]))
  if(is.na(w) || w <= 0) w <- fallback_w
  if(is.na(h) || h <= 0) h <- fallback_h
  list(width_px = w, height_px = h)
}

conceptual_diagram_plot_obj <- reactive({
  req(analysis_results())
  parsed <- diagram_parse_results()
  settings <- analysis_results()$settings
  mode_input <- input$diagram_coef_mode
  if(is.null(mode_input) || mode_input == "") mode_input <- "raw"
  fill_var_boxes_blue_opt <- if(is.null(input$diagram_fill_variable_boxes_blue)) FALSE else isTRUE(input$diagram_fill_variable_boxes_blue)
  dims <- get_plot_dims_px("conceptual_diagram_plot", 1200, 620)

  build_template_diagram(
    parsed = parsed,
    settings = settings,
    diagram_type = "conceptual",
    label_mode = mode_input,
    show_interactions = FALSE,
    include_ci = FALSE,
    include_p = FALSE,
    include_stars = FALSE,
    label_map = diagram_label_map(),
    fill_variable_boxes_blue = fill_var_boxes_blue_opt,
    plot_width_px = dims$width_px,
    plot_height_px = dims$height_px
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
  coef_digits_opt <- suppressWarnings(as.integer(input$diagram_coef_digits))
  if(is.na(coef_digits_opt) || !(coef_digits_opt %in% c(2L, 3L))) coef_digits_opt <- 3L
  coef_label_size_opt <- suppressWarnings(as.numeric(input$diagram_coef_font_size))
  if(is.na(coef_label_size_opt) || coef_label_size_opt < 2.0 || coef_label_size_opt > 5.0) coef_label_size_opt <- 3.3
  coef_label_orient_opt <- tolower(trimws(as.character(input$diagram_coef_orientation)))
  if(!coef_label_orient_opt %in% c("line", "horizontal")) coef_label_orient_opt <- "line"
  fill_var_boxes_blue_opt <- if(is.null(input$diagram_fill_variable_boxes_blue)) FALSE else isTRUE(input$diagram_fill_variable_boxes_blue)
  dims <- get_plot_dims_px("statistical_diagram_plot", 1500, 860)

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
    label_map = diagram_label_map(),
    coef_digits = coef_digits_opt,
    coef_label_size = coef_label_size_opt,
    coef_label_orientation = coef_label_orient_opt,
    fill_variable_boxes_blue = fill_var_boxes_blue_opt,
    plot_width_px = dims$width_px,
    plot_height_px = dims$height_px
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
    # Policy: default to unstandardized labels even when standardized values are available.
    selected <- "raw"
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
}, bg = "white", res = 96)

output$statistical_diagram_plot <- renderPlot({
  if(is.null(analysis_results())) {
    plot.new()
    text(0.5, 0.5, "Run an analysis to generate the statistical diagram.", col = "#666666")
    return(invisible(NULL))
  }
  print(statistical_diagram_plot_obj())
}, bg = "white", res = 96)

output$graphviz_statistical_ui <- renderUI({
  if(is.null(analysis_results())) return(NULL)
  model_num <- suppressWarnings(as.integer(analysis_results()$settings$model))
  if(!(model_num %in% c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 14L))) {
    return(tags$div(
      style = "margin-top: 8px; color: #666;",
      "Experimental Graphviz comparison is currently shown for Models 1, 2, 3, 4, 5, 6, 7, 8, and 14 only."
    ))
  }
  if(!requireNamespace("DiagrammeR", quietly = TRUE)) {
    return(tags$div(
      style = "margin-top: 8px; color: #a94442;",
      "DiagrammeR is not available in this runtime, so Graphviz comparison is disabled."
    ))
  }
  tagList(
    tags$hr(),
    h5("Statistical Diagram (Experimental Graphviz Comparison)"),
    DiagrammeR::grVizOutput("statistical_diagram_graphviz", height = "860px", width = "100%"),
    tags$div(
      style = "margin-top: 6px; color: #666;",
      "Graphviz preview is for visual comparison only. JPG export still uses the ggplot diagram path."
    )
  )
})

if(requireNamespace("DiagrammeR", quietly = TRUE)) {
  output$statistical_diagram_graphviz <- DiagrammeR::renderGrViz({
    req(analysis_results())
    settings <- analysis_results()$settings
    model_num <- suppressWarnings(as.integer(settings$model))
    if(!(model_num %in% c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 14L))) {
      return(DiagrammeR::grViz("digraph G { graph [bgcolor='white']; note [shape=box, label='Graphviz comparison is currently available for Models 1, 2, 3, 4, 5, 6, 7, 8, and 14 only.']; }"))
    }
    parsed <- diagram_parse_results()
    mode_input <- input$diagram_coef_mode
    if(is.null(mode_input) || mode_input == "") mode_input <- "raw"
    show_int <- if(is.null(input$diagram_show_interactions)) TRUE else isTRUE(input$diagram_show_interactions)
    show_mod_main <- if(is.null(input$diagram_show_mod_main_effects)) TRUE else isTRUE(input$diagram_show_mod_main_effects)
    include_ci_opt <- if(is.null(input$diagram_include_ci)) FALSE else isTRUE(input$diagram_include_ci)
    include_p_opt <- if(is.null(input$diagram_include_p)) FALSE else isTRUE(input$diagram_include_p)
    include_stars_opt <- if(is.null(input$diagram_include_stars)) TRUE else isTRUE(input$diagram_include_stars)
    coef_digits_opt <- suppressWarnings(as.integer(input$diagram_coef_digits))
    if(is.na(coef_digits_opt) || !(coef_digits_opt %in% c(2L, 3L))) coef_digits_opt <- 3L
    dot_txt <- build_graphviz_statistical_dot(
      parsed = parsed,
      settings = settings,
      label_mode = mode_input,
      show_interactions = show_int,
      show_moderator_main_effects = show_mod_main,
      include_ci = include_ci_opt,
      include_p = include_p_opt,
      include_stars = include_stars_opt,
      label_map = diagram_label_map(),
      coef_digits = coef_digits_opt
    )
    if(is.null(dot_txt) || !nzchar(dot_txt)) {
      dot_txt <- "digraph G { graph [bgcolor='white']; note [shape=box, label='Unable to build Graphviz preview for current settings.']; }"
    }
    DiagrammeR::grViz(dot_txt)
  })
}

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
    dims <- get_plot_dims_px("conceptual_diagram_plot", 1200, 620)
    width_in <- dims$width_px / 96
    height_in <- dims$height_px / 96
    p <- conceptual_diagram_plot_obj()
    ggplot2::ggsave(file, plot = p, device = "jpeg", width = width_in, height = height_in, dpi = 600, units = "in", bg = "white")
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
    dims <- get_plot_dims_px("statistical_diagram_plot", 1500, 860)
    width_in <- dims$width_px / 96
    height_in <- dims$height_px / 96
    p <- statistical_diagram_plot_obj()
    ggplot2::ggsave(file, plot = p, device = "jpeg", width = width_in, height = height_in, dpi = 600, units = "in", bg = "white")
  }
)

observe({
  has_results <- !is.null(analysis_results())
  elig <- if(has_results) diagram_eligibility() else list(eligible = FALSE)
  can_download <- has_results && isTRUE(elig$eligible)
  shinyjs::toggleState("download_conceptual_diagram", condition = can_download)
  shinyjs::toggleState("download_statistical_diagram", condition = can_download)
})
