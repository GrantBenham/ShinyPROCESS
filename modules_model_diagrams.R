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
      rhs <- gsub("\\s+", "", rhs)
      rhs <- gsub("\\*", "x", rhs)
      rhs <- toupper(gsub("x", "", rhs))
      if(nzchar(rhs)) {
        aliases[[int_name]] <- rhs
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
  
  if(is.na(est)) return("NA")
  
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
  key <- tolower(var_name)
  if(key %in% names(alias_map)) {
    return(alias_map[[key]])
  }
  var_name
}

build_template_diagram <- function(parsed, settings, diagram_type = c("conceptual", "statistical"),
                                   label_mode = "auto", show_interactions = TRUE,
                                   include_ci = FALSE, include_p = FALSE, include_stars = TRUE) {
  diagram_type <- match.arg(diagram_type)
  edges <- parsed$paths
  model_num <- suppressWarnings(as.integer(settings$model))
  alias_map <- parsed$metadata$product_aliases
  if(is.null(alias_map)) alias_map <- character(0)

  if(nrow(edges) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No diagram data available for current analysis", size = 6) +
        ggplot2::theme_void() +
        ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1)
    )
  }
  
  # Drop covariates entirely from diagram logic.
  edges <- edges[edges$path_kind != "covariate", , drop = FALSE]
  if(!isTRUE(show_interactions)) {
    edges <- edges[!(edges$path_kind == "interaction" | grepl("^int_[0-9]+$", edges$from)), , drop = FALSE]
  }

  x_var <- settings$predictor_var
  y_var <- settings$outcome_var
  mediators <- if(!is.null(settings$mediator_vars)) settings$mediator_vars else character(0)
  w_var <- if(!is.null(settings$moderator_var) && nzchar(settings$moderator_var)) settings$moderator_var else character(0)
  z_var <- if(!is.null(settings$moderator2_var) && nzchar(settings$moderator2_var)) settings$moderator2_var else character(0)
  int_edges <- edges[edges$path_kind == "interaction" | grepl("^int_[0-9]+$", edges$from), , drop = FALSE]

  # Support only first scoped set for now.
  if(!(model_num %in% c(1L, 2L, 3L, 4L, 8L, 14L))) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0.05, label = paste0("Model ", model_num, " template not implemented yet"), size = 6) +
        ggplot2::annotate("text", x = 0, y = -0.10, label = "Supported currently: 1, 2, 3, 4, 8, 14", size = 4.4, color = "gray35") +
        ggplot2::theme_void() +
        ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1)
    )
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
    if(model_num %in% c(1L, 2L, 3L)) {
      if(length(w_var) > 0) add_node(w_var, -0.10, 0.45, "mod")
      if(length(z_var) > 0) add_node(z_var, -0.45, 0.45, "mod")
    }
    if(model_num %in% c(4L, 8L, 14L)) {
      n_m <- length(mediators)
      if(n_m == 1) {
        add_node(mediators[[1]], 0.00, 0.45, "m")
      } else if(n_m == 2) {
        add_node(mediators[[1]], 0.00, 0.52, "m")
        add_node(mediators[[2]], 0.00, -0.22, "m")
      } else if(n_m > 2) {
        med_y <- seq(0.55, -0.30, length.out = n_m)
        for(i in seq_along(mediators)) add_node(mediators[[i]], 0.00, med_y[[i]], "m")
      }
      if(length(w_var) > 0 && model_num %in% c(8L, 14L)) add_node(w_var, -0.35, 0.55, "mod")
    }
  } else {
    # Statistical templates
    if(model_num == 1L) {
      add_node(x_var, -0.75, 0.45, "x")
      if(length(w_var) > 0) add_node(w_var, -0.75, 0.05, "mod")
      for(i in seq_len(nrow(int_edges))) add_node(resolve_interaction_label(int_edges$from[[i]], alias_map), -0.75, -0.35 - 0.25 * (i - 1), "int")
      add_node(y_var, 0.75, 0.00, "y")
    } else if(model_num %in% c(2L, 3L)) {
      add_node(x_var, -0.75, 0.55, "x")
      if(length(w_var) > 0) add_node(w_var, -0.75, 0.20, "mod")
      if(length(z_var) > 0) add_node(z_var, -0.75, -0.10, "mod")
      if(nrow(int_edges) > 0) {
        y_pos <- seq(-0.40, -0.85, length.out = nrow(int_edges))
        for(i in seq_len(nrow(int_edges))) add_node(resolve_interaction_label(int_edges$from[[i]], alias_map), -0.75, y_pos[[i]], "int")
      }
      add_node(y_var, 0.75, 0.00, "y")
    } else if(model_num == 4L) {
      add_node(x_var, -0.75, 0.00, "x")
      add_node(y_var, 0.75, 0.00, "y")
      n_m <- length(mediators)
      if(n_m == 1) {
        add_node(mediators[[1]], 0.00, 0.50, "m")
      } else if(n_m == 2) {
        add_node(mediators[[1]], 0.00, 0.52, "m")
        add_node(mediators[[2]], 0.00, -0.22, "m")
      } else if(n_m > 2) {
        med_y <- seq(0.58, -0.35, length.out = n_m)
        for(i in seq_along(mediators)) add_node(mediators[[i]], 0.00, med_y[[i]], "m")
      }
      if(nrow(int_edges) > 0) {
        for(i in seq_len(nrow(int_edges))) add_node(resolve_interaction_label(int_edges$from[[i]], alias_map), 0.25, -0.70 - 0.20 * (i - 1), "int")
      }
    } else if(model_num == 8L) {
      add_node(x_var, -0.75, 0.25, "x")
      if(length(w_var) > 0) add_node(w_var, -0.75, -0.10, "mod")
      for(i in seq_len(nrow(int_edges))) add_node(resolve_interaction_label(int_edges$from[[i]], alias_map), -0.25, -0.48 - 0.20 * (i - 1), "int")
      if(length(mediators) > 0) add_node(mediators[[1]], 0.05, 0.55, "m")
      add_node(y_var, 0.75, 0.00, "y")
    } else if(model_num == 14L) {
      add_node(x_var, -0.75, 0.10, "x")
      if(length(mediators) > 0) add_node(mediators[[1]], 0.00, 0.55, "m")
      if(length(w_var) > 0) add_node(w_var, -0.15, -0.20, "mod")
      for(i in seq_len(nrow(int_edges))) add_node(resolve_interaction_label(int_edges$from[[i]], alias_map), 0.35, -0.55 - 0.20 * (i - 1), "int")
      add_node(y_var, 0.75, 0.00, "y")
    }
  }

  nodes <- unique(nodes)
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
  
  edge_plot$dx <- edge_plot$x_to - edge_plot$x_from
  edge_plot$dy <- edge_plot$y_to - edge_plot$y_from
  edge_plot$seg_len <- pmax(sqrt(edge_plot$dx^2 + edge_plot$dy^2), 1e-6)
  edge_plot$xm <- (edge_plot$x_from + edge_plot$x_to) / 2
  edge_plot$ym <- (edge_plot$y_from + edge_plot$y_to) / 2
  edge_plot$label_nudge <- ifelse(edge_plot$path_kind == "covariate", 0.03, 0.045)
  edge_plot$x_label <- edge_plot$xm - edge_plot$label_nudge * (edge_plot$dy / edge_plot$seg_len)
  edge_plot$y_label <- edge_plot$ym + edge_plot$label_nudge * (edge_plot$dx / edge_plot$seg_len)

  # Conceptual moderation cue arrows (to path midpoint only; no coefficient label)
  mod_cue <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0))
  if(identical(diagram_type, "conceptual") && model_num %in% c(1L, 2L, 3L, 8L, 14L)) {
    x_node <- nodes[nodes$name == x_var, , drop = FALSE]
    y_node <- nodes[nodes$name == y_var, , drop = FALSE]
    if(nrow(x_node) == 1 && nrow(y_node) == 1) {
      midx <- (x_node$x + y_node$x) / 2
      midy <- (x_node$y + y_node$y) / 2
      if(length(w_var) > 0 && any(nodes$name == w_var)) {
        w_node <- nodes[nodes$name == w_var, , drop = FALSE]
        mod_cue <- rbind(mod_cue, data.frame(x = w_node$x, y = w_node$y, xend = midx - 0.03, yend = midy + 0.04))
      }
      if(length(z_var) > 0 && any(nodes$name == z_var)) {
        z_node <- nodes[nodes$name == z_var, , drop = FALSE]
        mod_cue <- rbind(mod_cue, data.frame(x = z_node$x, y = z_node$y, xend = midx + 0.03, yend = midy + 0.02))
      }
    }
  }

  title_txt <- paste0("Model ", settings$model, " ", if(identical(diagram_type, "conceptual")) "Conceptual" else "Statistical", " Diagram")
  subtitle_txt <- paste0(
    "Coefficient mode: ",
    switch(label_mode,
           "raw" = "unstandardized",
           "std" = "standardized",
           "auto" = parsed$metadata$label_mode_used,
           parsed$metadata$label_mode_used)
  )
  
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_plot,
      ggplot2::aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linetype = path_kind),
      arrow = grid::arrow(length = grid::unit(0.15, "cm")),
      color = "black",
      linewidth = 0.65
    ) +
    ggplot2::geom_label(
      data = edge_plot,
      ggplot2::aes(x = x_label, y = y_label, label = label),
      size = 3.3,
      label.size = 0.2,
      fill = "white",
      color = "black"
    ) +
    ggplot2::geom_label(
      data = nodes,
      ggplot2::aes(x = x, y = y, label = name),
      size = 4.8,
      label.size = 0.45,
      fill = "white",
      color = "black",
      fontface = "bold"
    ) +
    ggplot2::geom_segment(
      data = mod_cue,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      linetype = "dashed",
      linewidth = 0.5,
      color = "black",
      arrow = grid::arrow(length = grid::unit(0.12, "cm"))
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        direct = "solid",
        mediator = "solid",
        moderator = "solid",
        covariate = "dotted",
        unknown = "solid"
      ),
      guide = "none"
    ) +
    ggplot2::coord_fixed(xlim = c(-1.0, 1.0), ylim = c(-1.05, 0.85), ratio = 1.0, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0.5, color = "black"),
      plot.subtitle = ggplot2::element_text(size = 10.5, hjust = 0.5, color = "black")
    ) +
    ggplot2::labs(title = title_txt, subtitle = subtitle_txt)
}

conceptual_diagram_plot_obj <- reactive({
  req(analysis_results())
  parsed <- diagram_parse_results()
  settings <- analysis_results()$settings
  
  mode_input <- input$diagram_coef_mode
  if(is.null(mode_input) || mode_input == "") mode_input <- "auto"
  show_int <- FALSE
  include_ci_opt <- FALSE
  include_p_opt <- FALSE
  include_stars_opt <- FALSE

  build_template_diagram(
    parsed = parsed,
    settings = settings,
    diagram_type = "conceptual",
    label_mode = mode_input,
    show_interactions = show_int,
    include_ci = include_ci_opt,
    include_p = include_p_opt,
    include_stars = include_stars_opt
  )
})

statistical_diagram_plot_obj <- reactive({
  req(analysis_results())
  parsed <- diagram_parse_results()
  settings <- analysis_results()$settings

  mode_input <- input$diagram_coef_mode
  if(is.null(mode_input) || mode_input == "") mode_input <- "auto"
  show_int <- if(is.null(input$diagram_show_interactions)) TRUE else isTRUE(input$diagram_show_interactions)
  include_ci_opt <- if(is.null(input$diagram_include_ci)) FALSE else isTRUE(input$diagram_include_ci)
  include_p_opt <- if(is.null(input$diagram_include_p)) FALSE else isTRUE(input$diagram_include_p)
  include_stars_opt <- if(is.null(input$diagram_include_stars)) TRUE else isTRUE(input$diagram_include_stars)

  build_template_diagram(
    parsed = parsed,
    settings = settings,
    diagram_type = "statistical",
    label_mode = mode_input,
    show_interactions = show_int,
    include_ci = include_ci_opt,
    include_p = include_p_opt,
    include_stars = include_stars_opt
  )
})

output$conceptual_diagram_plot <- renderPlot({
  req(analysis_results())
  print(conceptual_diagram_plot_obj())
}, bg = "white")

output$statistical_diagram_plot <- renderPlot({
  req(analysis_results())
  print(statistical_diagram_plot_obj())
}, bg = "white")

output$model_diagram_notes <- renderUI({
  req(analysis_results())
  parsed <- diagram_parse_results()
  warn <- parsed$metadata$warnings
  if(is.null(warn) || length(warn) == 0) return(NULL)
  HTML(paste0("<div style='margin-top:8px; color:#555;'><strong>Notes:</strong><br>", paste(unique(warn), collapse = "<br>"), "</div>"))
})

output$download_conceptual_diagram <- downloadHandler(
  filename = function() {
    paste0("conceptual_diagram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
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
    paste0("statistical_diagram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
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
  shinyjs::toggleState("download_conceptual_diagram", condition = has_results)
  shinyjs::toggleState("download_statistical_diagram", condition = has_results)
})
