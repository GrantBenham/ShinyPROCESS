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

build_model_diagram_nodes <- function(settings, edges, show_covariates = TRUE, show_interactions = TRUE) {
  x_var <- settings$predictor_var
  y_var <- settings$outcome_var
  mediators <- if(!is.null(settings$mediator_vars)) settings$mediator_vars else character(0)
  w_var <- if(!is.null(settings$moderator_var) && nzchar(settings$moderator_var)) settings$moderator_var else character(0)
  z_var <- if(!is.null(settings$moderator2_var) && nzchar(settings$moderator2_var)) settings$moderator2_var else character(0)
  covars <- if(isTRUE(show_covariates) && !is.null(settings$covariates)) settings$covariates else character(0)
  
  vars <- unique(c(x_var, y_var, mediators, w_var, z_var, covars, edges$from, edges$to))
  vars <- vars[nzchar(vars)]
  
  interactions <- vars[grepl("\\bx\\b|\\bint_[0-9]+\\b", vars, ignore.case = TRUE)]
  if(!isTRUE(show_interactions) && length(interactions) > 0) {
    vars <- setdiff(vars, interactions)
  }
  
  n_m <- length(mediators)
  m_x <- if(n_m > 0) seq(-0.2, 0.2, length.out = n_m) else numeric(0)
  
  node_df <- data.frame(
    name = vars,
    role = "other",
    x = 0,
    y = 0,
    stringsAsFactors = FALSE
  )
  
  node_df$role[node_df$name == x_var] <- "x"
  node_df$role[node_df$name == y_var] <- "y"
  node_df$role[node_df$name %in% mediators] <- "m"
  if(length(w_var) > 0) node_df$role[node_df$name == w_var] <- "w"
  if(length(z_var) > 0) node_df$role[node_df$name == z_var] <- "z"
  if(length(covars) > 0) node_df$role[node_df$name %in% covars] <- "cov"
  if(length(interactions) > 0) node_df$role[node_df$name %in% interactions] <- "int"
  
  node_df$x[node_df$role == "x"] <- -1
  node_df$y[node_df$role == "x"] <- 0
  node_df$x[node_df$role == "y"] <- 1
  node_df$y[node_df$role == "y"] <- 0
  
  if(n_m > 0) {
    for(i in seq_along(mediators)) {
      node_df$x[node_df$name == mediators[[i]]] <- m_x[[i]]
      node_df$y[node_df$name == mediators[[i]]] <- 0.55
    }
  }
  
  if(length(w_var) > 0) {
    node_df$x[node_df$role == "w"] <- -0.45
    node_df$y[node_df$role == "w"] <- -0.6
  }
  if(length(z_var) > 0) {
    node_df$x[node_df$role == "z"] <- 0.45
    node_df$y[node_df$role == "z"] <- -0.6
  }
  
  if(length(covars) > 0) {
    cov_x <- seq(-1, 1, length.out = length(covars))
    for(i in seq_along(covars)) {
      node_df$x[node_df$name == covars[[i]]] <- cov_x[[i]]
      node_df$y[node_df$name == covars[[i]]] <- -1.05
    }
  }
  
  int_names <- node_df$name[node_df$role == "int"]
  if(length(int_names) > 0) {
    int_x <- seq(-0.2, 0.2, length.out = length(int_names))
    for(i in seq_along(int_names)) {
      node_df$x[node_df$name == int_names[[i]]] <- int_x[[i]]
      node_df$y[node_df$name == int_names[[i]]] <- -0.2
    }
  }
  
  node_df
}

build_model_diagram_plot <- function(parsed, settings, label_mode = "auto",
                                     show_covariates = TRUE, show_interactions = TRUE,
                                     include_ci = FALSE, include_p = FALSE, include_stars = TRUE) {
  edges <- parsed$paths
  if(nrow(edges) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No diagram data available for current analysis", size = 6) +
        ggplot2::theme_void() +
        ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1)
    )
  }
  
  if(!isTRUE(show_covariates)) {
    edges <- edges[edges$path_kind != "covariate", , drop = FALSE]
  }
  if(!isTRUE(show_interactions)) {
    edges <- edges[edges$path_kind != "interaction", , drop = FALSE]
  }
  if(nrow(edges) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0, label = "No edges to display with current options", size = 6) +
        ggplot2::theme_void() +
        ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1)
    )
  }
  
  nodes <- build_model_diagram_nodes(settings, edges, show_covariates = show_covariates, show_interactions = show_interactions)
  
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
  
  edge_plot$xm <- (edge_plot$x_from + edge_plot$x_to) / 2
  edge_plot$ym <- (edge_plot$y_from + edge_plot$y_to) / 2
  
  title_txt <- paste0("Model ", settings$model, " Path Diagram")
  subtitle_txt <- paste0(
    "Coefficient mode: ",
    switch(label_mode,
           "raw" = "unstandardized",
           "std" = "standardized",
           "auto" = parsed$metadata$label_mode_used,
           parsed$metadata$label_mode_used)
  )
  
  ggplot2::ggplot() +
    ggplot2::geom_curve(
      data = edge_plot,
      ggplot2::aes(x = x_from, y = y_from, xend = x_to, yend = y_to, linetype = path_kind),
      arrow = grid::arrow(length = grid::unit(0.15, "cm")),
      color = "black",
      linewidth = 0.5,
      curvature = 0.05
    ) +
    ggplot2::geom_label(
      data = edge_plot,
      ggplot2::aes(x = xm, y = ym, label = label),
      size = 3,
      label.size = 0.2,
      fill = "white",
      color = "black"
    ) +
    ggplot2::geom_label(
      data = nodes,
      ggplot2::aes(x = x, y = y, label = name),
      size = 4.2,
      label.size = 0.35,
      fill = "white",
      color = "black",
      fontface = "bold"
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        direct = "solid",
        mediator = "solid",
        moderator = "solid",
        interaction = "dashed",
        covariate = "dotted",
        unknown = "solid"
      ),
      guide = "none"
    ) +
    ggplot2::coord_cartesian(xlim = c(-1.25, 1.25), ylim = c(-1.2, 0.85), clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5, color = "black"),
      plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, color = "black")
    ) +
    ggplot2::labs(title = title_txt, subtitle = subtitle_txt)
}

diagram_plot_obj <- reactive({
  req(analysis_results())
  parsed <- diagram_parse_results()
  settings <- analysis_results()$settings
  
  mode_input <- input$diagram_coef_mode
  if(is.null(mode_input) || mode_input == "") mode_input <- "auto"
  show_cov <- if(is.null(input$diagram_show_covariates)) TRUE else isTRUE(input$diagram_show_covariates)
  show_int <- if(is.null(input$diagram_show_interactions)) TRUE else isTRUE(input$diagram_show_interactions)
  include_ci_opt <- if(is.null(input$diagram_include_ci)) FALSE else isTRUE(input$diagram_include_ci)
  include_p_opt <- if(is.null(input$diagram_include_p)) FALSE else isTRUE(input$diagram_include_p)
  include_stars_opt <- if(is.null(input$diagram_include_stars)) TRUE else isTRUE(input$diagram_include_stars)
  
  build_model_diagram_plot(
    parsed = parsed,
    settings = settings,
    label_mode = mode_input,
    show_covariates = show_cov,
    show_interactions = show_int,
    include_ci = include_ci_opt,
    include_p = include_p_opt,
    include_stars = include_stars_opt
  )
})

output$model_diagram_plot <- renderPlot({
  req(analysis_results())
  print(diagram_plot_obj())
}, bg = "white")

output$model_diagram_notes <- renderUI({
  req(analysis_results())
  parsed <- diagram_parse_results()
  warn <- parsed$metadata$warnings
  if(is.null(warn) || length(warn) == 0) return(NULL)
  HTML(paste0("<div style='margin-top:8px; color:#555;'><strong>Notes:</strong><br>", paste(unique(warn), collapse = "<br>"), "</div>"))
})

output$download_model_diagram <- downloadHandler(
  filename = function() {
    paste0("model_diagram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
  },
  contentType = "image/jpeg",
  content = function(file) {
    req(analysis_results())
    p <- diagram_plot_obj()
    ggplot2::ggsave(file, plot = p, device = "jpeg", width = 12, height = 8, dpi = 600, units = "in", bg = "white")
  }
)

observe({
  has_results <- !is.null(analysis_results())
  shinyjs::toggleState("download_model_diagram", condition = has_results)
})
