#!/usr/bin/env Rscript

# Export Shinylive build safely from repo root.
# - Hard stop if process.R exists (copyrighted; must not be bundled)
# - Build from a temporary minimal app directory (only required files)
# - Temporarily set runtime.txt to shinylive
# - Export app to docs/
# - Restore runtime.txt to rshiny on exit (safe local default)

main <- function() {
  app_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  process_path <- file.path(app_root, "process.R")
  runtime_path <- file.path(app_root, "runtime.txt")
  dest_dir <- file.path(app_root, "docs")
  temp_app_dir <- tempfile(pattern = "gbprocess_shinylive_")
  
  message("Starting Shinylive export from: ", app_root)
  
  if (file.exists(process_path)) {
    stop(
      "HARD STOP: process.R exists in repo root. Remove it before Shinylive export to avoid bundling copyrighted code.",
      call. = FALSE
    )
  }
  
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    stop("Package 'shinylive' is required. Install with install.packages('shinylive').", call. = FALSE)
  }
  
  # Required runtime files for app export (minimal whitelist).
  required_files <- c(
    "gbPROCESS.R",
    "model_specs.R",
    "modules_ui.R",
    "modules_analysis.R",
    "modules_assumptions.R",
    "modules_assumption_outputs.R",
    "modules_data_management.R",
    "modules_results.R",
    "modules_save_load.R",
    "runtime.txt"
  )
  
  missing_required <- required_files[!file.exists(file.path(app_root, required_files))]
  if (length(missing_required) > 0) {
    stop(
      paste0("Missing required app file(s): ", paste(missing_required, collapse = ", ")),
      call. = FALSE
    )
  }
  
  if (!file.exists(runtime_path)) {
    message("runtime.txt not found. Creating default runtime.txt with 'rshiny'.")
    writeLines("rshiny", runtime_path, useBytes = TRUE)
  }
  
  on.exit({
    writeLines("rshiny", runtime_path, useBytes = TRUE)
    message("runtime.txt restored to: rshiny")
  }, add = TRUE)
  
  writeLines("shinylive", runtime_path, useBytes = TRUE)
  message("runtime.txt temporarily set to: shinylive")
  
  dir.create(temp_app_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(temp_app_dir)) {
    stop(paste0("Failed to create temporary app directory: ", temp_app_dir), call. = FALSE)
  }
  on.exit({
    if (dir.exists(temp_app_dir)) {
      unlink(temp_app_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  
  for (f in required_files) {
    ok <- file.copy(
      from = file.path(app_root, f),
      to = temp_app_dir,
      overwrite = TRUE
    )
    if (!isTRUE(ok)) {
      stop(paste0("Failed to copy required file into temporary app directory: ", f), call. = FALSE)
    }
  }
  
  # Shinylive expects app.R or server.R as entrypoint.
  # Create app.R by copying the canonical entrypoint content.
  app_lines <- readLines(file.path(temp_app_dir, "gbPROCESS.R"), warn = FALSE)
  preloads <- c("S7", "munsell")
  preload_lines <- character(0)
  for (pkg in preloads) {
    if (!any(grepl(paste0("^\\s*library\\(", pkg, "\\)"), app_lines))) {
      preload_lines <- c(preload_lines, paste0("library(", pkg, ")"))
    }
  }
  if (length(preload_lines) > 0) {
    app_lines <- c(
      "# Added for Shinylive dependency resolution",
      preload_lines,
      app_lines
    )
  }
  writeLines(app_lines, con = file.path(temp_app_dir, "app.R"), useBytes = TRUE)
  
  export_appdir <- normalizePath(temp_app_dir, winslash = "/", mustWork = TRUE)
  if (!dir.exists(export_appdir)) {
    stop(paste0("Temporary app directory is not available: ", export_appdir), call. = FALSE)
  }
  
  message("Exporting with shinylive::export(appdir=temp_app_dir, destdir='docs') ...")
  shinylive::export(appdir = export_appdir, destdir = "docs")
  
  message("Shinylive export complete: ", dest_dir)
}

main()
