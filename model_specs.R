# ============================================================================
# MODEL SPECS MODULE
# ============================================================================
# Canonical PROCESS model metadata used to centralize model capabilities.
# Phase 1: definition only (no behavior changes yet).
# ============================================================================

# Build full model table (1..92) with conservative defaults
process_model_specs <- data.frame(
  model = 1:92,
  supported = FALSE,
  selectable = FALSE,
  internal_only = FALSE,
  has_m = FALSE,
  has_w = FALSE,
  has_z = FALSE,
  requires_w_input = FALSE,
  requires_z_input = FALSE,
  min_mediators = 0L,
  max_mediators = 0L,
  supports_plot = FALSE,
  plot_mode = rep("none", 92),
  notes = rep("", 92),
  stringsAsFactors = FALSE
)

# Supported/selectable model subsets for this app
SUPPORTED_MODELS <- c(1:22, 28:29, 58:76, 80:92)
SELECTABLE_MODELS <- setdiff(SUPPORTED_MODELS, 74)
INVALID_MODELS <- c(23:27, 30:57, 77:79)

# Draft canonical model groupings (validated against current app intent)
MODELS_ONE_W <- c(1, 5, 7, 8, 14, 15, 58, 59, 83:92)
MODELS_TWO_WZ <- c(2, 3, 9:13, 16:22, 28, 29, 60:73, 75, 76)
MODELS_WITH_W <- c(MODELS_ONE_W, MODELS_TWO_WZ)
MODELS_WITH_Z <- MODELS_TWO_WZ
MODELS_WITH_M <- c(4:22, 28, 29, 58:76, 80:92)

# Populate support/selectability flags
process_model_specs$supported[process_model_specs$model %in% SUPPORTED_MODELS] <- TRUE
process_model_specs$selectable[process_model_specs$model %in% SELECTABLE_MODELS] <- TRUE
process_model_specs$internal_only[process_model_specs$model == 74] <- TRUE

# Populate structural capability flags
process_model_specs$has_m[process_model_specs$model %in% MODELS_WITH_M] <- TRUE
process_model_specs$has_w[process_model_specs$model %in% MODELS_WITH_W] <- TRUE
process_model_specs$has_z[process_model_specs$model %in% MODELS_WITH_Z] <- TRUE

# Populate UI requirement flags
process_model_specs$requires_w_input[process_model_specs$model %in% MODELS_WITH_W] <- TRUE
process_model_specs$requires_z_input[process_model_specs$model %in% MODELS_WITH_Z] <- TRUE

# Model 74 is internal-only in this app (Model 4 + xmint path), so no W input required
process_model_specs$requires_w_input[process_model_specs$model == 74] <- FALSE

# Populate mediator constraints
process_model_specs$min_mediators[process_model_specs$has_m] <- 1L
process_model_specs$max_mediators[process_model_specs$has_m] <- 10L
process_model_specs$max_mediators[process_model_specs$model == 6] <- 6L
process_model_specs$min_mediators[process_model_specs$model == 82] <- 4L
process_model_specs$max_mediators[process_model_specs$model == 82] <- 4L
process_model_specs$min_mediators[process_model_specs$model %in% 83:92] <- 2L
process_model_specs$max_mediators[process_model_specs$model %in% 83:92] <- 2L
process_model_specs$min_mediators[process_model_specs$model %in% 80:81] <- 3L
process_model_specs$max_mediators[process_model_specs$model %in% 80:81] <- 6L

# Populate plotting capability flags (current app behavior)
process_model_specs$supports_plot[process_model_specs$model %in% c(1, 3)] <- TRUE
process_model_specs$plot_mode[process_model_specs$model == 1] <- "model1"
process_model_specs$plot_mode[process_model_specs$model == 3] <- "model3"

# Notes for special handling
process_model_specs$notes[process_model_specs$model == 74] <- "Internal only via model 4 + xmint; W=X"

# Convenience derived vectors (from the table; avoid extra hard-coding elsewhere)
VALID_USER_MODELS <- process_model_specs$model[process_model_specs$supported & process_model_specs$selectable]
MODELS_REQUIRING_W <- process_model_specs$model[process_model_specs$requires_w_input]
MODELS_REQUIRING_Z <- process_model_specs$model[process_model_specs$requires_z_input]
MODELS_WITH_PLOTS <- process_model_specs$model[process_model_specs$supports_plot]
