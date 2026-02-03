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

# Increase file upload size to 50MB
options(shiny.maxRequestSize = 50 * 1024^2)

# ============================================================================
# ASSUMPTION CHECKS MODULE (Self-contained for easy maintenance)
# ============================================================================

# Helper function to detect if a variable is binary (0/1 or only 2 unique values)
is_binary_variable <- function(data, var_name) {
  if(is.null(data) || is.null(var_name) || !var_name %in% names(data)) {
    return(FALSE)
  }
  var_data <- data[[var_name]]
  var_data <- var_data[!is.na(var_data)]
  unique_vals <- unique(var_data)
  # Check if only 2 unique values
  if(length(unique_vals) == 2) {
    return(TRUE)
  }
  return(FALSE)
}

# Helper to detect continuous variables (numeric/integer/labelled and not binary)
is_continuous_variable <- function(data, var_name) {
  if(is.null(data) || is.null(var_name) || !var_name %in% names(data)) {
    return(FALSE)
  }
  if(is_binary_variable(data, var_name)) return(FALSE)
  v <- data[[var_name]]
  is.numeric(v) || is.integer(v) || inherits(v, "labelled")
}

# Function to identify outliers based on model type
identify_outliers_assumption <- function(data, outcome_var, predictor_var, 
                                         mediator_vars = NULL, moderator_var = NULL,
                                         moderator2_var = NULL, covariates = NULL, 
                                         residual_threshold = 2,
                                         cooks_threshold_type = "conservative", 
                                         cooks_threshold_custom = 0.01) {
  # Build formula based on model type
  formula_terms <- c(outcome_var, "~", predictor_var)
  
  # Add interaction term for moderation models (only if moderator is provided)
  if(!is.null(moderator_var) && moderator_var != "") {
    formula_terms <- c(formula_terms, "*", moderator_var)
  }
  
  # Add second moderator interaction if provided
  if(!is.null(moderator2_var) && moderator2_var != "") {
    # For models with two moderators, add the second moderator interaction
    formula_terms <- c(formula_terms, "*", moderator2_var)
  }
  
  # Add mediators for mediation models
  if(!is.null(mediator_vars) && length(mediator_vars) > 0) {
    formula_terms <- c(formula_terms, "+", paste(mediator_vars, collapse = " + "))
  }
  
  # Add covariates
  if(!is.null(covariates) && length(covariates) > 0) {
    formula_terms <- c(formula_terms, "+", paste(covariates, collapse = " + "))
  }
  
  model_formula <- as.formula(paste(formula_terms, collapse = " "))
  
  # Check if outcome is binary
  outcome_is_binary <- is_binary_variable(data, outcome_var)
  
  if(outcome_is_binary) {
    # For binary outcomes, use logistic regression diagnostics
    model <- glm(model_formula, data = data, family = binomial())
    
    # Calculate leverage (hat values) and Cook's distance
    leverage <- hatvalues(model)
    cooks_d <- cooks.distance(model)
    
    # Get threshold
    n <- nrow(data)
    threshold <- if(cooks_threshold_type == "conservative") {
      4 / n
    } else if(cooks_threshold_type == "liberal") {
      1.0
    } else {
      cooks_threshold_custom
    }
    
    # Identify influential cases based on Cook's distance
    influential_cases <- which(cooks_d > threshold)
    
    return(list(
      cases = influential_cases,
      values = cooks_d[influential_cases],
      leverage = leverage[influential_cases],
      count = length(influential_cases),
      percentage = length(influential_cases) / length(cooks_d) * 100,
      is_binary = TRUE,
      threshold = threshold,
      method = "Cook's Distance"
    ))
  } else {
    # For continuous outcomes, use linear regression
    model <- lm(model_formula, data = data)
    std_resid <- rstandard(model)
    
    # Identify outliers
    outlier_cases <- which(abs(std_resid) > residual_threshold)
    outlier_values <- std_resid[outlier_cases]
    
    return(list(
      cases = outlier_cases,
      values = outlier_values,
      count = length(outlier_cases),
      percentage = length(outlier_cases) / length(std_resid) * 100,
      is_binary = FALSE,
      threshold = residual_threshold,
      method = "Standardized Residuals"
    ))
  }
}

# Function to check normality
check_normality <- function(model) {
  # Shapiro-Wilk test
  sw_test <- shapiro.test(residuals(model))
  # Create Q-Q plot and capture it
  qq_plot <- ggplot(data.frame(residuals = residuals(model)), aes(sample = residuals)) +
    stat_qq() + stat_qq_line() +
    theme_minimal() +
    labs(title = "Normal Q-Q Plot of Residuals")
  
  return(list(
    test = sw_test,
    plot = qq_plot,
    text = sprintf("Normality (Shapiro-Wilk): W = %.3f, p %s",
                  sw_test$statistic,
                  ifelse(sw_test$p.value < .001, "< .001",
                        sprintf("= %.3f", sw_test$p.value)))
  ))
}

# Function to test homoscedasticity
test_homoscedasticity <- function(model) {
  # Breusch-Pagan test
  bp_test <- car::ncvTest(model)
  return(sprintf("Homoscedasticity (Breusch-Pagan): χ²(%d) = %.3f, p %s",
                bp_test$Df,
                bp_test$ChiSquare,
                ifelse(bp_test$p < .001, "< .001", sprintf("= %.3f", bp_test$p))))
}

# Function for diagnostic report
diagnostic_report <- function(model) {
  tryCatch({
    # Basic model diagnostics
    n <- nobs(model)
    
    # Check if model is logistic (glm with binomial family)
    is_logistic <- inherits(model, "glm") && model$family$family == "binomial"
    
    # Handle VIF calculation
    vif_result <- tryCatch({
      # Get model terms
      terms <- attr(terms(model), "term.labels")
      
      if(length(terms) > 1) {
        # Calculate VIF for all predictors
        vif_values <- suppressWarnings(car::vif(model))
        
        # Format VIF results
        sprintf("VIF for predictors: %s", 
                paste(names(vif_values), sprintf("%.2f", vif_values), 
                collapse = ", "))
      } else {
        "VIF not calculated (insufficient predictors)"
      }
    }, error = function(e) {
      "VIF calculation unavailable"
    })
    
    # Return diagnostics
    c(
      sprintf("Sample size: %d", n),
      vif_result,
      if(is_logistic) {
        "Note: VIF calculated for all predictors. For binary outcomes, VIF interpretation is similar to linear regression."
      } else {
        "Note: VIF calculated for all predictors"
      }
    )
  }, error = function(e) {
    c(
      "Unable to compute some diagnostic measures",
      paste("Error details:", e$message)
    )
  })
}

# Helper function to check if all required variables are selected for assumption checks
check_required_vars_for_assumptions <- function(model_num, predictor_var, outcome_var, 
                                                 moderator_var, moderator2_var, mediator_vars) {
  # Always need predictor and outcome
  if(is.null(predictor_var) || predictor_var == "" || 
     is.null(outcome_var) || outcome_var == "") {
    return(list(valid = FALSE, message = "Please select both Predictor (X) and Outcome (Y) variables."))
  }
  
  # Models with one moderator (W): 1, 5, 14, 15, 58, 59, 74, 83-92
  models_with_moderator <- c(1, 5, 14, 15, 58, 59, 74, 83:92)
  # Models with two moderators (W and Z): 2, 3
  models_with_second_moderator <- c(2, 3)
  
  # Check for moderator if required
  if(model_num %in% models_with_moderator || model_num %in% models_with_second_moderator) {
    if(is.null(moderator_var) || moderator_var == "") {
      return(list(valid = FALSE, message = "Please select Moderator (W) variable for this model."))
    }
  }
  
  # Check for second moderator if required
  if(model_num %in% models_with_second_moderator) {
    if(is.null(moderator2_var) || moderator2_var == "") {
      return(list(valid = FALSE, message = "Please select Second Moderator (Z) variable for this model."))
    }
  }
  
  # Models 4-92: All require at least one mediator
  if(model_num >= 4 && model_num <= 92) {
    if(is.null(mediator_vars) || length(mediator_vars) == 0) {
      return(list(valid = FALSE, message = "Please select at least one Mediator variable for this model."))
    }
  }
  
  return(list(valid = TRUE, message = ""))
}

# Helper to summarize binary counts
binary_count_lines <- function(data, var, label) {
  if (is.null(var) || var == "" || is.null(data)) return(NULL)
  vals <- data[[var]]
  vals <- vals[!is.na(vals)]
  if (length(unique(vals)) > 2) return(NULL)
  tab <- table(vals)
  levels_sorted <- sort(unique(vals))
  counts <- sapply(levels_sorted, function(x) tab[as.character(x)])
  names(counts) <- levels_sorted
  sprintf("%s (%s): %s", label, var,
          paste(sprintf("%s = %d", names(counts), counts), collapse = "; "))
}

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  useShinyjs(),
  tags$style(type="text/css", "body { max-width: 1400px; margin: auto; }"),
  titlePanel("PROCESS V5 Analysis with Hayes PROCESS for R"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Upload Data"),
      div(
        id = "file_input_div",
        fileInput("data_file", "Choose CSV or SAV File", accept = c(".csv", ".sav"))
      ),
      
      h4("Model Selection"),
      selectInput("process_model", "PROCESS Model Number",
                  choices = c("Select model" = "", 
                              "1" = "1", "2" = "2", "3" = "3", "4" = "4", 
                              "5" = "5", "6" = "6", "7" = "7", "8" = "8",
                              "9" = "9", "10" = "10", "11" = "11", "12" = "12",
                              "13" = "13", "14" = "14", "15" = "15", "16" = "16",
                              "17" = "17", "18" = "18", "19" = "19", "20" = "20",
                              "21" = "21", "22" = "22", "23" = "23", "24" = "24",
                              "25" = "25", "26" = "26", "27" = "27", "28" = "28",
                              "29" = "29", "30" = "30", "31" = "31", "32" = "32",
                              "33" = "33", "34" = "34", "35" = "35", "36" = "36",
                              "37" = "37", "38" = "38", "39" = "39", "40" = "40",
                              "41" = "41", "42" = "42", "43" = "43", "44" = "44",
                              "45" = "45", "46" = "46", "47" = "47", "48" = "48",
                              "49" = "49", "50" = "50", "51" = "51", "52" = "52",
                              "53" = "53", "54" = "54", "55" = "55", "56" = "56",
                              "57" = "57", "58" = "58", "59" = "59", "60" = "60",
                              "61" = "61", "62" = "62", "63" = "63", "64" = "64",
                              "65" = "65", "66" = "66", "67" = "67", "68" = "68",
                              "69" = "69", "70" = "70", "71" = "71", "72" = "72",
                              "73" = "73", "75" = "75", "76" = "76",
                              "77" = "77", "78" = "78", "79" = "79", "80" = "80",
                              "81" = "81", "82" = "82", "83" = "83", "84" = "84",
                              "85" = "85", "86" = "86", "87" = "87", "88" = "88",
                              "89" = "89", "90" = "90", "91" = "91", "92" = "92"),
                  selected = ""),
      
      # Select Variables (collapsible)
      tags$details(id = "details_select_vars",
        tags$summary(style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9; margin-top: 15px;", 
                    h4(style = "display: inline; margin: 0;", "Select Variables")),
        div(style = "margin-left: 15px; margin-top: 10px;",
          uiOutput("variable_selectors")
        )
      ),
      
      # Assumption Checks Section (collapsible)
      tags$details(id = "details_assumption_checks",
        tags$summary(style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9; margin-top: 15px;", 
                    h4(style = "display: inline; margin: 0;", "Assumption Checks")),
        div(style = "margin-left: 15px; margin-top: 10px;",
          # Outlier detection settings
          conditionalPanel(
            condition = "output.outcome_is_continuous === true",
            h5("Outlier Detection (Continuous Outcomes)"),
            numericInput("residual_threshold", "Standardized Residual Threshold", 
                        value = 2, min = 1, max = 10, step = 0.1),
            p(em("Cases with |standardized residual| > threshold will be identified as outliers"))
          ),
          conditionalPanel(
            condition = "output.outcome_is_continuous === false",
            h5("Influential Case Detection (Binary Outcomes)"),
            radioButtons("cooks_threshold_type", "Cook's Distance Threshold:",
              choices = list(
                "Conservative (4/n)" = "conservative",
                "Liberal (1.0)" = "liberal",
                "Custom" = "custom"
              ),
              selected = "conservative"
            ),
            conditionalPanel(
              condition = "input.cooks_threshold_type == 'custom'",
              numericInput("cooks_threshold_custom", "Custom Cook's Distance Threshold", 
                          value = 0.01, min = 0, max = 1, step = 0.001)
            ),
            p(em("Cases with Cook's distance > threshold will be identified as influential"))
          ),
          p(em("Note: Additional assumption check features (e.g., univariate outlier analysis) may be added here in future updates."))
        )
      ),
      
      # PROCESS Options - Organized in collapsible sections
      conditionalPanel(
        condition = "input.process_model != ''",
        h4("PROCESS Options"),
        
        # Centering Options (collapsible)
        tags$details(id = "details_basic_options",
          tags$summary(style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9;", 
                      "Centering Options"),
          div(style = "margin-left: 15px; margin-top: 10px;",
            radioButtons("centering", "Mean Centering:",
              choices = list(
                "No centering" = "0",
                "All variables that define products" = "1",
                "Only continuous variables that define products" = "2"
              ),
              selected = "0"
            )
          )
        ),
        
        # Bootstrap Settings (collapsible)
        tags$details(id = "details_bootstrap_options",
          tags$summary(style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9; margin-top: 15px;", 
                      "Bootstrap Settings"),
          div(style = "margin-left: 15px; margin-top: 10px;",
            checkboxInput("use_bootstrap", "Use bootstrapping", TRUE),
            conditionalPanel(
              condition = "input.use_bootstrap == true",
              numericInput("boot_samples", "Number of bootstrap samples:", 5000, min = 1000, max = 10000),
              radioButtons("bootstrap_ci_method", "Bootstrap Confidence Interval Method:",
                choices = list(
                  "Percentile bootstrap" = "0",
                  "Bias-corrected bootstrap" = "1"
                ),
                selected = "0"
              )
            ),
            numericInput("conf_level", "Confidence Level (%)", 95, min = 80, max = 99, step = 1),
            numericInput("seed", "Random Seed (optional)", value = NA, min = 1, max = 999999),
            p(em("Leave blank to use a random seed. Enter a number (1-999999) to set a specific seed for reproducibility."))
          )
        ),
        
        # Advanced Options (collapsible)
        tags$details(id = "details_advanced_options",
          tags$summary(style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9; margin-top: 15px;", 
                      "Advanced Options"),
          div(style = "margin-left: 15px; margin-top: 10px;",
            # NOTE: Cluster-robust standard errors removed from UI because they require a clustering variable
            # (PROCESS requires cluster parameter when robustse=1). To implement in future:
            # 1. Add clustering variable selector in UI
            # 2. Pass cluster variable to PROCESS: cluster = input$cluster_var
            # 3. Set robustse=1 and hc=5 when cluster-robust is selected
            selectInput("hc_method", "Standard Errors:",
              choices = list(
                "OLS" = "none",
                "HC0 (Huber-White)" = "0",
                "HC1 (Hinkley)" = "1",
                "HC2" = "2",
                "HC3 (Davidson-MacKinnon)" = "3",
                "HC4 (Cribari-Neto)" = "4"
              ),
              selected = "none"
            ),
            tags$div(title = "Requests standardized regression coefficients. Useful for comparing effects across variables with different scales.",
              checkboxInput("stand", "Standardized coefficients", FALSE)
            ),
            tags$div(title = "Uses normal theory (z-tests) instead of t-tests for significance testing. Less conservative than t-tests but assumes large sample sizes.",
              checkboxInput("normal", "Normal theory tests", FALSE)
            ),
            conditionalPanel(
              condition = "output.is_mediation_model === true && output.mediator_count > 1",
              tags$div(title = "Compares indirect effects to determine which mediators are most important. Only available when multiple mediators are selected.",
                checkboxInput("pairwise_contrasts", "Pairwise contrasts of indirect effects", FALSE)
              )
            ),
            # Note: Johnson-Neyman and conditioning values moved to "Probing Moderation" section
            # Note: Seed moved to "Bootstrap Settings" section
          )
        ),
        
        # Output Options (collapsible)
        tags$details(id = "details_output_options",
          tags$summary(style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9; margin-top: 15px;", 
                      "Output Options"),
          div(style = "margin-left: 15px; margin-top: 10px;",
            numericInput("decimals", "Decimal Places", 4, min = 0, max = 10, step = 1),
            tags$div(title = "Displays descriptive statistics (means, SDs, min, max) and correlation matrices for all variables in the analysis.",
              checkboxInput("describe", "Descriptives and variable correlations", TRUE)
            ),
            tags$div(title = "Shows the covariance matrix of regression parameter estimates. Useful for understanding relationships between coefficients and for advanced analyses.",
              checkboxInput("covcoeff", "Show regression coefficient covariance matrix", FALSE)
            ),
            tags$div(title = "Provides scale-free measures including partial correlations, semi-partial correlations, and standardized coefficients. Useful for comparing effects across variables with different scales.",
              checkboxInput("effsize", "Scale-free measures of (partial) association", FALSE)
            ),
            tags$div(title = "Lists all cases that were excluded from analysis due to missing data on any variable. Helps identify data quality issues.",
              checkboxInput("listmiss", "List cases deleted due to missing data", FALSE)
            ),
            tags$div(title = "Displays sums of squares (regression, residual, total), degrees of freedom, mean squares, and adjusted R-squared. Provides detailed model fit information.",
              checkboxInput("ssquares", "Sums of squares and adjusted R-squared", FALSE)
            ),
            tags$div(title = "Shows shrunken R estimates, which are cross-validated R-squared values that adjust for overfitting. These estimates indicate how well the model would perform on new data. Only available for continuous outcomes with multiple mediators.",
              checkboxInput("modelres", "Shrunken R estimates", FALSE)
            ),
            tags$div(title = "Provides comprehensive regression diagnostics including residual analysis, influential cases, multicollinearity (VIF), and assumption tests. Essential for evaluating model quality.",
              checkboxInput("diagnose", "Model diagnostics and assumptions", FALSE)
            ),
            conditionalPanel(
              condition = "output.is_mediation_model === true && input.process_model != '74'",
              tags$div(title = "Tests whether X*M interaction terms are significant. Determines if the effect of mediators (M) on the outcome (Y) depends on the level of X, without changing the model structure. For Model 4, use this to decide whether to enable 'Allow X by M interaction'. Mutually exclusive with 'Allow X by M interaction' for Model 4.",
                checkboxInput("xmtest", "Test for X by M interaction", FALSE)
              )
            ),
            conditionalPanel(
              condition = "input.process_model == '4'",
              tags$div(title = "Includes the X*M interaction term in Model 4, allowing the effect of the mediator (M) on the outcome (Y) to depend on the level of X. When enabled, PROCESS automatically converts Model 4 to Model 74 internally, using the predictor variable (X) as the moderator variable (W). This changes the model to a counterfactual framework with different effect interpretations (natural direct and indirect effects). Mutually exclusive with 'Test for X by M interaction'.",
                checkboxInput("xmint", "Allow X by M interaction (model 4 only)", FALSE)
              )
            ),
            conditionalPanel(
              condition = "output.is_mediation_model === true && (input.process_model == '4' || input.process_model == '6' || input.process_model == '80' || input.process_model == '81' || input.process_model == '82')",
              tags$div(title = "Shows the total effect of X on Y (direct + indirect effects combined). Useful for understanding the overall relationship before examining mediation pathways.",
                checkboxInput("total", "Total effect of X", FALSE)
              )
            ),
            tags$div(title = "Displays model definition matrices showing which paths are estimated and which variables moderate which paths. Useful for understanding the model structure.",
              checkboxInput("matrices", "Matrices output", FALSE)
            ),
            tags$div(title = "When checked, covariates are excluded from the outcome (Y) equation. This option affects model specification but does not produce a separate covariance matrix output. For Model 1, this option works around a PROCESS limitation by not passing covariates when enabled.",
              checkboxInput("covmy", "Exclude covariates from Y equation", FALSE)
            )
          )
        ),
        
        # Probing Moderation Options (collapsible) - Only for moderation models
        conditionalPanel(
          condition = "output.is_moderation_model === true",
          tags$details(id = "details_probing_moderation",
            tags$summary(style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9; margin-top: 15px;", 
                        "Probing Moderation"),
            div(style = "margin-left: 15px; margin-top: 10px;",
              checkboxInput("probe_interactions", "Probe interactions", FALSE),
              conditionalPanel(
                condition = "input.probe_interactions == true",
                textInput("probe_threshold", "When to probe:", value = "p < .10"),
                p(em("Enter threshold for probing (e.g., 'p < .10' or 'p < .05')")),
                radioButtons("conditioning_values", "Values for nondiscrete moderators:",
                  choices = list(
                    "Percentiles (16th, 50th, 84th)" = "1",
                    "Moments (Mean and ±1 SD)" = "0"
                  ),
                  selected = "1"
                ),
                checkboxInput("jn", "Johnson-Neyman technique", FALSE)
              )
            )
          )
        ),
        
        # Visualizing Moderation Options (collapsible) - Only for moderation models
        conditionalPanel(
          condition = "output.is_plot_model === true",
          tags$details(
            tags$summary(style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9; margin-top: 15px;", 
                        "Plot Options"),
            div(style = "margin-left: 15px; margin-top: 10px;",
              h5("Simple Slopes Plot Settings"),
              textInput("slopes_title", "Plot Title", "Simple Slopes Plot"),
              checkboxInput("use_color_lines", "Use color for lines", value = TRUE),
              checkboxInput("custom_y_axis", "Customize y-axis range", value = FALSE),
              conditionalPanel(
                condition = "input.custom_y_axis == true",
                numericInput("y_axis_min", "Y-axis minimum", value = 0),
                numericInput("y_axis_max", "Y-axis maximum", value = 100)
              ),
              textInput("x_label", "Label for Predictor", ""),
              textInput("y_label", "Label for Outcome", ""),
              textInput("moderator_label", "Label for Moderator", ""),
              conditionalPanel(
                condition = "output.has_second_moderator === true",
                textInput("moderator2_label", "Label for Second Moderator (Z)", "")
              ),
              numericInput("decimal_places", "Decimal Places for Moderator Levels", 2, min = 0, max = 5),
              checkboxInput("show_confidence_intervals", "Show confidence intervals", value = TRUE)
            )
          )
        )
      ),
      
      # Run Analysis buttons
      conditionalPanel(
        condition = "input.process_model != ''",
        h4("Run Analysis"),
        div(style = "margin-bottom: 10px; width: 100%;",
          actionButton("run_analysis", "With Original Dataset", 
            class = "btn-primary",
            style = "width: 100%;"
          )
        ),
        div(style = "margin-bottom: 20px; width: 100%;",
          uiOutput("outlier_removal_button")
        ),
        
        h4("Download Options"),
        div(style = "margin-top: 10px;",
          tags$div(title = "Download analysis results as an HTML file. Contains all output from the PROCESS analysis.",
            downloadButton("download_results", "Results output (html)", class = "btn-success", style = "background-color: #90EE90; border-color: #90EE90; color: #000; margin-bottom: 10px;")
          )
        ),
        conditionalPanel(
          condition = "input.run_analysis_no_outliers > input.run_analysis",
          h4("Download Reduced Dataset"),
          radioButtons("filtered_data_format", "Format:",
            choices = list(
              "CSV (.csv)" = "csv",
              "SPSS (.sav)" = "sav"
            ),
            selected = "csv",
            inline = TRUE
          ),
          downloadButton("download_filtered_data", "Dataset Without Outliers",
            class = "btn-warning")
        )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(id = "tabset_panel",
        tabPanel("Assumption Checks",
          div(style = "margin-bottom: 20px;",
            h4("Detailed Assumption Check Results"),
            conditionalPanel(
              condition = "output.outcome_is_selected",
              div(style = "margin-top: 10px;",
                downloadButton("download_assumptions", "Download Assumption Checks (html)", 
                  class = "btn-success", 
                  style = "background-color: #90EE90; border-color: #90EE90; color: #000;")
              )
            )
          ),
          
          # Show validation message when required variables are missing
          conditionalPanel(
            condition = "!output.all_vars_selected_for_assumptions",
            htmlOutput("assumption_validation_message")
          ),
          
          # Show all assumption check content only when all required variables are selected
          conditionalPanel(
            condition = "output.all_vars_selected_for_assumptions",
            tagList(
              # Blue and Yellow info boxes (appear at top, before Standardized Residual Outliers)
              htmlOutput("assumption_info_boxes"),
              
              # Continuous outcome guidance
              conditionalPanel(
                condition = "output.outcome_is_continuous === true",
                div(style = "margin-bottom: 20px",
                  p(strong("Standardized Residual Outliers"), style = "font-weight: bold; font-size: 1.1em;"),
                  p("Standardized residuals (SR) represent how many standard deviations an observed value deviates from the model's prediction. Outliers can affect analyses in several ways:",
                    tags$ul(
                      tags$li(strong("Impact on Results:"), " Outliers can inflate or deflate effects and influence statistical significance"),
                      tags$li(strong("Threshold Guidelines:"), " Common cutoffs include:"),
                      tags$ul(
                        tags$li("|SR| > 2: Potentially influential cases"),
                        tags$li("|SR| > 2.5: More stringent criterion"),
                        tags$li("|SR| > 3: Very conservative criterion")
                      ),
                      tags$li(strong("Handling Outliers:"), " This program offers two approaches:"),
                      tags$ul(
                        tags$li("Run analysis with all cases to maintain complete data"),
                        tags$li("Remove cases above the threshold to assess impact on results")
                      )
                    )
                  )
                )
              ),
              
              # Binary outcome guidance
              conditionalPanel(
                condition = "output.outcome_is_continuous === false",
                div(style = "margin-bottom: 20px",
                  p(strong("Note:"), " For binary outcomes (0/1), the app uses logistic regression diagnostics. Normality and homoscedasticity assumptions do not apply.")
                ),
                div(style = "margin-bottom: 20px",
                  h5("Understanding Influential Cases (Cook's Distance)"),
                  p("Cook's distance measures the influence of each case on all parameter estimates. High values suggest influential cases:",
                    tags$ul(
                      tags$li(strong("Impact on Results:"), " Influential cases can materially change coefficient estimates and significance"),
                      tags$li(strong("Threshold Guidelines:"), " Common choices: 4/n (conservative), 1.0 (liberal), or a custom threshold"),
                      tags$li(strong("Handling Influential Cases:"), " This program offers two approaches:"),
                      tags$ul(
                        tags$li("Run analysis with all cases to maintain sample size"),
                        tags$li("Remove cases with Cook's D above the threshold to assess impact")
                      )
                    )
                  )
                )
              ),
              
              htmlOutput("assumption_details"),
              
              h4("Diagnostic Plots"),
              conditionalPanel(
                condition = "output.outcome_is_continuous === true",
                div(style = "margin-bottom: 30px",
                  h5("Normal Q-Q Plot (Outcome Model)"),
                  p("This plot checks if residuals follow a normal distribution. Points should follow the diagonal line closely.",
                    "Deviations at the ends are common and usually not problematic.",
                    "When bootstrapping is used, normality is less crucial as bootstrap methods don't assume normality."),
                  plotOutput("qq_plot", height = "400px", width = "600px")
                )
              ),
              
              div(style = "margin-bottom: 30px",
                h5("Residuals vs Fitted Plot (Outcome Model)"),
                conditionalPanel(
                  condition = "output.outcome_is_continuous === true",
                  p("This plot checks for linearity and homoscedasticity (constant variance).",
                    "Look for:",
                    tags$ul(
                      tags$li("Random scatter around the horizontal line (linearity)"),
                      tags$li("Even spread of points vertically (homoscedasticity)"),
                      tags$li("No clear patterns or curves in the blue line")
                    ),
                    "With bootstrapping, minor violations of homoscedasticity are less concerning.")
                ),
                conditionalPanel(
                  condition = "output.outcome_is_continuous === false",
                  p("For binary outcomes, this plot shows Pearson residuals from logistic regression.",
                    "The patterns will differ from linear regression:",
                    tags$ul(
                      tags$li("Residuals form distinct bands (one for each outcome level)"),
                      tags$li("Heteroscedasticity is expected and not a violation"),
                      tags$li("Focus on identifying potential model misspecification or influential cases")
                    ),
                    "Note: Homoscedasticity is not an assumption of logistic regression.")
                ),
                plotOutput("residual_plot", height = "400px", width = "600px")
              ),
              
              conditionalPanel(
                condition = "output.outcome_is_continuous === true",
                div(style = "margin-bottom: 30px",
                  h5("Scale-Location Plot (Outcome Model)"),
                  p("This plot helps assess if the variance of residuals changes across the range of predicted values.",
                    "Look for:",
                    tags$ul(
                      tags$li("Relatively horizontal blue line"),
                      tags$li("Even spread of points around the line"),
                      tags$li("No clear funnel or fan shapes")
                    ),
                    "When bootstrapping is used, this assumption is relaxed somewhat."),
                  plotOutput("scale_location_plot", height = "400px", width = "600px")
                )
              ),
              
              # Violin plots for continuous variables
              conditionalPanel(
                condition = "output.has_continuous_selected === true",
                h4("Continuous Variable Distributions"),
                p("Distributions are shown for the original dataset and, if cases were removed, the analysis dataset. Only continuous variables are included."),
                plotOutput("violin_plot", height = "400px", width = "700px")
              )
            )
          )
        ),
        tabPanel("Analysis Results",
          conditionalPanel(
            condition = "output.analysis_ready === true",
            h4("Analysis Results"),
            htmlOutput("analysis_output")
          ),
          conditionalPanel(
            condition = "output.analysis_ready === false",
            p("Run an analysis to see results here.")
          )
        ),
        tabPanel("Plots",
          conditionalPanel(
            condition = "output.is_plot_model === true && output.analysis_ready === true",
            # Only show JN plot for Model 1
            conditionalPanel(
              condition = "input.process_model == '1'",
              h4("Johnson-Neyman Plot"),
              plotOutput("jn_plot", height = "500px", width = "800px"),
              br(),
              div(style = "margin-top: 20px;",
                downloadButton("download_jn", "Download JN Plot (JPG)", 
                  class = "btn-success", 
                  style = "background-color: #90EE90; border-color: #90EE90; color: #000;")
              ),
              br(), br()
            ),
            h4("Simple Slopes Plot"),
            plotOutput("slopes_plot", height = "500px", width = "800px"),
            br(),
            div(style = "margin-top: 20px;",
              uiOutput("download_slopes_button")
            )
          ),
          conditionalPanel(
            condition = "output.is_plot_model === false",
            p("Plots are only available for Models 1 and 3.")
          ),
          conditionalPanel(
            condition = "output.is_plot_model === true && output.analysis_ready === false",
            p("Run an analysis to see plots here.")
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
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
    mediator_count_last_model = NULL  # Track last model to detect when to reset mediator_count
  )
  
  # Load PROCESS function
  source("process.R", local = TRUE)
  
  # DEBUG: Observer to track button clicks
  observeEvent(input$run_analysis, {
    print("DEBUG: Run Analysis button clicked!")
    print(paste("DEBUG: Button click count:", input$run_analysis))
  })
  
  observeEvent(input$run_analysis_no_outliers, {
    print("DEBUG: Run Analysis (No Outliers) button clicked!")
    print(paste("DEBUG: Button click count:", input$run_analysis_no_outliers))
  })
  
  # Update dataset handling
  observeEvent(input$data_file, {
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$datapath)
    
    if (ext == "sav") {
      data <- read_sav(input$data_file$datapath)
    } else if (ext == "csv") {
      data <- read.csv(input$data_file$datapath)
    } else {
      stop("Invalid file type. Please upload a CSV or SAV file.")
    }
    
    # Set clearing flag to prevent observers from repopulating and validation from running
    isolate({
      rv$is_clearing <- TRUE
      rv$mediator_order <- NULL
    })
    
    rv$original_dataset <- data
    rv$current_dataset <- data
    print(paste("DEBUG - Dataset loaded with", nrow(data), "rows"))
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
    print("DEBUG - Cleared all variable selections after dataset upload")
  })
  
  # Clear analysis results and all variables when model changes
  observeEvent(input$process_model, {
    print("DEBUG: ===== MODEL CHANGE OBSERVER STARTED =====")
    print(paste("DEBUG: New model number:", input$process_model))
    print(paste("DEBUG: Previous model number:", rv$previous_model))
    
    # DEBUG: Print current values BEFORE clearing
    print("DEBUG: Values BEFORE clearing:")
    print(paste("  predictor_var:", if(is.null(input$predictor_var) || input$predictor_var == "") "EMPTY" else input$predictor_var))
    print(paste("  outcome_var:", if(is.null(input$outcome_var) || input$outcome_var == "") "EMPTY" else input$outcome_var))
    print(paste("  moderator_var:", if(is.null(input$moderator_var) || input$moderator_var == "") "EMPTY" else input$moderator_var))
    print(paste("  moderator2_var:", if(is.null(input$moderator2_var) || input$moderator2_var == "") "EMPTY" else input$moderator2_var))
    print(paste("  mediator_count:", if(is.null(input$mediator_count) || input$mediator_count == "") "EMPTY" else input$mediator_count))
    print(paste("  mediator_vars_collected:", if(is.null(mediator_vars_collected()) || length(mediator_vars_collected()) == 0) "EMPTY" else paste(mediator_vars_collected(), collapse=", ")))
    print(paste("  covariates:", if(is.null(input$covariates) || length(input$covariates) == 0) "EMPTY" else paste(input$covariates, collapse=", ")))
    print(paste("  rv$mediator_order:", if(is.null(rv$mediator_order) || length(rv$mediator_order) == 0) "NULL/EMPTY" else paste(rv$mediator_order, collapse=", ")))
    
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
    
    print("DEBUG - Model changed, resetting all variables to initial state")
    
    # Clear all reactive values first
    isolate({
      rv$mediator_order <- NULL
      rv$validation_error <- NULL
    })
    
    # CRITICAL: Now that ALL inputs are always rendered (just disabled when not relevant),
    # we can simply use updateSelectInput to clear all of them - it will always work!
    if(!is.null(rv$original_dataset)) {
      vars <- names(rv$original_dataset)
      
      # Clear all variable inputs - they all exist in DOM now, so this will always work
      updateSelectInput(session, "predictor_var", choices = c("Select variable" = "", vars), selected = "")
      updateSelectInput(session, "outcome_var", choices = c("Select variable" = "", vars), selected = "")
      updateSelectInput(session, "moderator_var", choices = c("Select variable" = "", vars), selected = "")
      updateSelectInput(session, "moderator2_var", choices = c("Select variable" = "", vars), selected = "")
      updateSelectInput(session, "covariates", choices = vars, selected = NULL)
      
      # Clear mediator count and all individual mediator selects (M1, M2, M3, etc.)
      updateSelectInput(session, "mediator_count", selected = "")
      for(i in 1:10) {  # Clear up to 10 (max for Model 4)
        updateSelectInput(session, paste0("mediator_m", i), choices = c("Select variable" = "", vars), selected = "")
      }
      
      print("DEBUG: All variable inputs cleared via updateSelectInput (all inputs exist in DOM)")
    }
    
    # Analysis results already cleared above in the first isolate block
    # This ensures old results don't persist when model changes
    
    print("DEBUG - All clearing methods attempted")
    print("DEBUG: ===== MODEL CHANGE OBSERVER COMPLETED =====")
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
        print("DEBUG - Cleared is_clearing flag")
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
      print("DEBUG: Real-time validation skipped - is_clearing is TRUE")
      return()
    }
    
    # Get current model to determine which inputs are actually in use
    current_model <- if(!is.null(input$process_model) && input$process_model != "") {
      as.numeric(input$process_model)
    } else {
      NULL
    }
    
    # DEBUG: Print all current input values
    print("DEBUG: ===== Real-time validation check =====")
    print(paste("DEBUG: Current model:", if(is.null(current_model)) "NULL" else current_model))
    print(paste("DEBUG: predictor_var:", if(is.null(input$predictor_var) || input$predictor_var == "") "EMPTY" else input$predictor_var))
    print(paste("DEBUG: outcome_var:", if(is.null(input$outcome_var) || input$outcome_var == "") "EMPTY" else input$outcome_var))
    print(paste("DEBUG: moderator_var:", if(is.null(input$moderator_var) || input$moderator_var == "") "EMPTY" else input$moderator_var))
    print(paste("DEBUG: moderator2_var:", if(is.null(input$moderator2_var) || input$moderator2_var == "") "EMPTY" else input$moderator2_var))
    mediator_vars_current <- mediator_vars_collected()
    print(paste("DEBUG: mediator_count:", if(is.null(input$mediator_count) || input$mediator_count == "") "EMPTY" else input$mediator_count))
    print(paste("DEBUG: mediator_vars_collected:", if(is.null(mediator_vars_current) || length(mediator_vars_current) == 0) "EMPTY" else paste(mediator_vars_current, collapse=", ")))
    print(paste("DEBUG: covariates:", if(is.null(input$covariates) || length(input$covariates) == 0) "EMPTY" else paste(input$covariates, collapse=", ")))
    
    # Collect all selected variables - but only check enabled inputs
    # Disabled inputs can't be changed by user, so we only validate enabled ones
    models_with_moderator <- c(1, 5, 14, 15, 58, 59, 74, 83:92)
    models_with_second_moderator <- c(2, 3)
    models_with_moderators_disabled <- c(4, 6, 80:82)
    
    all_vars <- character(0)
    
    # Always include predictor and outcome (all models use these)
    if(!is.null(input$predictor_var) && input$predictor_var != "") {
      all_vars <- c(all_vars, input$predictor_var)
    }
    if(!is.null(input$outcome_var) && input$outcome_var != "") {
      all_vars <- c(all_vars, input$outcome_var)
    }
    
    # Only include moderator_var if current model uses moderators (and it's enabled)
    if(!is.null(current_model) && 
       (current_model %in% models_with_moderator || current_model %in% models_with_second_moderator) &&
       !(current_model %in% models_with_moderators_disabled)) {
      if(!is.null(input$moderator_var) && input$moderator_var != "") {
        all_vars <- c(all_vars, input$moderator_var)
      }
    }
    
    # Only include moderator2_var if current model uses second moderator (and it's enabled)
    if(!is.null(current_model) && current_model %in% models_with_second_moderator) {
      if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
        all_vars <- c(all_vars, input$moderator2_var)
      }
    }
    
    # Only include mediators if current model uses mediators (models 4-92)
    # Use mediator_vars_collected() - collects M1, M2, M3... in order
    if(!is.null(current_model) && current_model >= 4 && current_model <= 92) {
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        all_vars <- c(all_vars, mediator_vars_current)
      }
    }
    
    # Always include covariates (all models can use them)
    if(!is.null(input$covariates) && length(input$covariates) > 0) {
      all_vars <- c(all_vars, input$covariates)
    }
    
    print(paste("DEBUG: All collected variables (only enabled inputs):", paste(all_vars, collapse=", ")))
    print(paste("DEBUG: Unique variables:", paste(unique(all_vars), collapse=", ")))
    print(paste("DEBUG: Length all_vars:", length(all_vars), "Length unique:", length(unique(all_vars))))
    
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
          print("DEBUG: Model 74 - X=W is allowed, clearing duplicate error")
          rv$validation_error <- NULL
        } else {
          # There are other duplicates beyond X=W
          print(paste("DEBUG: DUPLICATES FOUND:", paste(unique(duplicate_vars), collapse=", ")))
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
        print(paste("DEBUG: DUPLICATES FOUND:", paste(unique(duplicate_vars), collapse=", ")))
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
      print("DEBUG: No duplicates found - clearing validation error")
      rv$validation_error <- NULL
    }
    print("DEBUG: ===== End real-time validation check =====")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Clear analysis results when key variables change (to prevent stale data)
  observeEvent(c(input$outcome_var, input$predictor_var, input$moderator_var, input$moderator2_var), {
    # Only clear if we have existing results (to avoid clearing on initial load)
    if(!is.null(rv$analysis_results)) {
      rv$analysis_results <- NULL
      print("DEBUG - Key variables changed, clearing analysis results")
    }
  })
  
  # Model description removed - users should refer to Hayes' book for model diagrams
  
  # Determine if model is moderation or mediation type
  output$is_moderation_model <- reactive({
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    # Models 1, 2, 3, 5, 14, 15, 58, 59, 74 are moderation models
    # Model 4 is mediation, not moderation
    model_num %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74)
  })
  outputOptions(output, "is_moderation_model", suspendWhenHidden = FALSE)
  
  # Output to track if model has second moderator (Z)
  output$has_second_moderator <- reactive({
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    model_num %in% models_with_second_moderator
  })
  outputOptions(output, "has_second_moderator", suspendWhenHidden = FALSE)
  
  # Output to track if model supports plots (only Models 1 and 3)
  output$is_plot_model <- reactive({
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    model_num %in% c(1, 3)
  })
  outputOptions(output, "is_plot_model", suspendWhenHidden = FALSE)
  
  output$is_mediation_model <- reactive({
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    # Models 4, 5, 6, 7, 8, 14 are mediation models (Model 5 has both moderator and mediator)
    model_num %in% c(4, 5, 6, 7, 8, 14)
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
    if(model_num < 4 || model_num > 92) {
      return(NULL)  # No mediators for models 1-3
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
    # Check if dataset and model are available
    if(is.null(rv$original_dataset) || is.null(input$process_model) || input$process_model == "") {
      return(NULL)
    }
    
    vars <- names(rv$original_dataset)
    model_num <- as.numeric(input$process_model)
    mediator_enabled <- model_num >= 4 && model_num <= 92
    
    if(!mediator_enabled) {
      return(NULL)
    }
    
    # Determine max mediators based on model
    max_mediators <- if(model_num == 4) {
      10
    } else if(model_num == 6) {
      6
    } else if(model_num == 82) {
      4
    } else if(model_num >= 83 && model_num <= 92) {
      2
    } else {
      10  # Default max for other models
    }
    
    # Create choices for dropdown: "Select..." option + numbers 1 to max_mediators
    count_choices <- c("Select number..." = "", as.character(1:max_mediators))
    names(count_choices)[2:(max_mediators + 1)] <- 1:max_mediators
    
    # Get current count for determining how many M1, M2, M3... selects to show
    current_count <- if(!is.null(input$mediator_count) && input$mediator_count != "" && !is.na(as.numeric(input$mediator_count)) && as.numeric(input$mediator_count) > 0) {
      min(as.integer(input$mediator_count), max_mediators)
    } else {
      0
    }
    
    tagList(
      # Number of mediators dropdown
      selectInput("mediator_count", 
                  "Number of Mediators:", 
                  choices = count_choices,
                  selected = if(current_count > 0) as.character(current_count) else ""),
      
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
  
  # Observer to clear all mediator inputs when mediator_count changes
  # This ensures a clean slate when user changes the number of mediators
  observeEvent(input$mediator_count, {
    # Skip if we're in the middle of clearing (model change)
    if(isTRUE(rv$is_clearing)) {
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
    
    print(paste("DEBUG: Mediator count changed to", new_count, "- all mediator inputs cleared"))
  }, ignoreInit = TRUE)  # ignoreInit = TRUE prevents clearing on initial load
  
  # Define which models require a second moderator (Z)
  # This list can be easily extended by adding model numbers
  # Note: Model 74 requires W = X (moderator must equal predictor), not a second moderator Z
  models_with_second_moderator <- c(2, 3, 9, 10, 58, 59)
  
  # Dynamically generate variable selectors based on model
  output$variable_selectors <- renderUI({
    # Debug output
    print(paste("DEBUG: variable_selectors renderUI called"))
    print(paste("DEBUG: rv$original_dataset is NULL?", is.null(rv$original_dataset)))
    print(paste("DEBUG: input$process_model:", input$process_model))
    
    # Check if dataset and model are available
    if(is.null(rv$original_dataset) || is.null(input$process_model) || input$process_model == "") {
      print("DEBUG: variable_selectors - dataset or model not available, returning NULL")
      return(tags$p("Please load a dataset and select a model number first."))
    }
    
    vars <- names(rv$original_dataset)
    model_num <- as.numeric(input$process_model)
    print(paste("DEBUG: variable_selectors - rendering for model", model_num))
    
    # Determine which inputs should be enabled/disabled for this model
    # Models with one moderator (W): 1, 5, 14, 15, 58, 59, 74, 83-92
    models_with_moderator <- c(1, 5, 14, 15, 58, 59, 74, 83:92)
    # Models with two moderators (W and Z): 2, 3
    models_with_second_moderator <- c(2, 3)
    # Models with moderators disabled: 4, 6, 80-82
    models_with_moderators_disabled <- c(4, 6, 80:82)
    
    # Moderator W is enabled if model uses moderators AND moderators are not disabled for this model
    # Note: Model 74 is not user-selectable (created automatically from Model 4 with xmint)
    moderator_enabled <- (model_num %in% models_with_moderator || model_num %in% models_with_second_moderator) && 
                         !(model_num %in% models_with_moderators_disabled)
    # Moderator Z is enabled only for models with second moderator
    moderator2_enabled <- model_num %in% models_with_second_moderator
    # Mediators are enabled for models 4-92 (disabled only for models 1-3)
    mediator_enabled <- model_num >= 4 && model_num <= 92
    
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
  
  # Output to track if analysis results exist
  output$analysis_ready <- reactive({
    result <- !is.null(rv$analysis_results)
    print(paste("DEBUG: analysis_ready reactive called. Result:", result))
    result
  })
  outputOptions(output, "analysis_ready", suspendWhenHidden = FALSE)
  
  # Reactive to compute Cook's distance threshold
  cooks_threshold_value <- reactive({
    req(rv$original_dataset, input$outcome_var)
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    
    if(!outcome_is_binary) {
      return(NULL)
    }
    
    n <- nrow(rv$original_dataset)
    
    if(input$cooks_threshold_type == "conservative") {
      return(4 / n)
    } else if(input$cooks_threshold_type == "liberal") {
      return(1.0)
    } else {
      return(input$cooks_threshold_custom)
    }
  })
  
  # Function to identify outliers using assumption checks module
  identify_outliers <- reactive({
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    
    identify_outliers_assumption(
      data = rv$original_dataset,
      outcome_var = input$outcome_var,
      predictor_var = input$predictor_var,
      mediator_vars = mediator_vars_collected(),
      moderator_var = input$moderator_var,
      moderator2_var = input$moderator2_var,
      covariates = input$covariates,
      residual_threshold = input$residual_threshold,
      cooks_threshold_type = input$cooks_threshold_type,
      cooks_threshold_custom = input$cooks_threshold_custom
    )
  })
  
  # Outlier summary reactive
  outlier_summary <- reactive({
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    
    tryCatch({
      # Build formula based on model type
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      
      # Add interaction for moderation models (only if moderator is selected)
      if(!is.null(input$moderator_var) && input$moderator_var != "") {
        formula_terms <- c(formula_terms, "*", input$moderator_var)
      }
      
      # Add second moderator interaction if provided
      if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
        formula_terms <- c(formula_terms, "*", input$moderator2_var)
      }
      
      # Add mediators for mediation models
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
      }
      
      # Add covariates
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        # For binary outcomes, use logistic regression diagnostics
        model <- glm(model_formula, data = rv$original_dataset, family = binomial())
        
        leverage <- hatvalues(model)
        cooks_d <- cooks.distance(model)
        
        # Get threshold
        n <- nrow(rv$original_dataset)
        threshold <- if(input$cooks_threshold_type == "conservative") {
          4 / n
        } else if(input$cooks_threshold_type == "liberal") {
          1.0
        } else {
          input$cooks_threshold_custom
        }
        
        # Identify influential cases
        influential_cases <- which(cooks_d > threshold)
        
        if(length(influential_cases) > 0) {
          # Sort by Cook's distance
          sorted_indices <- order(-cooks_d[influential_cases])
          sorted_cases <- influential_cases[sorted_indices]
          sorted_cooks <- cooks_d[sorted_cases]
          sorted_leverage <- leverage[sorted_cases]
          
          case_summaries <- sapply(seq_along(sorted_cases), function(i) {
            sprintf("Case %d: Cook's D = %.4f, Leverage = %.4f", 
                    sorted_cases[i], sorted_cooks[i], sorted_leverage[i])
          })
          
          threshold_label <- if(input$cooks_threshold_type == "conservative") {
            sprintf("4/n = %.4f", threshold)
          } else if(input$cooks_threshold_type == "liberal") {
            "1.0"
          } else {
            sprintf("%.4f", threshold)
          }
          
          return(c(
            "<strong>Influential Case Analysis (Logistic Regression):</strong>",
            "<em>Note: Your outcome variable is binary (0/1). Using leverage and Cook's distance.</em>",
            "<br>",
            sprintf("Cases exceeding threshold (Cook's D > %s):", threshold_label),
            sprintf("Number of cases: %d (%.1f%%)", 
                    length(influential_cases),
                    100 * length(influential_cases) / length(cooks_d)),
            "<br>",
            "<strong>Understanding these metrics:</strong>",
            "<ul>",
            "<li><strong>Cook's Distance:</strong> Measures the influence of each case on all parameter estimates. Values > 4/n (conservative) or > 1.0 (liberal) suggest influential cases.</li>",
            "<li><strong>Leverage:</strong> Measures how unusual a case's predictor values are. High leverage cases can have disproportionate influence on the model.</li>",
            "</ul>",
            "<strong>Top cases (sorted by Cook's D):</strong>",
            paste0('<div style="max-height: 100px; overflow-y: auto; border: 1px solid #ddd; padding: 5px; margin: 5px 0;">', 
                  paste(case_summaries, collapse = "<br>"),
                  '</div>')
          ))
        } else {
          threshold_label <- if(input$cooks_threshold_type == "conservative") {
            sprintf("4/n = %.4f", threshold)
          } else if(input$cooks_threshold_type == "liberal") {
            "1.0"
          } else {
            sprintf("%.4f", threshold)
          }
          
          return(c(
            "<strong>Influential Case Analysis (Logistic Regression):</strong>",
            "<em>Note: Your outcome variable is binary (0/1). Using leverage and Cook's distance.</em>",
            "<br>",
            sprintf("No cases exceed the threshold (Cook's D > %s)", threshold_label)
          ))
        }
      }
      
      # For continuous outcomes, use linear regression
      model <- lm(model_formula, data = rv$original_dataset)
      
      # Get standardized residuals
      sresid <- rstandard(model)
      
      # Find outliers
      outliers <- which(abs(sresid) > input$residual_threshold)
      
      if(length(outliers) > 0) {
        # Sort outliers by absolute value of residuals
        sorted_indices <- order(-abs(sresid[outliers]))
        sorted_cases <- outliers[sorted_indices]
        sorted_resids <- sresid[sorted_cases]
        
        # Create case summaries
        case_summaries <- sapply(seq_along(sorted_cases), function(i) {
          sprintf("Case %d: SR = %.3f", sorted_cases[i], sorted_resids[i])
        })
        
        # Create summary text with scrollable div for cases
        c(
          "<strong>Standardized Residual Analysis:</strong>",
          sprintf("Cases exceeding threshold (|SR| > %.1f):", input$residual_threshold),
          sprintf("Number of cases: %d (%.1f%%)", 
                  length(outliers),
                  100 * length(outliers) / length(sresid)),
          "Top cases (sorted by |SR|):",
          paste0('<div style="max-height: 100px; overflow-y: auto; border: 1px solid #ddd; padding: 5px; margin: 5px 0;">', 
                paste(case_summaries, collapse = "<br>"),
                '</div>')
        )
      } else {
        c(
          "<strong>Standardized Residual Analysis:</strong>",
          sprintf("No cases exceed the threshold (|SR| > %.1f)", input$residual_threshold)
        )
      }
    }, error = function(e) {
      c(
        "<strong>Standardized Residual Analysis:</strong>",
        "Unable to compute residuals. Please check that all variables are numeric.",
        paste("Error details:", e$message)
      )
    })
  })
  
  # Validation message output (shown when required variables are missing)
  output$assumption_validation_message <- renderUI({
    if(is.null(input$process_model) || input$process_model == "" || 
       is.null(rv$original_dataset) || is.null(input$outcome_var) || is.null(input$predictor_var)) {
      return(HTML(""))
    }
    model_num <- as.numeric(input$process_model)
    mediator_vars_current <- mediator_vars_collected()
    validation <- check_required_vars_for_assumptions(
      model_num, input$predictor_var, input$outcome_var,
      input$moderator_var, input$moderator2_var, mediator_vars_current
    )
    
    if(!validation$valid) {
      return(HTML(paste(
        "<div class='alert alert-info' style='font-family: Arial, sans-serif; margin-bottom: 20px;'>",
        "<strong>Please select all required variables:</strong><br>",
        validation$message,
        "<br><br><em>Assumption checks will appear once all required variables for this model are selected.</em>",
        "</div>"
      )))
    }
    return(HTML(""))
  })
  
  # Info boxes output (blue and yellow boxes) - shown when all variables are selected
  output$assumption_info_boxes <- renderUI({
    if(is.null(input$process_model) || input$process_model == "" || 
       is.null(rv$original_dataset) || is.null(input$outcome_var) || is.null(input$predictor_var)) {
      return(HTML(""))
    }
    model_num <- as.numeric(input$process_model)
    mediator_vars_current <- mediator_vars_collected()
    validation <- check_required_vars_for_assumptions(
      model_num, input$predictor_var, input$outcome_var,
      input$moderator_var, input$moderator2_var, mediator_vars_current
    )
    
    if(!validation$valid) {
      return(HTML(""))
    }
    
    HTML(paste(
      "<div style='background-color: #e7f3ff; padding: 10px; margin-bottom: 15px; border-left: 4px solid #2196F3; font-family: Arial, sans-serif;'>",
      "<strong>Note on Assumption Checks:</strong><br>",
      "These assumption checks are always performed on the original dataset. Results update automatically based on your selected variables and standardized residual threshold value. ",
      "These assumption checks examine the <strong>outcome model</strong> (Y ~ X + M + W*X + covariates) only. ",
      "Mediator equations (e.g., M ~ X) are not checked here but may be examined separately if needed. ",
      "This approach is standard practice in mediation analysis and provides appropriate diagnostic information for the outcome equation.",
      "</div>",
      "<div style='background-color: #fff9e6; padding: 10px; margin-bottom: 15px; border-left: 4px solid #FF9800; font-family: Arial, sans-serif;'>",
      "<strong>Example Reporting Format:</strong><br>",
      "<em>Prior to analysis, we examined assumptions for the outcome model. Standardized residuals were calculated from a regression model predicting [outcome] from [predictor], [mediators], and [covariates]. A Q-Q plot indicated residuals were approximately normally distributed, and a Breusch-Pagan test confirmed homoscedasticity, χ²(df) = X.XX, p = .XX. Variance inflation factors (VIF) for all predictors were below 5, indicating no multicollinearity concerns. [X] cases with standardized residuals > 2.0 were identified as outliers [and removed/retained based on your decision].</em>",
      "</div>"
    ))
  })
  
  # Assumption details output
  output$assumption_details <- renderUI({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$process_model)
    
    tryCatch({
      # Check if all required variables are selected for this model
      model_num <- as.numeric(input$process_model)
      mediator_vars_current <- mediator_vars_collected()
      validation <- check_required_vars_for_assumptions(
        model_num, input$predictor_var, input$outcome_var,
        input$moderator_var, input$moderator2_var, mediator_vars_current
      )
      
      if(!validation$valid) {
        return(HTML(""))
      }
      
      # Build formula based on model type
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      
      # Add interaction for moderation models (only if moderator is selected)
      if(!is.null(input$moderator_var) && input$moderator_var != "") {
        formula_terms <- c(formula_terms, "*", input$moderator_var)
      }
      
      # Add second moderator interaction if provided
      if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
        formula_terms <- c(formula_terms, "*", input$moderator2_var)
      }
      
      # Add mediators for mediation models
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
      }
      
      # Add covariates
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        # For binary outcomes, use logistic regression
        model <- glm(model_formula, data = rv$original_dataset, family = binomial())
        
        # Get outlier/influential summary
        outlier_text <- paste(outlier_summary(), collapse = "<br>")
        
        # Build filtered data after removal
        outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
        filtered_data <- rv$original_dataset
        if(!is.null(outliers) && length(outliers$cases) > 0) {
          filtered_data <- rv$original_dataset[-outliers$cases, ]
        }
        
        # Binary counts
        bin_counts <- c(
          binary_count_lines(rv$original_dataset, input$outcome_var, "Outcome (original)"),
          binary_count_lines(rv$original_dataset, input$predictor_var, "Predictor (original)")
        )
        if(!is.null(input$moderator_var)) {
          bin_counts <- c(bin_counts,
            binary_count_lines(rv$original_dataset, input$moderator_var, "Moderator (original)"))
        }
        mediator_vars_current <- mediator_vars_collected()
        if(!is.null(mediator_vars_current)) {
          for(med in mediator_vars_current) {
            bin_counts <- c(bin_counts,
              binary_count_lines(rv$original_dataset, med, paste0("Mediator ", med, " (original)")))
          }
        }
        
        # For binary outcomes, skip normality and homoscedasticity tests
        diagnostics <- diagnostic_report(model)
        
        output_text <- paste(
          "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
          "<strong>Note: Binary Outcome Detected</strong><br>",
          "<em>Your outcome variable is binary (0/1). PROCESS will use logistic regression for this analysis.</em><br><br>",
          "<strong>Important:</strong> Standard regression assumptions (normality, homoscedasticity) do not apply to logistic regression.<br>",
          "For binary outcomes, different diagnostic approaches are needed:<br>",
          "<ul>",
          "<li><strong>Linearity:</strong> Check linearity of continuous predictors with the logit of the outcome</li>",
          "<li><strong>Influential observations:</strong> Review leverage values and Cook's distance in the outlier summary above</li>",
          "<li><strong>Model fit:</strong> Use pseudo-R² measures (McFadden, Cox-Snell, Nagelkerke) shown in PROCESS output</li>",
          "<li><strong>Multicollinearity:</strong> VIF can still be calculated for predictors</li>",
          "</ul><br>",
          if(length(na.omit(bin_counts)) > 0) {
            paste(
              "<strong>Binary Variable Counts (original dataset):</strong><br>",
              paste(na.omit(bin_counts), collapse = "<br>"),
              "<br><br>"
            )
          } else { "" },
          outlier_text,
          "<br><br>",
          "<strong>Additional Diagnostics:</strong><br>",
          paste(diagnostics, collapse = "<br>"),
          "<br><em>Note: VIF calculated for all predictors. For binary outcomes, focus on model fit statistics and residual patterns rather than normality/homoscedasticity.</em>",
          "</div>",
          sep = ""
        )
      } else {
        # For continuous outcomes, use linear regression
        model <- lm(model_formula, data = rv$original_dataset)
        
        # Get outlier summary
        outlier_text <- paste(outlier_summary(), collapse = "<br>")
        
        # Build filtered data after removal
        outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
        filtered_data <- rv$original_dataset
        if(!is.null(outliers) && length(outliers$cases) > 0) {
          filtered_data <- rv$original_dataset[-outliers$cases, ]
        }
        
        # Binary counts (only if binary)
        bin_counts <- c(
          binary_count_lines(rv$original_dataset, input$outcome_var, "Outcome (original)"),
          binary_count_lines(rv$original_dataset, input$predictor_var, "Predictor (original)")
        )
        if(!is.null(input$moderator_var)) {
          bin_counts <- c(bin_counts,
            binary_count_lines(rv$original_dataset, input$moderator_var, "Moderator (original)"))
        }
        mediator_vars_current <- mediator_vars_collected()
        if(!is.null(mediator_vars_current)) {
          for(med in mediator_vars_current) {
            bin_counts <- c(bin_counts,
              binary_count_lines(rv$original_dataset, med, paste0("Mediator ", med, " (original)")))
          }
        }
        
        # Run other diagnostics
        normality <- check_normality(model)
        homoscedasticity <- test_homoscedasticity(model)
        diagnostics <- diagnostic_report(model)
        
        # Create final output
        output_text <- paste(
          "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
          if(length(na.omit(bin_counts)) > 0) {
            paste(
              "<strong>Binary Variable Counts (original dataset):</strong><br>",
              paste(na.omit(bin_counts), collapse = "<br>"),
              "<br><br>"
            )
          } else { "" },
          outlier_text,
          "<br><br>",
          "<strong>Normality Test:</strong><br>",
          normality$text,
          "<br><em>Interpretation: A significant p-value (< .05) suggests non-normality. ",
          "However, with large samples, minor deviations often become significant. ",
          "Visual inspection of the Q-Q plot is often more informative.</em>",
          "<br><br>",
          "<strong>Homoscedasticity Test:</strong><br>",
          homoscedasticity,
          "<br><em>Interpretation: A significant p-value suggests non-constant variance. ",
          "Consider the Residuals vs Fitted plot for visual confirmation.</em>",
          "<br><br>",
          "<strong>Additional Diagnostics:</strong><br>",
          paste(diagnostics, collapse = "<br>"),
          "<br><em>Interpretation:<br>",
          "- VIF > 5 suggests potential multicollinearity issues<br>",
          "- With bootstrapping, these diagnostics become less crucial as bootstrap methods are more robust to violations</em>",
          "</div>",
          sep = ""
        )
      }
      
      HTML(output_text)
      
    }, error = function(e) {
      print("Error in assumption checks:")
      print(e$message)
      return(HTML(paste(
        "<div class='alert alert-danger'>",
        "Error in assumption checks: ", e$message,
        "</div>"
      )))
    })
  })
  
  # Diagnostic plots
  output$qq_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$process_model)
    
    tryCatch({
      # Check if all required variables are selected for this model
      model_num <- as.numeric(input$process_model)
      mediator_vars_current <- mediator_vars_collected()
      validation <- check_required_vars_for_assumptions(
        model_num, input$predictor_var, input$outcome_var,
        input$moderator_var, input$moderator2_var, mediator_vars_current
      )
      
      if(!validation$valid) {
        plot.new()
        text(0.5, 0.5, "Please select all required\nvariables for this model", cex = 1.2)
        return(NULL)
      }
      
      # Build formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      if(!is.null(input$moderator_var)) {
        formula_terms <- c(formula_terms, "*", input$moderator_var)
      }
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
      }
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        return(NULL)
      } else {
        model <- lm(model_formula, data = rv$original_dataset)
        
        ggplot(data.frame(residuals = rstandard(model)), aes(sample = residuals)) +
          stat_qq() + 
          stat_qq_line() +
          theme_minimal() +
          labs(title = "Normal Q-Q Plot",
               x = "Theoretical Quantiles",
               y = "Sample Quantiles") +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Error generating Q-Q plot", cex = 1.2)
    })
  })
  
  output$residual_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$process_model)
    
    tryCatch({
      # Check if all required variables are selected for this model
      model_num <- as.numeric(input$process_model)
      mediator_vars_current <- mediator_vars_collected()
      validation <- check_required_vars_for_assumptions(
        model_num, input$predictor_var, input$outcome_var,
        input$moderator_var, input$moderator2_var, mediator_vars_current
      )
      
      if(!validation$valid) {
        plot.new()
        text(0.5, 0.5, "Please select all required\nvariables for this model", cex = 1.2)
        return(NULL)
      }
      
      # Build formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      if(!is.null(input$moderator_var)) {
        formula_terms <- c(formula_terms, "*", input$moderator_var)
      }
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
      }
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        model <- glm(model_formula, data = rv$original_dataset, family = binomial())
        fitted_values <- fitted(model)
        pearson_resid <- residuals(model, type = "pearson")
        
        outliers <- identify_outliers()
        is_outlier <- seq_along(fitted_values) %in% outliers$cases
        
        subtitle_text <- if(outliers$count > 0) {
          sprintf("Cook's D > %.4f: %d influential case%s highlighted",
                  outliers$threshold, outliers$count,
                  ifelse(outliers$count == 1, "", "s"))
        } else {
          sprintf("Cook's D ≤ %.4f: No influential cases detected", outliers$threshold)
        }
        
        plot_data <- data.frame(
          fitted = fitted_values,
          residuals = pearson_resid,
          is_outlier = is_outlier
        )
        
        ggplot(plot_data, aes(x = fitted, y = residuals)) +
          geom_point(aes(color = is_outlier), alpha = 0.6) +
          scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme_minimal() +
          labs(title = "Residuals vs Fitted (Logistic Regression)",
               x = "Fitted probabilities",
               y = "Pearson residuals",
               subtitle = subtitle_text,
               color = "Influential (Cook's D)") +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            legend.position = "none",
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
      } else {
        model <- lm(model_formula, data = rv$original_dataset)
        
        fitted_values <- fitted(model)
        residuals <- residuals(model)
        std_resid <- rstandard(model)
        is_outlier <- abs(std_resid) > input$residual_threshold
        
        plot_data <- data.frame(
          fitted = fitted_values,
          residuals = residuals,
          is_outlier = is_outlier
        )
        
        ggplot(plot_data, aes(x = fitted, y = residuals)) +
          geom_point(aes(color = is_outlier), alpha = 0.6) +
          scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme_minimal() +
          labs(title = "Residuals vs Fitted",
               x = "Fitted values",
               y = "Residuals") +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Error generating residuals plot", cex = 1.2)
    })
  })
  
  output$scale_location_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    
    tryCatch({
      # Build formula
      formula_terms <- c(input$outcome_var, "~", input$predictor_var)
      if(!is.null(input$moderator_var)) {
        formula_terms <- c(formula_terms, "*", input$moderator_var)
      }
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
      }
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
      }
      model_formula <- as.formula(paste(formula_terms, collapse = " "))
      
      # Check if outcome is binary
      outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
      
      if(outcome_is_binary) {
        return(NULL)
      } else {
        model <- lm(model_formula, data = rv$original_dataset)
        
        fitted_values <- fitted(model)
        std_residuals <- rstandard(model)
        sqrt_abs_resid <- sqrt(abs(std_residuals))
        is_outlier <- abs(std_residuals) > input$residual_threshold
        
        plot_data <- data.frame(
          fitted = fitted_values,
          sqrt_abs_resid = sqrt_abs_resid,
          is_outlier = is_outlier
        )
        
        ggplot(plot_data, aes(x = fitted, y = sqrt_abs_resid)) +
          geom_point(aes(color = is_outlier), alpha = 0.6) +
          scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          theme_minimal() +
          labs(title = "Scale-Location Plot",
               x = "Fitted values",
               y = expression(sqrt("|Standardized residuals|"))) +
          theme(
            text = element_text(size = 14),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            plot.title = element_text(size = 18, hjust = 0.5),
            legend.position = "none",
            axis.line = element_line(color = "black", linewidth = 0.5),
            axis.ticks = element_line(color = "black", linewidth = 0.5)
          )
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Error generating scale-location plot", cex = 1.2)
    })
  })
  
  output$violin_plot <- renderPlot({
    req(rv$original_dataset, input$outcome_var, input$predictor_var)
    
    tryCatch({
      selected_vars <- c(input$outcome_var, input$predictor_var)
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current)) selected_vars <- c(selected_vars, mediator_vars_current)
      if(!is.null(input$moderator_var)) selected_vars <- c(selected_vars, input$moderator_var)
      
      cont_vars <- selected_vars[vapply(selected_vars, function(v) is_continuous_variable(rv$original_dataset, v), logical(1))]
      
      if(length(cont_vars) == 0) {
        plot.new()
        text(0.5, 0.5, "No continuous variables selected.\nViolin plot not shown.", cex = 1.1)
        return(NULL)
      }
      
      # Build data for original dataset
      orig_long <- do.call(rbind, lapply(cont_vars, function(v) {
        data.frame(
          variable = v,
          value = rv$original_dataset[[v]],
          dataset = "Original",
          stringsAsFactors = FALSE
        )
      }))
      
      # Build analysis dataset by removing identified outliers
      outliers <- tryCatch(identify_outliers(), error = function(e) NULL)
      filtered_data <- rv$original_dataset
      if(!is.null(outliers) && length(outliers$cases) > 0) {
        filtered_data <- rv$original_dataset[-outliers$cases, ]
      }
      
      analysis_long <- do.call(rbind, lapply(cont_vars, function(v) {
        data.frame(
          variable = v,
          value = filtered_data[[v]],
          dataset = "After removal",
          stringsAsFactors = FALSE
        )
      }))
      
      plot_data <- rbind(orig_long, analysis_long)
      plot_data <- plot_data[!is.na(plot_data$value), ]
      plot_data$value <- suppressWarnings(as.numeric(plot_data$value))
      plot_data$variable <- factor(plot_data$variable, levels = cont_vars)
      plot_data$dataset <- factor(plot_data$dataset, levels = c("Original", "After removal"))
      
      if(nrow(plot_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for violin plot.", cex = 1.1)
        return(NULL)
      }
      
      ggplot(plot_data, aes(x = dataset, y = value, fill = dataset)) +
        geom_violin(trim = FALSE, alpha = 0.5, color = NA) +
        geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.6) +
        facet_wrap(~ variable, scales = "free_y", ncol = 1) +
        labs(title = "Continuous Variable Distributions",
             x = "Dataset (Original vs After removal)",
             y = "Value",
             fill = "Dataset") +
        theme_minimal() +
        theme(
          text = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.position = "right",
          strip.text = element_text(size = 14)
        )
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error generating violin plot:\n", e$message), cex = 1.1)
    })
  })
  
  # UI output for outlier removal button
  output$outlier_removal_button <- renderUI({
    req(rv$original_dataset, input$outcome_var)
    
    outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
    
    button_text <- if(outcome_is_binary) {
      "With Cook's Distance Outliers Removed"
    } else {
      "With Standardized Residual Outliers Removed"
    }
    
    # Check if button should be enabled
    button_enabled <- FALSE
    if (!is.null(input$predictor_var) && input$predictor_var != "" && 
        !is.null(input$outcome_var) && input$outcome_var != "") {
      tryCatch({
        outliers <- identify_outliers()
        if(!is.null(outliers) && !is.null(outliers$count) && outliers$count > 0 && 
           !is.null(outliers$cases) && length(outliers$cases) > 0) {
          # Check if removing outliers would leave enough cases
          reduced_data <- rv$original_dataset[-outliers$cases, ]
          all_vars <- c(input$outcome_var, input$predictor_var)
          mediator_vars_current <- mediator_vars_collected()
          if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
            all_vars <- c(all_vars, mediator_vars_current)
          }
          if(!is.null(input$moderator_var) && input$moderator_var != "") {
            all_vars <- c(all_vars, input$moderator_var)
          }
          if(!is.null(input$covariates) && length(input$covariates) > 0) {
            all_vars <- c(all_vars, input$covariates)
          }
          complete_cases <- complete.cases(reduced_data[all_vars])
          n_complete <- sum(complete_cases)
          button_enabled <- (n_complete >= 3)
        }
      }, error = function(e) {
        print(paste("DEBUG: Error checking outliers for button:", e$message))
        button_enabled <- FALSE
      })
    }
    
    actionButton("run_analysis_no_outliers", button_text, 
                class = "btn-warning",
                style = "width: 100%;",
                disabled = !button_enabled)
  })
  
  # Analysis execution - generic function for all PROCESS models
  original_analysis <- eventReactive(input$run_analysis, {
    # Clear any previous validation errors at the start
    rv$validation_error <- NULL
    
    print("DEBUG: ===== original_analysis eventReactive STARTED =====")
    print(paste("DEBUG: rv$is_clearing is TRUE?", isTRUE(rv$is_clearing)))
    
    # Basic validation with detailed debug
    print("DEBUG: Checking basic validation...")
    print(paste("DEBUG: rv$original_dataset is NULL?", is.null(rv$original_dataset)))
    print(paste("DEBUG: input$outcome_var:", input$outcome_var))
    print(paste("DEBUG: input$predictor_var:", input$predictor_var))
    print(paste("DEBUG: input$process_model:", input$process_model))
    
    validate(
      need(rv$original_dataset, "Dataset not loaded"),
      need(input$outcome_var, "Outcome variable not selected"),
      need(input$predictor_var, "Predictor variable not selected"),
      need(input$process_model, "PROCESS model not selected"),
      need(input$outcome_var != input$predictor_var, 
           "Outcome and predictor must be different variables")
    )
    print("DEBUG: Basic validation passed")
    
    # Model-specific validation
    model_num <- as.numeric(input$process_model)
    print(paste("DEBUG: Model number:", model_num))
    
    # Moderation models require moderator (Model 5 handled separately)
    # Models with one moderator: 1, 5, 14, 15, 58, 59, 74, 83-92
    # Models with two moderators: 2, 3
    if(model_num %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74, 83:92)) {
      print("DEBUG: This is a moderation model - checking for moderator")
      print(paste("DEBUG: input$moderator_var:", input$moderator_var))
      print(paste("DEBUG: moderator_var is NULL?", is.null(input$moderator_var)))
      print(paste("DEBUG: moderator_var is empty?", is.null(input$moderator_var) || input$moderator_var == ""))
      validate(
        need(!is.null(input$moderator_var) && input$moderator_var != "", 
             "Moderator variable (W) is required for this model")
      )
      print("DEBUG: Moderator validation passed")
      
      # Models with second moderator require it to be selected
      if(model_num %in% models_with_second_moderator) {
        print("DEBUG: This model requires a second moderator (Z)")
        print(paste("DEBUG: input$moderator2_var:", input$moderator2_var))
        validate(
          need(!is.null(input$moderator2_var) && input$moderator2_var != "", 
               "Second moderator variable (Z) is required for this model")
        )
        print("DEBUG: Second moderator validation passed")
      }
    }
    
    # Models 4-92: All require at least one mediator
    if(model_num >= 4 && model_num <= 92) {
      print("DEBUG: This model requires mediator(s) - checking for mediator(s)")
      mediator_vars_current <- mediator_vars_collected()
      print(paste("DEBUG: mediator_vars_collected:", if(is.null(mediator_vars_current) || length(mediator_vars_current) == 0) "EMPTY" else paste(mediator_vars_current, collapse=", ")))
      validate(
        need(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0, 
             "At least one mediator variable is required for this model")
      )
      
      # Model 4 allows up to 10 mediators
      if(model_num == 4) {
        validate(
          need(length(mediator_vars_current) <= 10,
               "Model 4 allows up to 10 mediators")
        )
      }
      
      # Model 6 requires 2-6 mediators
      if(model_num == 6) {
        validate(
          need(length(mediator_vars_current) >= 2 && length(mediator_vars_current) <= 6,
               "Model 6 requires between 2 and 6 mediators")
        )
      }
      
      # Model 82 requires exactly 4 mediators
      if(model_num == 82) {
        mediator_count <- length(mediator_vars_current)
        if(mediator_count != 4) {
          if(mediator_count < 4) {
            error_msg <- paste0("Model 82 requires exactly 4 mediators. You have selected ", 
                               mediator_count, " mediator(s). Please select ", (4 - mediator_count), 
                               " more mediator(s).")
          } else {
            error_msg <- paste0("Model 82 requires exactly 4 mediators. You have selected ", 
                               mediator_count, " mediators. Please remove ", (mediator_count - 4), 
                               " mediator(s).")
          }
          showNotification(error_msg, type = "error", duration = 10)
          validate(need(FALSE, error_msg))
        }
      }
      
      # Models 83-92 require exactly 2 mediators
      if(model_num >= 83 && model_num <= 92) {
        mediator_count <- length(mediator_vars_current)
        if(mediator_count != 2) {
          if(mediator_count < 2) {
            error_msg <- paste0("Model ", model_num, " requires exactly 2 mediators. You have selected ", 
                               mediator_count, " mediator(s). Please select ", (2 - mediator_count), 
                               " more mediator(s).")
          } else {
            error_msg <- paste0("Model ", model_num, " requires exactly 2 mediators. You have selected ", 
                               mediator_count, " mediators. Please remove ", (mediator_count - 2), 
                               " mediator(s).")
          }
          showNotification(error_msg, type = "error", duration = 10)
          validate(need(FALSE, error_msg))
        }
      }
      print("DEBUG: Mediator validation passed")
    }
    
    # Model 5: First and Second Stage Moderation (requires moderator W and mediator M)
    if(model_num == 5) {
      print("DEBUG: This is Model 5 - checking for moderator W")
      validate(
        need(!is.null(input$moderator_var) && input$moderator_var != "", 
             "Model 5 requires moderator variable W")
      )
      print("DEBUG: Model 5 validation passed")
    }
    
    # Check for validation errors (set by real-time validation observer)
    if(!is.null(rv$validation_error)) {
      showNotification(
        rv$validation_error,
        type = "error",
        duration = 10
      )
      return(NULL)
    }
    
    # Simple duplicate check: collect all selected variables and check for duplicates
    # IMPORTANT: Only check inputs that are relevant to the current model
    # This prevents old values from previous models from causing false duplicate errors
    print("DEBUG: ===== Analysis duplicate check STARTED (original_analysis) =====")
    print(paste("DEBUG: Current model:", model_num))
    print(paste("DEBUG: predictor_var:", if(is.null(input$predictor_var) || input$predictor_var == "") "EMPTY" else input$predictor_var))
    print(paste("DEBUG: outcome_var:", if(is.null(input$outcome_var) || input$outcome_var == "") "EMPTY" else input$outcome_var))
    print(paste("DEBUG: moderator_var:", if(is.null(input$moderator_var) || input$moderator_var == "") "EMPTY" else input$moderator_var))
    print(paste("DEBUG: moderator2_var:", if(is.null(input$moderator2_var) || input$moderator2_var == "") "EMPTY" else input$moderator2_var))
    mediator_vars_current <- mediator_vars_collected()
    print(paste("DEBUG: mediator_count:", if(is.null(input$mediator_count) || input$mediator_count == "") "EMPTY" else input$mediator_count))
    print(paste("DEBUG: mediator_vars_collected:", if(is.null(mediator_vars_current) || length(mediator_vars_current) == 0) "EMPTY" else paste(mediator_vars_current, collapse=", ")))
    print(paste("DEBUG: covariates:", if(is.null(input$covariates) || length(input$covariates) == 0) "EMPTY" else paste(input$covariates, collapse=", ")))
    
    # Determine which inputs are actually used by the current model
    models_with_moderator <- c(1, 5, 14, 15, 58, 59, 74, 83:92)
    models_with_second_moderator <- c(2, 3)
    models_with_moderators_disabled <- c(4, 6, 80:82)
    
    all_vars <- character(0)
    # Always include predictor and outcome
    if(!is.null(input$predictor_var) && input$predictor_var != "") {
      all_vars <- c(all_vars, input$predictor_var)
    }
    if(!is.null(input$outcome_var) && input$outcome_var != "") {
      all_vars <- c(all_vars, input$outcome_var)
    }
    # Only include moderator_var if current model uses moderators (and moderators aren't disabled)
    if((model_num %in% models_with_moderator || model_num %in% models_with_second_moderator) &&
       !(model_num %in% models_with_moderators_disabled)) {
      if(!is.null(input$moderator_var) && input$moderator_var != "") {
        all_vars <- c(all_vars, input$moderator_var)
        print("DEBUG: Including moderator_var (model uses moderators)")
      }
    } else {
      print("DEBUG: Ignoring moderator_var (current model doesn't use moderators or moderators are disabled)")
    }
    # Only include moderator2_var if current model uses second moderator
    if(model_num %in% models_with_second_moderator) {
      if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
        all_vars <- c(all_vars, input$moderator2_var)
        print("DEBUG: Including moderator2_var (model uses second moderator)")
      }
    } else {
      print("DEBUG: Ignoring moderator2_var (current model doesn't use second moderator)")
    }
    # Only include mediators if current model uses mediators (models 4-92)
    if(model_num >= 4 && model_num <= 92) {
      current_mediators <- mediator_vars_collected()
      if(!is.null(current_mediators) && length(current_mediators) > 0) {
        all_vars <- c(all_vars, current_mediators)
        print("DEBUG: Including mediator_vars (model uses mediators)")
      }
    } else {
      print("DEBUG: Ignoring mediator_vars (current model doesn't use mediators or mediators are disabled)")
    }
    # Always include covariates
    if(!is.null(input$covariates) && length(input$covariates) > 0) {
      all_vars <- c(all_vars, input$covariates)
    }
    
    print(paste("DEBUG: All collected variables:", paste(all_vars, collapse=", ")))
    print(paste("DEBUG: Unique variables:", paste(unique(all_vars), collapse=", ")))
    print(paste("DEBUG: Length all_vars:", length(all_vars), "Length unique:", length(unique(all_vars))))
    
    # Final duplicate check (with exception for Model 74 where X and W can be the same)
    if(length(all_vars) > 0 && length(all_vars) != length(unique(all_vars))) {
      duplicate_vars <- all_vars[duplicated(all_vars)]
      
      # Exception: For Model 74, allow predictor_var and moderator_var to be the same
      if(model_num == 74) {
        # Check if the only duplicate is X=W (which is allowed for Model 74)
        if(length(duplicate_vars) == 1 && 
           !is.null(input$predictor_var) && input$predictor_var != "" &&
           !is.null(input$moderator_var) && input$moderator_var != "" &&
           input$predictor_var == input$moderator_var &&
           duplicate_vars[1] == input$predictor_var) {
          # This is the expected X=W for Model 74, so allow it
          print("DEBUG: Model 74 - X=W is allowed, proceeding with analysis")
        } else {
          # There are other duplicates beyond X=W
          print(paste("DEBUG: DUPLICATES FOUND IN ANALYSIS:", paste(unique(duplicate_vars), collapse=", ")))
          error_msg <- paste0("Error: The same variable cannot be used for multiple roles. Variable(s) '", 
                              paste(unique(duplicate_vars), collapse = "', '"), 
                              "' is/are used in more than one role.")
          showNotification(error_msg, type = "error", duration = 10)
          rv$validation_error <- error_msg
          return(NULL)
        }
      } else {
        # Not Model 74, so duplicates are not allowed
        print(paste("DEBUG: DUPLICATES FOUND IN ANALYSIS:", paste(unique(duplicate_vars), collapse=", ")))
        error_msg <- paste0("Error: The same variable cannot be used for multiple roles. Variable(s) '", 
                            paste(unique(duplicate_vars), collapse = "', '"), 
                            "' is/are used in more than one role.")
        showNotification(error_msg, type = "error", duration = 10)
        rv$validation_error <- error_msg
        return(NULL)
      }
    }
    print("DEBUG: ===== Analysis duplicate check PASSED (original_analysis) =====")
    
    print("DEBUG: All validation passed, proceeding with req()")
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$process_model)
    print("DEBUG: req() passed, entering withProgress")
    
    withProgress(message = 'Running analysis...', value = 0, {
      print("DEBUG: Inside withProgress - Running analysis with original dataset")
      rv$outliers_info <- NULL
      
      # Build variable list for complete cases check
      print("DEBUG: Building variable list for complete cases check")
      all_vars_orig <- c(input$outcome_var, input$predictor_var)
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        all_vars_orig <- c(all_vars_orig, mediator_vars_current)
      }
      if(!is.null(input$moderator_var) && input$moderator_var != "") {
        all_vars_orig <- c(all_vars_orig, input$moderator_var)
      }
      if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
        all_vars_orig <- c(all_vars_orig, input$moderator2_var)
      }
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        all_vars_orig <- c(all_vars_orig, input$covariates)
      }
      print(paste("DEBUG: Variables for complete cases:", paste(all_vars_orig, collapse=", ")))
      
      # Check complete cases
      print("DEBUG: Checking complete cases...")
      complete_cases_orig <- complete.cases(rv$original_dataset[all_vars_orig])
      n_complete_orig <- sum(complete_cases_orig)
      print(paste("DEBUG: Complete cases:", n_complete_orig, "out of", nrow(rv$original_dataset)))
      
      if(n_complete_orig < 3) {
        print("DEBUG: ERROR - Insufficient complete cases")
        stop(sprintf("Only %d complete cases available. This is insufficient for analysis.", n_complete_orig))
      }
      
      process_data_orig <- rv$original_dataset[complete_cases_orig, ]
      print(paste("DEBUG: Process data prepared with", nrow(process_data_orig), "rows"))
      
      # Store settings
      analysis_settings <- list(
        model = as.numeric(input$process_model),
        centering = input$centering,
        use_bootstrap = input$use_bootstrap,
        boot_samples = if(input$use_bootstrap) input$boot_samples else NULL,
        hc_method = input$hc_method,
        conf_level = input$conf_level,
        dataset_name = tools::file_path_sans_ext(basename(input$data_file$name)),
        original_n = nrow(rv$original_dataset),
        outliers_removed = FALSE,
        outliers_count = 0,
        outliers_threshold = if(is_binary_variable(rv$original_dataset, input$outcome_var)) {
          cooks_threshold_value()
        } else {
          input$residual_threshold
        },
        outliers_method = if(is_binary_variable(rv$original_dataset, input$outcome_var)) {
          "Cook's Distance"
        } else {
          "Standardized Residuals"
        },
        predictor_var = input$predictor_var,
        outcome_var = input$outcome_var,
        mediator_vars = mediator_vars_collected(),
        moderator_var = input$moderator_var,
        moderator2_var = if(!is.null(input$moderator2_var) && input$moderator2_var != "") input$moderator2_var else NULL,
        covariates = if(!is.null(input$covariates) && length(input$covariates) > 0) input$covariates else NULL
      )
      
      # Early validation check for W and Z same variable
      model_num <- as.numeric(input$process_model)
      if(model_num %in% models_with_second_moderator) {
        if(!is.null(input$moderator_var) && !is.null(input$moderator2_var) && 
           input$moderator_var != "" && input$moderator2_var != "" &&
           input$moderator_var == input$moderator2_var) {
          showNotification(
            "Error: W and Z moderators must be different variables. Please select different variables for the first and second moderators.",
            type = "error",
            duration = 10
          )
          rv$validation_error <- "W and Z moderators must be different variables."
          return(NULL)
        } else {
          # Clear validation error if variables are different
          rv$validation_error <- NULL
        }
      } else {
        # Clear validation error for models without second moderator
        rv$validation_error <- NULL
      }
      
      # Build PROCESS arguments
      print("DEBUG: Building PROCESS arguments...")
      
      # Handle xmint option for Model 4 - center must be 0 when xmint is used
      center_value <- as.numeric(input$centering)
      if(model_num == 4 && input$xmint) {
        center_value <- 0  # xmint requires center = 0
      }
      
      process_args <- list(
        data = process_data_orig,
        y = input$outcome_var,
        x = input$predictor_var,
        model = model_num,
        center = center_value,
        conf = input$conf_level,
        modelbt = if(input$use_bootstrap) 1 else 0,
        boot = if(input$use_bootstrap) input$boot_samples else 0,
        bc = if(input$use_bootstrap) as.numeric(input$bootstrap_ci_method) else 0,
        hc = if(input$hc_method == "none") 5 else as.numeric(input$hc_method),
        # NOTE: robustse removed - cluster-robust option requires clustering variable (see comment above)
        covcoeff = if(input$covcoeff) 1 else 0,
        cov = if(!is.null(input$covariates) && length(input$covariates) > 0) input$covariates else "xxxxx"
      )
      
      # Add model-specific variables
      # Models 4-92: Mediation models (with mediators)
      # Note: model_num was already defined above
      if(model_num >= 4 && model_num <= 92) {
        # Get mediators from collected reactive (M1, M2, M3... in order)
        current_mediators <- mediator_vars_collected()
        
        if(!is.null(current_mediators) && length(current_mediators) > 0) {
          mediator_arg <- if(length(current_mediators) == 1) {
            current_mediators[1]
          } else {
            current_mediators
          }
          process_args$m <- mediator_arg
          print(paste("DEBUG: Added mediator(s) to process_args$m:", paste(mediator_arg, collapse=", ")))
          
          # Add contrast for mediation models (Model 4 and 6 with multiple mediators)
          if(model_num %in% c(4, 6) && input$pairwise_contrasts && length(current_mediators) > 1) {
            process_args$contrast <- 1
          }
        }
      }
      
      # Add moderators ONLY for models that require them
      # Models 1, 5, 14, 15, 58, 59, 74, 83-92 have one moderator (W)
      # Models 2, 3 have two moderators (W and Z)
      if(model_num %in% c(1, 2, 3, 5, 14, 15, 16, 17, 18, 58, 59, 74, 83:92)) {
        if(!is.null(input$moderator_var) && input$moderator_var != "") {
          process_args$w <- input$moderator_var
          # Always generate JN data for moderation models (for plotting), regardless of checkbox
          # The checkbox only controls whether it appears in the output text
          process_args$jn <- 1
          if(isTRUE(input$probe_interactions) && !is.null(input$conditioning_values) && length(input$conditioning_values) > 0) {
            process_args$moments <- ifelse(input$conditioning_values == "0", 1, 0)
          }
        }
        
        # Only add Z if model requires it AND moderator2_var is set
        model_num <- as.numeric(input$process_model)
        if(model_num %in% models_with_second_moderator) {
          if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
            process_args$z <- input$moderator2_var
          }
        }
      }
      
      # Add other options
      print("DEBUG: Adding optional PROCESS arguments...")
      if(isTRUE(input$effsize)) process_args$effsize <- 1
      if(isTRUE(input$stand)) process_args$stand <- 1
      if(isTRUE(input$normal)) process_args$normal <- 1
      if(isTRUE(input$matrices)) process_args$matrices <- 1
      if(isTRUE(input$covcoeff)) process_args$covcoeff <- 1
      if(isTRUE(input$covmy)) {
        # When covmy==1, PROCESS excludes covariates from the outcome equation
        # For Model 1 (simple moderation with only one equation), PROCESS has a bug:
        # If covariates are excluded from the only equation, PROCESS incorrectly flags them
        # as appearing in "all equations" as moderators, triggering error 51.
        # Workaround: For Model 1, when covmy=1, do not pass covariates to PROCESS.
        # The output is still valid - covariates are simply excluded from the Y equation.
        if(model_num == 1) {
          # Silently work around PROCESS bug by not passing covariates
          # This produces valid output (covariates excluded from Y equation)
          process_args$cov <- "xxxxx"
        } else {
          # For other models, check for variable name overlaps
          if(!is.null(input$covariates) && length(input$covariates) > 0) {
            # Check if any covariate is also X (predictor)
            if(input$predictor_var %in% input$covariates) {
              showNotification(
                sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and the predictor (X). Please remove it from one of these roles.", input$predictor_var),
                type = "error",
                duration = 10
              )
              rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and the predictor (X).", input$predictor_var)
              return(NULL)
            }
            # Check if any covariate is also Y (outcome)
            if(input$outcome_var %in% input$covariates) {
              showNotification(
                sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and the outcome (Y). Please remove it from one of these roles.", input$outcome_var),
                type = "error",
                duration = 10
              )
              rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and the outcome (Y).", input$outcome_var)
              return(NULL)
            }
            # Check if any covariate is also a moderator (W)
            if(!is.null(input$moderator_var) && input$moderator_var != "" && input$moderator_var %in% input$covariates) {
              showNotification(
                sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and a moderator (W). Please remove it from one of these roles.", input$moderator_var),
                type = "error",
                duration = 10
              )
              rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and a moderator (W).", input$moderator_var)
              return(NULL)
            }
            # Check if any covariate is also a second moderator (Z)
            if(!is.null(input$moderator2_var) && input$moderator2_var != "" && input$moderator2_var %in% input$covariates) {
              showNotification(
                sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and a moderator (Z). Please remove it from one of these roles.", input$moderator2_var),
                type = "error",
                duration = 10
              )
              rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and a moderator (Z).", input$moderator2_var)
              return(NULL)
            }
            # Check if any covariate is also a mediator (for models that have mediators)
            mediator_vars_current <- mediator_vars_collected()
            if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
              overlapping_mediators <- intersect(input$covariates, mediator_vars_current)
              if(length(overlapping_mediators) > 0) {
                showNotification(
                  sprintf("Error: When 'Covariance matrix for Y' is checked, variable(s) '%s' cannot be used as both a covariate and a mediator. Please remove from one of these roles.", paste(overlapping_mediators, collapse="', '")),
                  type = "error",
                  duration = 10
                )
                rv$validation_error <- sprintf("Variable(s) '%s' cannot be used as both a covariate and a mediator.", paste(overlapping_mediators, collapse="', '"))
                return(NULL)
              }
            }
          }
        }
        process_args$covmy <- 1
      }
      if(input$describe) process_args$describe <- 1
      if(input$listmiss) process_args$listmiss <- 1
      if(input$diagnose) process_args$diagnose <- 1
      if(isTRUE(input$ssquares)) process_args$ssquares <- 1
      if(isTRUE(input$modelres)) process_args$modelres <- 1
      # xmint only for Model 4 - explicitly set to 0 if not used
      # When xmint=1 for Model 4, PROCESS automatically:
      #   - Converts to Model 74 internally (line 807-816)
      #   - Sets w<-x and model<-74 (line 809)
      #   - Automatically generates X*M interaction terms (line 2115-2119)
      #   - Handles interaction term creation - no manual calculation needed
      # However, for continuous X, PROCESS requires xrefval != 999 (line 1507-1508)
      # For dichotomous X, PROCESS automatically sets xrefval to [xmn, xmx] if xrefval=999 (line 1509-1510)
      # xmint and xmtest are mutually exclusive: when xmint is enabled, model becomes 74 and xmtest won't run
      if(model_num == 4 && input$xmint) {
        # Validate that xmtest is not also enabled (they are mutually exclusive)
        if(input$xmtest) {
          stop("Error: 'Allow X by M interaction' and 'Test for X by M interaction' cannot both be enabled. When 'Allow X by M interaction' is enabled, Model 4 is converted to Model 74, which makes the test option unavailable. Please disable one of these options.")
        }
        process_args$xmint <- 1
        # When xmint is used, center must be 0 (already set above, line 814-815)
        # Check if X is dichotomous (binary)
        x_is_dichotomous <- is_binary_variable(process_data_orig, input$predictor_var)
        if(x_is_dichotomous) {
          # For dichotomous X, don't pass xrefval - PROCESS will handle it automatically (line 1509-1510)
          # It will set xrefval to [min, max] automatically
        } else {
          # For continuous X, PROCESS requires xrefval to be set (not 999)
          # Use mean as default - PROCESS will add xrefval+1 to create a range for probing (line 1515)
          x_mean <- mean(process_data_orig[[input$predictor_var]], na.rm = TRUE)
          process_args$xrefval <- x_mean
        }
      } else {
        # Explicitly set xmint to 0 when not used to prevent any default behavior
        process_args$xmint <- 0
      }
      # xmtest: Test for X by M interaction (available for mediation models including Model 4, but not Model 74)
      # Note: xmtest is mutually exclusive with xmint for Model 4 (when xmint is enabled, model becomes 74 and xmtest won't run)
      if(isTRUE(input$xmtest)) {
        process_args$xmtest <- 1
      } else {
        process_args$xmtest <- 0
      }
      if(isTRUE(input$total)) process_args$total <- 1
      # Capture bootstrap results for download when bootstrapping is enabled
      # Set save=1 to get bootstrap results in return value (but don't save to file)
      if(isTRUE(input$use_bootstrap)) {
        process_args$save <- 1  # This makes PROCESS return bootstrap results
      }
      # Only enable plot if user explicitly wants it (with warning)
      if(isTRUE(input$plot)) {
        process_args$plot <- 1
        showNotification("Note: This will open R graphics device windows that cannot be easily saved or customized.", type = "warning", duration = 5)
      } else {
        # Explicitly set plot to 0 to prevent any default plotting
        process_args$plot <- 0
      }
      if(isTRUE(input$probe_interactions)) {
        # Parse probe threshold (e.g., "p < .10" -> 0.10)
        probe_text <- input$probe_threshold
        probe_val <- tryCatch({
          # Extract number from "p < .10" or "p < 0.10"
          num_match <- regmatches(probe_text, regexpr("0?\\.?\\d+", probe_text))
          if(length(num_match) > 0) {
            as.numeric(num_match[1])
          } else {
            0.1
          }
        }, error = function(e) 0.1)
        process_args$intprobe <- probe_val
      }
      if(!is.na(input$seed) && !is.null(input$seed) && input$seed >= 1) process_args$seed <- input$seed
      # PROCESS expects decimals in format "9.4" (9 digits before decimal, 4 after)
      # Convert user's decimal places (e.g., 2) to format PROCESS expects (e.g., "9.2")
      if(input$decimals != 4) process_args$decimals <- paste0("9.", input$decimals)
      
      # Check if this is a moderation model
      model_num <- as.numeric(input$process_model)
      is_mod_model <- model_num %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74)
      
      # For moderation models, set plot=2 and save=2 to get plot data in result object
      # NOTE: plot=2 generates data with SE and CI. R pop-up plots are suppressed via graphics device
      # To enable R pop-up plots, remove the pdf(NULL) call in the PROCESS execution
      if(is_mod_model) {
        process_args$plot <- 2  # Generate estimates + SE + CI for visualization
        process_args$save <- 2  # Return plot data in result object
      } else {
        # For non-moderation models, handle bootstrap and plot options as before
        # Capture bootstrap results for download when bootstrapping is enabled
        # Set save=1 to get bootstrap results in return value (but don't save to file)
        if(isTRUE(input$use_bootstrap)) {
          process_args$save <- 1  # This makes PROCESS return bootstrap results
        }
        # Only enable plot if user explicitly wants it (with warning)
        if(isTRUE(input$plot)) {
          process_args$plot <- 1
          showNotification("Note: This will open R graphics device windows that cannot be easily saved or customized.", type = "warning", duration = 5)
        } else {
          # Explicitly set plot to 0 to prevent any default plotting
          process_args$plot <- 0
        }
      }
      print("DEBUG: All PROCESS arguments prepared")
      
      # DEBUG: Print process arguments
      print("=== DEBUG: PROCESS Arguments ===")
      print(paste("Model:", process_args$model))
      print(paste("Y:", process_args$y))
      print(paste("X:", process_args$x))
      if("w" %in% names(process_args)) print(paste("W:", process_args$w))
      if("m" %in% names(process_args)) print(paste("M:", paste(process_args$m, collapse=", ")))
      print(paste("Data rows:", nrow(process_args$data)))
      print("=================================")
      
      # Run PROCESS with error handling
      tryCatch({
        print("DEBUG: About to call PROCESS function...")
        # Suppress R graphics pop-ups (plots are generated in Shiny UI instead)
        # NOTE: To enable R pop-up plots, comment out the pdf(NULL) and dev.off() lines
        pdf(NULL)
        process_output <- capture.output({
          result <- do.call(process, process_args)
        })
        dev.off()  # Close the null device
        print(paste("DEBUG: PROCESS completed. Output lines:", length(process_output)))
        
        # Store results including bootstrap data and plot data if available
        bootstrap_data <- NULL
        plot_data <- NULL
        
        # Check if this is a moderation model
        model_num <- as.numeric(input$process_model)
        is_mod_model <- model_num %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74)
        
        if(is_mod_model && !is.null(result)) {
          # For moderation models with save=2, result contains plot data in resultm
          print("========================================")
          print("DEBUG: CAPTURING RESULT OBJECT FOR MODERATION MODEL")
          print("========================================")
          print(paste("Result type:", class(result)))
          print(paste("Result typeof:", typeof(result)))
          
          # Store the FULL result object for later inspection
          # This is critical - we need to see everything PROCESS returns
          if(is.list(result)) {
            print(paste("Result is list with", length(result), "elements"))
            if(!is.null(names(result))) {
              print("Named elements in result:")
              print(names(result))
            }
            # Store the full result for Model 3 conditional effect extraction
            rv$full_result <- result
          } else {
            rv$full_result <- list(result)
          }
          
          if(is.data.frame(result)) {
            plot_data <- result
            print(paste("DEBUG: Plot data captured (data.frame), rows:", nrow(plot_data), "cols:", ncol(plot_data)))
          } else if(is.matrix(result)) {
            # PROCESS returns resultm as a matrix when save=2
            plot_data <- as.data.frame(result)
            # Remove rows with all NA or 99999 (PROCESS uses 99999 as placeholder)
            plot_data <- plot_data[rowSums(is.na(plot_data) | plot_data == 99999) < ncol(plot_data), ]
            print(paste("DEBUG: Plot data captured (matrix->data.frame), rows:", nrow(plot_data), "cols:", ncol(plot_data)))
          } else if(is.list(result) && length(result) > 0) {
            print(paste("DEBUG: Result is list with", length(result), "elements"))
            # For save=2, result is resultm (plot data)
            # For save=3, result is list(boots, resultm) where resultm is plot data
            if(length(result) == 2) {
              # Second element is plot data when save=3
              if(is.data.frame(result[[2]])) {
                plot_data <- result[[2]]
              } else if(is.matrix(result[[2]])) {
                plot_data <- as.data.frame(result[[2]])
                plot_data <- plot_data[rowSums(is.na(plot_data) | plot_data == 99999) < ncol(plot_data), ]
              }
              # First element is bootstrap when save=3
              if(is.data.frame(result[[1]])) {
                bootstrap_data <- result[[1]]
              } else if(is.matrix(result[[1]])) {
                bootstrap_data <- as.data.frame(result[[1]])
              }
              print(paste("DEBUG: Plot data captured (list[[2]]), rows:", nrow(plot_data), "cols:", ncol(plot_data)))
            } else if(is.data.frame(result[[1]])) {
              plot_data <- result[[1]]  # First element is plot data when save=2
              print(paste("DEBUG: Plot data captured (list[[1]]), rows:", nrow(plot_data), "cols:", ncol(plot_data)))
            } else if(is.matrix(result[[1]])) {
              plot_data <- as.data.frame(result[[1]])
              plot_data <- plot_data[rowSums(is.na(plot_data) | plot_data == 99999) < ncol(plot_data), ]
              print(paste("DEBUG: Plot data captured (list[[1]] matrix->data.frame), rows:", nrow(plot_data), "cols:", ncol(plot_data)))
            }
          }
        } else if(input$use_bootstrap && !is.null(result)) {
          # For non-moderation models, handle bootstrap as before
          # PROCESS returns bootstrap results when save=1 and bootstrapping is enabled
          print(paste("DEBUG: Bootstrap enabled, result type:", class(result)))
          if(is.data.frame(result)) {
            bootstrap_data <- result
            print(paste("DEBUG: Bootstrap data captured (data.frame), rows:", nrow(bootstrap_data)))
          } else if(is.list(result) && length(result) > 0) {
            print(paste("DEBUG: Result is list with", length(result), "elements"))
            if(is.data.frame(result[[1]])) {
              bootstrap_data <- result[[1]]  # First element is bootstrap results
              print(paste("DEBUG: Bootstrap data captured (list[[1]]), rows:", nrow(bootstrap_data)))
            }
          }
        } else {
          print(paste("DEBUG: No special data to capture. is_mod_model:", is_mod_model, "use_bootstrap:", input$use_bootstrap, "result is null:", is.null(result)))
        }
        
        # Clear validation error on successful run
        rv$validation_error <- NULL
        
        rv$analysis_results <- list(
          output = process_output,
          data_used = process_data_orig,
          original_data = rv$original_dataset,
          settings = analysis_settings,
          bootstrap_data = bootstrap_data,
          plot_data = plot_data,
          result = result  # Store the full result object
        )
        # Track which model these results are for
        if(!is.null(input$process_model) && input$process_model != "") {
          rv$results_model <- as.numeric(input$process_model)
        }
        print("DEBUG: Results stored in rv$analysis_results")
        print(paste("DEBUG: rv$analysis_results is NULL?", is.null(rv$analysis_results)))
        print(paste("DEBUG: Number of output lines:", length(rv$analysis_results$output)))
        print(paste("DEBUG: rv$results_model set to", rv$results_model))
      }, error = function(e) {
        print(paste("DEBUG: ERROR in PROCESS execution:", e$message))
        print(paste("DEBUG: Error class:", class(e)))
        showNotification(paste("Error running PROCESS analysis:", e$message), type = "error", duration = 10)
        rv$analysis_results <- NULL
        rv$validation_error <- conditionMessage(e)
        return(NULL)
      })
      
      print("DEBUG: ===== original_analysis eventReactive COMPLETED =====")
      print(paste("DEBUG: Returning results. Is NULL?", is.null(rv$analysis_results)))
      rv$analysis_results
    })
  })
  
  # Analysis with outliers removed
  outliers_analysis <- eventReactive(input$run_analysis_no_outliers, {
    # Clear any previous validation errors
    rv$validation_error <- NULL
    
    # Basic validation
    tryCatch({
      validate(
        need(rv$original_dataset, "Dataset not loaded"),
        need(input$outcome_var, "Outcome variable not selected"),
        need(input$predictor_var, "Predictor variable not selected"),
        need(input$process_model, "PROCESS model not selected"),
        need(input$outcome_var != input$predictor_var, 
             "Outcome and predictor must be different variables")
      )
    }, error = function(e) {
      rv$validation_error <- conditionMessage(e)
      stop(e)
    })
    
    # Model-specific validation
    model_num <- as.numeric(input$process_model)
    # Models with one moderator: 1, 5, 14, 15, 58, 59, 74, 83-92
    # Models with two moderators: 2, 3
    if(model_num %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74, 83:92)) {
      validate(
        need(!is.null(input$moderator_var) && input$moderator_var != "", 
             "Moderator variable (W) is required for this model")
      )
      
      # Models with second moderator require it to be selected
      if(model_num %in% c(2, 3)) {
        validate(
          need(!is.null(input$moderator2_var) && input$moderator2_var != "", 
               "Second moderator variable (Z) is required for this model")
        )
      }
    }
    # Models 4-92: All require at least one mediator
    if(model_num >= 4 && model_num <= 92) {
      mediator_vars_current <- mediator_vars_collected()
      validate(
        need(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0, 
             "At least one mediator variable is required for this model")
      )
      
      # Model 4 allows up to 10 mediators
      if(model_num == 4) {
        validate(
          need(length(mediator_vars_current) <= 10,
               "Model 4 allows up to 10 mediators")
        )
      }
      
      # Model 6 requires 2-6 mediators
      if(model_num == 6) {
        validate(
          need(length(mediator_vars_current) >= 2 && length(mediator_vars_current) <= 6,
               "Model 6 requires between 2 and 6 mediators")
        )
      }
      
      # Model 82 requires exactly 4 mediators
      if(model_num == 82) {
        mediator_count <- length(mediator_vars_current)
        if(mediator_count != 4) {
          if(mediator_count < 4) {
            error_msg <- paste0("Model 82 requires exactly 4 mediators. You have selected ", 
                               mediator_count, " mediator(s). Please select ", (4 - mediator_count), 
                               " more mediator(s).")
          } else {
            error_msg <- paste0("Model 82 requires exactly 4 mediators. You have selected ", 
                               mediator_count, " mediators. Please remove ", (mediator_count - 4), 
                               " mediator(s).")
          }
          showNotification(error_msg, type = "error", duration = 10)
          validate(need(FALSE, error_msg))
        }
      }
      
      # Models 83-92 require exactly 2 mediators
      if(model_num >= 83 && model_num <= 92) {
        mediator_count <- length(mediator_vars_current)
        if(mediator_count != 2) {
          if(mediator_count < 2) {
            error_msg <- paste0("Model ", model_num, " requires exactly 2 mediators. You have selected ", 
                               mediator_count, " mediator(s). Please select ", (2 - mediator_count), 
                               " more mediator(s).")
          } else {
            error_msg <- paste0("Model ", model_num, " requires exactly 2 mediators. You have selected ", 
                               mediator_count, " mediators. Please remove ", (mediator_count - 2), 
                               " mediator(s).")
          }
          showNotification(error_msg, type = "error", duration = 10)
          validate(need(FALSE, error_msg))
        }
      }
    }
    
    # Check for validation errors (set by real-time validation observer)
    if(!is.null(rv$validation_error)) {
      showNotification(
        rv$validation_error,
        type = "error",
        duration = 10
      )
      return(NULL)
    }
    
    # Simple duplicate check: collect all selected variables and check for duplicates
    # IMPORTANT: Only check inputs that are relevant to the current model
    # This prevents old values from previous models from causing false duplicate errors
    print("DEBUG: ===== Analysis duplicate check STARTED (outliers_analysis) =====")
    print(paste("DEBUG: Current model:", model_num))
    print(paste("DEBUG: predictor_var:", if(is.null(input$predictor_var) || input$predictor_var == "") "EMPTY" else input$predictor_var))
    print(paste("DEBUG: outcome_var:", if(is.null(input$outcome_var) || input$outcome_var == "") "EMPTY" else input$outcome_var))
    print(paste("DEBUG: moderator_var:", if(is.null(input$moderator_var) || input$moderator_var == "") "EMPTY" else input$moderator_var))
    print(paste("DEBUG: moderator2_var:", if(is.null(input$moderator2_var) || input$moderator2_var == "") "EMPTY" else input$moderator2_var))
    mediator_vars_current <- mediator_vars_collected()
    print(paste("DEBUG: mediator_count:", if(is.null(input$mediator_count) || input$mediator_count == "") "EMPTY" else input$mediator_count))
    print(paste("DEBUG: mediator_vars_collected:", if(is.null(mediator_vars_current) || length(mediator_vars_current) == 0) "EMPTY" else paste(mediator_vars_current, collapse=", ")))
    print(paste("DEBUG: covariates:", if(is.null(input$covariates) || length(input$covariates) == 0) "EMPTY" else paste(input$covariates, collapse=", ")))
    
    # Determine which inputs are actually used by the current model
    models_with_moderator <- c(1, 5, 14, 15, 58, 59, 74, 83:92)
    models_with_second_moderator <- c(2, 3)
    models_with_moderators_disabled <- c(4, 6, 80:82)
    
    all_vars <- character(0)
    # Always include predictor and outcome
    if(!is.null(input$predictor_var) && input$predictor_var != "") {
      all_vars <- c(all_vars, input$predictor_var)
    }
    if(!is.null(input$outcome_var) && input$outcome_var != "") {
      all_vars <- c(all_vars, input$outcome_var)
    }
    # Only include moderator_var if current model uses moderators (and moderators aren't disabled)
    if((model_num %in% models_with_moderator || model_num %in% models_with_second_moderator) &&
       !(model_num %in% models_with_moderators_disabled)) {
      if(!is.null(input$moderator_var) && input$moderator_var != "") {
        all_vars <- c(all_vars, input$moderator_var)
        print("DEBUG: Including moderator_var (model uses moderators)")
      }
    } else {
      print("DEBUG: Ignoring moderator_var (current model doesn't use moderators or moderators are disabled)")
    }
    # Only include moderator2_var if current model uses second moderator
    if(model_num %in% models_with_second_moderator) {
      if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
        all_vars <- c(all_vars, input$moderator2_var)
        print("DEBUG: Including moderator2_var (model uses second moderator)")
      }
    } else {
      print("DEBUG: Ignoring moderator2_var (current model doesn't use second moderator)")
    }
    # Only include mediators if current model uses mediators (models 4-92)
    if(model_num >= 4 && model_num <= 92) {
      current_mediators <- mediator_vars_collected()
      if(!is.null(current_mediators) && length(current_mediators) > 0) {
        all_vars <- c(all_vars, current_mediators)
        print("DEBUG: Including mediator_vars (model uses mediators)")
      }
    } else {
      print("DEBUG: Ignoring mediator_vars (current model doesn't use mediators or mediators are disabled)")
    }
    # Always include covariates
    if(!is.null(input$covariates) && length(input$covariates) > 0) {
      all_vars <- c(all_vars, input$covariates)
    }
    
    print(paste("DEBUG: All collected variables:", paste(all_vars, collapse=", ")))
    print(paste("DEBUG: Unique variables:", paste(unique(all_vars), collapse=", ")))
    print(paste("DEBUG: Length all_vars:", length(all_vars), "Length unique:", length(unique(all_vars))))
    
    # Final duplicate check (with exception for Model 74 where X and W can be the same)
    if(length(all_vars) > 0 && length(all_vars) != length(unique(all_vars))) {
      duplicate_vars <- all_vars[duplicated(all_vars)]
      
      # Exception: For Model 74, allow predictor_var and moderator_var to be the same
      if(model_num == 74) {
        # Check if the only duplicate is X=W (which is allowed for Model 74)
        if(length(duplicate_vars) == 1 && 
           !is.null(input$predictor_var) && input$predictor_var != "" &&
           !is.null(input$moderator_var) && input$moderator_var != "" &&
           input$predictor_var == input$moderator_var &&
           duplicate_vars[1] == input$predictor_var) {
          # This is the expected X=W for Model 74, so allow it
          print("DEBUG: Model 74 - X=W is allowed, proceeding with analysis")
        } else {
          # There are other duplicates beyond X=W
          print(paste("DEBUG: DUPLICATES FOUND IN ANALYSIS:", paste(unique(duplicate_vars), collapse=", ")))
          error_msg <- paste0("Error: The same variable cannot be used for multiple roles. Variable(s) '", 
                              paste(unique(duplicate_vars), collapse = "', '"), 
                              "' is/are used in more than one role.")
          showNotification(error_msg, type = "error", duration = 10)
          rv$validation_error <- error_msg
          return(NULL)
        }
      } else {
        # Not Model 74, so duplicates are not allowed
        print(paste("DEBUG: DUPLICATES FOUND IN ANALYSIS:", paste(unique(duplicate_vars), collapse=", ")))
        error_msg <- paste0("Error: The same variable cannot be used for multiple roles. Variable(s) '", 
                            paste(unique(duplicate_vars), collapse = "', '"), 
                            "' is/are used in more than one role.")
        showNotification(error_msg, type = "error", duration = 10)
        rv$validation_error <- error_msg
        return(NULL)
      }
    }
    print("DEBUG: ===== Analysis duplicate check PASSED (outliers_analysis) =====")
    
    # Model 5: First and Second Stage Moderation (requires moderator W and mediator M)
    if(model_num == 5) {
      validate(
        need(!is.null(input$moderator_var) && input$moderator_var != "", 
             "Model 5 requires moderator variable W")
      )
    }
    
    req(rv$original_dataset, input$outcome_var, input$predictor_var, input$process_model)
    
    withProgress(message = 'Running analysis...', value = 0, {
      print("Running analysis with outliers removed")
      outliers <- identify_outliers()
      
      # Check if outliers were found
      if(is.null(outliers) || is.null(outliers$cases) || length(outliers$cases) == 0) {
        stop("No outliers found to remove. Please check your outlier detection settings.")
      }
      
      rv$outliers_info <- list(
        count = outliers$count,
        threshold = outliers$threshold
      )
      
      reduced_data <- rv$original_dataset[-outliers$cases, ]
      
      # Build variable list
      all_vars <- c(input$outcome_var, input$predictor_var)
      mediator_vars_current <- mediator_vars_collected()
      if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
        all_vars <- c(all_vars, mediator_vars_current)
      }
      if(!is.null(input$moderator_var)) {
        all_vars <- c(all_vars, input$moderator_var)
      }
      if(!is.null(input$moderator2_var)) {
        all_vars <- c(all_vars, input$moderator2_var)
      }
      if(!is.null(input$covariates) && length(input$covariates) > 0) {
        all_vars <- c(all_vars, input$covariates)
      }
      
      complete_cases <- complete.cases(reduced_data[all_vars])
      n_complete <- sum(complete_cases)
      
      if(n_complete < 3) {
        stop(sprintf("After removing outliers, only %d complete cases remain. This is insufficient for analysis.", n_complete))
      }
      
      process_data <- reduced_data[complete_cases, ]
      
      # Store settings
      analysis_settings <- list(
        model = as.numeric(input$process_model),
        centering = input$centering,
        use_bootstrap = input$use_bootstrap,
        boot_samples = if(input$use_bootstrap) input$boot_samples else NULL,
        bootstrap_ci_method = if(input$use_bootstrap) input$bootstrap_ci_method else NULL,
        seed = if(!is.na(input$seed) && !is.null(input$seed) && input$seed >= 1) input$seed else NULL,
        hc_method = input$hc_method,
        conf_level = input$conf_level,
        stand = input$stand,
        normal = input$normal,
        pairwise_contrasts = input$pairwise_contrasts,
        xmint = input$xmint,
        xmtest = input$xmtest,
        total = input$total,
        probe_interactions = input$probe_interactions,
        probe_threshold = if(input$probe_interactions) input$probe_threshold else NULL,
        conditioning_values = if(input$probe_interactions) input$conditioning_values else NULL,
        jn = input$jn,
        dataset_name = tools::file_path_sans_ext(basename(input$data_file$name)),
        original_n = nrow(rv$original_dataset),
        outliers_removed = TRUE,
        outliers_count = outliers$count,
        outliers_threshold = outliers$threshold,
        outliers_method = outliers$method,
        predictor_var = input$predictor_var,
        outcome_var = input$outcome_var,
        mediator_vars = mediator_vars_collected(),
        moderator_var = input$moderator_var,
        moderator2_var = if(!is.null(input$moderator2_var) && input$moderator2_var != "") input$moderator2_var else NULL,
        covariates = if(!is.null(input$covariates) && length(input$covariates) > 0) input$covariates else NULL
      )
      
      # Early validation check for W and Z same variable
      model_num <- as.numeric(input$process_model)
      if(model_num %in% models_with_second_moderator) {
        if(!is.null(input$moderator_var) && !is.null(input$moderator2_var) && 
           input$moderator_var != "" && input$moderator2_var != "" &&
           input$moderator_var == input$moderator2_var) {
          showNotification(
            "Error: W and Z moderators must be different variables. Please select different variables for the first and second moderators.",
            type = "error",
            duration = 10
          )
          rv$validation_error <- "W and Z moderators must be different variables."
          return(NULL)
        } else {
          # Clear validation error if variables are different
          rv$validation_error <- NULL
        }
      } else {
        # Clear validation error for models without second moderator
        rv$validation_error <- NULL
      }
      
      # Build PROCESS arguments (same as original_analysis)
      
      # Handle xmint option for Model 4 - center must be 0 when xmint is used
      center_value <- as.numeric(input$centering)
      if(model_num == 4 && input$xmint) {
        center_value <- 0  # xmint requires center = 0
      }
      
      process_args <- list(
        data = process_data,
        y = input$outcome_var,
        x = input$predictor_var,
        model = model_num,
        center = center_value,
        conf = input$conf_level,
        modelbt = if(input$use_bootstrap) 1 else 0,
        boot = if(input$use_bootstrap) input$boot_samples else 0,
        bc = if(input$use_bootstrap) as.numeric(input$bootstrap_ci_method) else 0,
        hc = if(input$hc_method == "none") 5 else as.numeric(input$hc_method),
        # NOTE: robustse removed - cluster-robust option requires clustering variable (see comment above)
        covcoeff = if(input$covcoeff) 1 else 0,
        cov = if(!is.null(input$covariates) && length(input$covariates) > 0) input$covariates else "xxxxx"
      )
      
      # Add model-specific variables
      # Models 4-92: Mediation models (with mediators)
      if(model_num >= 4 && model_num <= 92) {
        # Get mediators from collected reactive (M1, M2, M3... in order)
        current_mediators <- mediator_vars_collected()
        
        if(!is.null(current_mediators) && length(current_mediators) > 0) {
          # Check for duplicate mediators within the mediator list itself
          if(length(current_mediators) != length(unique(current_mediators))) {
            showNotification(
              "Error: The same variable cannot be used for multiple roles (predictor, outcome, moderator, or mediator). Please ensure each variable is used in only one role.",
              type = "error",
              duration = 10
            )
            rv$validation_error <- "The same variable cannot be used for multiple roles (predictor, outcome, moderator, or mediator)."
            return(NULL)
          }
          
          mediator_arg <- if(length(current_mediators) == 1) {
            current_mediators[1]
          } else {
            current_mediators
          }
          process_args$m <- mediator_arg
          print(paste("DEBUG: Added mediator(s) to process_args$m (outliers analysis):", paste(mediator_arg, collapse=", ")))
          
          # Add contrast for mediation models (Model 4 and 6 with multiple mediators)
          if(model_num %in% c(4, 6) && input$pairwise_contrasts && length(current_mediators) > 1) {
            process_args$contrast <- 1
          }
        }
      }
      
      # Add moderators ONLY for models that require them
      # Models 1, 5, 14, 15, 58, 59, 74, 83-92 have one moderator (W)
      # Models 2, 3 have two moderators (W and Z)
      if(model_num %in% c(1, 2, 3, 5, 14, 15, 16, 17, 18, 58, 59, 74, 83:92)) {
        if(!is.null(input$moderator_var) && input$moderator_var != "") {
          process_args$w <- input$moderator_var
          # Always generate JN data for moderation models (for plotting), regardless of checkbox
          # The checkbox only controls whether it appears in the output text
          process_args$jn <- 1
          if(isTRUE(input$probe_interactions) && !is.null(input$conditioning_values) && length(input$conditioning_values) > 0) {
            process_args$moments <- ifelse(input$conditioning_values == "0", 1, 0)
          }
        }
        
        # Only add Z if model requires it AND moderator2_var is set
        model_num <- as.numeric(input$process_model)
        if(model_num %in% models_with_second_moderator) {
          if(!is.null(input$moderator2_var) && input$moderator2_var != "") {
            process_args$z <- input$moderator2_var
          }
        }
      }
      
      if(input$effsize) process_args$effsize <- 1
      if(input$stand) process_args$stand <- 1
      if(input$normal) process_args$normal <- 1
      if(input$matrices) process_args$matrices <- 1
      if(input$covcoeff) process_args$covcoeff <- 1
      if(input$covmy) {
        # When covmy==1, PROCESS excludes covariates from the outcome equation
        # For Model 1 (simple moderation with only one equation), PROCESS has a bug:
        # If covariates are excluded from the only equation, PROCESS incorrectly flags them
        # as appearing in "all equations" as moderators, triggering error 51.
        # Workaround: For Model 1, when covmy=1, do not pass covariates to PROCESS.
        # The output is still valid - covariates are simply excluded from the Y equation.
        if(model_num == 1) {
          # Silently work around PROCESS bug by not passing covariates
          # This produces valid output (covariates excluded from Y equation)
          process_args$cov <- "xxxxx"
        } else {
          # For other models, check for variable name overlaps
          if(!is.null(input$covariates) && length(input$covariates) > 0) {
            # Check if any covariate is also X (predictor)
            if(input$predictor_var %in% input$covariates) {
              showNotification(
                sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and the predictor (X). Please remove it from one of these roles.", input$predictor_var),
                type = "error",
                duration = 10
              )
              rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and the predictor (X).", input$predictor_var)
              return(NULL)
            }
            # Check if any covariate is also Y (outcome)
            if(input$outcome_var %in% input$covariates) {
              showNotification(
                sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and the outcome (Y). Please remove it from one of these roles.", input$outcome_var),
                type = "error",
                duration = 10
              )
              rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and the outcome (Y).", input$outcome_var)
              return(NULL)
            }
            # Check if any covariate is also a moderator (W)
            if(!is.null(input$moderator_var) && input$moderator_var != "" && input$moderator_var %in% input$covariates) {
              showNotification(
                sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and a moderator (W). Please remove it from one of these roles.", input$moderator_var),
                type = "error",
                duration = 10
              )
              rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and a moderator (W).", input$moderator_var)
              return(NULL)
            }
            # Check if any covariate is also a second moderator (Z)
            if(!is.null(input$moderator2_var) && input$moderator2_var != "" && input$moderator2_var %in% input$covariates) {
              showNotification(
                sprintf("Error: When 'Covariance matrix for Y' is checked, variable '%s' cannot be used as both a covariate and a moderator (Z). Please remove it from one of these roles.", input$moderator2_var),
                type = "error",
                duration = 10
              )
              rv$validation_error <- sprintf("Variable '%s' cannot be used as both a covariate and a moderator (Z).", input$moderator2_var)
              return(NULL)
            }
            # Check if any covariate is also a mediator (for models that have mediators)
            mediator_vars_current <- mediator_vars_collected()
            if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
              overlapping_mediators <- intersect(input$covariates, mediator_vars_current)
              if(length(overlapping_mediators) > 0) {
                showNotification(
                  sprintf("Error: When 'Covariance matrix for Y' is checked, variable(s) '%s' cannot be used as both a covariate and a mediator. Please remove from one of these roles.", paste(overlapping_mediators, collapse="', '")),
                  type = "error",
                  duration = 10
                )
                rv$validation_error <- sprintf("Variable(s) '%s' cannot be used as both a covariate and a mediator.", paste(overlapping_mediators, collapse="', '"))
                return(NULL)
              }
            }
          }
        }
        process_args$covmy <- 1
      }
      if(input$describe) process_args$describe <- 1
      if(isTRUE(input$listmiss)) process_args$listmiss <- 1
      if(isTRUE(input$diagnose)) process_args$diagnose <- 1
      if(isTRUE(input$ssquares)) process_args$ssquares <- 1
      if(input$modelres) process_args$modelres <- 1
      # xmint only for Model 4
      # When xmint=1 for Model 4, PROCESS automatically:
      #   - Converts to Model 74 internally (line 807-816)
      #   - Sets w<-x and model<-74 (line 809)
      #   - Automatically generates X*M interaction terms (line 2115-2119)
      #   - Handles interaction term creation - no manual calculation needed
      # However, for continuous X, PROCESS requires xrefval != 999 (line 1507-1508)
      # For dichotomous X, PROCESS automatically sets xrefval to [xmn, xmx] if xrefval=999 (line 1509-1510)
      # xmint and xmtest are mutually exclusive: when xmint is enabled, model becomes 74 and xmtest won't run
      if(model_num == 4 && input$xmint) {
        # Validate that xmtest is not also enabled (they are mutually exclusive)
        if(input$xmtest) {
          stop("Error: 'Allow X by M interaction' and 'Test for X by M interaction' cannot both be enabled. When 'Allow X by M interaction' is enabled, Model 4 is converted to Model 74, which makes the test option unavailable. Please disable one of these options.")
        }
        process_args$xmint <- 1
        # When xmint is used, center must be 0 (already set above, line 814-815)
        # Check if X is dichotomous (binary)
        x_is_dichotomous <- is_binary_variable(process_data, input$predictor_var)
        if(x_is_dichotomous) {
          # For dichotomous X, don't pass xrefval - PROCESS will handle it automatically (line 1509-1510)
          # It will set xrefval to [min, max] automatically
        } else {
          # For continuous X, PROCESS requires xrefval to be set (not 999)
          # Use mean as default - PROCESS will add xrefval+1 to create a range for probing (line 1515)
          x_mean <- mean(process_data[[input$predictor_var]], na.rm = TRUE)
          process_args$xrefval <- x_mean
        }
      } else {
        # Explicitly set xmint to 0 when not used to prevent any default behavior
        process_args$xmint <- 0
      }
      # xmtest: Test for X by M interaction (available for mediation models including Model 4, but not Model 74)
      # Note: xmtest is mutually exclusive with xmint for Model 4 (when xmint is enabled, model becomes 74 and xmtest won't run)
      if(input$xmtest) {
        process_args$xmtest <- 1
      } else {
        process_args$xmtest <- 0
      }
      if(isTRUE(input$total)) process_args$total <- 1
      # Capture bootstrap results for download when bootstrapping is enabled
      if(isTRUE(input$use_bootstrap)) {
        process_args$save <- 1  # This makes PROCESS return bootstrap results
      }
      # Only enable plot if user explicitly wants it
      if(isTRUE(input$plot)) {
        process_args$plot <- 1
      } else {
        process_args$plot <- 0
      }
      if(isTRUE(input$probe_interactions)) {
        # Parse probe threshold (e.g., "p < .10" -> 0.10)
        probe_text <- input$probe_threshold
        probe_val <- tryCatch({
          # Extract number from "p < .10" or "p < 0.10"
          num_match <- regmatches(probe_text, regexpr("0?\\.?\\d+", probe_text))
          if(length(num_match) > 0) {
            as.numeric(num_match[1])
          } else {
            0.1
          }
        }, error = function(e) 0.1)
        process_args$intprobe <- probe_val
      }
      if(!is.na(input$seed) && !is.null(input$seed) && input$seed >= 1) process_args$seed <- input$seed
      # PROCESS expects decimals in format "9.4" (9 digits before decimal, 4 after)
      # Convert user's decimal places (e.g., 2) to format PROCESS expects (e.g., "9.2")
      if(input$decimals != 4) process_args$decimals <- paste0("9.", input$decimals)
      
      # DEBUG: Print process arguments
      print("=== DEBUG: PROCESS Arguments (Outliers Removed) ===")
      print(paste("Model:", process_args$model))
      print(paste("Y:", process_args$y))
      print(paste("X:", process_args$x))
      if("w" %in% names(process_args)) print(paste("W:", process_args$w))
      if("m" %in% names(process_args)) print(paste("M:", paste(process_args$m, collapse=", ")))
      print(paste("Data rows:", nrow(process_args$data)))
      print("=================================")
      
      # Run PROCESS with error handling
      tryCatch({
        print("DEBUG: About to call PROCESS function (outliers removed)...")
        process_output <- capture.output({
          result <- do.call(process, process_args)
        })
        print(paste("DEBUG: PROCESS completed. Output lines:", length(process_output)))
        
        # Store results including bootstrap data if available
        bootstrap_data <- NULL
        if(input$use_bootstrap && !is.null(result)) {
          # PROCESS returns bootstrap results when save=1 and bootstrapping is enabled
          print(paste("DEBUG: Bootstrap enabled, result type:", class(result)))
          if(is.data.frame(result)) {
            bootstrap_data <- result
            print(paste("DEBUG: Bootstrap data captured (data.frame), rows:", nrow(bootstrap_data)))
          } else if(is.list(result) && length(result) > 0) {
            print(paste("DEBUG: Result is list with", length(result), "elements"))
            if(is.data.frame(result[[1]])) {
              bootstrap_data <- result[[1]]  # First element is bootstrap results
              print(paste("DEBUG: Bootstrap data captured (list[[1]]), rows:", nrow(bootstrap_data)))
            }
          }
        } else {
          print(paste("DEBUG: Bootstrap not enabled or result is NULL. use_bootstrap:", input$use_bootstrap, "result is null:", is.null(result)))
        }
        
        rv$analysis_results <- list(
          output = process_output,
          data_used = process_data,
          original_data = rv$original_dataset,
          settings = analysis_settings,
          bootstrap_data = bootstrap_data
        )
        # Track which model these results are for
        if(!is.null(input$process_model) && input$process_model != "") {
          rv$results_model <- as.numeric(input$process_model)
        }
        # Clear validation error on successful run
        rv$validation_error <- NULL
        print("DEBUG: Results stored in rv$analysis_results (outliers removed)")
        print(paste("DEBUG: rv$results_model set to", rv$results_model))
      }, error = function(e) {
        print(paste("DEBUG: ERROR in PROCESS execution (outliers removed):", e$message))
        showNotification(paste("Error running PROCESS analysis:", e$message), type = "error")
        rv$analysis_results <- NULL
        stop(e)
      })
      
      rv$analysis_results
    })
  }, ignoreNULL = TRUE)
  
  # Observer to trigger when original_analysis completes
  observeEvent(original_analysis(), {
    # Only update if we're not in a clearing state (to prevent stale results from previous model)
    if(!isTRUE(rv$is_clearing)) {
      print("DEBUG: original_analysis() completed, checking if results match current model")
      result <- original_analysis()
      current_model_num <- if(!is.null(input$process_model) && input$process_model != "") {
        as.numeric(input$process_model)
      } else {
        NULL
      }
      
      # Verify results match current model before storing
      if(!is.null(result) && !is.null(result$settings) && !is.null(result$settings$model)) {
        if(is.null(current_model_num) || result$settings$model == current_model_num) {
          rv$analysis_results <- result
          rv$results_model <- result$settings$model
          print(paste("DEBUG: rv$analysis_results updated for model", rv$results_model))
        } else {
          print(paste("DEBUG: Skipping results update - results are for model", result$settings$model, "but current model is", current_model_num))
        }
      } else {
        print("DEBUG: Skipping results update - result doesn't have valid model information")
      }
    } else {
      print("DEBUG: Skipping analysis results update - in clearing state")
    }
  }, ignoreNULL = TRUE)
  
  # Observer to trigger when outliers_analysis completes
  observeEvent(outliers_analysis(), {
    # Only update if we're not in a clearing state (to prevent stale results from previous model)
    if(!isTRUE(rv$is_clearing)) {
      print("DEBUG: outliers_analysis() completed, checking if results match current model")
      result <- outliers_analysis()
      current_model_num <- if(!is.null(input$process_model) && input$process_model != "") {
        as.numeric(input$process_model)
      } else {
        NULL
      }
      
      # Verify results match current model before storing
      if(!is.null(result) && !is.null(result$settings) && !is.null(result$settings$model)) {
        if(is.null(current_model_num) || result$settings$model == current_model_num) {
          rv$analysis_results <- result
          rv$results_model <- result$settings$model
          print(paste("DEBUG: rv$analysis_results updated for model", rv$results_model))
        } else {
          print(paste("DEBUG: Skipping results update - results are for model", result$settings$model, "but current model is", current_model_num))
        }
      } else {
        print("DEBUG: Skipping results update - result doesn't have valid model information")
      }
    } else {
      print("DEBUG: Skipping analysis results update - in clearing state")
    }
  }, ignoreNULL = TRUE)
  
  # Combined results reactive - use stored results from rv
  analysis_results <- reactive({
    print(paste("DEBUG: analysis_results reactive called. rv$analysis_results is NULL?", is.null(rv$analysis_results)))
    
    # Get current model number
    current_model_num <- if(!is.null(input$process_model) && input$process_model != "") {
      as.numeric(input$process_model)
    } else {
      NULL
    }
    
    # Check if stored results match current model
    if(!is.null(rv$analysis_results) && !is.null(current_model_num)) {
      stored_model <- if(!is.null(rv$analysis_results$settings) && !is.null(rv$analysis_results$settings$model)) {
        rv$analysis_results$settings$model
      } else {
        NULL
      }
      if(!is.null(stored_model) && stored_model != current_model_num) {
        print(paste("DEBUG: Stored results are for model", stored_model, "but current model is", current_model_num, "- clearing results"))
        rv$analysis_results <- NULL
        rv$results_model <- NULL
        return(NULL)
      }
    }
    
    # If model has changed (tracked separately), don't try to recover old results
    if(!is.null(current_model_num) && !is.null(rv$results_model) && current_model_num != rv$results_model) {
      print(paste("DEBUG: Model changed from", rv$results_model, "to", current_model_num, "- not recovering old results"))
      return(NULL)
    }
    
    if(is.null(rv$analysis_results)) {
      # Try to get from eventReactive if rv is null
      # Check if inputs exist and have valid values before comparing
      run_analysis_val <- if(!is.null(input$run_analysis) && length(input$run_analysis) > 0) input$run_analysis else 0
      run_no_outliers_val <- if(!is.null(input$run_analysis_no_outliers) && length(input$run_analysis_no_outliers) > 0) input$run_analysis_no_outliers else 0
      
      if(run_analysis_val > 0 && (run_no_outliers_val == 0 || run_analysis_val >= run_no_outliers_val)) {
        print("DEBUG: rv$analysis_results is NULL, trying to get from original_analysis()")
        tryCatch({
          result <- original_analysis()
          # Verify the recovered results match the current model
          if(!is.null(result) && !is.null(result$settings) && !is.null(result$settings$model)) {
            if(is.null(current_model_num) || result$settings$model == current_model_num) {
              rv$analysis_results <- result
              rv$results_model <- if(!is.null(result$settings$model)) result$settings$model else current_model_num
              print(paste("DEBUG: Retrieved results from original_analysis() for model", rv$results_model))
            } else {
              print(paste("DEBUG: Recovered results are for model", result$settings$model, "but current model is", current_model_num, "- not using them"))
            }
          }
        }, error = function(e) {
          print(paste("DEBUG: Error getting from original_analysis():", e$message))
        })
      } else if(run_no_outliers_val > 0) {
        print("DEBUG: rv$analysis_results is NULL, trying to get from outliers_analysis()")
        tryCatch({
          result <- outliers_analysis()
          # Verify the recovered results match the current model
          if(!is.null(result) && !is.null(result$settings) && !is.null(result$settings$model)) {
            if(is.null(current_model_num) || result$settings$model == current_model_num) {
              rv$analysis_results <- result
              rv$results_model <- if(!is.null(result$settings$model)) result$settings$model else current_model_num
              print(paste("DEBUG: Retrieved results from outliers_analysis() for model", rv$results_model))
            } else {
              print(paste("DEBUG: Recovered results are for model", result$settings$model, "but current model is", current_model_num, "- not using them"))
            }
          }
        }, error = function(e) {
          print(paste("DEBUG: Error getting from outliers_analysis():", e$message))
        })
      }
    }
    
    # Final check: if we have results, verify they match current model
    if(!is.null(rv$analysis_results) && !is.null(current_model_num)) {
      stored_model <- if(!is.null(rv$analysis_results$settings) && !is.null(rv$analysis_results$settings$model)) {
        rv$analysis_results$settings$model
      } else {
        NULL
      }
      if(!is.null(stored_model) && stored_model != current_model_num) {
        print(paste("DEBUG: Final check - results model", stored_model, "doesn't match current", current_model_num, "- returning NULL"))
        return(NULL)
      }
    }
    
    rv$analysis_results
  })
  
  # Helper function to create bivariate correlations
  create_bivariate_correlations <- function(data, predictor_var, outcome_var, settings) {
    outcome_is_binary <- is_binary_variable(data, outcome_var)
    complete_data <- data[complete.cases(data[c(predictor_var, outcome_var)]), ]
    n <- nrow(complete_data)
    
    dataset_desc <- if(settings$outliers_removed) {
      sprintf("the original dataset (before %d outlier%s removed)", 
              settings$outliers_count, ifelse(settings$outliers_count == 1, "", "s"))
    } else {
      "the original dataset (all cases included)"
    }
    
    if(outcome_is_binary) {
      pearson_test <- cor.test(complete_data[[predictor_var]], complete_data[[outcome_var]], 
                               method = "pearson")
      pearson_r <- pearson_test$estimate
      pearson_p <- pearson_test$p.value
      pearson_ci <- pearson_test$conf.int
      
      output <- c(
        "<br><strong>BIVARIATE CORRELATION: PREDICTOR AND OUTCOME</strong>",
        sprintf("<em>This shows the zero-order (unadjusted) relationship between the predictor and outcome variables, calculated on %s:</em>", dataset_desc),
        "",
        sprintf("<strong>Point-biserial correlation (Pearson's r):</strong> %.4f, 95%% CI [%.4f, %.4f], p %s", 
                pearson_r, pearson_ci[1], pearson_ci[2],
                ifelse(pearson_p < .001, "< .001", sprintf("= %.3f", pearson_p))),
        sprintf("<em>Sample size: %d cases (listwise deletion for these two variables)</em>", n),
        ""
      )
    } else {
      pearson_test <- cor.test(complete_data[[predictor_var]], complete_data[[outcome_var]], 
                               method = "pearson")
      pearson_r <- pearson_test$estimate
      pearson_p <- pearson_test$p.value
      pearson_ci <- pearson_test$conf.int
      
      spearman_test <- cor.test(complete_data[[predictor_var]], complete_data[[outcome_var]], 
                                method = "spearman")
      spearman_rho <- spearman_test$estimate
      spearman_p <- spearman_test$p.value
      
      output <- c(
        "<br><strong>BIVARIATE CORRELATION: PREDICTOR AND OUTCOME</strong>",
        sprintf("<em>This shows the zero-order (unadjusted) relationship between the predictor and outcome variables, calculated on %s:</em>", dataset_desc),
        "",
        sprintf("<strong>Pearson's r:</strong> %.4f, 95%% CI [%.4f, %.4f], p %s", 
                pearson_r, pearson_ci[1], pearson_ci[2],
                ifelse(pearson_p < .001, "< .001", sprintf("= %.3f", pearson_p))),
        sprintf("<strong>Spearman's ρ:</strong> %.4f, p %s",
                spearman_rho,
                ifelse(spearman_p < .001, "< .001", sprintf("= %.3f", spearman_p))),
        sprintf("<em>Sample size: %d cases (listwise deletion for these two variables)</em>", n),
        ""
      )
    }
    
    paste(output, collapse = "<br>")
  }
  
  # Helper function to create missing data breakdown
  create_missing_data_breakdown <- function(data, settings) {
    all_vars <- c(settings$outcome_var, settings$predictor_var)
    if(!is.null(settings$mediator_vars)) all_vars <- c(all_vars, settings$mediator_vars)
    if(!is.null(settings$moderator_var)) all_vars <- c(all_vars, settings$moderator_var)
    if(!is.null(settings$moderator2_var)) all_vars <- c(all_vars, settings$moderator2_var)
    if(!is.null(settings$covariates)) all_vars <- c(all_vars, settings$covariates)
    
    missing_breakdown <- sapply(all_vars, function(var) {
      sum(is.na(data[[var]]))
    })
    
    missing_breakdown <- missing_breakdown[missing_breakdown > 0]
    
    if(length(missing_breakdown) > 0) {
      breakdown_lines <- sapply(names(missing_breakdown), function(var) {
        count <- missing_breakdown[var]
        sprintf("  %s: %d case%s missing", var, count, ifelse(count == 1, "", "s"))
      })
      paste(breakdown_lines, collapse = "<br>")
    } else {
      NULL
    }
  }
  
  # Format PROCESS output for display
  create_formatted_output <- function(analysis_results) {
    if(is.null(analysis_results)) return("")
    
    settings <- analysis_results$settings
    process_output <- analysis_results$output
    
    
    # Start with summary
    output_text <- c(
      "<strong>ANALYSIS SUMMARY</strong>",
      "",
      sprintf("Dataset name: %s", settings$dataset_name),
      sprintf("Dataset size: %d cases", settings$original_n),
      sprintf("PROCESS Model: %d", settings$model),
      if(settings$outliers_removed) {
        if(settings$outliers_method == "Cook's Distance") {
          sprintf("%d influential cases (Cook's D > %.4f) removed", 
                  settings$outliers_count, settings$outliers_threshold)
        } else {
          sprintf("%d standardized residual outliers (|SR| > %.1f) removed", 
                  settings$outliers_count, settings$outliers_threshold)
        }
      } else {
        "Original dataset with all cases"
      },
      "",
      sprintf("Predictor variable: %s", settings$predictor_var),
      sprintf("Outcome variable: %s", settings$outcome_var),
      if(!is.null(settings$mediator_vars)) {
        sprintf("Mediator variable(s): %s", paste(settings$mediator_vars, collapse = ", "))
      },
      # Only show moderator variables for models that use them
      if(!is.null(settings$moderator_var) && settings$moderator_var != "" && 
         settings$model %in% c(1, 2, 3, 5, 14, 15, 58, 59, 74)) {
        sprintf("Moderator variable: %s", settings$moderator_var)
      },
      if(!is.null(settings$moderator2_var) && settings$moderator2_var != "" &&
         settings$model %in% c(5, 58, 59, 74)) {
        sprintf("Second Moderator variable: %s", settings$moderator2_var)
      },
      if(!is.null(settings$covariates)) {
        sprintf("Covariate(s): %s", paste(settings$covariates, collapse = ", "))
      } else {
        "Covariate(s): none"
      },
      "",
      "<strong>ANALYSIS SETTINGS</strong>",
      paste("Centering:", switch(settings$centering,
        "0" = "No centering",
        "1" = "All variables that define products",
        "2" = "Only continuous variables that define products"
      )),
      "",
      "<strong>Bootstrap Settings:</strong>",
      if(!is.null(settings$use_bootstrap) && isTRUE(settings$use_bootstrap)) {
        bootstrap_info <- paste("Bootstrap samples:", if(!is.null(settings$boot_samples)) settings$boot_samples else "N/A")
        if(!is.null(settings$bootstrap_ci_method)) {
          ci_method_text <- if(settings$bootstrap_ci_method == "1") "Bias-corrected bootstrap" else "Percentile bootstrap"
          bootstrap_info <- paste(bootstrap_info, paste0("(", ci_method_text, ")"))
        }
        bootstrap_info
      } else {
        "Bootstrap: Not used"
      },
      if(!is.null(settings$seed)) {
        paste("Random seed:", settings$seed)
      },
      if(!is.null(settings$conf_level)) {
        paste("Confidence level:", settings$conf_level, "%")
      } else {
        "Confidence level: 95 %"
      },
      "",
      "<strong>Advanced Options:</strong>",
      if(!is.null(settings$hc_method)) {
        hc_display <- switch(settings$hc_method,
          "none" = "OLS",
          "0" = "HC0 (Huber-White)",
          "1" = "HC1 (Hinkley)",
          "2" = "HC2",
          "3" = "HC3 (Davidson-MacKinnon)",
          "4" = "HC4 (Cribari-Neto)"
        )
        if(is.null(hc_display)) hc_display <- "OLS"  # fallback
        paste("Standard Errors:", hc_display)
      } else {
        "Standard Errors: OLS"
      },
      if(!is.null(settings$stand) && isTRUE(settings$stand)) "Standardized coefficients: Yes",
      if(!is.null(settings$normal) && isTRUE(settings$normal)) "Normal theory tests: Yes",
      if(!is.null(settings$pairwise_contrasts) && isTRUE(settings$pairwise_contrasts)) "Pairwise contrasts of indirect effects: Yes",
      if(!is.null(settings$xmint) && isTRUE(settings$xmint)) "Allow X by M interaction: Yes",
      if(!is.null(settings$xmtest) && isTRUE(settings$xmtest)) "Test for X by M interaction: Yes",
      if(!is.null(settings$total) && isTRUE(settings$total)) "Total effect of X: Yes",
      "",
      if((!is.null(settings$probe_interactions) && isTRUE(settings$probe_interactions)) || 
         (!is.null(settings$jn) && isTRUE(settings$jn))) {
        c("<strong>Probing Moderation:</strong>",
          if(!is.null(settings$probe_interactions) && isTRUE(settings$probe_interactions)) {
            c(paste("Probe interactions: Yes"),
              if(!is.null(settings$probe_threshold)) paste("Probe threshold:", settings$probe_threshold),
              if(!is.null(settings$conditioning_values)) {
                paste("Conditioning values:", switch(settings$conditioning_values,
                  "0" = "Mean ± 1 SD",
                  "1" = "Mean ± 2 SD",
                  "2" = "16th, 50th, 84th percentiles"
                ))
              }
            )
          },
          if(!is.null(settings$jn) && isTRUE(settings$jn)) "Johnson-Neyman technique: Yes"
        )
      },
      ""
    )
    
    # Add bivariate correlations
    bivariate_cor <- create_bivariate_correlations(
      analysis_results$original_data,
      settings$predictor_var,
      settings$outcome_var,
      settings
    )
    
    # Process PROCESS output - basic formatting
    # Filter out bootstrap progress lines only
    filtered_output <- process_output[
      !grepl("^Bootstrap|^Percentile bootstrap|^\\*+ BOOTSTRAP|^Level of confidence|^\\s*$|^\\*+$|^\\s*\\||^\\s*\\d+%|^\\s*>+\\s*$", 
             process_output, ignore.case = TRUE)
    ]
    
    # Basic formatting - output lines as-is
    processed_output <- character(0)
    in_covariance_matrix <- FALSE
    xmtest_explanation_added <- FALSE
    xmint_explanation_added <- FALSE
    
    for(i in seq_along(filtered_output)) {
      line <- filtered_output[i]
      
      # Bold important headers
      if(grepl("^Outcome Variable:", line, ignore.case = TRUE)) {
        processed_output <- c(processed_output, 
          paste0("<strong style='font-size: 1.1em;'>", line, "</strong>"))
        next
      }
      if(grepl("^Test\\(s\\) of highest order unconditional interaction\\(s\\):", line, ignore.case = TRUE)) {
        processed_output <- c(processed_output, 
          paste0("<strong style='font-size: 1.1em;'>", line, "</strong>"))
        next
      }
      
      # Track sections and add explanatory text
      if(grepl("^Test\\(s\\) of X by M interaction|^Likelihood ratio test\\(s\\) of X by M interaction", line, ignore.case = TRUE)) {
        if(!xmtest_explanation_added) {
          processed_output <- c(processed_output, 
            "<br><em><strong>Understanding 'Test for X by M interaction':</strong> This section presents statistical tests (F-tests or likelihood ratio tests) to determine whether X*M interaction terms are significant. A significant result indicates that the effect of the mediator(s) (M) on the outcome (Y) depends on the level of the independent variable (X). This test does not change the model structure - it evaluates whether interactions should be included.</em><br>")
          xmtest_explanation_added <- TRUE
        }
        processed_output <- c(processed_output, line)
        next
      }
      if(grepl("^COUNTERFACTUALLY DEFINED|^Direct, indirect, and total effects are counterfactually defined", line, ignore.case = TRUE)) {
        if(!xmint_explanation_added) {
          processed_output <- c(processed_output, 
            "<br><em><strong>Understanding 'Allow X by M interaction' (Model 4):</strong> When this option is enabled, Model 4 is converted to a counterfactual framework (Model 74 internally). This includes X*M interaction terms in the model, allowing the effect of mediators (M) on the outcome (Y) to vary depending on the level of X. Effects are now defined counterfactually (natural direct and indirect effects), which provides a different interpretation than standard mediation. This changes the model structure and enables estimation of controlled and natural direct effects.</em><br>")
          xmint_explanation_added <- TRUE
        }
        processed_output <- c(processed_output, line)
        next
      }
      if(grepl("^Covariance matrix", line, ignore.case = TRUE)) {
        in_covariance_matrix <- TRUE
        processed_output <- c(processed_output, line)
        next
      }
      if(grepl("^ANALYSIS NOTES|^\\*+$", line, ignore.case = TRUE)) {
        in_covariance_matrix <- FALSE
        processed_output <- c(processed_output, line)
        next
      }
      
      # Skip all covariance matrix content
      if(in_covariance_matrix) {
        processed_output <- c(processed_output, line)
        next
      }
      
      # Skip header lines (but NOT variable names like "constant" or "int_1" which are actual data rows)
      # Only skip lines that are clearly headers (column names, section titles, etc.)
      if(grepl("^\\s*(Model|Outcome|coeff|se\\(|t|p|LLCI|ULCI|R|R-sq|MSE|F\\(|df|Product|Variable|Mean|SD|Min|Max|Pearson|Spearman|effect|Effect|BootSE|BootLLCI|BootULCI|^\\s*$|Direct effect|Indirect effect|TOTAL|Completely standardized|Partially standardized|Specific indirect)", line, ignore.case = TRUE)) {
        processed_output <- c(processed_output, line)
        next
      }
      
      # Output all other lines as-is
      processed_output <- c(processed_output, line)
    }
    
    # Combine all output
    paste(
      "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
      paste(c(output_text, "", bivariate_cor, "", processed_output), collapse = "<br>"),
      "</div>"
    )
  }
  
  # Display analysis results
  output$analysis_output <- renderUI({
    print("DEBUG: analysis_output renderUI called")
    
    # If analysis results are NULL (e.g., after model switch), show empty state
    if(is.null(rv$analysis_results)) {
      print("DEBUG: rv$analysis_results is NULL, showing empty state")
      return(HTML("<div style='padding: 20px; text-align: center; color: #666;'>Run an analysis to see results here.</div>"))
    }
    
    # Check for validation errors first
    if(!is.null(rv$validation_error)) {
      print(paste("DEBUG: Validation error found:", rv$validation_error))
      return(HTML(paste0(
        "<div style='color: red; font-weight: bold; padding: 15px; border: 2px solid red; background-color: #ffe6e6; margin: 10px 0; border-radius: 5px;'>",
        "<strong>ERROR:</strong><br><br>",
        rv$validation_error,
        "</div>"
      )))
    }
    
    tryCatch({
      results <- analysis_results()
      print(paste("DEBUG: analysis_results() returned NULL?", is.null(results)))
      if(is.null(results)) {
        return(HTML("<p>No analysis results available. Please run an analysis.</p>"))
      }
      print("DEBUG: Creating formatted output...")
      HTML(create_formatted_output(results))
    }, error = function(e) {
      # Catch other errors and display them in the output
      error_msg <- conditionMessage(e)
      print(paste("DEBUG: Error caught in analysis_output:", error_msg))
      HTML(paste0(
        "<div style='color: red; font-weight: bold; padding: 15px; border: 2px solid red; background-color: #ffe6e6; margin: 10px 0; border-radius: 5px;'>",
        "<strong>ERROR:</strong><br><br>",
        error_msg,
        "</div>"
      ))
    })
  })
  
  # Download handler for assumption checks
  output$download_assumptions <- downloadHandler(
    filename = function() {
      paste0("assumption_checks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      req(rv$original_dataset, input$outcome_var, input$predictor_var)
      
      # Get the assumption details HTML
      assumption_html <- tryCatch({
        # Build formula based on model type
        formula_terms <- c(input$outcome_var, "~", input$predictor_var)
        
        # Add interaction for moderation models
        if(!is.null(input$moderator_var) && input$moderator_var != "") {
          formula_terms <- c(formula_terms, "*", input$moderator_var)
        }
        
        # Add mediators for mediation models
        mediator_vars_current <- mediator_vars_collected()
        if(!is.null(mediator_vars_current) && length(mediator_vars_current) > 0) {
          formula_terms <- c(formula_terms, "+", paste(mediator_vars_current, collapse = " + "))
        }
        
        # Add covariates
        if(!is.null(input$covariates) && length(input$covariates) > 0) {
          formula_terms <- c(formula_terms, "+", paste(input$covariates, collapse = " + "))
        }
        
        model_formula <- as.formula(paste(formula_terms, collapse = " "))
        outcome_is_binary <- is_binary_variable(rv$original_dataset, input$outcome_var)
        
        if(outcome_is_binary) {
          model <- glm(model_formula, data = rv$original_dataset, family = binomial())
          outlier_text <- paste(outlier_summary(), collapse = "<br>")
          bin_counts <- c(
            binary_count_lines(rv$original_dataset, input$outcome_var, "Outcome (original)"),
            binary_count_lines(rv$original_dataset, input$predictor_var, "Predictor (original)")
          )
          if(!is.null(input$moderator_var)) {
            bin_counts <- c(bin_counts,
              binary_count_lines(rv$original_dataset, input$moderator_var, "Moderator (original)"))
          }
          diagnostics <- diagnostic_report(model)
          
          paste(
            "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
            "<div style='background-color: #e7f3ff; padding: 10px; margin-bottom: 15px; border-left: 4px solid #2196F3; font-family: Arial, sans-serif;'>",
            "<strong>Note on Assumption Checks:</strong><br>",
            "These assumption checks are always performed on the original dataset. Results update automatically based on your selected variables and standardized residual threshold value. ",
            "These assumption checks examine the <strong>outcome model</strong> (Y ~ X + M + W*X + covariates) only. ",
            "Mediator equations (e.g., M ~ X) are not checked here but may be examined separately if needed. ",
            "This approach is standard practice in mediation analysis and provides appropriate diagnostic information for the outcome equation.",
            "</div>",
            "<div style='background-color: #fff9e6; padding: 10px; margin-bottom: 15px; border-left: 4px solid #FF9800; font-family: Arial, sans-serif;'>",
            "<strong>Example Reporting Format:</strong><br>",
            "<em>Prior to analysis, we examined assumptions for the outcome model. Standardized residuals were calculated from a regression model predicting [outcome] from [predictor], [mediators], and [covariates]. A Q-Q plot indicated residuals were approximately normally distributed, and a Breusch-Pagan test confirmed homoscedasticity, χ²(df) = X.XX, p = .XX. Variance inflation factors (VIF) for all predictors were below 5, indicating no multicollinearity concerns. [X] cases with standardized residuals > 2.0 were identified as outliers [and removed/retained based on your decision].</em>",
            "</div>",
            "<strong>Note: Binary Outcome Detected</strong><br>",
            "<em>Your outcome variable is binary (0/1). PROCESS will use logistic regression for this analysis.</em><br><br>",
            "<strong>Important:</strong> Standard regression assumptions (normality, homoscedasticity) do not apply to logistic regression.<br>",
            "For binary outcomes, different diagnostic approaches are needed:<br>",
            "<ul>",
            "<li><strong>Linearity:</strong> Check linearity of continuous predictors with the logit of the outcome</li>",
            "<li><strong>Influential observations:</strong> Review leverage values and Cook's distance in the outlier summary above</li>",
            "<li><strong>Model fit:</strong> Use pseudo-R² measures (McFadden, Cox-Snell, Nagelkerke) shown in PROCESS output</li>",
            "<li><strong>Multicollinearity:</strong> VIF can still be calculated for predictors</li>",
            "</ul><br>",
            if(length(na.omit(bin_counts)) > 0) {
              paste(
                "<strong>Binary Variable Counts (original dataset):</strong><br>",
                paste(na.omit(bin_counts), collapse = "<br>"),
                "<br><br>"
              )
            } else { "" },
            outlier_text,
            "<br><br>",
            "<strong>Additional Diagnostics:</strong><br>",
            paste(diagnostics, collapse = "<br>"),
            "<br><em>Note: VIF calculated for all predictors. For binary outcomes, focus on model fit statistics and residual patterns rather than normality/homoscedasticity.</em>",
            "</div>",
            sep = ""
          )
        } else {
          model <- lm(model_formula, data = rv$original_dataset)
          outlier_text <- paste(outlier_summary(), collapse = "<br>")
          bin_counts <- c(
            binary_count_lines(rv$original_dataset, input$outcome_var, "Outcome (original)"),
            binary_count_lines(rv$original_dataset, input$predictor_var, "Predictor (original)")
          )
          if(!is.null(input$moderator_var)) {
            bin_counts <- c(bin_counts,
              binary_count_lines(rv$original_dataset, input$moderator_var, "Moderator (original)"))
          }
          normality <- check_normality(model)
          homoscedasticity <- test_homoscedasticity(model)
          diagnostics <- diagnostic_report(model)
          
          paste(
            "<div style='font-family: Courier, monospace; white-space: pre-wrap;'>",
            "<div style='background-color: #e7f3ff; padding: 10px; margin-bottom: 15px; border-left: 4px solid #2196F3; font-family: Arial, sans-serif;'>",
            "<strong>Note on Assumption Checks:</strong><br>",
            "These assumption checks are always performed on the original dataset. Results update automatically based on your selected variables and standardized residual threshold value. ",
            "These assumption checks examine the <strong>outcome model</strong> (Y ~ X + M + W*X + covariates) only. ",
            "Mediator equations (e.g., M ~ X) are not checked here but may be examined separately if needed. ",
            "This approach is standard practice in mediation analysis and provides appropriate diagnostic information for the outcome equation.",
            "</div>",
            "<div style='background-color: #fff9e6; padding: 10px; margin-bottom: 15px; border-left: 4px solid #FF9800; font-family: Arial, sans-serif;'>",
            "<strong>Example Reporting Format:</strong><br>",
            "<em>Prior to analysis, we examined assumptions for the outcome model. Standardized residuals were calculated from a regression model predicting [outcome] from [predictor], [mediators], and [covariates]. A Q-Q plot indicated residuals were approximately normally distributed, and a Breusch-Pagan test confirmed homoscedasticity, χ²(df) = X.XX, p = .XX. Variance inflation factors (VIF) for all predictors were below 5, indicating no multicollinearity concerns. [X] cases with standardized residuals > 2.0 were identified as outliers [and removed/retained based on your decision].</em>",
            "</div>",
            if(length(na.omit(bin_counts)) > 0) {
              paste(
                "<strong>Binary Variable Counts (original dataset):</strong><br>",
                paste(na.omit(bin_counts), collapse = "<br>"),
                "<br><br>"
              )
            } else { "" },
            outlier_text,
            "<br><br>",
            "<strong>Normality Test:</strong><br>",
            normality$text,
            "<br><em>Interpretation: A significant p-value (< .05) suggests non-normality. ",
            "However, with large samples, minor deviations often become significant. ",
            "Visual inspection of the Q-Q plot is often more informative.</em>",
            "<br><br>",
            "<strong>Homoscedasticity Test:</strong><br>",
            homoscedasticity,
            "<br><em>Interpretation: A significant p-value suggests non-constant variance. ",
            "Consider the Residuals vs Fitted plot for visual confirmation.</em>",
            "<br><br>",
            "<strong>Additional Diagnostics:</strong><br>",
            paste(diagnostics, collapse = "<br>"),
            "<br><em>Interpretation:<br>",
            "- VIF > 5 suggests potential multicollinearity issues<br>",
            "- With bootstrapping, these diagnostics become less crucial as bootstrap methods are more robust to violations</em>",
            "</div>",
            sep = ""
          )
        }
      }, error = function(e) {
        paste("<div class='alert alert-danger'>Error in assumption checks: ", e$message, "</div>")
      })
      
      writeLines(sprintf('
        <!DOCTYPE html>
        <html>
        <head>
          <style>
            body { font-family: Arial, sans-serif; padding: 20px; }
            table { border-collapse: collapse; margin: 10px 0; }
            th, td { border: 1px solid #dee2e6; padding: 8px; }
            th { background-color: #f8f9fa; }
          </style>
        </head>
        <body>
          %s
        </body>
      </html>
      ', assumption_html), file)
    },
    contentType = "text/html"
  )
  
  # Download handlers
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("process_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      req(analysis_results())
      output_content <- create_formatted_output(analysis_results())
      
      writeLines(sprintf('
        <!DOCTYPE html>
        <html>
        <head>
          <style>
            body { font-family: Courier, monospace; white-space: pre-wrap; padding: 20px; }
            table { border-collapse: collapse; }
            th, td { border: 1px solid #dee2e6; padding: 8px; }
          </style>
        </head>
        <body>
          %s
        </body>
        </html>
      ', output_content), file)
    },
    contentType = "text/html"
  )
  
  # Disable download buttons until analysis is run and results match current model
  observe({
    tryCatch({
      results <- analysis_results()
      current_model_num <- if(!is.null(input$process_model) && input$process_model != "") {
        as.numeric(input$process_model)
      } else {
        NULL
      }
      
      # Check if results exist and match current model
      has_results <- !is.null(results)
      if(has_results && !is.null(current_model_num) && !is.null(results$settings) && !is.null(results$settings$model)) {
        has_results <- results$settings$model == current_model_num
        if(!has_results) {
          print(paste("DEBUG: Download button disabled - results model", results$settings$model, "doesn't match current", current_model_num))
        }
      }
      
      shinyjs::toggleState("download_results", condition = has_results)
    }, error = function(e) {
      # If analysis_results() fails (e.g., inputs not initialized), disable all buttons
      shinyjs::disable("download_results")
    })
  })
  
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      ext <- if(input$filtered_data_format == "sav") "sav" else "csv"
      paste0("filtered_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    },
    content = function(file) {
      req(rv$original_dataset, input$outcome_var, input$predictor_var)
      
      outliers <- identify_outliers()
      
      if(outliers$count == 0) {
        stop("No outliers/influential cases found to remove")
      }
      
      filtered_data <- rv$original_dataset[-outliers$cases, ]
      
      if (input$filtered_data_format == "sav") {
        haven::write_sav(filtered_data, file)
      } else {
        write.csv(filtered_data, file, row.names = FALSE)
      }
    }
  )
  
  # ============================================================================
  # PLOTS MODULE - Moderation Visualizations
  # ============================================================================
  
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
  
  # Simple Slopes Plot
  output$slopes_plot <- renderPlot({
    # Only allow plots for Models 1 and 3
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    if(!model_num %in% c(1, 3)) {
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
    
    # Verify that the analysis results match the current model
    results <- analysis_results()
    if(is.null(results) || is.null(results$settings)) {
      return()
    }
    
    # Check if model matches
    current_model <- as.numeric(input$process_model)
    if(results$settings$model != current_model) {
      return()
    }
    
    data_used <- results$data_used
    outcome_is_binary <- is_binary_variable(data_used, input$outcome_var)
    
    # Determine model type
    model_num <- as.numeric(input$process_model)
    has_second_mod <- model_num %in% models_with_second_moderator
    
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
        print("DEBUG: COMPREHENSIVE RESULT OBJECT INSPECTION FOR MODEL 3")
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
        print("DEBUG: Checking results$plot_data structure")
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
          
          print("DEBUG: Searching for conditional effect data in plot_data matrix...")
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
            print(paste("DEBUG: Z variable range:", z_range[1], "to", z_range[2]))
            
            # Look for rows where column 1 values are within Z range and look like Z values
            # These should be the conditional effect rows
            potential_z_col <- plot_matrix[, 1]
            z_matches <- which(potential_z_col >= z_range[1] & potential_z_col <= z_range[2])
            
            if(length(z_matches) > 0) {
              print(paste("DEBUG: Found", length(z_matches), "rows with Z values in column 1"))
              
              # Check if these rows form a continuous sequence (conditional effect data)
              # vs scattered (simple slopes data)
              z_values <- unique(sort(potential_z_col[z_matches]))
              print(paste("DEBUG: Unique Z values found:", length(z_values)))
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
                  print("DEBUG: Column 1 values appear to form a sequence (conditional effect data)")
                  
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
                        print("DEBUG: Found conditional effect data section!")
                        cond_effect_data <- as.data.frame(cond_section[, 1:7])
                        colnames(cond_effect_data) <- c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")
                        print("DEBUG: Conditional effect data extracted:")
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
          print("DEBUG: This matrix is the plot_data (X, W, Z, predicted Y, SE, LLCI, ULCI), not conditional effect data")
          print("DEBUG: For Model 3, conditional effect data must be found elsewhere (text output or calculated from coefficients)")
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
              print("DEBUG: Result object has named elements:")
              print(names(result_obj))
              
              # Check for common names that might contain conditional effect data
              possible_names <- c("cond_effect", "conditional_effect", "probeplt", "probe", "effect", "modvals", "modvals3")
              for(name in possible_names) {
                if(name %in% names(result_obj)) {
                  elem <- result_obj[[name]]
                  print(paste("DEBUG: Found element named '", name, "'"))
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
                        print(paste("DEBUG: Found conditional effect data in result$", name))
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
              print("DEBUG: Checking all result object elements for conditional effect data...")
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
                    print(paste("DEBUG: Found potential conditional effect data in result[[", i, "]]"))
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
                    print(paste("DEBUG: Found potential conditional effect data in result[[", i, "]] (matrix)"))
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
            print("DEBUG: Searching PROCESS output for conditional effect section...")
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
              print(paste("DEBUG: Found conditional effect section at line", start_line))
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
              
              print(paste("DEBUG: Data section from line", data_start, "to", end_idx))
              
              if(end_idx > data_start) {
                data_lines <- process_output[data_start:end_idx]
                data_lines <- data_lines[grepl("^\\s*-?\\d", data_lines)]
                
                if(length(data_lines) > 0) {
                  print(paste("DEBUG: Found", length(data_lines), "potential data lines"))
                  print("First few data lines:")
                  print(head(data_lines, 5))
                  
                  # Parse the data - for Model 3, format is: Z, Effect, se, t, p, LLCI, ULCI
                  parsed <- tryCatch({
                    # Use read.table with proper column names
                    data_text <- paste(data_lines, collapse = "\n")
                    parsed <- read.table(text = data_text,
                                        col.names = c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI"),
                                        stringsAsFactors = FALSE, 
                                        fill = TRUE, 
                                        blank.lines.skip = TRUE,
                                        na.strings = c("NA", ""))
                    
                    # Convert to numeric
                    for(col in names(parsed)) {
                      parsed[[col]] <- as.numeric(parsed[[col]])
                    }
                    
                    parsed
                  }, error = function(e) {
                    print(paste("DEBUG: Error parsing table:", e$message))
                    # Try alternative parsing
                    tryCatch({
                      # Split by whitespace and parse manually
                      lines_split <- strsplit(data_lines, "\\s+")
                      # Filter to lines with at least 7 elements
                      valid_lines <- sapply(lines_split, function(x) length(x) >= 7)
                      if(sum(valid_lines) > 0) {
                        lines_split <- lines_split[valid_lines]
                        # Convert to matrix
                        data_matrix <- do.call(rbind, lapply(lines_split, function(x) {
                          as.numeric(x[1:7])
                        }))
                        as.data.frame(data_matrix, col.names = c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI"))
                      } else {
                        NULL
                      }
                    }, error = function(e2) {
                      print(paste("DEBUG: Alternative parsing also failed:", e2$message))
                      NULL
                    })
                  })
                  
                  if(!is.null(parsed) && nrow(parsed) > 0) {
                    print(paste("DEBUG: Parsed", nrow(parsed), "rows"))
                    print("First few rows of parsed data:")
                    print(head(parsed, 10))
                    
                    # Validate rows - all 7 columns should be numeric
                    valid_rows <- apply(parsed, 1, function(row) {
                      !any(is.na(row[1:7])) && is.numeric(row[1]) && is.numeric(row[2])
                    })
                    
                    if(sum(valid_rows) > 0) {
                      print(paste("DEBUG: Found", sum(valid_rows), "valid rows"))
                      # Assign to cond_effect_data - this is in the outer tryCatch scope
                      cond_effect_data <- parsed[valid_rows, 1:7]
                      colnames(cond_effect_data) <- c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")
                      print("DEBUG: Conditional effect data extracted:")
                      print(head(cond_effect_data, 10))
                      # Try to get ranges, but don't fail if it errors
                      tryCatch({
                        z_range_debug <- range(cond_effect_data$Z, na.rm = TRUE)
                        effect_range_debug <- range(cond_effect_data$Effect, na.rm = TRUE)
                        print("DEBUG: Z range:", z_range_debug)
                        print("DEBUG: Effect range:", effect_range_debug)
                      }, error = function(e) {
                        print("DEBUG: Could not calculate ranges (non-fatal)")
                      })
                    } else {
                      print("DEBUG: No valid rows found after parsing")
                    }
                  } else {
                    print("DEBUG: Parsing returned NULL or empty")
                  }
                } else {
                  print("DEBUG: No data lines found matching pattern")
                }
              }
            }
          }
        }  # Close if(is.null(cond_effect_data)) block
          
      }, error = function(e) {
        print(paste("DEBUG: Error in conditional effect extraction:", e$message))
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
              print(paste("DEBUG: Error parsing visualization data:", e$message))
              NULL
            })
            
            if(!is.null(parsed_viz) && ncol(parsed_viz) == 7 && nrow(parsed_viz) > 0) {
              print(paste("DEBUG: Successfully parsed visualization data with", nrow(parsed_viz), "rows"))
              print("DEBUG: First few rows:")
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
                    print(paste("DEBUG: Extracted conditional effect data from visualization section"))
                    print(paste("DEBUG: Using X =", x_median, "and W =", w_median))
                    print(paste("DEBUG: Found", nrow(cond_effect_data), "unique Z values"))
                    print("DEBUG: Z range:", range(cond_effect_data$Z, na.rm = TRUE))
                    print("DEBUG: Effect range:", range(cond_effect_data$Effect, na.rm = TRUE))
                  } else {
                    print("DEBUG: Not enough valid rows after filtering")
                    cond_effect_data <- NULL
                  }
                } else {
                  print("DEBUG: No rows found at median X and W")
                  cond_effect_data <- NULL
                }
              } else {
                print("DEBUG: Could not determine unique X or W values")
                cond_effect_data <- NULL
              }
            } else {
              print("DEBUG: Failed to parse visualization data as 7 columns")
              cond_effect_data <- NULL
            }
          } else {
            print("DEBUG: No data lines found in visualization section")
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
      print("DEBUG: Successfully assigned cond_effect_data to plot_data")
      print(paste("DEBUG: plot_data has", nrow(plot_data), "rows and", ncol(plot_data), "columns"))
      print(paste("DEBUG: plot_data column names:", paste(names(plot_data), collapse=", ")))
    } else {
      # Single moderation: X, W, predicted, se, LLCI, ULCI (6 columns)
      if(ncol(plot_data_df) < 6) {
        return()
      }
      plot_data <- data.frame(
        Predictor = as.numeric(plot_data_df[, 1]),
        Moderator = as.numeric(plot_data_df[, 2]),
        Outcome = as.numeric(plot_data_df[, 3]),
        LLCI = as.numeric(plot_data_df[, 5]),
        ULCI = as.numeric(plot_data_df[, 6])
      )
      active_moderator <- input$moderator_var
      
      # Remove any rows with NA values
      plot_data <- plot_data[complete.cases(plot_data), ]
      if(nrow(plot_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No valid plot data after filtering.", cex = 1.2)
        return()
      }
      
      # Get the 3 percentile moderator levels that PROCESS should generate (16th, 50th, 84th)
      # Calculate from the original data to ensure we get exactly 3 values
      if(is_binary_variable(data_used, active_moderator)) {
        # For binary moderators, use the actual unique values
        moderator_levels_raw <- sort(unique(data_used[[active_moderator]][!is.na(data_used[[active_moderator]])]))
      } else {
        # For continuous moderators, use percentiles (what PROCESS uses)
        moderator_levels_raw <- as.numeric(quantile(data_used[[active_moderator]], 
                                                    probs = c(0.16, 0.50, 0.84), 
                                                    na.rm = TRUE))
      }
      
      # Filter plot_data to only include rows where Moderator is close to one of these 3 percentile values
      # This handles floating point precision issues in PROCESS output
      plot_data$Moderator_matched <- FALSE
      for(mod_val in moderator_levels_raw) {
        # Match values within 0.1% of the percentile value
        tolerance <- abs(mod_val) * 0.001 + 0.01
        plot_data$Moderator_matched[abs(plot_data$Moderator - mod_val) < tolerance] <- TRUE
      }
      plot_data <- plot_data[plot_data$Moderator_matched, ]
      plot_data$Moderator_matched <- NULL  # Remove temporary column
      
      if(nrow(plot_data) == 0) {
        return()
      }
      
      # Now we should have exactly 3 moderator levels
      moderator_levels <- round(moderator_levels_raw, input$decimal_places)
    }
    
    # Verify plot_data exists before using it
    if(is.null(plot_data) || !is.data.frame(plot_data) || nrow(plot_data) == 0) {
      return()
    }
    
    # Create plot labels and determine plot type
    if(has_second_mod && !is.null(input$moderator2_var) && input$moderator2_var != "") {
      # For Model 3: Conditional effect plot (X*W effect across Z)
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
      plot_title <- if(input$slopes_title != "Simple Slopes Plot") input$slopes_title else "Conditional Effect Plot"
      
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
      
      plot_title <- if(input$slopes_title != "Simple Slopes Plot") input$slopes_title else "Simple Slopes Plot"
      
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
  })
  
  # Johnson-Neyman Plot
  output$jn_plot <- renderPlot({
    # Only allow plots for Models 1 and 3
    req(input$process_model)
    model_num <- as.numeric(input$process_model)
    if(!model_num %in% c(1, 3)) {
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
    
    # JN plots are only appropriate for Model 1 (simple moderation with one moderator)
    if(model_num != 1) {
      return()
    }
    
    if(!jn_available()) {
      return()
    }
    
    # Verify that the analysis results match the current model
    results <- analysis_results()
    if(is.null(results) || is.null(results$settings)) {
      return()
    }
    
    # Check if model matches
    current_model <- as.numeric(input$process_model)
    if(results$settings$model != current_model) {
      return()
    }
    
    tryCatch({
      process_output <- results$output
      start_idx <- which(grepl("Conditional effect of focal predictor", process_output))
      
      if(length(start_idx) == 0) {
        return()
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
        return()
      }
      
      # Extract data lines and filter out header/non-data lines
      data_lines <- process_output[data_start:end_idx]
      
      # Filter lines to only include those that look like data rows (start with number or negative number)
      data_lines <- data_lines[grepl("^\\s*-?\\d", data_lines)]
      
      if(length(data_lines) == 0) {
        return()
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
      transition_point <- jn_data$Moderator[which.min(abs(jn_data$p - 0.05))]
      
      x_label_text <- if(input$moderator_label != "") input$moderator_label else input$moderator_var
      y_label_text <- if(input$x_label != "") paste("Effect of", input$x_label) else paste("Effect of", input$predictor_var)
      
      p <- ggplot(jn_data, aes(x = Moderator, y = Effect)) +
        geom_ribbon(aes(ymin = LLCI, ymax = ULCI, fill = !significant), alpha = 0.4) +
        scale_fill_manual(values = if(input$use_color_lines) 
                          c(`TRUE` = "pink", `FALSE` = "lightblue") else 
                          c(`TRUE` = "grey70", `FALSE` = "grey50"),
                        labels = c(`TRUE` = "n.s.", `FALSE` = "p < .05"),
                        name = "") +
        geom_line(linewidth = 1, 
                 color = if(input$use_color_lines) "blue" else "black") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = transition_point,
                  linetype = "dashed", 
                  color = if(input$use_color_lines) "cyan" else "grey40") +
        theme_minimal() +
        labs(title = "Johnson-Neyman Plot",
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
      
      print(p)
    }, error = function(e) {
      # Return without showing error - UI will handle messaging
      return()
      print(paste("Error in JN plot:", e$message))
    })
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
      paste0("jn_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
    },
    content = function(file) {
      tryCatch({
        # Only allow downloads for Models 1 and 3
        req(input$process_model)
        model_num <- as.numeric(input$process_model)
        if(!model_num %in% c(1, 3)) {
          stop("Plots are only available for Models 1 and 3.")
        }
        
        req(analysis_results())
        process_output <- analysis_results()$output
        
        start_idx <- which(grepl("Conditional effect of focal predictor", process_output))
        if(length(start_idx) > 0) {
          # Find the data section - skip header line
          data_start <- start_idx + 2
          
          # Find the end of the data section
          end_idx <- which(grepl("^\\s*$|^Data for visualizing|^----------", process_output[data_start:length(process_output)]))[1]
          if(!is.na(end_idx)) {
            end_idx <- end_idx + data_start - 1
          } else {
            end_idx <- which(grepl("^-----------|^\\*+", process_output[data_start:length(process_output)]))[1]
            if(!is.na(end_idx)) {
              end_idx <- end_idx + data_start - 1
            } else {
              end_idx <- min(data_start + 50, length(process_output))
            }
          }
          
          if(!is.na(end_idx) && end_idx > data_start) {
            # Extract data lines and filter to only data rows
            data_lines <- process_output[data_start:end_idx]
            data_lines <- data_lines[grepl("^\\s*-?\\d", data_lines)]
            
            if(length(data_lines) > 0) {
              # Parse with improved error handling
              jn_data <- tryCatch({
                parsed <- read.table(text = paste(data_lines, collapse = "\n"),
                                    col.names = c("Moderator", "Effect", "se", "t", "p", "LLCI", "ULCI"),
                                    stringsAsFactors = FALSE,
                                    fill = TRUE,
                                    blank.lines.skip = TRUE)
                
                # Filter to only rows with exactly 7 numeric columns
                valid_rows <- apply(parsed, 1, function(row) {
                  all(!is.na(suppressWarnings(as.numeric(row[1:7]))))
                })
                
                if(sum(valid_rows) > 0) {
                  parsed[valid_rows, 1:7]
                } else {
                  # Fallback: manual parsing
                  parsed_lines <- lapply(data_lines, function(line) {
                    parts <- strsplit(trimws(line), "\\s+")[[1]]
                    nums <- suppressWarnings(as.numeric(parts))
                    nums <- nums[!is.na(nums)]
                    if(length(nums) >= 7) return(nums[1:7])
                    return(NULL)
                  })
                  valid_data <- do.call(rbind, Filter(Negate(is.null), parsed_lines))
                  if(!is.null(valid_data) && nrow(valid_data) > 0) {
                    colnames(valid_data) <- c("Moderator", "Effect", "se", "t", "p", "LLCI", "ULCI")
                    as.data.frame(valid_data)
                  } else {
                    stop("No valid data rows found")
                  }
                }
              }, error = function(e) {
                print(paste("Error parsing JN data in download:", e$message))
                stop(e)
              })
            
            jn_data$significant <- jn_data$p < 0.05
            transition_point <- jn_data$Moderator[which.min(abs(jn_data$p - 0.05))]
            
            x_label_text <- if(input$moderator_label != "") input$moderator_label else input$moderator_var
            y_label_text <- if(input$x_label != "") paste("Effect of", input$x_label) else paste("Effect of", input$predictor_var)
            
            p <- ggplot(jn_data, aes(x = Moderator, y = Effect)) +
              geom_ribbon(aes(ymin = LLCI, ymax = ULCI, fill = !significant), alpha = 0.4) +
              scale_fill_manual(values = if(input$use_color_lines) 
                                c(`TRUE` = "pink", `FALSE` = "lightblue") else 
                                c(`TRUE` = "grey70", `FALSE` = "grey50"),
                              labels = c(`TRUE` = "n.s.", `FALSE` = "p < .05"),
                              name = "") +
              geom_line(linewidth = 1, 
                       color = if(input$use_color_lines) "blue" else "black") +
              geom_hline(yintercept = 0, linetype = "dashed") +
              geom_vline(xintercept = transition_point,
                        linetype = "dashed", 
                        color = if(input$use_color_lines) "cyan" else "grey40") +
              theme_minimal() +
              labs(title = "Johnson-Neyman Plot",
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
            
              ggsave(file, plot = p, device = "jpeg", width = 10, height = 8, dpi = 600, units = "in")
            } else {
              stop("No valid JN data found")
            }
          } else {
            stop("Could not find JN data section boundaries")
          }
        } else {
          stop("JN data section not found in PROCESS output")
        }
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
      has_second_mod <- !is.null(model_num) && model_num %in% models_with_second_moderator
      if(has_second_mod && !is.null(input$moderator2_var) && input$moderator2_var != "") {
        paste0("conditional_effect_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
      } else {
        paste0("simple_slopes_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg")
      }
    },
    content = function(file) {
      tryCatch({
        # Only allow downloads for Models 1 and 3
        req(input$process_model)
        model_num <- as.numeric(input$process_model)
        if(!model_num %in% c(1, 3)) {
          stop("Plots are only available for Models 1 and 3.")
        }
        
        req(analysis_results())
        req(input$moderator_var)
        req(input$predictor_var)
        
        results <- analysis_results()
        data_used <- results$data_used
        outcome_is_binary <- is_binary_variable(data_used, input$outcome_var)
        
        # Determine model type
        has_second_mod <- model_num %in% models_with_second_moderator
        
        # Use plot data from PROCESS directly (plot=2, save=2) - always uses percentiles
        plot_data_df <- results$plot_data
        
        if(is.null(plot_data_df) || !is.data.frame(plot_data_df) || nrow(plot_data_df) == 0) {
          stop("Plot data not available. Please run the analysis first.")
        }
        
        # For Model 3, extract conditional effect data from result object (same as output$slopes_plot)
        if(has_second_mod && !is.null(input$moderator2_var) && input$moderator2_var != "") {
          # Try to extract from result object first (same logic as output$slopes_plot)
          cond_effect_data <- NULL
          result_obj <- results$result
          
          tryCatch({
            # Check if result object contains conditional effect data
            if(is.list(result_obj)) {
              # Look for conditional effect data in result object
              # This data should be in a data frame with columns: Z, Effect, se, t, p, LLCI, ULCI
              for(i in 1:length(result_obj)) {
                elem <- result_obj[[i]]
                if(is.data.frame(elem) && ncol(elem) >= 7) {
                  # Check if this looks like conditional effect data
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
                    cond_effect_data <- as.data.frame(elem[, 1:7])
                    colnames(cond_effect_data) <- c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")
                    break
                  }
                }
              }
            }
          }, error = function(e) {
            # If extraction from result fails, will try text parsing below
          })
          
          # If not found in result object, try parsing text output (fallback)
          if(is.null(cond_effect_data)) {
            process_output <- results$output
            
            # Look for "Conditional X*W interaction at values of the moderator Z:"
            start_idx <- which(grepl("Conditional X\\*W interaction at values of the moderator Z:", process_output, ignore.case = TRUE))
            
            if(length(start_idx) == 0) {
              start_idx <- which(grepl("Conditional effect.*X.*W.*interaction.*values.*moderator.*Z", process_output, ignore.case = TRUE))
            }
            
            if(length(start_idx) == 0) {
              start_idx <- which(grepl("Conditional effect.*focal predictor|Conditional effects.*focal predictor", process_output, ignore.case = TRUE))
            }
            
            if(length(start_idx) > 0) {
              start_line <- start_idx[1]
              data_start <- start_line + 2
              
              # Find where the data section ends - use safer approach
              search_start <- data_start
              search_end <- min(data_start + 50, length(process_output))
              search_subset <- process_output[search_start:search_end]
              end_candidates <- which(grepl("^\\s*$|^Data for visualizing|^----------|^\\*+", search_subset))
              
              if(length(end_candidates) > 0) {
                end_idx <- search_start + end_candidates[1] - 1
              } else {
                end_idx <- min(data_start + 100, length(process_output))
              }
              
              if(end_idx > data_start) {
                data_lines <- process_output[data_start:end_idx]
                data_lines <- data_lines[grepl("^\\s*-?\\d", data_lines)]
                
                if(length(data_lines) > 0) {
                  parsed <- tryCatch({
                    read.table(text = paste(data_lines, collapse = "\n"),
                              col.names = c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI"),
                              stringsAsFactors = FALSE,
                              fill = TRUE,
                              blank.lines.skip = TRUE)
                  }, error = function(e) NULL)
                  
                  if(!is.null(parsed) && nrow(parsed) > 0) {
                    # Convert to numeric and validate
                    for(col in names(parsed)) {
                      parsed[[col]] <- as.numeric(parsed[[col]])
                    }
                    valid_rows <- apply(parsed, 1, function(row) {
                      !any(is.na(row[1:7])) && is.numeric(row[1]) && is.numeric(row[2])
                    })
                    if(sum(valid_rows) > 0) {
                      cond_effect_data <- parsed[valid_rows, 1:7]
                      colnames(cond_effect_data) <- c("Z", "Effect", "se", "t", "p", "LLCI", "ULCI")
                    }
                  }
                }
              }
            }
          }
          
          if(is.null(cond_effect_data)) {
            stop("Conditional effect data not found. This plot requires probing interactions to be enabled.")
          }
          
          plot_data <- cond_effect_data
          active_moderator <- input$moderator2_var
        } else {
          # Single moderation: X, W, predicted, se, LLCI, ULCI (6 columns)
          if(ncol(plot_data_df) < 6) {
            stop("Plot data structure unexpected for moderation model.")
          }
          plot_data <- data.frame(
            Predictor = as.numeric(plot_data_df[, 1]),
            Moderator = as.numeric(plot_data_df[, 2]),
            Outcome = as.numeric(plot_data_df[, 3]),
            LLCI = as.numeric(plot_data_df[, 5]),
            ULCI = as.numeric(plot_data_df[, 6])
          )
          active_moderator <- input$moderator_var
        }
        
        # Create plot labels and determine plot type
        if(has_second_mod && !is.null(input$moderator2_var) && input$moderator2_var != "") {
          # For Model 3: Conditional effect plot
          # X-axis should be Z (second moderator), not W (first moderator)
          # Use moderator2_label if provided, otherwise use moderator2_var
          x_label_text <- if(!is.null(input$moderator2_label) && input$moderator2_label != "") input$moderator2_label else input$moderator2_var
          y_label_text <- paste("Conditional effect of", if(input$x_label != "") input$x_label else input$predictor_var, 
                               "*", if(input$moderator_label != "") input$moderator_label else input$moderator_var,
                               "on", if(input$y_label != "") input$y_label else input$outcome_var)
          # For Model 3, default title should be "Conditional Effect Plot"
          plot_title <- if(input$slopes_title != "Simple Slopes Plot") input$slopes_title else "Conditional Effect Plot"
          
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
          
          ggsave(file, plot = p, device = "jpeg", width = 10, height = 8, dpi = 600, units = "in")
        } else {
          # Single moderation: Get the 3 percentile moderator levels
          if(is_binary_variable(data_used, active_moderator)) {
            moderator_levels_raw <- sort(unique(data_used[[active_moderator]][!is.na(data_used[[active_moderator]])]))
          } else {
            moderator_levels_raw <- as.numeric(quantile(data_used[[active_moderator]], 
                                                        probs = c(0.16, 0.50, 0.84), 
                                                        na.rm = TRUE))
          }
          
          # Filter plot_data to only include rows where Moderator is close to one of these 3 percentile values
          plot_data$Moderator_matched <- FALSE
          for(mod_val in moderator_levels_raw) {
            tolerance <- abs(mod_val) * 0.001 + 0.01
            plot_data$Moderator_matched[abs(plot_data$Moderator - mod_val) < tolerance] <- TRUE
          }
          plot_data <- plot_data[plot_data$Moderator_matched, ]
          plot_data$Moderator_matched <- NULL
          
          if(nrow(plot_data) == 0) {
            stop("No valid plot data after filtering to percentile levels.")
          }
          
          moderator_levels <- round(moderator_levels_raw, input$decimal_places)
          predictor_range <- range(plot_data$Predictor, na.rm = TRUE)
          
          # Create plot labels
          y_label_text <- if(outcome_is_binary) {
            if(input$y_label != "") paste(input$y_label, "(Probability)") else "Predicted Probability"
          } else {
            if(input$y_label != "") input$y_label else input$outcome_var
          }
          
          x_label_text <- if(input$x_label != "") input$x_label else input$predictor_var
          mod_label_text <- if(input$moderator_label != "") input$moderator_label else paste0(active_moderator, " Levels")
          
          plot_title <- if(input$slopes_title != "Simple Slopes Plot") input$slopes_title else "Simple Slopes Plot"
          
          # Single moderation: single plot with 3 lines
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
          
          ggsave(file, plot = p, device = "jpeg", width = 10, height = 8, dpi = 600, units = "in")
        }
      }, error = function(e) {
        print(paste("Error saving slopes plot:", e$message))
        # Create an error message file instead
        writeLines(paste("Error generating plot:", e$message), file)
      })
    }
  )
  
  # Dynamic button label for download slopes plot
  output$download_slopes_button <- renderUI({
    model_num <- tryCatch(as.numeric(input$process_model), error = function(e) NULL)
    has_second_mod <- !is.null(model_num) && model_num %in% models_with_second_moderator
    has_z_var <- !is.null(input$moderator2_var) && input$moderator2_var != ""
    
    button_label <- if(has_second_mod && has_z_var) {
      "Download Conditional Effect Plot (JPG)"
    } else {
      "Download Simple Slopes Plot (JPG)"
    }
    
    downloadButton("download_slopes", button_label, 
                  class = "btn-success", 
                  style = "background-color: #90EE90; border-color: #90EE90; color: #000;")
  })
  
  # Disable plot download buttons until analysis is run
  observe({
    tryCatch({
      has_results <- !is.null(analysis_results())
      shinyjs::toggleState("download_slopes", condition = has_results)
      shinyjs::toggleState("download_assumptions", condition = !is.null(input$outcome_var) && input$outcome_var != "")
    }, error = function(e) {
      shinyjs::disable("download_slopes")
      shinyjs::disable("download_assumptions")
    })
  })
  
  # Auto-populate plot labels from selected variables (only for moderation models)
  # This automatically updates labels when variables are selected, matching the old app behavior
  # Users can then edit these labels if they want clearer/more descriptive text
  observe({
    if(!is.null(input$process_model) && input$process_model %in% c("1", "2", "3", "5", "14", "15", "58", "59", "74")) {
      # Only update if all required variables are selected (matching old app req() behavior)
      if(!is.null(input$predictor_var) && input$predictor_var != "" &&
         !is.null(input$outcome_var) && input$outcome_var != "" &&
         !is.null(input$moderator_var) && input$moderator_var != "") {
        updateTextInput(session, "x_label", value = input$predictor_var)
        updateTextInput(session, "y_label", value = input$outcome_var)
        updateTextInput(session, "moderator_label", value = input$moderator_var)
        
        # For models with second moderator, also auto-populate moderator2_label
        model_num <- as.numeric(input$process_model)
        if(model_num %in% models_with_second_moderator && 
           !is.null(input$moderator2_var) && input$moderator2_var != "") {
          updateTextInput(session, "moderator2_label", value = input$moderator2_var)
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
