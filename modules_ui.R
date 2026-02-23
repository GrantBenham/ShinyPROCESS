# ============================================================================
# UI DEFINITION MODULE
# ============================================================================
# Copyright (c) 2026 Dr. Grant Benham. See LICENSE for usage terms.
# This module contains the complete UI definition for the gbPROCESS application
# Extracted from gbPROCESS.R as part of Stage 2 modularization

process_model_choices <- if(exists("VALID_USER_MODELS", inherits = TRUE)) {
  c(
    "Select model" = "",
    setNames(as.character(VALID_USER_MODELS), as.character(VALID_USER_MODELS))
  )
} else {
  # Fallback to previous static list if specs are unavailable
  c(
    "Select model" = "",
    setNames(as.character(c(1:22, 28:29, 58:73, 75:76, 80:92)),
             c(1:22, 28:29, 58:73, 75:76, 80:92))
  )
}

# Runtime-aware download button helper:
# - Shinylive (Chromium): remove HTML5 "download" attribute to avoid failed downloads.
# - Local R Shiny: keep default shiny::downloadButton behavior.
is_shinylive_runtime <- local({
  runtime_val <- tryCatch({
    if(!file.exists("runtime.txt")) {
      return("rshiny")
    }
    lines <- readLines("runtime.txt", n = 1L, warn = FALSE)
    if(length(lines) == 0) {
      return("rshiny")
    }
    val <- tolower(trimws(lines[[1]]))
    sub("^\ufeff", "", val)
  }, error = function(e) {
    "rshiny"
  })
  identical(runtime_val, "shinylive")
})

downloadButton <- if(is_shinylive_runtime) {
  function(...) {
    tag <- shiny::downloadButton(...)
    tag$attribs$download <- NULL
    tag
  }
} else {
  shiny::downloadButton
}

ui <- fluidPage(
  useShinyjs(),
  tags$style(type="text/css", "body { max-width: 1800px; margin: auto; }"),
  titlePanel("ShinyPROCESS - R Shiny App for Hayes PROCESS Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "output.process_ready !== true",
        htmlOutput("process_warning")
      ),
      h4("Upload Data"),
      div(
        id = "file_input_div",
        fileInput("data_file", "Choose CSV or SAV File", accept = c(".csv", ".sav"))
      ),
      htmlOutput("process_status_line"),
      
      # Save/Load Analysis Settings
      h4("Analysis Settings"),
      conditionalPanel(
        condition = "output.dataset_loaded === true",
        div(
          tags$div(
            title = "Load previously saved analysis settings from a JSON file. The file must contain variables that exist in your current dataset. If any variables are missing, an error message will be displayed.",
            div(
              id = "load_settings_file",
              fileInput("load_settings_file", "Load Analysis Settings", 
                       accept = ".json",
                       buttonLabel = "Choose JSON File",
                       placeholder = "No file selected"),
              style = "margin-bottom: 10px;"
            )
          ),
          tags$div(
            title = "Save all current analysis settings (model, variables, options, plot labels) to a JSON file. This file can be loaded later to restore your exact analysis configuration.",
            downloadButton("save_settings", "Save Analysis Settings", 
                           class = "btn-info", 
                           style = "width: 100%;")
          )
        )
      ),
      conditionalPanel(
        condition = "output.dataset_loaded !== true",
        div(
          style = "color: #666; font-style: italic; padding: 10px;",
          p("Load a dataset to enable save/load settings.")
        )
      ),
      
      h4("Model Selection"),
      tags$div(
        title = "Select the PROCESS model number that matches your research question. Model 74 is not directly selectable - use Model 4 with 'Allow X by M interaction' enabled. Changing the model clears all variable selections and previous results.",
        selectInput("process_model", "PROCESS Model Number",
                    choices = process_model_choices,
                    selected = "")
      ),
      
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
          h5("Univariate Outlier Screening (Detection Only)"),
          tags$div(
            title = "Optional screening of all selected continuous variables (X, Y, mediators, moderators, and continuous covariates) for unusual values. This flags potential univariate outliers for review only and does not exclude any cases from the analysis dataset.",
            checkboxInput(
              "use_univariate_outlier_screen",
              "Enable univariate outlier screening (detection only)",
              value = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.use_univariate_outlier_screen == true",
            tags$div(
              title = "Choose a screening rule. IQR fences are common and easy to interpret. MAD-based robust z scores are more robust to skew and extreme values.",
              selectInput(
                "univariate_outlier_method",
                "Univariate screening method",
                choices = c(
                  "IQR fences (Tukey)" = "iqr",
                  "MAD-based robust z-score" = "mad"
                ),
                selected = "iqr"
              )
            ),
            conditionalPanel(
              condition = "input.univariate_outlier_method == 'iqr'",
              tags$div(
                title = "Tukey IQR fence multiplier. 1.5 is a common default; larger values flag fewer cases.",
                numericInput(
                  "univariate_iqr_multiplier",
                  "IQR multiplier (k)",
                  value = 1.5, min = 0.5, max = 5, step = 0.1
                )
              )
            ),
            conditionalPanel(
              condition = "input.univariate_outlier_method == 'mad'",
              tags$div(
                title = "Threshold for absolute MAD-based robust z scores. 3.5 is a common default; larger values flag fewer cases.",
                numericInput(
                  "univariate_mad_threshold",
                  "MAD robust z threshold (|z| >)",
                  value = 3.5, min = 2, max = 10, step = 0.1
                )
              )
            ),
            tags$div(
              style = "font-size: 12px; color: #555; margin: 6px 0 12px 0;",
              "Detection only: flagged values are shown in the Assumption Checks tab for review. This does not change the current multivariate standardized residual / Cook's distance filtering workflow."
            )
          ),

          # Outlier detection settings (multivariate / model-based)
          conditionalPanel(
            condition = "output.outcome_is_continuous === true",
            h5("Multivariate Outlier Detection (Continuous Outcomes)"),
            tags$div(
              title = "Cases with |standardized residual| > threshold will be identified as outliers. Standardized residuals represent how many standard deviations an observed value deviates from the model's prediction. Common thresholds: |SR| > 2 (standard), |SR| > 2.5 (stringent), |SR| > 3 (very conservative).",
              numericInput("residual_threshold", "Standardized Residual Threshold", 
                          value = 2, min = 1, max = 10, step = 0.1)
            )
          ),
          conditionalPanel(
            condition = "output.outcome_is_continuous === false",
            h5("Multivariate Influential Case Detection (Binary Outcomes)"),
            tags$div(
              tags$label("Cook's Distance Threshold:", style = "font-weight: bold;"),
              radioButtons(
                inputId = "cooks_threshold_type",
                label = NULL,
                choiceNames = list(
                  tags$span(
                    "Conservative (4/n)",
                    title = "Conservative threshold: 4/n (where n = sample size). This is the recommended default and adjusts for sample size. It is usually stricter and flags more cases as influential than a threshold of 1.0."
                  ),
                  tags$span(
                    "Liberal (1.0)",
                    title = "Liberal threshold: Fixed value of 1.0. This is less strict and usually flags fewer cases as influential than 4/n."
                  ),
                  tags$span(
                    "Custom",
                    title = "Custom threshold: Specify your own threshold between 0 and 1. Lower values are stricter and flag more cases as influential. Allows fine-tuning based on your analysis needs."
                  )
                ),
                choiceValues = c("conservative", "liberal", "custom"),
                selected = "conservative"
              )
            ),
            conditionalPanel(
              condition = "input.cooks_threshold_type == 'custom'",
              tags$div(
                title = "Enter a custom Cook's Distance threshold between 0 and 1. Lower values are stricter and flag more cases as influential.",
                numericInput("cooks_threshold_custom", "Custom Cook's Distance Threshold", 
                            value = 0.01, min = 0, max = 1, step = 0.001)
              )
            )
          )
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
            tags$div(
              tags$label("Mean Centering:", style = "font-weight: bold;"),
              radioButtons(
                inputId = "centering",
                label = NULL,
                choiceNames = list(
                  tags$span(
                    "No centering",
                    title = "No centering: Uses variables in their original scale. Appropriate when variables are already centered or when you want to interpret effects at zero values of predictors."
                  ),
                  tags$span(
                    "All variables that define products",
                    title = "All variables that define products: Centers all variables (both continuous and categorical) that are involved in interaction terms. Simplifies interpretation of main effects when interactions are present."
                  ),
                  tags$span(
                    "Only continuous variables that define products",
                    title = "Only continuous variables that define products: Centers only continuous variables in interaction terms, leaving categorical variables uncentered. Useful when you want to preserve the original scale of categorical variables."
                  )
                ),
                choiceValues = c("0", "1", "2"),
                selected = "0"
              )
            )
          )
        ),
        
        # Bootstrap Settings (collapsible)
        tags$details(id = "details_bootstrap_options",
          tags$summary(style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9; margin-top: 15px;", 
                      "Bootstrap Settings"),
          div(style = "margin-left: 15px; margin-top: 10px;",
            tags$div(
              title = "Bootstrap resampling provides robust confidence intervals that don't assume normality. Recommended for both mediation and moderation analyses. Higher sample counts provide more stable estimates but take longer to compute.",
              checkboxInput("use_bootstrap", "Use bootstrapping", TRUE)
            ),
            conditionalPanel(
              condition = "input.use_bootstrap == true",
              tags$div(
                title = "Number of bootstrap resamples. 5000 is generally sufficient for most analyses. Higher values (up to 10000) provide more stable estimates but take longer.",
                numericInput("boot_samples", "Number of bootstrap samples:", 5000, min = 1000, max = 10000)
              ),
              tags$div(
                tags$label("Bootstrap Confidence Interval Method:", style = "font-weight: bold;"),
                radioButtons(
                  inputId = "bootstrap_ci_method",
                  label = NULL,
                  choiceNames = list(
                    tags$span(
                      "Percentile bootstrap",
                      title = "Percentile bootstrap: Uses the percentiles (e.g., 2.5th and 97.5th) of the bootstrap distribution to form confidence intervals. Simpler and more straightforward. Generally recommended for most analyses."
                    ),
                    tags$span(
                      "Bias-corrected bootstrap",
                      title = "Bias-corrected bootstrap: Adjusts for bias in the bootstrap distribution. Can be more accurate than percentile bootstrap but may be less stable with small samples. Use when you suspect bias in the distribution."
                    )
                  ),
                  choiceValues = c("0", "1"),
                  selected = "0"
                )
              )
            ),
            tags$div(
              title = "Confidence level for confidence intervals. Common choices: 90%, 95% (default), 99%.",
              numericInput("conf_level", "Confidence Level (%)", 95, min = 80, max = 99, step = 1)
            ),
            tags$div(
              title = "Set a specific seed for random number generation to ensure reproducible results. Leave blank for a random seed (different results each run). Enter a number (1-999999) to set a specific seed.",
              numericInput("seed", "Random Seed (optional)", value = NA, min = 1, max = 999999)
            )
          )
        ),
        
        # Moderator Settings (collapsible) - Only for moderation models
        conditionalPanel(
          condition = "output.is_moderation_model === true",
          tags$details(id = "details_moderator_settings",
            tags$summary(style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9; margin-top: 15px;", 
                        "Moderator Settings"),
            div(style = "margin-left: 15px; margin-top: 10px;",
              tags$div(
                tags$label("Moderator values for probing and plots:", style = "font-weight: bold;"),
                radioButtons(
                  inputId = "conditioning_values",
                  label = NULL,
                  choiceNames = list(
                    tags$span(
                      "Percentiles (16th, 50th, 84th)",
                      title = "Uses the 16th, 50th (median), and 84th percentiles of the moderator distribution. This affects conditional effects in the text output and the visualization data used for plots. More robust to outliers and non-normal distributions. Recommended when the moderator distribution is skewed or has outliers."
                    ),
                    tags$span(
                      "Moments (Mean +/- 1 SD)",
                      title = "Uses the mean and one standard deviation above and below the mean. This affects conditional effects in the text output and the visualization data used for plots. More interpretable and commonly used in reporting."
                    )
                  ),
                  choiceValues = c("1", "0"),
                  selected = "1"
                ),
                tags$small(
                  "This setting also drives the visualization data used for plots.",
                  style = "display: block; margin-top: 4px; color: #555;"
                )
              )
            )
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
            tags$div(
              title = "Standard error methods: OLS (default, assumes homoscedasticity). HC0-HC4 are robust standard errors that account for heteroscedasticity (unequal variance). Use robust standard errors if you suspect heteroscedasticity. HC3 is recommended for small samples, HC4 is more conservative and good for influential cases.",
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
              )
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
            ),
            
            # Probing Moderation Options (at end of Output Options) - Only for moderation models
            conditionalPanel(
              condition = "output.is_moderation_model === true",
              tags$div(
                title = "Probing moderation shows conditional effects at different levels of the moderator. When enabled, displays how the effect of X on Y changes across moderator values.",
                checkboxInput("probe_interactions", "Probe moderation", TRUE)
              ),
              conditionalPanel(
                condition = "input.probe_interactions == true",
                tags$div(
                  title = "Enter the threshold for when to probe interactions (e.g., 'p < .10' or 'p < .05'). Interactions meeting this threshold will be probed.",
                  textInput("probe_threshold", "When to probe:", value = "p < .10")
                ),
                tags$div(
                  title = "Controls whether the 'Moderator value(s) defining Johnson-Neyman significance region(s)' section appears in the results text. Note: Johnson-Neyman plot requires 'Probe moderation' to be enabled and the interaction p-value to meet the 'When to probe' threshold. The plot will be generated automatically when these conditions are met, regardless of this checkbox setting.",
                  checkboxInput("show_jn_regions", "Show Johnson-Neyman significance regions in output", TRUE)
                )
              )
            )
          )
        ),
        
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
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(id = "tabset_panel",
        tabPanel("Assumption Checks",
          div(style = "margin-bottom: 20px;",
            h4("Detailed Assumption Check Results"),
            p(style = "margin-top: 8px; color: #555;",
              "Assumption checks are based on the variables currently selected and update as selections change. ",
              "For final interpretation, review these checks after all required variables for the selected model are specified."
            ),
            conditionalPanel(
              condition = "output.all_vars_selected_for_assumptions",
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
              uiOutput("univariate_outlier_summary"),
              
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
                uiOutput("violin_plot_ui"),
                conditionalPanel(
                  condition = "output.has_continuous_covariates === true",
                  br(),
                  h4("Covariate Distributions"),
                  uiOutput("violin_plot_covariates_ui")
                )
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
            condition = "output.is_plot_model === true",
            div(style = "margin-bottom: 18px;",
              tags$details(
                tags$summary(
                  style = "cursor: pointer; font-weight: bold; background-color: #e3f2fd; color: #1976d2; padding: 8px; border-radius: 4px; border: 1px solid #90caf9;",
                  "Live Plot Settings"
                ),
                div(style = "margin-left: 15px; margin-top: 10px;",
                  h5("Simple Slopes Plot Settings"),
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
            h4("Plots"),
            conditionalPanel(
              condition = "input.process_model == '3'",
              h4("Stacked Simple Slopes Plot"),
              plotOutput("stacked_slopes_plot", height = "500px", width = "800px"),
              br(),
              div(style = "margin-top: 20px;",
                downloadButton("download_stacked_slopes", "Download Stacked Simple Slopes Plot (JPG)",
                  class = "btn-success",
                  style = "background-color: #90EE90; border-color: #90EE90; color: #000;")
              ),
              br(), br(),
              h4("Conditional Effect Plot"),
              plotOutput("conditional_effect_plot", height = "500px", width = "800px"),
              br(),
              div(style = "margin-top: 20px;",
                downloadButton("download_conditional_effect", "Download Conditional Effect Plot (JPG)",
                  class = "btn-success",
                  style = "background-color: #90EE90; border-color: #90EE90; color: #000;")
              )
            ),
            conditionalPanel(
              condition = "input.process_model == '1'",
              h4("Simple Slopes Plot"),
              plotOutput("slopes_plot", height = "500px", width = "800px"),
              br(),
              div(style = "margin-top: 20px;",
                downloadButton("download_slopes", "Download Simple Slopes Plot (JPG)",
                  class = "btn-success",
                  style = "background-color: #90EE90; border-color: #90EE90; color: #000;")
              )
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
        ),
        tabPanel("Model Diagram",
          conditionalPanel(
            condition = "output.analysis_ready === true",
            div(style = "margin-bottom: 15px;",
              h4("Model Diagram"),
              p("Black-and-white conceptual and statistical diagrams generated from current analysis output."),
              fluidRow(
                column(
                  4,
                  selectInput(
                    "diagram_coef_mode",
                    "Coefficient Mode",
                    choices = c(
                      "Unstandardized coefficients" = "raw"
                    ),
                    selected = "raw"
                  )
                ),
                column(
                  4,
                    tags$div(
                      style = "padding-top: 4px;",
                      selectInput(
                        "diagram_coef_digits",
                        "Diagram Decimal Places (b, CI)",
                        choices = c("3" = "3", "2" = "2"),
                        selected = "3"
                      ),
                      sliderInput(
                        "diagram_coef_font_size",
                        "Coefficient Label Font Size",
                        min = 2.6,
                        max = 3.8,
                        value = 3.3,
                        step = 0.1
                      ),
                      selectInput(
                        "diagram_coef_orientation",
                        "Coefficient Label Orientation",
                        choices = c(
                          "Follow path" = "line",
                          "Horizontal" = "horizontal"
                        ),
                        selected = "line"
                      ),
                      conditionalPanel(
                        condition = "output.diagram_show_mod_controls === true",
                        checkboxInput("diagram_show_mod_main_effects", "Show moderator main effects", value = TRUE),
                        checkboxInput("diagram_show_interactions", "Show interaction terms", value = TRUE)
                      ),
                      checkboxInput("diagram_include_stars", "Include significance stars", value = TRUE),
                      checkboxInput("diagram_include_ci", "Include confidence intervals (PROCESS model-table CIs; not bootstrap CIs)", value = FALSE),
                      checkboxInput("diagram_include_p", "Include p-values", value = FALSE),
                      checkboxInput("diagram_fill_variable_boxes_blue", "Fill variable label boxes (light blue)", value = FALSE)
                  )
                )
              ),
              NULL
            ),
            uiOutput("diagram_label_editor"),
            tags$div(
              title = "Use this after renaming variables or changing diagram display checkboxes so interaction labels and path annotations refresh from current settings.",
              actionButton(
                "diagram_regenerate",
                "Regenerate Diagrams",
                class = "btn-primary"
              ),
              style = "margin: 6px 0 12px 0;"
            ),
            htmlOutput("diagram_eligibility_msg"),
            h5("Conceptual Diagram"),
            plotOutput("conceptual_diagram_plot", height = "620px", width = "100%"),
            div(
              style = "margin: 8px 0 18px 0;",
              downloadButton(
                "download_conceptual_diagram",
                "Download Conceptual (JPG)",
                class = "btn-success",
                style = "background-color: #90EE90; border-color: #90EE90; color: #000;"
              )
            ),
            br(),
            h5("Statistical Diagram"),
            plotOutput("statistical_diagram_plot", height = "860px", width = "100%"),
            div(
              style = "margin: 8px 0 8px 0;",
              downloadButton(
                "download_statistical_diagram",
                "Download Statistical (JPG)",
                class = "btn-success",
                style = "background-color: #90EE90; border-color: #90EE90; color: #000;"
              )
            ),
            uiOutput("graphviz_statistical_ui"),
            htmlOutput("model_diagram_notes")
          ),
          conditionalPanel(
            condition = "output.analysis_ready === false",
            p("Run an analysis to generate a model diagram.")
          )
        ),
        tabPanel("User Guide",
          div(style = "padding: 10px 5px;",
            h3("ShinyPROCESS User Guide"),
            p("ShinyPROCESS is a graphical interface for Hayes' PROCESS for R (version 5.0)."),
            p("This app was developed by Dr. Grant Benham, The University of Texas Rio Grande Valley (grant.benham@utrgv.edu)."),
            tags$hr(),

            h4("IMPORTANT"),
            p(
              tags$strong("Hayes' process.R file is not included in this GitHub repository due to copyright restrictions, but it is required for PROCESS analyses to run.")
            ),
            p(
              "This app is designed for PROCESS for R version 5.0 and therefore requires that version to run the PROCESS analysis. The filename and version number (contained within process.R) are used for verification."
            ),
            p(
              "Download the file and place it in the same folder as ",
              tags$code("gbPROCESS.R"),
              "."
            ),
            p(
              tags$strong("PROCESS download: "),
              tags$a(
                href = "https://haskayne.ucalgary.ca/CCRAM/resource-hub",
                target = "_blank",
                "https://haskayne.ucalgary.ca/CCRAM/resource-hub"
              )
            ),
            p(
              tags$strong("PROCESS information: "),
              tags$a(
                href = "https://processmacro.org/index.html",
                target = "_blank",
                "https://processmacro.org/index.html"
              )
            ),
            p(
              tags$strong("Runtime note: "),
              "In local R Shiny, placing ",
              tags$code("process.R"),
              " in the app folder can satisfy this requirement automatically. In Shinylive (browser build), upload ",
              tags$code("process.R"),
              " each launch/session."
            ),

            tags$hr(),
            h4("Workflow"),
            tags$ol(
              tags$li(
                tags$strong("Upload Data"),
                ": Use the ",
                tags$code("Choose CSV or SAV File"),
                " control in the sidebar to load your dataset. The PROCESS status box under Upload Data shows whether PROCESS is ready."
              ),
              tags$li(
                tags$strong("Save Analysis Settings (then Load Later)"),
                ": Under ",
                tags$code("Analysis Settings"),
                ", save your current setup (model number, selected variables, and analysis/plot options) to a JSON file. You can later use the load option to restore those saved settings."
              ),
              tags$li(
                tags$strong("Select Model"),
                ": Choose a supported PROCESS model under ",
                tags$code("Model Selection"),
                ". Unsupported models are hidden."
              ),
              tags$li(
                tags$strong("Select Variables"),
                ": Use the ",
                tags$code("Select Variables"),
                " section to pick X, Y, and model-specific W/Z/M inputs. The app customizes visible inputs based on the selected model and enforces model-specific mediator count limits."
              ),
              tags$li(
                tags$strong("Review Assumption Checks"),
                ": Assumption outputs update as selected variables change. Review the final interpretation after all required variables are selected."
              ),
              tags$li(
                tags$strong("Configure PROCESS Options"),
                ": Use the options sections (centering, bootstrap, moderator, advanced, output, and live plot settings). Hover over controls to view tooltips that explain what each option does."
              ),
              tags$li(
                tags$strong("Run Analysis"),
                ": Choose whether to run the PROCESS analysis with your original dataset or with flagged cases removed (standardized residual outliers for continuous outcomes; Cook's distance influential cases for binary outcomes, if identified)."
              ),
              tags$li(
                tags$strong("Review Results and Plots"),
                ": Results are shown in the Analysis Results tab. Plots are generated only for Models 1 and 3."
              ),
              tags$li(
                tags$strong("Review Model Diagrams"),
                ": Use the ",
                tags$code("Model Diagram"),
                " tab to view conceptual and statistical diagrams for supported models (currently ",
                tags$code("1,2,3,4,5,6,7,8,14"),
                "). The statistical diagram uses the same ggplot rendering path for on-screen display and JPG export. An experimental Graphviz comparison may also be shown when DiagrammeR is available."
              ),
              tags$li(
                tags$strong("Adjust Plot Display (Models 1 and 3)"),
                ": At the top of the ",
                tags$code("Plots"),
                " tab (",
                tags$code("Live Plot Settings"),
                "), you can edit displayed variable labels, change formatting, and turn confidence intervals on or off. These changes are updated in real time."
              ),
              tags$li(
                tags$strong("Adjust Diagram Display"),
                ": In the ",
                tags$code("Model Diagram"),
                " tab, you can choose coefficient mode (unstandardized/standardized when available), edit diagram labels, toggle stars/CIs/p-values, set coefficient label orientation/font size, and optionally fill variable label boxes light blue. Confidence interval labels in diagrams use PROCESS model-table CIs (not bootstrap coefficient CIs)."
              ),
              tags$li(
                tags$strong("Use Assumption Checks for Screening"),
                ": The Assumption Checks section includes optional univariate outlier screening (IQR or MAD robust z-score; detection only) and model-based multivariate checks (standardized residuals for continuous outcomes, Cook's distance for binary outcomes). Univariate flags are for review and do not automatically remove cases from analyses."
              )
            ),

            tags$hr(),
            h4("Default Settings Guidance"),
            tags$ul(
              tags$li(tags$strong("Bootstrapping"), ": Enabled by default (5000 samples, percentile bootstrap CI) as a practical default for PROCESS analyses and interval estimation."),
              tags$li(tags$strong("Standard Errors"), ": OLS is the default for comparability with common PROCESS examples. If heteroskedasticity is a concern, consider HC3 (or HC4 for a more conservative option)."),
              tags$li(tags$strong("Standardized Coefficients"), ": Off by default. Turn on when you want standardized effects reported and available in the Model Diagram tab."),
              tags$li(tags$strong("Normal Theory Tests"), ": Off by default. Leave off unless you specifically want normal-theory tests (e.g., z-tests) rather than the default t-test framework."),
              tags$li(tags$strong("Model Diagnostics and Assumptions (PROCESS output option)"), ": Off by default to keep results output concise. Enable only when you want PROCESS's additional regression diagnostics printed in the analysis output.")
            ),
            p(style = "color: #555;",
              "Bootstrapping can improve interval estimation, but it does not correct data-entry errors or eliminate the influence of extreme observations. Use the Assumption Checks tab to review both univariate screening flags and model-based diagnostics."
            ),
            tags$hr(),
            h4("Outlier Scope"),
            tags$ul(
              tags$li("Flagged-case removal in this app is based on model diagnostics (standardized residuals for continuous outcomes; Cook's distance for binary outcomes)."),
              tags$li("The app now includes optional univariate outlier screening (IQR or MAD robust z-score) in the Assumption Checks section, but this is detection-only and does not currently drive case removal."),
              tags$li("Use violin plots in Assumption Checks to review distributions before deciding whether to run sensitivity analyses with the model-based outlier/influential-case exclusion option.")
            ),

            tags$hr(),
            h4("Saving and Exporting Files"),
            tags$ul(
              tags$li(tags$strong("Analysis Settings JSON"), ": Save your current configuration first, then load that saved JSON later to restore the same setup."),
              tags$li(tags$strong("Assumption Checks HTML"), ": Download the assumption output text from the Assumption Checks tab."),
              tags$li(tags$strong("Results HTML"), ": Download full PROCESS analysis output from the sidebar."),
              tags$li(tags$strong("Plots (JPG)"), ": Download available plots in the Plots tab (Models 1 and 3 only)."),
              tags$li(tags$strong("Conceptual Diagram (JPG)"), ": Download from the Model Diagram tab. Uses the same ggplot rendering path as the on-screen conceptual diagram."),
              tags$li(tags$strong("Statistical Diagram (JPG)"), ": Download from the Model Diagram tab. Uses the same ggplot rendering path as the on-screen statistical diagram."),
              tags$li(tags$strong("Reduced Dataset"), ": Download a dataset with flagged cases removed (standardized residual outliers for continuous outcomes; Cook's distance influential cases for binary outcomes) in CSV or SAV format.")
            ),

            tags$hr(),
            h4("Support and Contact"),
            tags$ul(
              tags$li(tags$strong("Shiny app questions"), ": contact Dr. Grant Benham at ", tags$code("grant.benham@utrgv.edu")),
              tags$li(tags$strong("PROCESS macro/methodology questions"), ": consult PROCESS resources and Dr. Andrew F. Hayes.")
            )
          )
        )
      )
    )
  )
)

