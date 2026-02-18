# ShinyPROCESS: Interface for Hayes PROCESS for R v5.0

## Overview

ShinyPROCESS provides a user-friendly interface for conducting moderation and mediation analyses using Hayes' PROCESS for R (Version 5.0). The app currently supports the bundled subset of selectable models `1-22, 28-29, 58-73, 75-76, 80-92` (Model 74 is intentionally not selectable and is created internally from Model 4 when needed).

## Installation and Setup (Start Here)

This section is written for first-time users.
Tested on R 4.5.1 (Windows 11).

### Option A (Recommended): Reproducible setup with `renv`

Use this option if you want the best chance of matching the developer-tested package versions.

1. Download or clone this repository.
2. Download Hayes' `process.R` (version 5.0) and place it in the same folder as `gbPROCESS.R`.
3. Open this project folder in RStudio.
4. In the RStudio Console, run:

```r
install.packages("renv")
renv::restore()
shiny::runApp("gbPROCESS.R")
```

Notes:
- `renv::restore()` installs package versions from `renv.lock`.
- `renv` does not automatically install/change your R version.
- If your R version is newer than the lockfile version, restore may still work, but occasional package-version conflicts are possible.
- If prompted by `renv` to activate the project, select `1` ("Activate the project and use the project library").

### Option B (Manual): Install packages without `renv`

Use this if you do not want to use `renv`. This may still work, but version differences can cause issues.

1. Download or clone this repository.
2. Download Hayes' `process.R` (version 5.0) and place it in the same folder as `gbPROCESS.R`.
3. Open this project folder in RStudio.
4. In the RStudio Console, run:

```r
install.packages(c(
  "shiny", "bslib", "ggplot2", "stringr", "dplyr",
  "shinyjs", "car", "haven", "jsonlite", "gridExtra"
))
shiny::runApp("gbPROCESS.R")
```

Optional dependency check:

```r
needed <- c("shiny", "bslib", "ggplot2", "stringr", "dplyr",
            "shinyjs", "car", "haven", "jsonlite", "gridExtra")
setdiff(needed, rownames(installed.packages()))
```

If this returns `character(0)`, required packages are installed.

### Common Setup Problems and Fixes

- App shows PROCESS missing/version warning:
  - Confirm file name is exactly `process.R`.
  - Confirm it is in the same folder as `gbPROCESS.R`.
  - Confirm first lines indicate PROCESS for R version `5.0`.
- `renv::restore()` fails on one or more packages:
  - Copy the exact error and retry in a clean R session.
  - If using a much newer R version, try the manual install path.
- App launches but errors on startup:
  - Restart RStudio, reopen the project, and rerun the setup commands above.

### Shinylive Runtime Notes

- Local R Shiny can auto-load `process.R` v5.0 from the app root (same folder as `gbPROCESS.R`).
- Shinylive requires uploading `process.R` each session using the in-app upload control.
- Uploaded `process.R` is session-only and is not persisted by the app.
- Use `tools/export_shinylive.R` for browser export; it hard-stops if root `process.R` exists and restores `runtime.txt` after export.
- Repeatable update steps are documented in `SHINYLIVE_UPDATE_WORKFLOW.txt`.

### Sharing the Browser Version (No R Required for End Users)

If you want others to run the app without installing R/RStudio, share the **Shinylive export** (the `docs/` output), not the R project.

Important:
- Shinylive files must be served over `http://` (not opened with `file://`).
- End users should expect to upload `process.R` v5.0 in each browser session before running analysis.

Recommended sharing methods:
1. Host the exported `docs/` folder on any static web host and send users the URL.
2. Send users the exported folder plus a lightweight local web-server method for their OS.

If users only receive files and no hosting:
- They cannot reliably run by double-clicking files directly.
- They need a local static server, then open the local URL in a browser.

## Important: Attribution, Licensing, and Required File

- **Shiny app developer**: Dr. Grant Benham, The University of Texas Rio Grande Valley  
  Contact: `grant.benham@utrgv.edu`
- **PROCESS macro author**: Dr. Andrew F. Hayes
- This repository contains the Shiny interface, but **does not include** Hayes' copyrighted `process.R` file.
- You must download `process.R` (PROCESS for R **version 5.0**) separately and place it in the same folder as `gbPROCESS.R`.
- Download site: https://haskayne.ucalgary.ca/CCRAM/resource-hub
- PROCESS information: https://processmacro.org/index.html

Contact policy:
- Questions about this Shiny app (UI/workflow/integration): contact Dr. Grant Benham.
- Questions about PROCESS methodology or the PROCESS macro itself: consult PROCESS resources and contact Dr. Hayes as appropriate.

### Key Features

- **Comprehensive Model Support**: Selectable models `1-22, 28-29, 58-73, 75-76, 80-92` (Model 74 is internal-only via Model 4 + X by M interaction)
- **Flexible Data Input**: Supports CSV and SPSS (.sav) file formats
- **Assumption Checking**: Built-in diagnostic tools for regression assumptions
- **Flagged-Case Detection by Outcome Type**: Uses standardized residual outliers for continuous outcomes and Cook's distance influential cases for binary outcomes
- **Bootstrap Confidence Intervals**: Support for percentile and bias-corrected bootstrap methods
- **Visualization**: Simple slopes plots, conditional effect plots, and Johnson-Neyman plots for moderation models
- **Export Options**: Download results as HTML files and filtered datasets
- **Settings Management**: Save and load analysis settings as JSON files

---

## Application Structure

This application uses a modular architecture for improved maintainability and organization. The codebase is split into logical modules that are sourced by the main application file.

### Main Application File

**`gbPROCESS.R`** (~2,588 lines)
- Entry point for the Shiny application
- Loads libraries and sources all modules
- Defines reactive values (`rv`) for state management
- Contains plot generation helpers and output renderers (cohesive plot functionality)
- Contains miscellaneous observers (button states, mutual exclusivity checks)

### Module Files

**`modules_assumptions.R`** (~523 lines)
- Assumption checking helper functions
- Functions: `is_binary_variable()`, `is_continuous_variable()`, `identify_outliers_assumption()`, `check_normality()`, `test_homoscedasticity()`, `diagnostic_report()`, `generate_assumption_checks_html()`, etc.
- **Dependencies**: `car` package (VIF, ncvTest), `ggplot2` (Q-Q plots)

**`modules_ui.R`** (~660 lines)
- Complete UI definition (`fluidPage` structure)
- All sidebar and main panel components
- Tab definitions (Assumption Checks, Analysis Results, Plots)
- Conditional panels and styling
- **Dependencies**: `shiny`, `bslib`, `shinyjs`

**`modules_data_management.R`** (~734 lines)
- File upload handling (CSV, SAV)
- Variable selection observers
- Model change detection and clearing logic
- Mediator UI dynamic generation
- Variable validation
- UI conditional output reactives
- **Dependencies**: `shiny`, `haven` (SPSS files), `shinyjs`

**`modules_assumption_outputs.R`** (~847 lines)
- Assumption check output renderers
- `identify_outliers()` reactive
- `outlier_summary()` reactive
- Diagnostic plots (Q-Q, residuals, scale-location)
- Violin plots for continuous variables
- Download handler for assumption checks
- **Dependencies**: `modules_assumptions.R`, `ggplot2`

**`modules_analysis.R`** (~945 lines)
- Core analysis execution logic
- `run_process_analysis()` function (shared by both analysis types)
- `original_analysis()` eventReactive
- `outliers_analysis()` eventReactive
- `analysis_results()` reactive
- Observers for analysis completion
- **Dependencies**: `process.R`, `modules_assumptions.R`

**`modules_results.R`** (~508 lines)
- Results display and download functionality
- `create_bivariate_correlations()` function
- `create_missing_data_breakdown()` function
- `create_formatted_output()` function
- `wrap_results_html()` function
- `output$analysis_output` renderer
- Download handlers for results and filtered datasets
- Button state observers
- **Dependencies**: `modules_analysis.R`

**`modules_save_load.R`** (~645 lines)
- Save/load analysis settings to/from JSON files
- Validates variables exist in current dataset when loading
- Handles model changes and variable restoration
- **Dependencies**: `jsonlite` package

### External Files

**`process.R`** (~7,581 lines, required but not included in this repository)
- Core PROCESS macro functionality (Hayes' PROCESS V5)
- Must be downloaded separately and placed next to `gbPROCESS.R`
- On startup, the app checks for this file and verifies the version header indicates **5.0**

### Module Loading Order

The modules are sourced in the following order in `gbPROCESS.R`:

1. **Outside server function** (no Shiny context needed):
   - `modules_assumptions.R` - Helper functions only

2. **Inside server function** (requires Shiny context):
   - `process.R` - PROCESS macro (loaded only when present and version check passes)
   - `modules_save_load.R` - Save/load functionality
   - `modules_data_management.R` - Data upload and variable selection
   - `modules_assumption_outputs.R` - Assumption check outputs
   - `modules_analysis.R` - Analysis execution
   - `modules_results.R` - Results display

All modules are sourced with `local = TRUE` to ensure proper variable scope within the Shiny server function.

### Dependencies

**Required R Packages**:
- `shiny` - Shiny web application framework
- `bslib` - Bootstrap themes for Shiny
- `ggplot2` - Plotting and visualization
- `stringr` - String manipulation
- `dplyr` - Data manipulation
- `shinyjs` - JavaScript functionality for Shiny
- `car` - Regression diagnostics (VIF, ncvTest)
- `haven` - SPSS file support (.sav files)
- `jsonlite` - JSON file handling (for save/load settings)
- `grid` - Grid graphics (for plot layouts)
- `gridExtra` - Extended grid graphics (for stacked plots)
- `grDevices` - Graphics devices (for plot downloads)

**Installation**:
```r
install.packages(c("shiny", "bslib", "ggplot2", "stringr", "dplyr", 
                   "shinyjs", "car", "haven", "jsonlite", "grid", 
                   "gridExtra"))
```

---

---

## User Guide

### Step 1: Upload Your Data

1. Click the **"Choose CSV or SAV File"** button in the **Upload Data** section
2. Select your data file (CSV or SPSS .sav format)
3. Wait for the upload to complete
4. The application will display "Upload complete" when ready

**Note**: File size limit is 50MB.

### Step 2: Select Your Model

1. In the **Model Selection** section, click the **"PROCESS Model Number"** dropdown
2. Select the appropriate model number (`1-22, 28-29, 58-73, 75-76, 80-92`) for your research question
3. Refer to Hayes' PROCESS documentation or book for model diagrams and descriptions
4. **Note**: Model 74 is not directly selectable. To use Model 74, select Model 4 and enable "Allow X by M interaction"

**Important**: When you change the model number, all previously selected variables and analysis results are automatically cleared to prevent confusion.

### Step 3: Select Variables

1. Expand the **"Select Variables"** section (click the header to expand/collapse)
2. Select variables from the dropdown menus based on your chosen model:
   - **Predictor Variable (X)**: Your independent variable
   - **Outcome Variable (Y)**: Your dependent variable
   - **Moderator Variable (W)**: If your model requires a moderator (enabled for applicable models)
   - **Second Moderator Variable (Z)**: If your model requires a second moderator (Models 2, 3)
   - **Number of Mediators**: Select the number of mediators from the dropdown (for models with mediators)
   - **M1, M2, M3...**: Select your mediator variables in order (appears after selecting number of mediators)
   - **Covariates**: Select one or more covariates (optional, multiple selection allowed)

**Note**: Variable selection menus are automatically enabled/disabled based on your selected model. Variables that aren't needed for your model will be grayed out.

### Step 4: Review Assumption Checks (Optional but Recommended)

1. Expand the **"Assumption Checks"** section
2. Review the outlier detection settings:
   - **For continuous outcomes**: Set your standardized residual threshold (default: 2.0)
   - **For binary outcomes**: Choose Cook's Distance threshold (Conservative, Liberal, or Custom)
3. Navigate to the **"Assumption Checks"** tab in the main panel to view:
   - Outlier/influential case identification
   - Diagnostic plots (Q-Q plots, residual plots, scale-location plots)
   - Distribution plots for continuous variables
4. Review the diagnostic information to assess model assumptions

### Step 5: Configure PROCESS Options

Expand and configure the following sections as needed:

#### Centering Options
- Select mean centering approach (see detailed options below)

#### Bootstrap Settings
- Enable/disable bootstrapping (recommended for indirect effects)
- Set number of bootstrap samples (default: 5000)
- Choose confidence interval method
- Set confidence level (default: 95%)
- Optionally set a random seed for reproducibility

#### Advanced Options
- Select standard error method
- Enable standardized coefficients if needed
- Enable normal theory tests if appropriate
- Enable pairwise contrasts for multiple mediators (if applicable)

#### Output Options
- Configure decimal places for output
- Select additional output options (descriptives, diagnostics, etc.)

#### Probing Moderation (for moderation models only)
- Enable interaction probing
- Set probing threshold
- Choose conditioning values
- Enable Johnson-Neyman technique if desired

#### Plot Options (for Models 1 and 3 only)
- Customize plot titles and labels
- Adjust visualization settings

### Step 6: Run Your Analysis

1. Click **"With Original Dataset"** to run the analysis with all cases
2. OR click **"With Outliers Removed"** (if available) to run the analysis after removing flagged cases identified in Step 4:
   - Continuous outcomes: standardized residual outliers
   - Binary outcomes: Cook's distance influential cases
3. Wait for the analysis to complete (progress indicators will show)
4. View results in the **"Analysis Results"** tab

### Step 7: Review Results

1. Navigate to the **"Analysis Results"** tab to view:
   - Analysis summary
   - Model coefficients
   - Direct and indirect effects (for mediation models)
   - Conditional effects (for moderation models)
   - Bootstrap confidence intervals (if enabled)
2. Navigate to the **"Plots"** tab (for Models 1 and 3) to view:
   - Simple slopes plots (Model 1)
   - Conditional effect plots or stacked simple slopes plots (Model 3)
   - Johnson-Neyman plots (Model 1 only)

### Step 8: Download Results

1. Click **"Results output (html)"** to download a formatted HTML file with all analysis results
2. If you ran analysis with flagged cases removed, you can also download the filtered dataset:
   - Choose format (CSV or SPSS .sav)
   - Click **"Dataset Without Outliers"**

### Step 9: Save/Load Analysis Settings (Optional)

**Save Settings**:
1. After loading a dataset and configuring your analysis, click **"Save Analysis Settings"** in the sidebar
2. A JSON file will be downloaded containing all your current settings (model, variables, options, plot labels, etc.)
3. This file can be loaded later to restore your exact analysis configuration

**Load Settings**:
1. Load a dataset that contains all variables referenced in the saved settings file
2. Click **"Choose JSON File"** in the sidebar
3. Select a previously saved JSON settings file
4. The application will:
   - Validate that all variables in the JSON file exist in the current dataset
   - Show an error message listing any missing variables if validation fails
   - Restore all settings (model, variables, options, plot labels) if validation passes

**Note**: On model changes triggered by JSON load, the app skips input clearing to avoid wiping restored values during the same update cycle. This helps prevent first-load timing issues after restarting the R session.

**Note**: The save/load buttons are automatically disabled until a dataset is loaded. If you try to load a JSON file with variables that don't exist in the current dataset, you'll see a clear error message listing the missing variables.

---

## Detailed Options Reference

This section explains each option organized by UI section headers.

### Upload Data

**Choose CSV or SAV File**
- Upload your dataset in either CSV (comma-separated values) or SPSS (.sav) format
- Maximum file size: 50MB
- The application automatically detects variable types (continuous, binary, categorical)

### Model Selection

**PROCESS Model Number**
- Select from models `1-22, 28-29, 58-73, 75-76, 80-92` (Model 74 is not user-selectable - see note below)
- Each model represents a different conceptual framework for examining relationships
- Model diagrams and descriptions should be referenced from Hayes' PROCESS documentation or book
- **Important**: Changing the model number clears all variable selections and previous results

**Note on Model 0**: Model 0 (multiple regression with 2-15 predictors) is not included in this application. Model 0 requires unique syntax that is not well-suited for the graphical interface. For multiple regression analyses, users should use standard R regression functions or other statistical software.

**Note on Model 74**: Model 74 is not directly selectable in this application. It is automatically created internally when Model 4 has the "Allow X by M interaction" option enabled. When this option is checked, PROCESS converts Model 4 to Model 74 and automatically uses the predictor variable (X) as the moderator variable (W). To use Model 74, select Model 4 and enable the "Allow X by M interaction" checkbox.

### Select Variables

**Predictor Variable (X)**
- Your independent variable (the variable that predicts or causes changes)
- Required for all models

**Outcome Variable (Y)**
- Your dependent variable (the variable being predicted or affected)
- Required for all models

**Moderator Variable (W)**
- A variable that moderates (changes the strength/direction of) the relationship between X and Y
- Enabled for models that include moderation (Models 1, 2, 3, 5, 14, 15, 58, 59, 83-92)
- Disabled for models that don't use moderators (e.g., Models 4, 6, 80-82)
- **Note**: Model 74 is not user-selectable. It is automatically created from Model 4 when "Allow X by M interaction" is enabled, and PROCESS automatically uses X as W internally.

**Second Moderator Variable (Z)**
- A second moderating variable
- Only enabled for models with two moderators (Models 2, 3)

**Number of Mediators**
- Dropdown menu to select how many mediator variables you want to include
- Available for models that support mediators (Models 4-92, except Models 1-3)
- Maximum number depends on model:
  - Model 4: up to 10 mediators
  - Model 6: up to 6 mediators
  - Models 80-81: 3 to 6 mediators
  - Model 82: up to 4 mediators
  - Models 83-92: 2 to 6 mediators
  - Other models: up to 10 mediators (default)
- **Note**: Changing the number of mediators clears all mediator variable selections

**M1, M2, M3... (Mediator Variables)**
- Select your mediator variables in order
- Appears after you select the number of mediators
- The order matters for models with multiple mediators
- Each mediator represents a pathway through which X affects Y

**Covariates (optional)**
- Additional variables to control for in the analysis
- Multiple selection allowed
- Included in all regression equations unless "Exclude covariates from Y equation" is enabled

### Assumption Checks

This section allows you to configure how outliers and influential cases are identified.

**Scope note**:
- Flagged-case removal in this app is based on model diagnostics (standardized residuals for continuous outcomes; Cook's distance for binary outcomes).
- The app does **not** currently include automatic univariate outlier detection/removal.
- For univariate screening, use the violin plots in the Assumption Checks tab and optional PROCESS diagnostics output.

**For Continuous Outcomes:**

**Standardized Residual Threshold**
- Threshold for identifying outliers based on standardized residuals
- Cases with |standardized residual| > threshold are flagged as outliers
- Default: 2.0
- Common values:
  - 2.0: Standard criterion (flags ~5% of cases if normally distributed)
  - 2.5: More stringent
  - 3.0: Very conservative

**For Binary Outcomes:**

**Cook's Distance Threshold**
- Method for identifying influential cases in logistic regression
- Options:
  - **Conservative (4/n)**: Uses 4 divided by sample size (recommended default)
  - **Liberal (1.0)**: Uses a fixed threshold of 1.0
  - **Custom**: Allows you to specify your own threshold (0-1)

**Note**: The assumption checks are performed on the original dataset and update automatically as you change variable selections or threshold values.

### PROCESS Options

#### Centering Options

**Mean Centering**
- Controls how variables are centered before creating interaction terms
- Options:
  - **No centering (0)**: Variables used as-is (default)
  - **All variables that define products (1)**: Centers all variables involved in interaction terms
  - **Only continuous variables that define products (2)**: Centers only continuous variables in interactions, leaves categorical variables uncentered
- **When to use**: Centering is recommended when interaction terms are included, especially for interpretation of simple slopes

#### Bootstrap Settings

**Use bootstrapping**
- Enable bootstrap confidence intervals for indirect effects and other statistics
- **Recommended**: Keep enabled (default: ON) for mediation analyses
- Bootstrap methods don't assume normality and provide more robust confidence intervals

**Number of bootstrap samples**
- Number of bootstrap resamples to perform
- Default: 5000
- Range: 1000-10000
- Higher values provide more stable estimates but take longer to compute
- 5000 is generally sufficient for most analyses

**Bootstrap Confidence Interval Method**
- **Percentile bootstrap (0)**: Uses percentiles of bootstrap distribution (default)
- **Bias-corrected bootstrap (1)**: Adjusts for bias in bootstrap distribution
- Bias-corrected is more accurate but can be less stable with small samples

**Confidence Level (%)**
- Confidence level for confidence intervals
- Default: 95%
- Range: 80-99%
- Common choices: 90%, 95%, 99%

**Random Seed (optional)**
- Set a specific seed for random number generation
- Leave blank for random seed (different results each run)
- Enter a number (1-999999) to set a specific seed for reproducibility
- **Useful for**: Reproducing exact results, sharing analyses with exact same bootstrap samples

#### Advanced Options

**Standard Errors**
- Method for calculating standard errors
- Options:
  - **OLS**: Ordinary least squares (default, assumes homoscedasticity)
  - **HC0 (Huber-White)**: Robust to heteroscedasticity
  - **HC1 (Hinkley)**: Small-sample correction for HC0
  - **HC2**: Alternative small-sample correction
  - **HC3 (Davidson-MacKinnon)**: Recommended for small samples
  - **HC4 (Cribari-Neto)**: More conservative, good for influential cases
- **When to use**: Use robust standard errors if you suspect heteroscedasticity (unequal variance)

**Standardized coefficients**
- Requests standardized regression coefficients (beta weights)
- **Useful for**: Comparing effects across variables with different scales
- Default: OFF

**Normal theory tests**
- Uses z-tests instead of t-tests for significance testing
- Less conservative than t-tests
- **Assumes**: Large sample sizes
- Default: OFF

**Pairwise contrasts of indirect effects**
- Compares indirect effects when multiple mediators are included
- Determines which mediators are most important
- **Only available**: When 2+ mediators are selected
- Default: OFF

#### Output Options

**Decimal Places**
- Number of decimal places to display in output
- Default: 4
- Range: 0-10

**Descriptives and variable correlations**
- Displays descriptive statistics (means, SDs, min, max) and correlation matrices
- **Useful for**: Understanding your data and bivariate relationships
- Default: ON

**Show regression coefficient covariance matrix**
- Displays the covariance matrix of regression parameter estimates
- **Useful for**: Understanding relationships between coefficients, advanced analyses
- Default: OFF

**Scale-free measures of (partial) association**
- Provides partial correlations, semi-partial correlations, and standardized coefficients
- **Useful for**: Comparing effects across variables with different scales
- Default: OFF

**List cases deleted due to missing data**
- Lists all cases excluded from analysis due to missing data
- **Useful for**: Identifying data quality issues, understanding sample size reductions
- Default: OFF

**Sums of squares and adjusted R-squared**
- Displays detailed model fit information (regression SS, residual SS, total SS, df, mean squares, adjusted R²)
- **Useful for**: Detailed model evaluation
- Default: OFF

**Shrunken R estimates**
- Cross-validated R-squared values that adjust for overfitting
- Indicates how well the model would perform on new data
- **Only available**: For continuous outcomes with multiple mediators
- Default: OFF

**Model diagnostics and assumptions**
- Comprehensive regression diagnostics including:
  - Residual analysis
  - Influential cases
  - Multicollinearity (VIF)
  - Assumption tests
- **Essential for**: Evaluating model quality
- Default: OFF

**Allow X by M interaction (model 4 only)**
- Includes X*M interaction term in Model 4
- When enabled, PROCESS automatically converts Model 4 to Model 74 internally, using the predictor variable (X) as the moderator variable (W)
- Allows the effect of mediator (M) on outcome (Y) to depend on level of X
- Changes model to counterfactual framework with different effect interpretations (natural direct and indirect effects)
- **Mutually exclusive with**: "Test for X by M interaction"
- Default: OFF

**Test for X by M interaction**
- Tests whether X*M interaction terms are significant
- Determines if effect of mediators (M) on outcome (Y) depends on level of X
- Does not change model structure
- **For Model 4**: Use this to decide whether to enable "Allow X by M interaction"
- **Mutually exclusive with**: "Allow X by M interaction" for Model 4
- **Available for**: Mediation models
- Default: OFF

**Total effect of X**
- Shows the total effect of X on Y (direct + indirect effects combined)
- **Useful for**: Understanding overall relationship before examining mediation pathways
- **Available for**: Models 4, 6, 80, 81, 82
- Default: OFF

**Matrices output**
- Displays model definition matrices showing which paths are estimated and which variables moderate which paths
- **Useful for**: Understanding model structure
- Default: OFF

**Exclude covariates from Y equation**
- When checked, covariates are excluded from the outcome (Y) equation
- Affects model specification
- **Note**: For Model 1, this option works around a PROCESS limitation
- Default: OFF

#### Probing Moderation

**Available only for moderation models (Models 1, 2, 3, 5, 14, 15, 58, 59, 83-92)**

**Probe interactions**
- Enables probing of interaction effects
- When enabled, shows conditional effects at different levels of the moderator
- Default: OFF

**When to probe:**
- Threshold for determining when to probe interactions
- Enter threshold as text (e.g., "p < .10" or "p < .05")
- Default: "p < .10"
- Interactions meeting this threshold will be probed

**Values for nondiscrete moderators:**
- Determines which values of continuous moderators to use for probing
- Options:
  - **Percentiles (16th, 50th, 84th)**: Uses 16th percentile (low), median (medium), 84th percentile (high) - Default
  - **Moments (Mean and ±1 SD)**: Uses mean minus 1 SD (low), mean (medium), mean plus 1 SD (high)
- **When to use**: Percentiles are more robust to outliers; moments are more interpretable

**Johnson-Neyman technique**
- Identifies regions of significance where the effect of X on Y is statistically significant
- Shows the range of moderator values where the effect is significant vs. non-significant
- **Useful for**: Finding exact boundaries of significance
- Default: OFF

#### Plot Options

**Available only for Models 1 and 3**

**Plot Type for Model 3** (Model 3 only)
- Choose which type of plot to display:
  - **Conditional Effect Plot**: Shows how the conditional effect of X*W changes across levels of Z (second moderator)
  - **Stacked Simple Slopes Plot**: Shows simple slopes of X on Y at different W levels, with separate plots for each Z level stacked vertically
- Default: "Conditional Effect Plot"

**Plot Title**
- Custom title for the plot
- Default: "Simple Slopes Plot" (for Model 1) or "Conditional Effect Plot" (for Model 3 conditional effect plot)

**Use color for lines**
- Uses different colors for different moderator levels
- Default: ON
- If OFF, uses different line types instead

**Customize y-axis range**
- Allows you to set custom minimum and maximum values for the y-axis
- Default: OFF
- When enabled, specify:
  - **Y-axis minimum**: Lower bound for y-axis
  - **Y-axis maximum**: Upper bound for y-axis

**Label for Predictor**
- Custom label for the X-axis (predictor variable)
- Leave blank to use variable name

**Label for Outcome**
- Custom label for the Y-axis (outcome variable)
- Leave blank to use variable name

**Label for Moderator**
- Custom label for the moderator in the legend
- Leave blank to use variable name

**Label for Second Moderator (Z)** (Models 2, 3 only)
- Custom label for the second moderator
- Leave blank to use variable name

**Decimal Places for Moderator Levels**
- Number of decimal places shown for moderator level labels in the plot
- Default: 2
- Range: 0-5

**Show confidence intervals**
- Displays confidence bands around the simple slopes lines
- Default: ON
- Helps visualize uncertainty in the conditional effects

### Run Analysis

**With Original Dataset**
- Runs the analysis using all cases in your dataset
- No cases are removed
- Use this for your primary analysis

**With Outliers Removed** (appears after assumption checks identify outliers)
- Runs the analysis after removing cases identified as outliers/influential
- Number of cases removed is shown in the button text
- Use this to assess the impact of outliers on your results
- **Note**: Results will differ from the original dataset analysis

### Download Options

**Results output (html)**
- Downloads a formatted HTML file containing all analysis results
- Includes all output from the PROCESS analysis
- Can be opened in any web browser
- **Available**: Only when analysis has been run

**Download Reduced Dataset** (appears after running analysis with outliers removed)
- Downloads the dataset with outliers/influential cases removed
- Format options:
  - **CSV (.csv)**: Comma-separated values format
  - **SPSS (.sav)**: SPSS format
- **Useful for**: Further analyses on the cleaned dataset

### Save/Load Analysis Settings

**Save Analysis Settings**
- Saves all current analysis settings to a JSON file
- Includes: model number, variable selections, all PROCESS options, plot labels, etc.
- **Available**: Only when a dataset is loaded (button is disabled until dataset is loaded)
- **Useful for**: Reproducing analyses, sharing configurations, working across sessions

**Load Analysis Settings**
- Loads previously saved analysis settings from a JSON file
- **Available**: Only when a dataset is loaded (button is disabled until dataset is loaded)
- **Validation**: The application automatically validates that all variables referenced in the JSON file exist in the current dataset
- **Error Handling**: If any variables are missing, a clear error message is displayed listing the missing variables, and settings are not loaded
- **Useful for**: Restoring previous analysis configurations quickly

**Note**: The save/load buttons are automatically disabled until a dataset is loaded. When loading settings, ensure your current dataset contains all variables referenced in the saved JSON file.

---

## Main Panel Tabs

### Assumption Checks Tab

This tab provides comprehensive diagnostic information:

**Detailed Assumption Check Results**
- Summary of outliers/influential cases identified
- Lists specific cases and their diagnostic values
- Updates automatically based on selected variables and thresholds

**Diagnostic Plots**
- **Normal Q-Q Plot** (continuous outcomes only): Checks normality of residuals
- **Residuals vs Fitted Plot**: Checks linearity and homoscedasticity
- **Scale-Location Plot** (continuous outcomes only): Checks variance stability

**Continuous Variable Distributions**
- Violin plots showing distributions of continuous variables
- Shows both original dataset and analysis dataset (if cases were removed)

**Download Assumption Checks (html)**
- Downloads all assumption check results and plots as an HTML file

### Analysis Results Tab

Displays the complete PROCESS analysis output including:
- Analysis summary (dataset info, model number, variables used)
- Model coefficients
- Direct and indirect effects (mediation models)
- Conditional effects (moderation models)
- Bootstrap confidence intervals (if enabled)
- All requested output options

### Plots Tab

**Available for Models 1 and 3 only**

**For Model 1:**
- **Simple Slopes Plot**: Visualizes the relationship between X and Y at different levels of the moderator (W). Shows how the effect of X on Y changes across moderator values. Customizable titles, labels, and appearance.

**For Model 3:**
- **Conditional Effect Plot** (default): Shows how the conditional effect of the X*W interaction changes across levels of the second moderator (Z). The x-axis represents Z values, and the y-axis shows the conditional effect of X*W on Y. Useful for understanding how the interaction effect varies across Z.
- **Stacked Simple Slopes Plot**: Shows simple slopes of X on Y at different W levels, with separate plots stacked vertically for each Z level. Each subplot displays the relationship between X and Y at a specific Z value, with different lines for different W levels. Useful for visualizing the three-way interaction pattern.

**Johnson-Neyman Plot** (Model 1 only)
- Shows regions of significance
- Identifies moderator values where the effect of X on Y is significant vs. non-significant
- Only available when Johnson-Neyman technique is enabled

**Download Options**
- Download plots as JPG image files
- Download button label and filename automatically match the selected plot type

---

## Tips and Best Practices

1. **Always check assumptions first**: Review the Assumption Checks tab before running your main analysis
2. **Use bootstrapping for mediation**: Bootstrap confidence intervals are recommended for indirect effects
3. **Consider centering for interactions**: Mean centering improves interpretation of interaction effects
4. **Document your settings**: Note your choices (especially bootstrap samples, seed, thresholds) for reproducibility
5. **Compare with and without outliers**: Run both analyses to assess robustness of your findings
6. **Review diagnostic plots**: Check Q-Q plots and residual plots to assess model assumptions
7. **Use appropriate thresholds**: Choose outlier detection thresholds based on your field's conventions and sample size

---

## Technical Notes

- **File Format Support**: CSV files should have headers. SPSS files (.sav) preserve variable labels and value labels.
- **PROCESS File Requirement**: `process.R` (PROCESS for R v5.0) must be present in the app folder. If missing or mismatched, the app shows a warning and blocks analysis execution.
- **Missing Data**: Cases with missing data on any analysis variable are automatically excluded (listwise deletion).
- **Variable Types**: The application automatically detects binary variables (0/1 or exactly 2 unique values) and continuous variables.
- **Model Validation**: The application validates variable selections based on model requirements and prevents duplicate variable selection.
- **Results Clearing**: Analysis results are automatically cleared when you change the model number to prevent confusion.
- **Settings Validation**: When loading saved settings, the application validates that all referenced variables exist in the current dataset. If any variables are missing, an error message is displayed listing the missing variables, and settings are not loaded.

---

## References

This application interfaces with Hayes' PROCESS macro for R (Version 5). For detailed information about PROCESS models, interpretation, and methodology, refer to:

- Hayes, A. F. (2018). *Introduction to Mediation, Moderation, and Conditional Process Analysis: A Regression-Based Approach* (2nd ed.). Guilford Press.
- PROCESS resource hub: https://haskayne.ucalgary.ca/CCRAM/resource-hub
- PROCESS website: https://processmacro.org/index.html

---

## Support

For issues, questions, or feature requests related to this Shiny app interface, contact:

- **Dr. Grant Benham**
- The University of Texas Rio Grande Valley
- `grant.benham@utrgv.edu`

For questions about PROCESS methodology, model logic, or the `process.R` macro itself, refer to PROCESS resources and Dr. Andrew F. Hayes.
