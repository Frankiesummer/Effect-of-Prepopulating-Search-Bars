# Effect of Prepopulating Search Bars

## Table of Contents
1. [Project Overview](#1-project-overview)
2. [Experimental Design](#2-experimental-design)
3. [Key Metrics and Findings](#3-key-metrics-and-findings)
4. [Project Structure and Analysis Flow](#4-project-structure-and-analysis-flow)
5. [Prerequisites and Setup](#5-prerequisites-and-setup)
6. [How to Run the Code](#6-how-to-run-the-code)

---

## 1. Project Overview

### ▸ Project Background
**What is Prepopulating Search Bars?**
Search bar prepopulation is a UI/UX strategy where keywords or suggestions are displayed inside a search bar before the user initiates any input.

**Why run this experiment?**
Although search functionality is central to the online shopping journey, limited empirical research exists on how prefilled keywords causally influence user behavior. This study addresses this gap by conducting a large-scale **Randomized Field Experiment** on a major e-commerce platform, involving **72,587 mobile users**, to quantify the differential impact of various keyword types (Trending, Personalized, Niche) on search engagement, purchasing decisions, and site navigation.

### ▸ Core Finding Summary
- **T1 (Trending) & T2 (Personalized)** significantly increase search engagement (Query, PageClick) and overall spending.
- **T2 (Personalized)** shows the strongest effect but leads to a **substitution effect** (fewer user-initiated queries).
- **T3 (Niche)** has a minimal impact on search but increases **homepage menu usage**, indicating a **complementary effect** on site navigation.
- **Heterogeneous Treatment Effects (HTE)** by gender are largely insignificant, suggesting the strategy is robust across key demographics.

---

## 2. Experimental Design

### ▸ Treatment Groups vs. Control Group
Participants were randomly assigned to one of four conditions:

| Group | Treatment Type | Description |
| :--- | :--- | :--- |
| **Control (C)** | None | The search bar is empty upon landing. |
| **Treatment T1** | **Trending** | Prepopulated with popular category keywords (e.g., "shoes", "dresses"). |
| **Treatment T2** | **Personalized** | Keywords generated based on the individual user's historical purchase/search patterns. |
| **Treatment T3** | **Niche** | Less popular, specific keywords to encourage exploratory browsing. |

### ▸ Key Metrics (Dependent Variables)
The analysis focuses on the following outcome variables, corresponding to the tables in the paper:

| Dimension | Variable (Used in Code) | Description | Corresponding Tables |
| :--- | :--- | :--- | :--- |
| **Search Engagement** | `Query`, `PageClick` | Number of searches, search result page clicks (search depth). | Table 3, 4, 5 |
| **Conversion** | `Purchase`, `Spending` | Binary purchase outcome, total spending amount. | Table 3, 5 |
| **Substitution/Comp.** | `Menu`, `PageClickMenu` | Usage frequency of the homepage navigation menu. | Table 6 |

---

## 3. Project Structure and Analysis Flow

This repository adopts a standardized R Project structure to ensure modularity and reproducibility.

```text
Effect of Prepopulating Search Bars/
├── .gitignore             # Files ignored by Git (e.g., .dta data)
├── README.md              # Project documentation
├── <project>.Rproj        # RStudio Project file
├── R/
│   └── utils.R            # Helper functions for formatting coefficients and p-values
├── data/
│   └── raw/               # Location for raw .dta data files
└── analysis/              # Main analysis scripts
    ├── 01_table3_4_search_behavior.R  # Main ATE and Query Composition (Table 3 & 4)
    ├── 02_table5_conversion.R         # Conversion and Spending Analysis (Table 5)
    └── 03_table6_menu_usage.R         # Menu Usage (Substitution/Complementarity) (Table 6)
```

### ▸ Analysis Flow

The R scripts follow a consistent empirical analysis methodology:

1. **Data Loading & Path Management**: Uses the `here()` package to load data from `data/raw/` using relative paths, ensuring portability.
2. **ATE Estimation**: Uses Ordinary Least Squares (OLS) regression to estimate the Average Treatment Effect (ATE) for each treatment group ($\beta_k$).
3. **Robust Inference**: Standard errors are corrected for heteroskedasticity using White standard errors (via `vcovHC` from the `sandwich` package).
4. **Control Variables & Balance Check**: All models include demographic controls (`Female`, `factor(AgeTier)`) to account for any residual imbalance and improve precision.
5. **Hypothesis Testing**: Uses `linearHypothesis` (from `car` package) to test the difference between treatment effects (e.g., $\beta_1 = \beta_2$).
6. **HTE Estimation**: Interaction terms (e.g., `T1:Female`) are included in regression models to estimate Heterogeneous Treatment Effects (HTE) across genders.

---

## 4. Prerequisites and Setup

### ▸ Dependencies

The project requires **R** and the following packages. Example installation:

```r
install.packages(c(
  "haven",       # To read .dta files
  "data.table",  # For efficient data manipulation
  "dplyr",       # Data manipulation
  "here",        # Essential for path management
  "sandwich",    # For Heteroskedasticity-Consistent (HC) standard errors
  "lmtest",      # To use coeftest with vcovHC
  "car",         # For linearHypothesis testing
  "broom",       # To tidy model outputs
  "knitr", "kableExtra", "stargazer" # For table generation
))
```

### ▸ Data Placement

The code relies on the following directory structure within `data/raw/` (as derived from the analysis scripts):

```text
data/raw/
├── Table 3/
├── Table 4 LeftPanel/
├── Table 4 MidPanel/
├── Table 4 RightPanel/
├── Table 5 LeftPanel/
├── Table 5 MidPanel/
├── Table 5 RightPanel/
└── Table 6/
```

Place the required `.dta` files into the appropriate folders above. The repository does not include raw `.dta` files due to size and licensing.

---

## 5. How to Run the Code

1. **Open Project**: Open the R project file (e.g., double-click the `.Rproj` file) in RStudio, which sets the working directory to the project root.
2. **Run Scripts**: Execute the scripts in numerical order using RStudio or from the command line.

From a command prompt (Windows `cmd.exe`) with `Rscript` available:

```bash
# 1. Main ATE and Query Composition (Tables 3 & 4)
Rscript analysis/01_table3_4_search_behavior.R

# 2. Conversion and Spending Analysis (Table 5)
Rscript analysis/02_table5_conversion.R

# 3. Menu Usage and Substitution Analysis (Table 6)
Rscript analysis/03_table6_menu_usage.R
```

Notes:
- If you run within RStudio, use the Source button for each script or run the script content interactively.
- Ensure `here::here()` resolves to the project root; if not, run `setwd("path/to/project")` or open the `.Rproj` file.

The analysis summary (tables and HTE results) will be printed to the console; table outputs may require the listed reporting packages.

### ▸ Windows: one-click run

A convenience batch script `run.bat` is included at the project root to run all three analysis scripts sequentially on Windows (`cmd.exe`).

Usage (from project root or by double-clicking):

```bat
run.bat
```

What it does:
- Checks that `Rscript` is available on `PATH`.
- Runs the scripts in order: `analysis/01_table3_4_search_behavior.R`, `analysis/02_table5_conversion.R`, `analysis/03_table6_menu_usage.R`.
- Stops and reports if any script returns an error.


If you prefer to run scripts individually in `cmd.exe`, use the `Rscript` commands shown above.

