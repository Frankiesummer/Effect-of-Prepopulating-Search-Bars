# ---------------------------------------------------------------------
# Script: 02_table5_conversion.R
# Content: Replicates Table 5 (Conversion & Spending)
# ---------------------------------------------------------------------

library(data.table)
library(haven)
library(here)
library(dplyr)

# Load helper functions
source(here("R", "utils.R"))

# ------------------------------------------------------------------
# 1. Configuration & File Paths
# ------------------------------------------------------------------
# Based on the screenshot, Table 5 data is split into Panels.
# Mapping: T1 (Trending) -> LeftPanel, T2 (Personalized) -> MidPanel, T3 (Niche) -> RightPanel

path_t1 <- here("data", "raw", "Table 5 LeftPanel")
path_t2 <- here("data", "raw", "Table 5 MidPanel")
path_t3 <- here("data", "raw", "Table 5 RightPanel")

# Define File Lists (Original filenames)
t1_files <- c(
  "Table5_T1_PageClick.dta", "Table5_T1_PCOther.dta", "Table5_T1_PCTrending.dta",
  "Table5_T1_POther.dta", "Table5_T1_PTrending.dta", "Table5_T1_Purchase.dta",
  "Table5_T1_SOther.dta", "Table5_T1_spending.dta", "Table5_T1_STrending.dta"
)

t2_files <- c(
  "Table5_T2_PageClick.dta", "Table5_T2_PCOther.dta", "Table5_T2_PCPersonalized.dta",
  "Table5_T2_POther.dta", "Table5_T2_PPersonalized.dta", "Table5_T2_Purchase.dta",
  "Table5_T2_SOther.dta", "Table5_T2_spending.dta", "Table5_T2_SPersonalized.dta"
)

t3_files <- c(
  "Table5_T3_PageClick.dta", "Table5_T3_PCNiche.dta", "Table5_T3_PCOther.dta",
  "Table5_T3_PNiche.dta", "Table5_T3_POther.dta", "Table5_T3_Purchase.dta",
  "Table5_T3_SNiche.dta", "Table5_T3_SOther.dta", "Table5_T3_Spending.dta"
)

# Combine into a list with paths
group_configs <- list(
  T1 = list(path = path_t1, files = t1_files),
  T2 = list(path = path_t2, files = t2_files),
  T3 = list(path = path_t3, files = t3_files)
)

# ------------------------------------------------------------------
# 2. Analysis Function
# ------------------------------------------------------------------
analyze_dataset <- function(data, dataset_name) {
  # Detect treatment variable (T1/T2/T3)
  treatment_vars <- c("T1", "T2", "T3")
  treatment_var  <- intersect(names(data), treatment_vars)
  
  if (length(treatment_var) == 0) return(NULL)
  treatment_var <- treatment_var[1]
  
  # Identify outcome variable
  fixed_cols   <- c("ID", "Female", "AgeTier", treatment_var)
  outcome_vars <- setdiff(names(data), fixed_cols)
  if (length(outcome_vars) == 0) return(NULL)
  outcome_var <- outcome_vars[1]
  
  # Regression
  model <- lm(as.formula(paste(outcome_var, "~", treatment_var,
                               "+ Female + factor(AgeTier)")),
              data = data)
  
  coef_summary <- summary(model)$coefficients
  treatment_coef <- coef_summary[treatment_var, ]
  
  control_mean <- mean(data[[outcome_var]][data[[treatment_var]] == 0])
  effect_size  <- treatment_coef["Estimate"] / control_mean
  
  # Return data table
  data.table(
    Dataset      = dataset_name,
    Outcome      = outcome_var,
    TreatmentVar = treatment_var,
    Coefficient  = round(treatment_coef["Estimate"], 3),
    StdError     = round(treatment_coef["Std. Error"], 3),
    PValue       = round(treatment_coef["Pr(>|t|)"], 4),
    ControlMean  = round(control_mean, 3),
    EffectSize   = paste0(round(effect_size * 100, 1), "%"),
    Significance = ifelse(treatment_coef["Pr(>|t|)"] < 0.001, "***",
                          ifelse(treatment_coef["Pr(>|t|)"] < 0.01, "**",
                                 ifelse(treatment_coef["Pr(>|t|)"] < 0.05, "*", "")))
  )
}

# ------------------------------------------------------------------
# 3. Batch Processing
# ------------------------------------------------------------------
all_results <- list()

for (grp_name in names(group_configs)) {
  cfg <- group_configs[[grp_name]]
  cat(sprintf("\n=== Processing Group %s in path: %s ===\n", grp_name, cfg$path))
  
  group_results <- list()
  
  for (file in cfg$files) {
    full_path <- file.path(cfg$path, file)
    
    if (file.exists(full_path)) {
      data <- read_dta(full_path)
      # Ensure numeric types
      data[] <- lapply(data, as.numeric)
      
      var_name <- gsub("\\.dta$", "", file)
      res <- analyze_dataset(data, var_name)
      if (!is.null(res)) group_results[[var_name]] <- res
      
    } else {
      warning("File not found: ", full_path)
    }
  }
  
  if (length(group_results) > 0) {
    grp_dt <- rbindlist(group_results)
    grp_dt[, Group := grp_name]
    all_results[[grp_name]] <- grp_dt
  }
}

final_all_groups <- rbindlist(all_results)

# ------------------------------------------------------------------
# 4. Display Results (Three-way comparison style)
# ------------------------------------------------------------------
cat("\n=== Table 5 Summary ===\n")

# Create a simplified display table
if (nrow(final_all_groups) > 0) {
  simple_comparison <- final_all_groups[, .(
    Outcome, Group, 
    Result = paste0(Coefficient, Significance, " (", StdError, ")"),
    EffectSize
  )]
  print(simple_comparison)
}

# ------------------------------------------------------------------
# 5. Gender HTE (Heterogeneous Treatment Effects)
# ------------------------------------------------------------------
analyze_gender_hte <- function(data, dataset_name, outcome_var) {
  treatment_vars <- c("T1", "T2", "T3")
  treatment_var  <- intersect(names(data), treatment_vars)[1]
  
  model_formula <- as.formula(paste(outcome_var, "~", treatment_var,
                                    "* Female + factor(AgeTier)"))
  model <- lm(model_formula, data = data)
  coef_summary <- summary(model)$coefficients
  
  interaction_row <- paste0(treatment_var, ":Female")
  
  if (interaction_row %in% rownames(coef_summary)) {
    interaction_coef <- coef_summary[interaction_row, ]
    
    data.table(
      Dataset            = dataset_name,
      Outcome            = outcome_var,
      Interaction_Coef   = round(interaction_coef["Estimate"], 3),
      Interaction_PValue = round(interaction_coef["Pr(>|t|)"], 4),
      Significance       = add_stars(interaction_coef["Pr(>|t|)"])
    )
  } else {
    NULL
  }
}

cat("\n=== Running Gender HTE Analysis ===\n")
hte_results <- list()

# Iterate again through configs to find specific "Total" files for HTE
# Usually HTE is run on the main outcomes: PageClick, Purchase, Spending
target_outcomes <- c("PageClick", "Purchase", "Spending", "spending")

for (grp_name in names(group_configs)) {
  cfg <- group_configs[[grp_name]]
  
  for (file in cfg$files) {
    # Check if this file is one of the main outcomes
    is_target <- FALSE
    for (tgt in target_outcomes) {
      if (grepl(tgt, file, ignore.case = TRUE) && !grepl("Other", file) && !grepl("Trend", file) && !grepl("Person", file) && !grepl("Niche", file)) {
        is_target <- TRUE # This logic tries to isolate the total outcome files e.g. Table5_T1_PageClick.dta
      }
    }
    # Or explicitly match the filenames known to be totals
    if (file %in% c(paste0("Table5_", grp_name, "_PageClick.dta"), 
                    paste0("Table5_", grp_name, "_Purchase.dta"),
                    paste0("Table5_", grp_name, "_spending.dta"),
                    paste0("Table5_", grp_name, "_Spending.dta"))) {
      
      full_path <- file.path(cfg$path, file)
      if (file.exists(full_path)) {
        data <- read_dta(full_path)
        data[] <- lapply(data, as.numeric)
        outcome_var <- setdiff(names(data), c("ID", "Female", "AgeTier", grp_name))[1]
        
        res <- analyze_gender_hte(data, gsub("\\.dta$", "", file), outcome_var)
        if (!is.null(res)) hte_results[[length(hte_results) + 1]] <- res
      }
    }
  }
}

if (length(hte_results) > 0) {
  hte_final <- rbindlist(hte_results)
  print(hte_final)
}