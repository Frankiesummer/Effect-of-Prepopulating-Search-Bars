# ---------------------------------------------------------------------
# Script: 03_table6_menu_usage.R
# Content: Replicates Table 6 (Homepage Menu Usage)
# ---------------------------------------------------------------------

library(data.table)
library(haven)
library(here)

# Load helper functions
source(here("R", "utils.R"))

# ------------------------------------------------------------------
# 1. File list & Paths
# ------------------------------------------------------------------
# Path based on screenshot: data/raw/Table 6
path6 <- here("data", "raw", "Table 6")

table6_files <- c(
  "Table6_Menu.dta",
  "Table6_PageClickMenu.dta",
  "Table6_PurchaseMenu.dta",
  "Table6_SpendingMenu.dta"
)

# ------------------------------------------------------------------
# 2. Bulk import
# ------------------------------------------------------------------
cat("Importing Table 6 datasets...\n")
table6_datasets <- list()

for (file in table6_files) {
  full_path <- file.path(path6, file)
  if (file.exists(full_path)) {
    var_name <- gsub("\\.dta$", "", file)
    table6_datasets[[var_name]] <- read_dta(full_path)
    # coerce to numeric
    for (col in names(table6_datasets[[var_name]])) {
      table6_datasets[[var_name]][[col]] <- as.numeric(table6_datasets[[var_name]][[col]])
    }
    cat("Imported:", file, "\n")
  } else {
    cat("File not found:", full_path, "\n")
  }
}

# ------------------------------------------------------------------
# 3. Core analysis function
# ------------------------------------------------------------------
analyze_table6 <- function(data, dataset_name) {
  
  fixed_cols   <- c("ID", "Female", "AgeTier", "T1", "T2", "T3")
  outcome_vars <- setdiff(names(data), fixed_cols)
  if (length(outcome_vars) == 0) return(NULL)
  outcome_var <- outcome_vars[1]
  
  # Single model with all treatments
  model <- lm(as.formula(paste(outcome_var, "~ T1 + T2 + T3 + Female + factor(AgeTier)")),
              data = data)
  coef_summary <- summary(model)$coefficients
  
  results <- list()
  for (treatment_var in c("T1", "T2", "T3")) {
    if (treatment_var %in% rownames(coef_summary)) {
      treatment_coef <- coef_summary[treatment_var, ]
      
      # control mean: units with T1=T2=T3=0
      control_mean <- mean(data[[outcome_var]][data$T1 == 0 & data$T2 == 0 & data$T3 == 0])
      effect_size  <- treatment_coef["Estimate"] / control_mean
      
      results[[treatment_var]] <- data.table(
        Dataset      = dataset_name,
        Outcome      = outcome_var,
        TreatmentVar = treatment_var,
        Coefficient  = round(treatment_coef["Estimate"], 3),
        StdError     = round(treatment_coef["Std. Error"], 3),
        PValue       = round(treatment_coef["Pr(>|t|)"], 4),
        ControlMean  = round(control_mean, 3),
        EffectSize   = paste0(round(effect_size * 100, 1), "%"),
        Significance = add_stars(treatment_coef["Pr(>|t|)"])
      )
    }
  }
  return(rbindlist(results))
}

# ------------------------------------------------------------------
# 4. Run Analysis
# ------------------------------------------------------------------
cat("\n=== Analysing Table 6 ===\n")
table6_results <- list()

for (name in names(table6_datasets)) {
  res <- analyze_table6(table6_datasets[[name]], name)
  if (!is.null(res)) table6_results[[name]] <- res
}

table6_final <- rbindlist(table6_results)
table6_final[, Group := TreatmentVar]

print(table6_final)

# ------------------------------------------------------------------
# 5. Gender-HTE for Table 6
# ------------------------------------------------------------------
analyze_table6_gender_hte <- function(data, dataset_name, outcome_var) {
  treatment_vars <- c("T1", "T2", "T3")
  results <- list()
  
  for (treatment_var in treatment_vars) {
    model_formula <- as.formula(paste(outcome_var, "~", treatment_var,
                                      "* Female + factor(AgeTier)"))
    model <- lm(model_formula, data = data)
    coef_summary <- summary(model)$coefficients
    
    int_term <- paste0(treatment_var, ":Female")
    
    if(int_term %in% rownames(coef_summary)) {
      treatment_coef  <- coef_summary[treatment_var, ]
      interaction_coef <- coef_summary[int_term, ]
      
      results[[treatment_var]] <- data.table(
        Dataset               = dataset_name,
        TreatmentVar          = treatment_var,
        Interaction_Coefficient = round(interaction_coef["Estimate"], 3),
        Interaction_PValue    = round(interaction_coef["Pr(>|t|)"], 4),
        Significance          = add_stars(interaction_coef["Pr(>|t|)"])
      )
    }
  }
  return(rbindlist(results))
}

cat("\n=== Running Table 6 HTE ===\n")
table6_hte_results <- list()

for (dataset_name in names(table6_datasets)) {
  data <- table6_datasets[[dataset_name]]
  outcome_var <- setdiff(names(data), c("ID", "Female", "AgeTier", "T1", "T2", "T3"))[1]
  
  res <- analyze_table6_gender_hte(data, dataset_name, outcome_var)
  if (!is.null(res)) table6_hte_results[[dataset_name]] <- res
}

table6_hte_final <- rbindlist(table6_hte_results)
print(table6_hte_final)