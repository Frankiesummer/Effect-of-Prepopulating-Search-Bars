# Script: R/utils.R
# Description: Helper functions for formatting statistics and regression tables.

library(dplyr)
library(knitr)
library(kableExtra)
library(sandwich)
library(lmtest)
library(broom)

# --- 1. Formatting Functions ---

# Add significance stars based on p-value
add_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01, "**",
                       ifelse(p < 0.05, "*", ""))))
}

# Format coefficient with standard error and stars
# Output format: "0.123*** (0.045)"
format_coef_se <- function(est, se, p = NULL) {
  stars <- add_stars(p)
  # sprintf creates a string, <br> is for HTML tables, newline for text
  sprintf("%.3f%s (%.3f)", est, stars, se)
}

# Format p-values strictly
format_pvalue <- function(p) {
  ifelse(is.na(p), "NA",
         ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

# --- 2. Regression Helper ---

# A wrapper to run robust LM and return tidy results
run_robust_lm <- function(formula_str, data) {
  fit <- lm(as.formula(formula_str), data = data)
  # Calculate Robust Standard Errors (HC1)
  vc <- vcovHC(fit, type = "HC1") 
  # Get tidy summary with robust SEs
  res <- coeftest(fit, vc)
  tidy_res <- tidy(res)
  
  return(list(
    model = fit,
    vcov = vc,
    tidy = tidy_res,
    n_obs = nobs(fit),
    r_squared = summary(fit)$r.squared
  ))
}