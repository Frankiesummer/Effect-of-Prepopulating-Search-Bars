# ---------------------------------------------------------------------
# Script: 01_table3_4_search_behavior.R
# Content: Replicates Table 3 (Main Effects) and Table 4 (Query Composition)
# ---------------------------------------------------------------------

# Packages
library(haven)
library(dplyr)
library(tidyr)
library(data.table)
library(sandwich)
library(lmtest)
library(car)
library(broom)
library(knitr)
library(kableExtra)
library(stargazer)
library(here) # For relative paths

# Load helper functions
source(here("R", "utils.R"))

# =====================================================================
# PART 1: Table 3 (Main Effects)
# =====================================================================
message("=== Generating Table 3 ===")

# 1. Load Data
# Path based on your screenshot: data/raw/Table 3
path3 <- here("data", "raw", "Table 3")

click    <- read_dta(file.path(path3, "Table3_PageClick.dta"))
purchase <- read_dta(file.path(path3, "Table3_Purchase.dta"))
query    <- read_dta(file.path(path3, "Table3_Query.dta"))
spending <- read_dta(file.path(path3, "Table3_Spending.dta"))

# Merge all
df <- click %>%
  select(ID, Female, AgeTier, T1, T2, T3, PageClick) %>%
  left_join(select(purchase, ID, Purchase),  by = "ID") %>%
  left_join(select(query,    ID, Query),     by = "ID") %>%
  left_join(select(spending, ID, Spending),  by = "ID")

df <- as.data.table(df)

# Define group for summary stats
df[, Group := fcase(
  T1 == 1, "Trending",
  T2 == 1, "Personalized",
  T3 == 1, "Niche",
  default  = "Control"
)]

# 2. Main Regression Analysis
run_main <- function(outcome) {
  f <- as.formula(paste0(outcome, " ~ T1 + T2 + T3 + Female + factor(AgeTier)"))
  fit <- lm(f, data = df)
  vc <- vcovHC(fit, type = "HC1")
  res <- coeftest(fit, vc)
  td <- tidy(res)
  
  ctrl_mean <- mean(df[T1==0 & T2==0 & T3==0][[outcome]], na.rm = TRUE)
  r2 <- summary(fit)$r.squared
  
  # Hypothesis Testing
  pv12 <- linearHypothesis(fit, "T1 - T2 = 0", vcov = vc)$`Pr(>F)`[2]
  pv13 <- linearHypothesis(fit, "T1 - T3 = 0", vcov = vc)$`Pr(>F)`[2]
  pv32 <- linearHypothesis(fit, "T3 - T2 = 0", vcov = vc)$`Pr(>F)`[2]
  
  return(list(name=outcome, td=td, ctrl=ctrl_mean, r2=r2,
              p12=pv12, p13=pv13, p32=pv32))
}

outs <- c("Query","PageClick","Purchase","Spending")
results3 <- lapply(outs, run_main)

# 3. Construct Table 3 Output
table3_final <- data.frame(
  row = c("Treat_Trending (T1)", "Treat_Personalized (T2)", "Treat_Niche (T3)",
          "Control (C) mean", "p-value (T1 - T2)", "p-value (T1 - T3)", "p-value (T3 - T2)",
          "Control variables", "Observations", "R2"),
  Query = "", PageClick = "", Purchase = "", Spending = "",
  stringsAsFactors = FALSE
)

for (i in 1:4) {
  r <- results3[[i]]
  outcome <- r$name
  col_idx <- which(names(table3_final) == outcome)
  
  t1 <- r$td[r$td$term == "T1", ]
  t2 <- r$td[r$td$term == "T2", ]
  t3 <- r$td[r$td$term == "T3", ]
  
  table3_final[1, col_idx] <- format_coef_se(t1$estimate, t1$std.error, t1$p.value)
  table3_final[2, col_idx] <- format_coef_se(t2$estimate, t2$std.error, t2$p.value)
  table3_final[3, col_idx] <- format_coef_se(t3$estimate, t3$std.error, t3$p.value)
  table3_final[4, col_idx] <- sprintf("%.3f", r$ctrl)
  table3_final[9, col_idx] <- "72,587"
  table3_final[10, col_idx] <- sprintf("%.3f", r$r2)
}

p12 <- sapply(results3, function(x) format_pvalue(x$p12))
p13 <- sapply(results3, function(x) format_pvalue(x$p13))
p32 <- sapply(results3, function(x) format_pvalue(x$p32))

table3_final[5, 2:5] <- p12
table3_final[6, 2:5] <- p13
table3_final[7, 2:5] <- p32
table3_final[8, 2:5] <- "Yes"

# Print Table 3
print(kable(table3_final, escape = FALSE, caption = "Table 3. Main Effects") %>%
        kable_styling(full_width = FALSE))


# =====================================================================
# PART 2: Table 4 (Query Composition)
# =====================================================================
message("=== Generating Table 4 ===")

# 1. Define Paths (Based on screenshot structure)
path_left  <- here("data", "raw", "Table 4 LeftPanel")
path_mid   <- here("data", "raw", "Table 4 MidPanel")
path_right <- here("data", "raw", "Table 4 RightPanel")

# 2. Helper to read triplet files
read_three <- function(path, prefix) {
  focus_suffix <- switch(prefix,
                         "T1" = "QTrending",
                         "T2" = "QPersonalized",
                         "T3" = "QNiche",
                         stop("Unknown prefix: ", prefix))
  
  # Note: Filenames are assumed to be consistent with original code
  f1 <- read_dta(file.path(path, paste0("Table4_", prefix, "_QOther.dta")))
  f2 <- read_dta(file.path(path, paste0("Table4_", prefix, "_", focus_suffix, ".dta")))
  f3 <- read_dta(file.path(path, paste0("Table4_", prefix, "_Query.dta")))
  list(QOther = f1, QFocus = f2, Query = f3)
}

left  <- read_three(path_left,  "T1")
mid   <- read_three(path_mid,   "T2")
right <- read_three(path_right, "T3")

# 3. Panel Analysis Function
run_panel_final <- function(datalist, treat_var, panel_name) {
  focus_name <- switch(treat_var,
                       "T1" = "QueryTrending",
                       "T2" = "QueryPersonalized",
                       "T3" = "QueryNiche")
  res <- data.frame()
  
  for (nm in names(datalist)) {
    dfp <- as.data.table(datalist[[nm]])
    outcome <- setdiff(names(dfp), c("ID","Female","AgeTier", treat_var))[1]
    
    f <- as.formula(paste0(outcome, " ~ ", treat_var, " + Female + factor(AgeTier)"))
    fit <- lm(f, data = dfp)
    vc <- vcovHC(fit, type = "HC1")
    td <- broom::tidy(coeftest(fit, vc))
    
    treat_row <- td[td$term == treat_var, ]
    est <- treat_row$estimate
    se  <- treat_row$std.error
    p   <- treat_row$p.value
    
    ctrl_mean <- mean(dfp[get(treat_var) == 0][[outcome]], na.rm = TRUE)
    N_total   <- nrow(dfp)
    r2        <- summary(fit)$r.squared
    
    dep_label <- switch(nm,
                        "Query" = "Query",
                        "QFocus" = focus_name,
                        "QOther" = "QueryOther")
    
    res <- rbind(res, data.frame(
      Panel = panel_name,
      Item = c("Treat_X", "Control (C) mean", "Control variables", "Observations", "R2"),
      Dependent = dep_label,
      Value = c(
        format_coef_se(est, se, p),
        sprintf("%.3f", ctrl_mean),
        "Yes",
        as.character(N_total),
        sprintf("%.3f", r2)
      ),
      stringsAsFactors = FALSE
    ))
  }
  return(res)
}

left4  <- run_panel_final(left,  "T1", "Panel A: T1")
mid4   <- run_panel_final(mid,   "T2", "Panel B: T2")
right4 <- run_panel_final(right, "T3", "Panel C: T3")

table4_all <- rbind(left4, mid4, right4)

# Print Table 4 Results
print(kable(table4_all, caption = "Table 4. Query Composition") %>%
        kable_styling(full_width = FALSE))

# =====================================================================
# PART 3: Table 4 HTE by Gender
# =====================================================================
message("=== Generating Table 4 HTE ===")

hte_gender <- function(df_list, treat_var, panel_name) {
  focus_name <- switch(treat_var, "T1" = "QTrending", "T2" = "QPersonalized", "T3" = "QNiche")
  res <- data.table()
  
  for (nm in names(df_list)) {
    dt <- as.data.table(df_list[[nm]])
    outcome <- setdiff(names(dt), c("ID", "Female", "AgeTier", treat_var))[1]
    f <- as.formula(paste0(outcome, " ~ ", treat_var, " + Female + factor(AgeTier) + ", treat_var, ":Female"))
    fit <- lm(f, data = dt)
    vc <- vcovHC(fit, type = "HC1")
    td <- tidy(coeftest(fit, vc))
    
    male_row <- td %>% filter(term == treat_var)
    int_row  <- td %>% filter(term == paste0(treat_var, ":Female"))
    
    male_est <- male_row$estimate; male_se <- male_row$std.error; male_p <- male_row$p.value
    int_est  <- int_row$estimate;  int_p   <- int_row$p.value
    female_est <- male_est + int_est
    
    # Calculate SE for female effect
    se_female <- sqrt(vc[treat_var, treat_var] + 
                        vc[paste0(treat_var, ":Female"), paste0(treat_var, ":Female")] +
                        2 * vc[treat_var, paste0(treat_var, ":Female")])
    
    ctrl_mean <- mean(dt[dt[[treat_var]] == 0][[outcome]], na.rm = TRUE)
    dep_label <- switch(nm, "Query" = "Query", "QFocus" = focus_name, "QOther" = "QOther")
    
    res <- rbind(res, data.table(
      Panel = panel_name, Dependent = dep_label,
      Male_Effect = format_coef_se(male_est, male_se, male_p),
      Female_Effect = format_coef_se(female_est, se_female, int_p),
      p_Diff_Gender = format_pvalue(int_p),
      Ctrl_Mean = sprintf("%.3f", ctrl_mean)
    ))
  }
  return(res)
}

hte_left  <- hte_gender(left,  "T1", "Panel A: Trending")
hte_mid   <- hte_gender(mid,   "T2", "Panel B: Personalized")
hte_right <- hte_gender(right, "T3", "Panel C: Niche")

hte_all <- rbind(hte_left, hte_mid, hte_right)

print(kable(hte_all, caption = "Table 4. HTE by Gender") %>% kable_styling())