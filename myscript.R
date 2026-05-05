pkgs <- c("haven", "dplyr", "tidyr", "ggplot2", "fixest", "modelsummary",
          "readxl", "stringr", "scales")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(haven)        # read_dta()
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)       # feols() — fast OLS + fixed effects
library(modelsummary) # publication-ready regression tables
library(readxl)       # read_excel() for WDI (optional merge)
library(stringr)
library(scales)

# Load subnational data
dta_path <- "Conceptualizing_and_Measuring_Subnational_Democracy_Across_Indian_States_raw.dta"
df_raw <- read_dta(dta_path)

glimpse(df_raw)

# 2. Variables
df <- df_raw %>%
  mutate(
    year       = as.integer(year),
    log_nbdeath = log1p(nbdeath),          # log(1 + deaths) — right-skewed
    year_c     = year - mean(year, na.rm = TRUE),  # center year for interpretation
    pr         = as.integer(pr),
    ut         = as.integer(ut),
    stateut    = as.factor(stateut)
  ) %>%
  filter(!is.na(mov))                      # drop the 1 obs with missing DV

cat("Observations after dropping missing DV:", nrow(df), "\n")
cat("Year range:", min(df$year), "–", max(df$year), "\n")
cat("States:", nlevels(df$stateut), "\n\n")

# 3. Statistics
desc_vars <- c("mov", "enpl", "sop", "pr", "log_nbdeath", "prc_consta", "ut", "year_c")

cat("=== Descriptive Statistics ===\n")
df %>%
  select(all_of(desc_vars)) %>%
  summary() %>%
  print()

annual_means <- df %>%
  group_by(year) %>%
  summarise(avg_mov = mean(mov), .groups = "drop")

state_enpl <- df %>%
  group_by(stateut) %>%
  summarise(
    mean_enpl = mean(enpl),
    sd_enpl   = sd(enpl),
    n         = n(),
    .groups   = "drop"
  ) 


# 4. Models 

# Predictors (same across all models)
xvars <- c("enpl", "sop", "pr", "log_nbdeath", "prc_consta", "ut", "year_c")

# Model 1: Pooled OLS (no fixed effects)
m1 <- feols(mov ~ enpl + sop + pr + log_nbdeath + prc_consta + ut + year_c,
            data = df, vcov = "HC1")   # HC1 = heteroskedasticity-robust SE

# Model 2: Fixed  OLS
m2 <- feols(mov ~ enpl + sop + pr + log_nbdeath + prc_consta + ut + year_c
            | stateut,
            data = df, vcov = "HC1")

# Model 3: Two-way Fixed Effects (state + year)
m3 <- feols(mov ~ enpl + sop + pr + log_nbdeath + prc_consta + ut
            | stateut + year,
            data = df, vcov = "HC1")

# Print results
cat("\n=== Model 1: Pooled OLS ===\n")
summary(m1)

cat("\n=== Model 2: State Fixed Effects ===\n")
summary(m2)

cat("\n=== Model 3: Two-way FE (state + year) ===\n")
summary(m3)

# 5. Summary Table (Regressions)
coef_rename <- c(
  "enpl"        = "Eff. no. of parties (ENPL)",
  "sop"         = "Ruling party vote share (SOP)",
  "pr"          = "President's Rule",
  "log_nbdeath" = "Log(1 + election deaths)",
  "prc_consta"  = "% uncontested constituencies",
  "ut"          = "Union territory",
  "year_c"      = "Year (centred)"
) # renaming coefficients for model summary

modelsummary(
  list("Pooled OLS" = m1, "State FE" = m2, "State + Year FE" = m3),
  coef_rename   = coef_rename,
  title         = "Predictors of Margin of Victory — Indian State Elections (1985–2013)",
  output        = "regression_table.txt"
) # saved results in a regression_table

# 6. Plots
# plot for average margin of victory
avg_mov <- ggplot(annual_means, aes(x = year, y = avg_mov)) + 
  geom_point() + 
  geom_smooth() + 
  labs(
    title = "Average margin of victory across Indian state elections", 
    x = "Election year", 
    y = "Avg. margin of victory"
  ) + 
  theme_minimal() 

# plot for mean enpl by state
mean_enpl <- ggplot(state_enpl, aes(x = mean_enpl, y = stateut)) + 
  geom_point() # need to sort by mean_enpl and work on labels


