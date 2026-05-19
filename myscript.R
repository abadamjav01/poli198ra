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
row <- data.frame(
  "Coefficients" = c("State FE", "Year FE"),
  "Pooled OLS"      = c("No",  "No"),
  "State FE"        = c("Yes", "No"),
  "State + Year FE" = c("Yes", "Yes")
) # renaming coefficients for model summary

attr(row, "position") <- c(17, 18)

modelsummary(
  list("Pooled OLS" = m1, "State FE" = m2, "State + Year FE" = m3),
  add_rows      = row,
  title         = "Predictors of Margin of Victory — Indian State Elections (1985–2013)",
  gof_map       = c("nobs", "r.squared", "adj.r.squared", "rmse"),
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


# Plots for the different economic indicators
api_df <- read_excel("~/Downloads/api.xlsx", col_names = FALSE)

# 3 different indicators
rm(to_df, df_gdp, df_infant, df_unemp, df_all)

years <- as.integer(as.numeric(api_df[1, -1]))

df_gdp    <- data.frame(year = years, value = gdp,    indicator = "GDP per capita (US$)")[!is.na(gdp), ]
df_infant <- data.frame(year = years, value = infant, indicator = "Infant mortality (per 1,000)")[!is.na(infant), ]
df_unemp  <- data.frame(year = years, value = unemp,  indicator = "Unemployment")[!is.na(unemp), ]

normalize <- function(x) (x - min(x)) / (max(x) - min(x))

df_gdp$nor <- normalize(df_gdp$value)
df_infant$nor <- normalize(df_infant$value)
df_unemp$nor <- normalize(df_unemp$value)

df_all <- rbind(df_gdp, df_infant, df_unemp)

colors <- c(
  "GDP per capita (US$)"         = "red",
  "Infant mortality (per 1,000)" = "blue",
  "Unemployment"                 = "yellow" 
)

# Normalized
ggplot(df_all, aes(x = year, y = nor, color = indicator)) +
  geom_line() +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(values = colors) + 
  labs(title = "India development indicators - normalized (0-1)", 
       x = NULL, y = "Normalized value") +
  theme(legend.position = "bottom")

ggplot(df_all, aes(x = year, y = value, color = indicator, fill = indicator)) +
  geom_area() +
  geom_line() +
  facet_wrap(~ indicator, scales = "free_y", ncol = 3) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(title = "India development indicators - facet_wrap",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

# GDP per Capita
ggplot(df_gdp, aes(x = year, y = value)) +
  geom_area(fill = "steelblue") + geom_line() + 
  labs(title = "GDP per Capita — India", x = NULL, y = "Current US$") +
  theme_minimal()

# Infant Mortality
ggplot(df_infant, aes(x = year, y = value)) +
  geom_area(fill = "steelblue") + geom_line() + 
  labs(title = "Infant Mortality — India", x = NULL, y = "Per 1,000 live births") +
  theme_minimal()

# Unemployment Rate
ggplot(df_unemp, aes(x = year, y = value)) +
  geom_area(fill = "steelblue") + geom_line() + 
  labs(title = "Unemployment — India", x = NULL, y = "% of labor force") +
  theme_minimal()
