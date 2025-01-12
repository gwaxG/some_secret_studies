# ------------------------------------------------------------------------------
#            The causal effect of economic sanctions on political
#         stability: A two-stage difference-in-differences analysis.
#
# This script reproduces stata do file calculations step-by-step.
# ------------------------------------------------------------------------------

# ---- INSTALLATIONS ----
# install.packages("fixest")
# install.packages("Rcpp")

# ---- IMPORTS ----
library(Rcpp)
library(haven)     # .dta files reading
library(dplyr)     # Data manipulation
library(fixest)    # Fixed-effects regressions
library(boot)      # For bootstrapping (if needed)

# ---- Reproduction ----
# Read dataset (assumes "DonganTanReplication.dta" is in your working directory)
# Equivalent to: `use DonganTanReplication, clear`
df <- haven::read_dta("DonganTanReplication.dta")

# ---- Step 1: Encode country as state (Stata: encode country, gen(state)) ----
# Convert country to a factor variable for panel data analysis
df <- df %>%
  mutate(state = as.factor(country))

# ---- Step 2: Define panel structure (Stata: xtset state year) ----
# R doesn't require this step explicitly. Panel structure is handled during regression using `fixest`.

# ---- Step 3: Summary statistics (Stata: sum treat mm $controls if e(sample)) ----
# Define controls as a vector
controls <- c("polity", "EcGI", "lgdp", "interwar", "intrawar", "lcinc", "efindex")

# Generate summary statistics for relevant variables
summary(df %>% select(treat, mm, all_of(controls)))

# ---- Step 4: Table 1 - Fixed-effects regression ----
# Equivalent to: `xtreg mm i.treat i.year $controls, fe vce(cluster state)`
model_table1 <- feols(
  mm ~ i(treat) + i(year) + polity + EcGI + lgdp + interwar + intrawar + lcinc + efindex | state,
  cluster = ~state,
  data = df
)
summary(model_table1)

# ---- Step 5: Drop observations for the year 1949 and treat == 1 ----
# Stata: `drop if year==1949 & treat==1`
df <- df %>% filter(!(year == 1949 & treat == 1))

# ---- Step 6: Table 2 - Effects of economic sanctions on mass mobilization ----

## Model (1): Fixed effects regression without controls
# Stata: `xtreg mm i.treat i.year, fe vce(cluster state)`
model_1 <- feols(mm ~ i(treat) + i(year) | state, cluster = ~state, data = df)
summary(model_1)

## Model (2): Fixed effects regression with controls
# Stata: `xtreg mm i.treat i.year $controls, fe vce(cluster state)`
model_2 <- feols(
  mm ~ i(treat) + i(year) + polity + EcGI + lgdp + interwar + intrawar + lcinc + efindex | state,
  cluster = ~state,
  data = df
)
summary(model_2)

# ---- Step 7: Model (3): Two-step regression with residuals ----

## Step 1: Partial out state and year (treat == 0)
# Stata: `reg mm i.state i.year if treat == 0, nocons`
df_treat0 <- df_treat0 %>% filter(complete.cases(state, year, mm))
model_3_step1 <- lm(mm ~ factor(state) + factor(year) - 1, data = df_treat0)
df_treat0 <- df_treat0 %>% mutate(adj = residuals(model_3_step1))

## Step 2: Regress residuals on treat
# Stata: `reg adj i.treat, vce(bootstrap)`
model_3_step2 <- lm(adj ~ treat, data = df_treat0)
summary(model_3_step2)

# ---- Step 8: Model (4): Add controls to the regression with residuals ----
# Stata: `reg adj i.treat $controls, vce(bootstrap)`
model_4 <- lm(
  adj ~ treat + polity + EcGI + lgdp + interwar + intrawar + lcinc + efindex,
  data = df_treat0
)
summary(model_4)

# ---- Step 9: Model (5): Repeat two-step regression for ever_sanctioned == 0 ----

## Step 1: Partial out state and year (ever_sanctioned == 0)
# Stata: `reg mm i.state i.year if ever_sanctioned == 0, nocons`
df_never_sanctioned <- df %>% filter(ever_sanctioned == 0)
model_5_step1 <- lm(mm ~ factor(state) + factor(year) - 1, data = df_never_sanctioned, na.action = na.exclude)
df_never_sanctioned <- df_never_sanctioned %>% mutate(adj = residuals(model_5_step1))

## Step 2: Regress residuals on treat
# Stata: `reg adj i.treat, vce(bootstrap)`
model_5_step2 <- lm(adj ~ treat, data = df_never_sanctioned)
summary(model_5_step2)

# ---- Step 10: Model (6): Add controls to the regression with residuals ----
# Stata: `reg adj i.treat $controls, vce(bootstrap)`
model_6 <- lm(
  adj ~ treat + polity + EcGI + lgdp + interwar + intrawar + lcinc + efindex,
  data = df_never_sanctioned
)
summary(model_6)

# ---- Notes ----
# - Models 1 and 2 use fixed-effects regressions (handled by `fixest`).
# - Models 3 through 6 implement a two-step regression process:
#   - Step 1: Residualize the dependent variable (partial out state and year effects).
#   - Step 2: Regress the residuals on treatment and controls.
# - The `bootstrap` option in Stata is not directly implemented here. You can use the `boot` package in R if needed.

