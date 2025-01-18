install.packages(c("rstudioapi"))
library(haven)         # For reading Stata .dta files
library(plm)      # for panel data models
library(dplyr)    # for data manipulation
library(boot)     # for bootstrap
library(lmtest)
library(sandwich)
library(modelsummary)  # For table display
library(did2s)      # For two-stage DiD estimation
library(fixest)     # For fixed effects models
library(stargazer)  # For formatted tables
library(margins)  # For marginal effects
library(ggplot2)  # For plotting

# Stata: use DonganTanReplication, clear
df <- read_dta("DonganTanReplication.dta")  

summary_stats <- df %>%
  summarise(
    mean_treat = mean(treat, na.rm = TRUE),
    sd_treat = sd(treat, na.rm = TRUE),
    mean_mm = mean(mm, na.rm = TRUE),
    sd_mm = sd(mm, na.rm = TRUE),
    mean_polity = mean(polity, na.rm = TRUE),
    sd_polity = sd(polity, na.rm = TRUE),
    mean_gdp = mean(gdp, na.rm = TRUE),
    sd_gdp = sd(gdp, na.rm = TRUE),
    mean_interwar = mean(interwar, na.rm = TRUE),
    sd_interwar = sd(interwar, na.rm = TRUE),
    mean_intrawar = mean(intrawar, na.rm = TRUE),
    sd_intrawar = sd(intrawar, na.rm = TRUE),
    mean_EcGI = mean(EcGI, na.rm = TRUE),
    sd_EcGI = sd(EcGI, na.rm = TRUE),
    mean_cinc = mean(cinc, na.rm = TRUE),
    sd_cinc = sd(cinc, na.rm = TRUE),
    mean_lgdp = mean(lgdp, na.rm = TRUE),
    sd_lgdp = sd(lgdp, na.rm = TRUE)
  )

# Display summary statistics in a formatted table
stargazer(summary_stats, type = "text", title = "Table 1: Summary Statistics")

# Table 2
# Convert country variable to factor for clustering
controls <- c("polity", "EcGI", "lgdp", "interwar", "intrawar", "lcinc", "efindex")

# Model 1: Fixed Effects Regression (Stata: xtreg mm i.treat i.year, fe vce(cluster state))
model_1 <- feols(mm ~ i(treat) | country + year, data = df, cluster = ~ country)

# Model 2: Fixed Effects with Controls (xtreg mm i.treat i.year $controls, fe vce(cluster state))
model_2 <- feols(as.formula(paste("mm ~ i(treat) +", paste(controls, collapse = " + "), "| country + year")), 
                 data = df, cluster = ~ country)

# Step 1: First-stage regression to remove fixed effects
first_stage <- feols(mm ~ 1 | country + year, data = df)

# Step 2: Extract residuals and merge back into df_clean
df_clean <- df[!is.na(df$mm), ]
df_clean$mm_resid <- residuals(first_stage)

# Model 3: Residualized Regression for All Units
model_3 <- feols(mm_resid ~ i(treat), data = df_clean, cluster = ~ country)

# Model 4: Residualized Regression with Controls
model_4 <- feols(as.formula(paste("mm_resid ~ i(treat) +", paste(controls, collapse = " + "))), 
                 data = df_clean, cluster = ~ country)

# Model 5: Residualized Regression with Interaction for Units Never Sanctioned
model_5 <- feols(mm_resid ~ i(treat) * i(ever_sanctioned), data = df_clean, cluster = ~ country)

# Model 6: Residualized Regression with Controls and Interaction
model_6 <- feols(as.formula(paste("mm_resid ~ i(treat) + i(ever_sanctioned) +", paste(controls, collapse = " + "))), 
                 data = df_clean, cluster = ~ country)

# Show results in a table
models <- list("Model 1" = model_1, "Model 2" = model_2, "Model 3" = model_3, "Model 4" = model_4, "Model 5" = model_5, "Model 6" = model_6)

modelsummary(models, gof_omit = "IC|Log|Adj|RMSE")

# Table 3
df <- read_dta("DonganTanReplication.dta")  
df$yearXdem <- df$year * df$dem
df$stateXdem <- as.numeric(df$country) * df$dem
df$yearXEcGI <- df$year * df$lowEcGI
df$stateXEcGI <- as.numeric(df$country) * df$lowEcGI

controls <- c("lgdp", "interwar", "intrawar", "lcinc", "efindex")

# --------------------- Model 1: Unsanctioned - Democracy ---------------------
df_no_treat <- subset(df, treat == 0)
df_no_treat <- na.omit(df_no_treat)

# First stage: Residualize mm (remove fixed effects)
first_stage_1 <- feols(mm ~ yearXdem + stateXdem | country + year, data = df_no_treat)
df_no_treat$mm_resid <- residuals(first_stage_1)

# Second stage: Estimate treatment effects
model_1 <- feols(as.formula(paste("mm_resid ~ i(treat) * i(dem) + EcGI +", paste(controls, collapse = " + "))),
                 data = df_no_treat, cluster = ~ country)

# Marginal effects of sanctions at different levels of democracy
margins_1 <- margins(model_1, variables = "treat", at = list(dem = c(0, 1)))

# --------------------- Model 2: Not-yet-Sanctioned - Democracy ---------------------
df_no_sanction <- subset(df, ever_sanctioned == 0)

# First stage: Residualize mm
first_stage_2 <- feols(mm ~ 1 | country + year + yearXdem + stateXdem, data = df_no_sanction)
df_no_sanction$mm_resid <- residuals(first_stage_2)

# Second stage: Estimate treatment effects
model_2 <- feols(as.formula(paste("mm_resid ~ i(treat) * i(dem) + EcGI +", paste(controls, collapse = " + "))),
                 data = df_no_sanction, cluster = ~ country)

# Marginal effects of sanctions at different levels of democracy
margins_2 <- margins(model_2, variables = "treat", at = list(dem = c(0, 1)))

# --------------------- Model 3: Unsanctioned - Globalization ---------------------
# First stage: Residualize mm
first_stage_3 <- feols(mm ~ 1 | country + year + yearXEcGI + stateXEcGI, data = df_no_treat)
df_no_treat$mm_resid <- residuals(first_stage_3)

# Second stage: Estimate treatment effects
model_3 <- feols(as.formula(paste("mm_resid ~ i(treat) * i(lowEcGI) + polity +", paste(controls, collapse = " + "))),
                 data = df_no_treat, cluster = ~ country)

# Marginal effects of sanctions at different levels of globalization
margins_3 <- margins(model_3, variables = "treat", at = list(lowEcGI = c(0, 1)))

# --------------------- Model 4: Not-yet-Sanctioned - Globalization ---------------------
# First stage: Residualize mm
first_stage_4 <- feols(mm ~ 1 | country + year + yearXEcGI + stateXEcGI, data = df_no_sanction)
df_no_sanction$mm_resid <- residuals(first_stage_4)

# Second stage: Estimate treatment effects
model_4 <- feols(as.formula(paste("mm_resid ~ i(treat) * i(lowEcGI) + polity +", paste(controls, collapse = " + "))),
                 data = df_no_sanction, cluster = ~ country)

# Marginal effects of sanctions at different levels of globalization
margins_4 <- margins(model_4, variables = "treat", at = list(lowEcGI = c(0, 1)))

# --------------------- Display Results ---------------------
models <- list("Model 1" = model_1, "Model 2" = model_2, "Model 3" = model_3, "Model 4" = model_4)

modelsummary(models, gof_omit = "IC|Log|Adj|RMSE")

# --------------------- Plot Marginal Effects ---------------------

# Convert margins to data frames
margins_1_df <- as.data.frame(margins_1)
margins_2_df <- as.data.frame(margins_2)
margins_3_df <- as.data.frame(margins_3)
margins_4_df <- as.data.frame(margins_4)

# Democracy Margins Plot
ggplot(margins_1_df, aes(x = dem, y = dydx_treat)) +
  geom_point() +
  geom_line() +
  labs(title = "A: Unsanctioned", x = "", y = "Marginal Effect of Sanctions") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Autocracy", "Democracy")) +
  theme_minimal()

ggplot(margins_2_df, aes(x = dem, y = dydx_treat)) +
  geom_point() +
  geom_line() +
  labs(title = "B: Not-yet-Sanctioned", x = "", y = "Marginal Effect of Sanctions") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Autocracy", "Democracy")) +
  theme_minimal()

# Globalization Margins Plot
ggplot(margins_3_df, aes(x = lowEcGI, y = dydx_treat)) +
  geom_point() +
  geom_line() +
  labs(title = "A: Unsanctioned", x = "", y = "Marginal Effect of Sanctions") +
  scale_x_continuous(breaks = c(0, 1), labels = c("High-globalized", "Low-globalized")) +
  theme_minimal()

ggplot(margins_4_df, aes(x = lowEcGI, y = dydx_treat)) +
  geom_point() +
  geom_line() +
  labs(title = "B: Not-yet-Sanctioned", x = "", y = "Marginal Effect of Sanctions") +
  scale_x_continuous(breaks = c(0, 1), labels = c("High-globalized", "Low-globalized")) +
  theme_minimal()