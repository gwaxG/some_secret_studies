# Load required packages
install.packages(c("stargazer"))

library(haven)         # For reading Stata .dta files
library(plm)      # for panel data models
library(dplyr)    # for data manipulation
library(boot)     # for bootstrap
library(lmtest)
library(clubSandwich)
library(stargazer)



# Stata: use DonganTanReplication, clear
data <- read_dta("DonganTanReplication.dta")  
summary(data)
# TABLE 1, just data summary, we skip.
# Stata: drop if year==1949 & treat==1
data <- data %>% 
  filter(!(year == 1949 & treat == 1))

# Stata: encode country, gen(state)
data <- data %>%
  mutate(state = as.numeric(factor(country)))

# Stata: xtset state year
# Convert to panel data format
data <- pdata.frame(data, index = c("state", "year"))

# Stata: global controls polity EcGI lgdp interwar intrawar lcinc efindex
controls <- c("polity", "EcGI", "lgdp", "interwar", "intrawar", "lcinc", "efindex")

controls_formula <- paste(controls, collapse = " + ")

# Stata: xtreg mm i.treat i.year $controls, fe vce(cluster state)
model_table1 <- plm(as.formula(paste("mm ~ factor(treat) + factor(year) +", controls_formula)),
                    data = data,
                    model = "within",
                    effect = "twoways",
                    cluster = "state")

# Stata: sum treat mm $controls if e(sample)
summary_stats <- data %>%
  select(treat, mm, all_of(controls)) %>%
  filter(complete.cases(.)) %>%  # equivalent to e(sample)
  summary()

# Table 2 Models
# TWFE

controls <- c("polity", "EcGI", "lgdp", "interwar", "intrawar", "lcinc", "efindex")
controls_formula <- paste(controls, collapse = " + ")

# TWFE Model (1): Without controls
data <- read_dta("DonganTanReplication.dta")  
model1 <- plm(mm ~ factor(treat) + factor(year),
              data = data,
              model = "within",  # Fixed effects (FE)
              effect = "twoways") # Two-way FE

# Clustered standard errors for Model (1)
se_model1 <- coef_test(model1, vcov = vcovCR(model1, cluster = data$state, type = "CR0"))

# TWFE Model (2): With controls
model2 <- plm(as.formula(paste("mm ~ factor(treat) + factor(year) +", controls_formula)),
              data = data,
              model = "within",
              effect = "twoways")

# Clustered standard errors for Model (2)
se_model2 <- coef_test(model2, vcov = vcovCR(model2, cluster = data$state, type = "CR0"))

# Format results in a table
stargazer(model1, model2,
          se = list(se_model1[, "SE"], se_model2[, "SE"]),
          title = "Effects of Economic Sanctions on Mass Mobilization",
          dep.var.labels = "Mass Mobilization",
          column.labels = c("(1)", "(2)"),
          covariate.labels = c("Sanctions Imposition", "Polity", "Economic Globalization",
                               "Ln(GDP)", "Interstate Conflicts", "Intrastate Conflicts",
                               "Ln(National Capability)", "Ethnic Fractionalization"),
          type = "text",
          omit = "factor",  # Exclude fixed effects from table
          add.lines = list(c("Observations", nobs(model1), nobs(model2)),
                           c("R-squared", round(summary(model1)$r.squared[1], 3),
                             round(summary(model2)$r.squared[1], 3))))
# NOTES, this output does not fully correspond to Table 1 TWFE output. 
# At least, it calculates something. TO REVISE MODELS

# Table 2, 2sDiD (unsanctioned), it corresponds to model 3 and 4 of the source stata code.
# Define controls
# Generate interaction terms for treatment and time
data <- read_dta("DonganTanReplication.dta")  
data <- data %>%
  mutate(treat_period = factor(treat) * factor(year))

# Filter complete cases for the first-stage regression
data_complete <- data %>%
  filter(complete.cases(select(., all_of(controls))))

# First-stage regression
stage1 <- lm(as.formula(paste("mm ~", controls_formula)), data = data_complete)

# Assign residuals back to the original dataset
data$residual_mm <- NA  # Initialize residual column
data$residual_mm[complete.cases(select(data, all_of(controls)))] <- residuals(stage1)

# Second-stage regression (Difference-in-Differences)
model_2sDID <- lm(residual_mm ~ factor(treat) * factor(year), data = data)

# Clustered standard errors
se_model_2sDID <- coef_test(model_2sDID, vcov = vcovCR(model_2sDID, cluster = data$state, type = "CR0"))

# Summarize results
summary(model_2sDID)

# Format results in a table (for consistency with your workflow)
stargazer(model_2sDID,
          se = list(se_model_2sDID[, "SE"]),
          title = "Effects of Economic Sanctions on Mass Mobilization",
          dep.var.labels = "Residual Mass Mobilization",
          column.labels = c("(1)"),
          covariate.labels = c("Sanctions Imposition", "Polity", "Economic Globalization",
                               "Ln(GDP)", "Interstate Conflicts", "Intrastate Conflicts",
                               "Ln(National Capability)", "Ethnic Fractionalization"),
          type = "text",
          omit = "factor",  # Exclude fixed effects from the table
          add.lines = list(c("Observations", nobs(model_2sDID)),
                           c("R-squared", round(summary(model_2sDID)$r.squared, 3))))

# Table 2, 2sDID not-yet-sanctioned
# Filter data to exclude observations with `ever_sanctioned == 1`
# Step 1: Filter the dataset for ever_sanctioned == 0
data <- read_dta("DonganTanReplication.dta")  
data <- data %>%
  filter(ever_sanctioned == 0 | treat == 1)  # Keep treated observations

# Filter complete cases for the first-stage regression
data_complete <- data %>%
  filter(complete.cases(select(., all_of(controls))))

# First-stage regression
stage1 <- lm(as.formula(paste("mm ~", controls_formula)), data = data_complete)

# Assign residuals back to the original dataset
data$residual_mm <- NA  # Initialize residual column
residual_rows <- which(complete.cases(select(data, all_of(controls))))  # Indices of complete cases
data$residual_mm[residual_rows] <- residuals(stage1)

# Second-stage regression (Difference-in-Differences)
model_2sDID <- lm(residual_mm ~ factor(treat) * factor(year), data = data)

# Clustered standard errors
se_model_2sDID <- coef_test(model_2sDID, vcov = vcovCR(model_2sDID, cluster = data$country, type = "CR0"))

# Summarize results
summary(model_2sDID)

# Format results in a table (for consistency with your workflow)
stargazer(model_2sDID,
          se = list(se_model_2sDID[, "SE"]),
          title = "Effects of Economic Sanctions on Mass Mobilization",
          dep.var.labels = "Mass Mobilization",
          column.labels = c("(1)"),
          covariate.labels = c("Sanctions Imposition", "Polity", "Economic Globalization",
                               "Ln(GDP)", "Interstate Conflicts", "Intrastate Conflicts",
                               "Ln(National Capability)", "Ethnic Fractionalization"),
          type = "text",
          omit = "factor",  # Exclude fixed effects from table
          add.lines = list(c("Observations", nobs(model_2sDID)),
                           c("R-squared", round(summary(model_2sDID)$r.squared, 3))))

######## Table 3: Conditional effects of economic sanctions on mass mobilization
library(margins)
###############################################################################
# 2. Generate the new variables (equivalent to Stata's 'gen yearXdem = ...')
###############################################################################
df <- read_dta("DonganTanReplication.dta")  
# If 'year' and 'state' are *numeric* in your dataset, you can do:
df <- df %>%
  mutate(
    state = as.numeric(as.factor(country)),  # Convert to numeric IDs
    yearXdem    = year * dem,
    stateXdem   = state * dem,
    yearXEcGI   = year * lowEcGI,
    stateXEcGI  = state * lowEcGI
  )

# If 'state' is a character or factor, and you truly need numeric x dem:
# df$stateXdem  <- as.numeric(df$state) * df$dem
# ...and similarly for yearXEcGI/stateXEcGI if needed.

###############################################################################
# 3. Drop 'adj' if it exists (like 'drop adj')
###############################################################################
df$adj <- NULL

###############################################################################
# 4. First regression (mirrors Stata's:
#    reg mm i.state i.year i.yearXdem i.stateXdem if treat == 0, nocons)
#
#  - '0 +' omits the intercept (nocons).
#  - 'factor(state)' and 'factor(year)' replicate i.state, i.year in Stata
#  - We subset rows where treat == 0
###############################################################################
mod1 <- lm(
  mm ~ 0 + factor(state) + factor(year) + yearXdem + stateXdem,
  data = df %>% filter(treat == 0)
)

###############################################################################
# 5. Predict residuals (mirrors Stata's: predict adj, residuals)
###############################################################################
# Put residuals into df$adj, but only for treat==0
df$adj <- NA
df$adj[df$treat == 0] <- residuals(mod1)

###############################################################################
# 6. Second regression (mirrors Stata's:
#    reg adj i.treat##i.dem EcGI lgdp interwar intrawar lcinc efindex, vce(bootstrap)
#
#    For exact equivalence in standard errors, you'd need to use a bootstrap
#    approach in R. Below is a simple OLS example.
###############################################################################
# Convert treat, dem to factors if you prefer a true factor interaction:
# Otherwise numeric * numeric is fine. We'll show factor approach for margins.
df <- df %>%
  mutate(
    treat_factor = factor(treat, levels = c(0,1)),
    dem_factor   = factor(dem,   levels = c(0,1))
  )
df_clean <- df_clean %>%
  mutate(
    treat_factor = droplevels(treat_factor),
    dem_factor   = droplevels(dem_factor)
  )
mod2 <- lm(
  adj ~ treat_factor * dem_factor + EcGI + lgdp + interwar + intrawar + lcinc + efindex,
  data = df_clean
)   ####################### <------ FAILS HERE
### levels(df$state) to debug
# If you need robust or bootstrap SEs:
#   library(sandwich)
#   library(lmtest)
#   coeftest(mod2, vcov = vcovHC(mod2, type = "HC1"))

###############################################################################
# 7. Marginal effects (mirrors:
#    margins, dydx(treat) at(dem=(0(1)1))
###############################################################################
marg_mod2 <- margins(
  mod2,
  variables = "treat_factor",
  at = list(dem_factor = c("0","1"))
)
marg_summary <- summary(marg_mod2)
marg_summary
# This table shows the marginal effect of treat at dem=0 and dem=1.

###############################################################################
# 8. Margins plot (similar to 'marginsplot, ytitle(...) ...')
#    We'll use ggplot2 to replicate some of Stata's plot features.
###############################################################################
# Convert summary(marg_mod2) to a data frame for plotting
marg_df <- as.data.frame(marg_summary)

# For clarity, rename factor levels to match labeling in Stata
# e.g., "0" -> "Autocracy", "1" -> "Democracy"
marg_df$dem_label <- ifelse(marg_df$dem_factor == "0", "Autocracy", "Democracy")

# Build plot
ggplot(marg_df, aes(x = dem_label, y = AME)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  labs(
    x        = "",
    y        = "Marginal effect of sanctions imposition",
    subtitle = "A: Unsanctioned"
  ) +
  ggtitle("") +
  theme_minimal(base_size = 14) +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.subtitle = element_text(size = 12)
  )

# If you want to save this plot (like Stata's 'saving(dem1)'):
# ggsave("dem1.png", width = 6, height = 4, dpi = 300)

###############################################################################
# 9. "Print the first column of this table" with stargazer
#    The user example has multiple columns, but if you only want 'mod2' shown
###############################################################################
stargazer(
  mod2,
  type  = "text",
  title = "Regression Results: First Column",
  # Keep minimal stats for a simpler table, adjust as desired:
  keep.stat = c("n", "rsq")
)