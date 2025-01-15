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

########