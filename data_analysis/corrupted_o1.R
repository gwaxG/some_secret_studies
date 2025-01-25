# Load necessary libraries
library(haven)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(tidyverse)
library(stargazer)
library(boot)

# Define a bootstrapping function
set.seed(123)
bootstrap_regression <- function(data, formula, R = 1000, seed = 123) {
  # 1) Fit the original (non-bootstrapped) model
  fit_original <- lm(formula, data = data)
  
  # 2) Define a function that:
  #    - takes a re-sampled version of data
  #    - fits the same linear model
  #    - returns the coefficient estimates
  boot_fn <- function(d, indices) {
    d_boot <- d[indices, ]
    fit_boot <- lm(formula, data = d_boot)
    coef(fit_boot)
  }
  
  # 3) Perform the bootstrap using the 'boot' package
  library(boot)
  set.seed(seed)
  boot_res <- boot(data, statistic = boot_fn, R = R)
  
  # 4) Extract bootstrap estimates & standard errors
  boot_estimates <- apply(boot_res$t, 2, mean, na.rm = TRUE)
  boot_se <- apply(boot_res$t, 2, sd, na.rm = TRUE)
  
  # (Optional) Combine into a tidy summary data frame
  results_df <- data.frame(
    Term = names(coef(fit_original)),
    Original_Coef = coef(fit_original),
    Boot_Mean_Coef = boot_estimates,
    Boot_SE = boot_se
  )
  
  # Return a list of useful objects
  list(
    original_model_summary = summary(fit_original),
    bootstrap_results = boot_res,
    bootstrap_coefs = results_df
  )
}


# Load the dataset
data <- read_dta("C:/dev/statistics/some_secret_studies/data_analysis/DonganTanReplication.dta")
# data <- read_dta("C:/dev/statistics/some_secret_studies/data_analysis/DonganTanRobustnessReplication.dta")

# Drop countries sanctioned in the first year of the dataset
# data <- data %>% filter(!(year == 1949 & treat == 1))
# Encode country as a factor
data <- data %>% mutate(state = as.factor(country))
# Set panel data structure
pdata <- pdata.frame(data, index = c("state", "year"))
# Define controls
controls <- c("polity", "EcGI", "lgdp", "interwar", "intrawar", "lcinc", "efindex")

# Model (1): Fixed effects regression without controls
model1 <- plm(mm ~ treat + year, data = pdata, model = "within", effect = "individual")
# Compute robust standard errors with clustering
robust_results <- coeftest(model1, vcov = vcovHC(model1, type = "HC1", cluster = "group"))
stargazer(robust_results, type  = "text", title = "Table 2 Model 2 after adjustment")

# Model (2): Fixed effects regression with controls
model2 <- plm(mm ~ treat + year + polity + EcGI + lgdp + interwar + intrawar + lcinc + efindex, 
              data = pdata, model = "within", effect = "individual")
stargazer(model2, type  = "text", title = "Table 2 Model 2 before adjustment")
# Compute robust standard errors with clustering
robust_results <- coeftest(model2, vcov = vcovHC(model2, type = "HC1", cluster = "group"))
stargazer(robust_results, type  = "text", title = "Table 2 Model 2 after adjustment")


# Model (3): Regression for non-treated countries and residuals adjustment
# reg mm i.state i.year if treat == 0, nocons
# predict adj, residuals
# reg adj i.treat, vce(bootstrap)
# 1) Filter for treat == 0 and valid mm/state/year
non_treated_data <- data %>%
  filter(treat == 0) %>%
  drop_na(mm, state, year)

# 2) Fit the first model
fit1 <- lm(mm ~ 0 + factor(state) + factor(year), data = non_treated_data)

# 3) Place residuals into the master 'data'
data$adj <- NA
rows_used <- as.numeric(row.names(non_treated_data))
data$adj[rows_used] <- residuals(fit1)

# 4) Subset for non-NA residuals and confirm both treat levels
fit2_data <- subset(data, !is.na(adj))
table(fit2_data$treat)   # Check you have 0 and 1

# 5) Fit second regression if both treat groups exist
if(length(unique(fit2_data$treat)) > 1) {
  fit2 <- lm(adj ~ factor(treat), data = fit2_data)
  summary(fit2)
  stargazer(fit2, type  = "text", title = "Table 2 Model 3")

} else {
  warning("Only one treatment level is present among non-NA rows, can't run a two-level model.")
}

# Model (4)
controls <- c("polity", "EcGI", "lgdp", "interwar", "intrawar", "lcinc", "efindex")

# Build a formula, e.g. adj ~ factor(treat) + polity + EcGI + ...
reg_formula <- as.formula(
  paste0("adj ~ factor(treat) + ", paste(controls, collapse = " + "))
)

# ---------------------------------------------------------
# 2) A simple bootstrap function for linear models
# ---------------------------------------------------------
bootstrap_regression <- function(data, formula, R = 1000, seed = 123) {
  # (a) Fit the model once on the full dataset
  fit_original <- lm(formula, data = data)

  # (b) Define the bootstrap procedure
  boot_fn <- function(d, idx) {
    d_boot <- d[idx, ]          # Resample rows
    fit_boot <- lm(formula, data = d_boot)
    coef(fit_boot)              # Return coefficients
  }

  set.seed(seed)
  boot_res <- boot(data, statistic = boot_fn, R = R)

  # (c) Calculate bootstrap means & standard errors
  boot_coefs <- colMeans(boot_res$t, na.rm = TRUE)
  boot_se    <- apply(boot_res$t, 2, sd, na.rm = TRUE)

  # Return a simple summary
  data.frame(
    Term            = names(coef(fit_original)),
    Original_Coef   = coef(fit_original),
    Bootstrap_Mean  = boot_coefs,
    Bootstrap_SE    = boot_se
  )
}

# ---------------------------------------------------------
# 3) Run the "adj i.treat $controls" regression with bootstrapping
# ---------------------------------------------------------
results_model4 <- bootstrap_regression(
  data    = data,
  formula = reg_formula,
  R       = 1000       # e.g. 1000 replications
)

# Original_Coef corresponds to the usual OLS coefficient.
# Bootstrap_Mean_Coef is the average of the coefficients across all bootstrap resamples.
# Bootstrap_SE is the bootstrap-based standard error, which is typically used for inference (confidence intervals, t-values, p-values).

# Model 5
# Suppose your full data frame is 'data'.
# 1) Drop 'adj' if it exists
data$adj <- NULL  # or remove it with: data <- within(data, rm(adj))

# 2) Subset data for non-sanctioned only
non_sanctioned_data <- subset(data, ever_sanctioned == 0)

# 3) Regress mm on state & year (no intercept)
fit1 <- lm(mm ~ 0 + factor(state) + factor(year), data = non_sanctioned_data)

# 4) Insert residuals back into the original 'data'
data$adj <- NA
# Track row indices actually used in 'fit1' (after NA omission, etc.)
rows_used <- as.numeric(row.names(non_sanctioned_data))
data$adj[rows_used] <- residuals(fit1)

final_data <- subset(data, !is.na(adj))

results <- bootstrap_regression(
  data    = final_data,
  formula = adj ~ factor(treat),  # i.treat in Stata
  R       = 1000
)

# model 6
controls <- c("polity", "EcGI", "lgdp", "interwar", "intrawar", "lcinc", "efindex")

# Create a formula: adj ~ factor(treat) + polity + EcGI + ...
reg_formula <- as.formula(
  paste0("adj ~ factor(treat) + ", paste(controls, collapse = " + "))
)
results_model <- bootstrap_regression(
  data    = data,
  formula = reg_formula,    # adj ~ factor(treat) + controls
  R       = 1000            # number of bootstrap replications
)
summary(results)
