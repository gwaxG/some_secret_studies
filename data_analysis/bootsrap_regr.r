library(boot)

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

# ---------------
# USAGE EXAMPLE
# ---------------
# Suppose 'data' has the columns 'adj' and 'treat' 
# and we want a model ~ factor(treat)

# res <- bootstrap_regression(
#   data = data,
#   formula = adj ~ factor(treat),
#   R = 1000  # number of replications
# )

# Inspect standard results (non-bootstrapped)
# res$original_model_summary  

# Inspect bootstrap estimates and SE
# res$bootstrap_coefs
