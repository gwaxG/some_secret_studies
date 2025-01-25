###############################################################################
# 1) LOAD LIBRARIES AND DATA
###############################################################################
# install.packages(c("haven","plm","lmtest","sandwich","boot","margins","ggplot2","dplyr","patchwork"))
library(haven)
library(plm)
library(lmtest)
library(sandwich)
library(boot)
library(margins)
library(ggplot2)
library(dplyr)
install.packages(c("patchwork"))
library(patchwork)  # for combining multiple ggplots

# Read the dataset
data <- read_dta("DonganTanRobustnessReplication.dta")

# Drop if year == 1949 & treat == 1 (mimicking Stata's "drop if year==1949 & treat==1")
data <- data %>%
  filter(!(year == 1949 & treat == 1))

# Encode country as factor (similar to encode country, gen(state) in Stata)
# We'll just store it in a column named 'state'
data <- data %>%
  mutate(state = as.factor(country))

# Create a panel-data frame
pdata <- pdata.frame(data, index = c("state", "year"))

# Define the controls used repeatedly
controls <- c("polity", "EcGI", "lgdp", "interwar", "intrawar", "lcinc", "efindex")


###############################################################################
# 2) TABLE 1: SUMMARY STATISTICS
###############################################################################
# Stata's approach: "sum treat mm $controls if e(sample)" after an xtreg
# We'll just show summary for treat, mm, and controls on the entire dataset
summary(data[, c("treat", "mm", controls)])


###############################################################################
# 3) TABLE 2: EFFECTS OF ECONOMIC SANCTIONS ON MASS MOBILIZATION
###############################################################################
# --- Model (1) in Stata:
#     xtreg mm i.treat i.year, fe vce(cluster state)

model_1 <- plm(mm ~ factor(treat) + factor(year),
               data = pdata, model = "within", effect = "individual")
model_1_vcov <- vcovHC(model_1, type = "HC1", cluster = "group")
cat("\n=== Table 2, Model (1) ===\n")
print(coeftest(model_1, model_1_vcov))

# --- Model (2) in Stata:
#     xtreg mm i.treat i.year $controls, fe vce(cluster state)

formula_2 <- as.formula(
  paste0("mm ~ factor(treat) + factor(year) + ", paste(controls, collapse = " + "))
)
model_2 <- plm(formula_2, data = pdata, model = "within", effect = "individual")
model_2_vcov <- vcovHC(model_2, type = "HC1", cluster = "group")
cat("\n=== Table 2, Model (2) ===\n")
print(coeftest(model_2, model_2_vcov))

# --- Helper function to mimic "reg ... , vce(bootstrap)"
bootstrap_regression <- function(data, formula, R = 1000, seed = 123) {
  fit_original <- lm(formula, data = data)
  
  boot_fn <- function(d, idx) {
    d_boot <- d[idx, ]
    fit_boot <- lm(formula, data = d_boot)
    coef(fit_boot)
  }
  
  set.seed(seed)
  boot_obj <- boot(data, statistic = boot_fn, R = R)
  
  boot_est <- apply(boot_obj$t, 2, mean, na.rm = TRUE)
  boot_se  <- apply(boot_obj$t, 2, sd,   na.rm = TRUE)
  
  results_df <- data.frame(
    Term          = names(coef(fit_original)),
    Original_Coef = coef(fit_original),
    Boot_Mean     = boot_est,
    Boot_SE       = boot_se
  )
  return(results_df)
}

# --- Model (3) in Stata:
#     reg mm i.state i.year if treat == 0, nocons
#     predict adj, residuals
#     reg adj i.treat, vce(bootstrap)

cat("\n=== Table 2, Model (3) ===\n")

# 1) Regress mm ~ 0 + factor(state) + factor(year), treat == 0
data$adj <- NULL
non_treated_data <- data %>%
  filter(treat == 0) %>%
  drop_na(mm, state, year)
fit3_part1 <- lm(mm ~ 0 + factor(state) + factor(year), data = non_treated_data)

# 2) Insert residuals
data$adj <- NA
rows_used_3 <- as.numeric(row.names(non_treated_data))
data$adj[rows_used_3] <- residuals(fit3_part1)

# 3) Regress adj ~ factor(treat) w/ bootstrap
model3_data <- subset(data, !is.na(adj))
res_model3 <- bootstrap_regression(model3_data, adj ~ factor(treat), R = 1000)
print(res_model3)

# --- Model (4) in Stata:
#     reg adj i.treat $controls, vce(bootstrap)

cat("\n=== Table 2, Model (4) ===\n")

formula_4 <- as.formula(
  paste0("adj ~ factor(treat) + ", paste(controls, collapse = " + "))
)
res_model4 <- bootstrap_regression(model3_data, formula_4, R = 1000)
print(res_model4)

# --- Model (5) in Stata:
#     drop adj
#     reg mm i.state i.year if ever_sanctioned == 0, nocons
#     predict adj, residuals
#     reg adj i.treat, vce(bootstrap)

cat("\n=== Table 2, Model (5) ===\n")

# 1) Remove 'adj'
data$adj <- NULL

never_sanctioned_data <- data %>%
  filter(ever_sanctioned == 0) %>%
  drop_na(mm, state, year)
fit5_part1 <- lm(mm ~ 0 + factor(state) + factor(year), data = never_sanctioned_data)

data$adj <- NA
rows_used_5 <- as.numeric(row.names(never_sanctioned_data))
data$adj[rows_used_5] <- residuals(fit5_part1)

model5_data <- subset(data, !is.na(adj))
res_model5 <- bootstrap_regression(model5_data, adj ~ factor(treat), R = 1000)
print(res_model5)

# --- Model (6) in Stata:
#     reg adj i.treat $controls, vce(bootstrap)

cat("\n=== Table 2, Model (6) ===\n")

formula_6 <- as.formula(
  paste0("adj ~ factor(treat) + ", paste(controls, collapse = " + "))
)
res_model6 <- bootstrap_regression(model5_data, formula_6, R = 1000)
print(res_model6)


###############################################################################
# 4) TABLE 3: CONDITIONAL EFFECTS OF ECONOMIC SANCTIONS ON MASS MOBILIZATION
###############################################################################
# Stata code: 
#   gen yearXdem = year*dem
#   gen stateXdem = state*dem
#   gen yearXEcGI = year*lowEcGI
#   gen stateXEcGI = state*lowEcGI
#
# Then four models:
#   - (1) & (2): democracy dimension
#   - (3) & (4): economic globalization dimension
#   Each with "if treat==0" or "if ever_sanctioned==0"
#   Then "margins" and "marginsplot"

cat("\n=== Creating interaction variables for Table 3 ===\n")

data <- data %>%
  mutate(
    yearXdem   = year * dem,
    stateXdem  = as.numeric(state) * dem,   # numeric encoding of 'state'
    yearXEcGI  = year * lowEcGI,
    stateXEcGI = as.numeric(state) * lowEcGI
  )

############### MODEL (1) from Table 3 ###############
#   drop adj
#   reg mm i.state i.year i.yearXdem i.stateXdem if treat == 0, nocons
#   predict adj, residuals
#   reg adj i.treat##i.dem EcGI lgdp interwar intrawar lcinc efindex, vce(bootstrap)

cat("\n=== Table 3, Model (1) ===\n")
data$adj <- NULL

mod1_data_part1 <- data %>%
  filter(treat == 0) %>%
  drop_na(mm, state, year, yearXdem, stateXdem)

fit_mod1_part1 <- lm(mm ~ 0 +
                       factor(state) +
                       factor(year) +
                       factor(yearXdem) +
                       factor(stateXdem),
                     data = mod1_data_part1)

data$adj <- NA
rows_mod1 <- as.numeric(row.names(mod1_data_part1))
data$adj[rows_mod1] <- residuals(fit_mod1_part1)

mod1_data <- subset(data, !is.na(adj))
form_mod1 <- as.formula(
  "adj ~ factor(treat)*factor(dem) + EcGI + lgdp + interwar + intrawar + lcinc + efindex"
)
res_mod1 <- bootstrap_regression(mod1_data, form_mod1, R = 1000)
print(res_mod1)

# margins, dydx(treat) at(dem=(0(1)1))
# We'll do a non-bootstrap fit for the margins
fit_mod1_noBoot <- lm(form_mod1, data = mod1_data)
marg_mod1 <- margins(
  fit_mod1_noBoot,
  variables = "treat",
  at = list(dem = c(0,1))
)
cat("\n=== Margins for Table 3, Model (1) ===\n")
print(summary(marg_mod1))

# marginsplot
marg_df_mod1 <- as.data.frame(marg_mod1)
marg_df_mod1$dem_value <- marg_df_mod1$dem
plot_mod1 <- ggplot(marg_df_mod1, aes(x = dem_value, y = dydx_treat)) +
  geom_point() +
  geom_errorbar(aes(
    ymin = dydx_treat - 1.96 * SE,
    ymax = dydx_treat + 1.96 * SE
  ), width = 0.1) +
  labs(
    x = "Democracy (0=Autocracy, 1=Democracy)",
    y = "Marginal Effect of Sanctions",
    title = "A: Unsanctioned (Model 1)"
  ) +
  theme_minimal()

############### MODEL (2) from Table 3 ###############
#   drop adj
#   reg mm i.state i.year i.yearXdem i.stateXdem if ever_sanctioned == 0, nocons
#   predict adj, residuals
#   reg adj i.treat##i.dem EcGI lgdp interwar intrawar lcinc efindex, vce(bootstrap)

cat("\n=== Table 3, Model (2) ===\n")
data$adj <- NULL

mod2_data_part1 <- data %>%
  filter(ever_sanctioned == 0) %>%
  drop_na(mm, state, year, yearXdem, stateXdem)

fit_mod2_part1 <- lm(mm ~ 0 +
                       factor(state) +
                       factor(year) +
                       factor(yearXdem) +
                       factor(stateXdem),
                     data = mod2_data_part1)

data$adj <- NA
rows_mod2 <- as.numeric(row.names(mod2_data_part1))
data$adj[rows_mod2] <- residuals(fit_mod2_part1)

mod2_data <- subset(data, !is.na(adj))
form_mod2 <- as.formula(
  "adj ~ factor(treat)*factor(dem) + EcGI + lgdp + interwar + intrawar + lcinc + efindex"
)
res_mod2 <- bootstrap_regression(mod2_data, form_mod2, R = 1000)
print(res_mod2)

# margins, dydx(treat) at(dem=(0,1))
fit_mod2_noBoot <- lm(form_mod2, data = mod2_data)
marg_mod2 <- margins(
  fit_mod2_noBoot,
  variables = "treat",
  at = list(dem = c(0,1))
)
cat("\n=== Margins for Table 3, Model (2) ===\n")
print(summary(marg_mod2))

marg_df_mod2 <- as.data.frame(marg_mod2)
marg_df_mod2$dem_value <- marg_df_mod2$dem
plot_mod2 <- ggplot(marg_df_mod2, aes(x = dem_value, y = dydx_treat)) +
  geom_point() +
  geom_errorbar(aes(
    ymin = dydx_treat - 1.96 * SE,
    ymax = dydx_treat + 1.96 * SE
  ), width = 0.1) +
  labs(
    x = "Democracy (0=Autocracy, 1=Democracy)",
    y = "Marginal Effect of Sanctions",
    title = "B: Not-yet-sanctioned (Model 2)"
  ) +
  theme_minimal()

# Combine the two democracy plots (like "gr combine dem1.gph dem2.gph")
plot_dem_combined <- plot_mod1 + plot_mod2
plot_dem_combined

############### MODEL (3) from Table 3 ###############
#   drop adj
#   reg mm i.state i.year i.yearXEcGI i.stateXEcGI if treat == 0, nocons
#   predict adj, residuals
#   reg adj i.treat##i.lowEcGI polity lgdp interwar intrawar lcinc efindex, vce(bootstrap)

cat("\n=== Table 3, Model (3) ===\n")
data$adj <- NULL

mod3_data_part1 <- data %>%
  filter(treat == 0) %>%
  drop_na(mm, state, year, yearXEcGI, stateXEcGI)

fit_mod3_part1 <- lm(mm ~ 0 +
                       factor(state) +
                       factor(year) +
                       factor(yearXEcGI) +
                       factor(stateXEcGI),
                     data = mod3_data_part1)

data$adj <- NA
rows_mod3 <- as.numeric(row.names(mod3_data_part1))
data$adj[rows_mod3] <- residuals(fit_mod3_part1)

mod3_data <- subset(data, !is.na(adj))
form_mod3 <- as.formula(
  "adj ~ factor(treat)*factor(lowEcGI) + polity + lgdp + interwar + intrawar + lcinc + efindex"
)
res_mod3 <- bootstrap_regression(mod3_data, form_mod3, R = 1000)
print(res_mod3)

# margins, dydx(treat) at(lowEcGI=(0,1))
fit_mod3_noBoot <- lm(form_mod3, data = mod3_data)
marg_mod3 <- margins(
  fit_mod3_noBoot,
  variables = "treat",
  at = list(lowEcGI = c(0,1))
)
cat("\n=== Margins for Table 3, Model (3) ===\n")
print(summary(marg_mod3))

marg_df_mod3 <- as.data.frame(marg_mod3)
marg_df_mod3$lowEcGI_value <- marg_df_mod3$lowEcGI
plot_mod3 <- ggplot(marg_df_mod3, aes(x = lowEcGI_value, y = dydx_treat)) +
  geom_point() +
  geom_errorbar(aes(
    ymin = dydx_treat - 1.96 * SE,
    ymax = dydx_treat + 1.96 * SE
  ), width = 0.1) +
  labs(
    x = "Globalization (0=High, 1=Low)",
    y = "Marginal Effect of Sanctions",
    title = "A: Unsanctioned (Model 3)"
  ) +
  theme_minimal()

############### MODEL (4) from Table 3 ###############
#   drop adj
#   reg mm i.state i.year i.yearXEcGI i.stateXEcGI if ever_sanctioned == 0, nocons
#   predict adj, residuals
#   reg adj i.treat##i.lowEcGI polity lgdp interwar intrawar lcinc efindex, vce(bootstrap)

cat("\n=== Table 3, Model (4) ===\n")
data$adj <- NULL

mod4_data_part1 <- data %>%
  filter(ever_sanctioned == 0) %>%
  drop_na(mm, state, year, yearXEcGI, stateXEcGI)

fit_mod4_part1 <- lm(mm ~ 0 +
                       factor(state) +
                       factor(year) +
                       factor(yearXEcGI) +
                       factor(stateXEcGI),
                     data = mod4_data_part1)

data$adj <- NA
rows_mod4 <- as.numeric(row.names(mod4_data_part1))
data$adj[rows_mod4] <- residuals(fit_mod4_part1)

mod4_data <- subset(data, !is.na(adj))
form_mod4 <- as.formula(
  "adj ~ factor(treat)*factor(lowEcGI) + polity + lgdp + interwar + intrawar + lcinc + efindex"
)
res_mod4 <- bootstrap_regression(mod4_data, form_mod4, R = 1000)
print(res_mod4)

# margins, dydx(treat) at(lowEcGI=(0,1))
fit_mod4_noBoot <- lm(form_mod4, data = mod4_data)
marg_mod4 <- margins(
  fit_mod4_noBoot,
  variables = "treat",
  at = list(lowEcGI = c(0,1))
)
cat("\n=== Margins for Table 3, Model (4) ===\n")
print(summary(marg_mod4))

marg_df_mod4 <- as.data.frame(marg_mod4)
marg_df_mod4$lowEcGI_value <- marg_df_mod4$lowEcGI
plot_mod4 <- ggplot(marg_df_mod4, aes(x = lowEcGI_value, y = dydx_treat)) +
  geom_point() +
  geom_errorbar(aes(
    ymin = dydx_treat - 1.96 * SE,
    ymax = dydx_treat + 1.96 * SE
  ), width = 0.1) +
  labs(
    x = "Globalization (0=High, 1=Low)",
    y = "Marginal Effect of Sanctions",
    title = "B: Not-yet-sanctioned (Model 4)"
  ) +
  theme_minimal()

# Combine the two globalization plots (like "gr combine EcGl1.gph EcGl2.gph")
plot_ecg_combined <- plot_mod3 + plot_mod4
plot_ecg_combined

###############################################################################
# 5) FINAL COMBINED FIGURES (FIGURE 2 & 3 in Stata)
###############################################################################
# Figure 2: Combining dem1.gph (Model 1) and dem2.gph (Model 2)
#   We already combined them into 'plot_dem_combined'
# Figure 3: Combining EcGl1.gph (Model 3) and EcGl2.gph (Model 4)
#   We combined them into 'plot_ecg_combined'
#
# Display or save them:
# For example:
# ggsave("Figure2_democracy.png", plot=plot_dem_combined, width=8, height=4)
# ggsave("Figure3_globalization.png", plot=plot_ecg_combined, width=8, height=4)

###############################################################################
# DONE
###############################################################################
cat("\n=== END OF SCRIPT ===\n")
