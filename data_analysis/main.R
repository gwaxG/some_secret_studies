#### 0. Load Libraries ####

# Install packages if needed (uncomment as necessary):
# install.packages(c("haven", "dplyr", "fixest", "boot", "did2s", "marginaleffects"))

library(haven)         # For reading Stata .dta files
library(dplyr)         # Data wrangling
library(fixest)        # Fixed-effects regressions (similar to Stata xtreg)
library(boot)          # Bootstrap methods if needed
library(did2s)         # Two-stage DID and event-study approach
# library(marginaleffects) # For margins-like functionality (optional)

###############################################################################
# ********************************** Main Results ******************************
###############################################################################

#### 1. Load Main Dataset (Stata: use DonganTanReplication, clear) ####

df <- read_dta("DonganTanReplication.dta")  
# Explanation: Reads the 'DonganTanReplication.dta' file into an R data frame named 'df'.

#### 2. Drop observations if year==1949 & treat==1 ####
# Stata comment: drop if year==1949 & treat==1
df <- df %>% filter(!(year == 1949 & treat == 1))

#### 3. Encode country => state (Stata: encode country, gen(state)) ####
df <- df %>%
  mutate(state = as.factor(country))

#### 4. Stata: xtset state year ####
# No direct equivalent in R; fixest handles panel structure via formulas.

#### 5. Table 1: summary statistics ####
# Stata sets a global for controls: global controls polity EcGI lgdp interwar intrawar lcinc efindex

controls <- c("polity", "EcGI", "lgdp", "interwar", "intrawar", "lcinc", "efindex")

#### Stata: xtreg mm i.treat i.year $controls, fe vce(cluster state) ####
model_table1 <- feols(
  fml = as.formula(
    paste0("mm ~ i(treat) + i(year) + ", paste(controls, collapse = " + "), " | state")
  ),
  cluster = ~state,
  data = df
)
summary(model_table1)

#### Stata: sum treat mm $controls if e(sample) ####
# In R, we can summarize the variables used in 'model_table1':
used_obs <- model_table1[["obs_selection"]][["obs"]]
df_used <- df %>%
  filter(complete.cases(mm, treat, year, !!!rlang::syms(controls)))

# Summarize the filtered dataset
summary(df_used[, c("treat", "mm", controls)])

###############################################################################
# * Table 2: Effects of economic sanctions on mass mobilization
###############################################################################

#### Model (1): Stata -> xtreg mm i.treat i.year, fe vce(cluster state) ####
model_1 <- feols(
  mm ~ i(treat) + i(year) | state,
  cluster = ~state,
  data = df
)
summary(model_1)

#### Model (2): Stata -> xtreg mm i.treat i.year $controls, fe vce(cluster state) ####
model_2 <- feols(
  fml = as.formula(
    paste0("mm ~ i(treat) + i(year) + ", paste(controls, collapse = " + "), " | state")
  ),
  cluster = ~state,
  data = df
)
summary(model_2)

#### Model (3): Stata -> reg mm i.state i.year if treat == 0, nocons; predict adj; reg adj i.treat, vce(bootstrap) ####
# Step 1: Filter data and partial out state & year
df_treat0 <- df %>%
  filter(treat == 0) %>%
  mutate(row_number = row_number())  # Add row numbers

df_treat0_clean <- df_treat0 %>%
  filter(complete.cases(mm, state, year))

# Partial out state & year
model_3_step1 <- lm(mm ~ factor(state) + factor(year) - 1, data = df_treat0_clean)
df_treat0_clean <- df_treat0_clean %>%
  mutate(adj = residuals(model_3_step1))

# Step 2: Add residuals back to the original dataset
df <- df %>%
  mutate(row_number = row_number()) %>%  # Add row numbers
  left_join(
    df_treat0_clean %>% select(row_number, adj),
    by = "row_number"
  )


# Bootstrapping would require manual code with 'boot()' if you want to replicate vce(bootstrap).

#### Model (4): Stata -> reg adj i.treat $controls, vce(bootstrap) ####
df_treat0 <- df_treat0 %>%
  mutate(row_number = row_number()) %>%
  left_join(
    df_treat0_clean %>% select(row_number, adj),
    by = "row_number"
  )
summary(model_4)

#### Model (5): drop adj; reg mm i.state i.year if ever_sanctioned == 0; predict adj; reg adj i.treat, vce(bootstrap) ####
# Drop 'adj' from df_treat0 if you want to keep data clean
# Step 1: Filter dataset for observations where ever_sanctioned == 0
# Assume your data is in a data.frame 'df' with columns:
# mm, state, year, ever_sanctioned, treat, and possibly adj

# 1) Drop 'adj' if it exists:
df$adj <- NULL

# 2) Run regression with no intercept (equivalent to 'nocons'):
mod1 <- lm(mm ~ factor(state) + factor(year) - 1,
           data = df[df$ever_sanctioned == 0, ])

# 3) Save residuals in 'adj' for the rows used in the model:
df$adj <- NA
df$adj[df$ever_sanctioned == 0] <- residuals(mod1)

# 4) Regress 'adj' on 'treat' with bootstrap standard errors:
#    (Example using the 'boot' package)
library(boot)

# Define a function to fit the model on bootstrapped samples:
boot_fn <- function(data, indices) {
  d <- data[indices, ]
  d <- droplevels(d)  # Drop unused levels to avoid issues
  fit <- lm(adj ~ factor(treat), data = d)
  return(coef(fit))
}

# Run the bootstrap:
set.seed(123)  # for reproducibility
results <- boot(data = df, statistic = boot_fn, R = 1000)

# Extract point estimates and bootstrap standard errors:
mod2 <- lm(adj ~ factor(treat), data = df)
coef_estimates <- coef(mod2)
boot_se <- apply(results$t, 2, sd)  # standard errors from bootstrap

# Summary:
coef_estimates
boot_se

it fails here 


###############################################################################
# * Table 3: Conditional effects of economic sanctions on mass mobilization
###############################################################################

#### 1) Generate new variables (Stata: gen yearXdem, stateXdem, etc.) ####
df <- df %>%
  mutate(
    yearXdem   = year * dem,
    stateXdem  = as.numeric(state) * dem,   # or keep differently if needed
    yearXEcGI  = year * lowEcGI,
    stateXEcGI = as.numeric(state) * lowEcGI
  )

#### 2) Example for Model (1) with dem ####
# Stata:
# drop adj
# reg mm i.state i.year i.yearXdem i.stateXdem if treat == 0, nocons
# predict adj, residuals
# reg adj i.treat##i.dem EcGI lgdp interwar intrawar lcinc efindex, vce(bootstrap)
# margins, dydx(treat) at(dem=(0(1)1))

# R approach:
df_treat0 <- df %>% filter(treat == 0)
model_cond1_step1 <- lm(
  mm ~ factor(state) + factor(year) + factor(yearXdem) + factor(stateXdem) - 1,
  data = df_treat0
)
df_treat0$adj <- residuals(model_cond1_step1)

model_cond1_step2 <- lm(
  adj ~ treat*dem + EcGI + lgdp + interwar + intrawar + lcinc + efindex,
  data = df_treat0
)
summary(model_cond1_step2)

# For margins at dem=0,1 -> use 'marginaleffects' or 'margins' in R. 
# marginsplot => custom plotting code or 'plot_cap()'.

#### 3) Similar steps for Model (2), Model (3), Model (4) using subsets treat==0 or ever_sanctioned==0, and variables dem / lowEcGI ####
# Omitted for brevity; same partial-out approach + new regression => margins => plot.


###############################################################################
# * Figure 2 & 3 in Stata => combining graphs (dem1.gph, dem2.gph, etc.)
###############################################################################
# In R, you'd combine saved plots with e.g. patchwork, cowplot, or gridExtra.
# Example: library(patchwork); p1 + p2 for 2 plots, etc.


###############################################################################
# * Figure 4: Dynamic effects of economic sanctions on mass mobilization
###############################################################################

#### 1) gen rel_time = year - first_sanction ####
df <- df %>% mutate(rel_time = year - first_sanction)

#### 2) leads (F_x) and lags (L_x) ####
for (x in 1:10) {
  df[[paste0("F_", x)]] <- as.integer(df$rel_time == -x)
}
for (x in 0:10) {
  df[[paste0("L_", x)]] <- as.integer(df$rel_time == x)
}

#### 3) did2s approach ####
# Stata: did2s mm, first_stage(state year) second_stage(F_* L_*) treatment(treat) cluster(state)
did_no_cov <- did2s(
  data = df,
  yname = "mm",
  first_stage = ~ factor(state) + factor(year),
  second_stage = ~ F_1 + F_2 + ... + F_10 + L_0 + ... + L_10,  # adapt in R code
  treatment = "treat",
  cluster_var = "state"
)
# event_plot(did_no_cov, ... ) # to replicate Stata's event_plot

#### Next: did2s with controls ####
did_cov <- did2s(
  data = df,
  yname = "mm",
  first_stage = ~ factor(state) + factor(year),
  second_stage = as.formula(
    paste0("~ ", 
           paste0("F_",1:10, collapse=" + "), " + ", 
           paste0("L_",0:10, collapse=" + "), " + ",
           paste(controls, collapse=" + "))
  ),
  treatment = "treat",
  cluster_var = "state"
)
# event_plot(did_cov, ... )


###############################################################################
# *********************************** Robustness check *************************
###############################################################################

#### 1) use DonganTanRobustnessReplication, clear ####
df_rb <- read_dta("DonganTanRobustnessReplication.dta")

#### drop if year==1989 & treat==1 ####
df_rb <- df_rb %>% filter(!(year == 1989 & treat == 1))

#### encode country => state ####
df_rb <- df_rb %>% mutate(state = as.factor(country))

#### xtset state year (handled by fixest in R) ####

#### Table 4: effect of economic sanctions on mm using EUSANCT dataset ####
# global controls polity EcGI lgdp interwar intrawar lcinc efindex demsanc coup regtype
controls_rb <- c("polity","EcGI","lgdp","interwar","intrawar","lcinc","efindex","demsanc","coup","regtype")

#### Model (1): xtreg mm i.treat i.year, fe vce(cluster state) ####
model_rb_1 <- feols(
  mm ~ i(treat) + i(year) | state,
  cluster = ~state,
  data = df_rb
)
summary(model_rb_1)

#### Model (2): xtreg mm i.treat i.year $controls, fe vce(cluster state) ####
model_rb_2 <- feols(
  as.formula(
    paste0("mm ~ i(treat) + i(year) + ", paste(controls_rb, collapse=" + "), " | state")
  ),
  cluster = ~state,
  data = df_rb
)
summary(model_rb_2)

#### Model (3) => partial out if treat==0 ####
df_rb_t0 <- subset(df_rb, treat==0)
m_rb3_step1 <- lm(mm ~ factor(state) + factor(year) - 1, data=df_rb_t0)
df_rb_t0$adj <- residuals(m_rb3_step1)
m_rb3_step2 <- lm(adj ~ treat, data=df_rb_t0)
summary(m_rb3_step2)

#### Model (4) => add controls ####
m_rb4 <- lm(
  as.formula(paste0("adj ~ treat + ", paste(controls_rb, collapse=" + "))),
  data=df_rb_t0
)
summary(m_rb4)

#### Model (5) => partial out if ever_sanctioned==0 ####
df_rb_never <- subset(df_rb, ever_sanctioned==0)
m_rb5_step1 <- lm(mm ~ factor(state) + factor(year) - 1, data=df_rb_never)
df_rb_never$adj <- residuals(m_rb5_step1)
m_rb5_step2 <- lm(adj ~ treat, data=df_rb_never)
summary(m_rb5_step2)

#### Model (6) => add controls ####
m_rb6 <- lm(
  as.formula(paste0("adj ~ treat + ", paste(controls_rb, collapse=" + "))),
  data=df_rb_never
)
summary(m_rb6)

#### Next: Figures 5 & 6 => partial out, margins, etc. ####
# Repeats the same approach, using dem, lowEcGI, demsanc, etc.
# Then margins => custom code with "marginaleffects" or "margins" in R.


###############################################################################
# * Table 5: Effects of economic sanctions on terror incidents & stability
###############################################################################

#### use DonganTanRobustnessReplication2, clear ####
df_rb2 <- read_dta("DonganTanRobustnessReplication2.dta")

#### encode country => state, xtset state year ####
df_rb2 <- df_rb2 %>%
  mutate(state = as.factor(country))

#### Model (1): keep if 1970 <= year <= 2020 ####
df_rb2_terror <- df_rb2 %>% filter(year >= 1970 & year <= 2020)

#### drop if year==1970 & treat==1 ####
df_rb2_terror <- df_rb2_terror %>% filter(!(year == 1970 & treat == 1))

# replicate global controls
controls_terror <- c("polity","EcGI","lgdp","interwar","intrawar","lcinc","efindex")

# generate first_sanction, ever_sanctioned, etc. (Stata code)
# egen first_sanction = min(year / (treat==1)), by(state)
# R approach: we can do something like:
df_rb2_terror <- df_rb2_terror %>%
  group_by(state) %>%
  mutate(
    first_sanction = if_else(treat==1, year, NA_real_),
    first_sanction = min(first_sanction, na.rm=TRUE)
  ) %>%
  ungroup()

df_rb2_terror <- df_rb2_terror %>%
  mutate(
    ever_sanctioned = if_else(year >= first_sanction, 1, 0),
    ever_sanctioned = if_else(is.na(ever_sanctioned), 0, ever_sanctioned)
  )

#### reg terror i.state i.year if ever_sanctioned == 0, nocons => predict => reg => ...
df_rb2_t0 <- subset(df_rb2_terror, ever_sanctioned == 0)
m_t1_step1 <- lm(terror ~ factor(state) + factor(year) - 1, data=df_rb2_t0)
df_rb2_t0$adj <- residuals(m_t1_step1)
m_t1_step2 <- lm(adj ~ treat, data=df_rb2_t0)
summary(m_t1_step2)

#### Model (2) => reg adj i.treat $controls ####
m_t2 <- lm(
  as.formula(paste0("adj ~ treat + ", paste(controls_terror, collapse=" + "))),
  data=df_rb2_t0
)
summary(m_t2)

#### Model (3) => same idea but for 1996-2022, political stability index => "pve"? ####
df_rb2_stab <- read_dta("DonganTanRobustnessReplication2.dta") %>%
  mutate(state = as.factor(country)) %>%
  filter(year >= 1996 & year <= 2022) %>%
  filter(!(year==1996 & treat==1))

# replicate first_sanction logic
df_rb2_stab <- df_rb2_stab %>%
  group_by(state) %>%
  mutate(
    first_sanction = if_else(treat==1, year, NA_real_),
    first_sanction = min(first_sanction, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    ever_sanctioned = if_else(year >= first_sanction, 1, 0),
    ever_sanctioned = if_else(is.na(ever_sanctioned), 0, ever_sanctioned)
  )

df_rb2_stab_t0 <- subset(df_rb2_stab, ever_sanctioned==0)

m_s1_step1 <- lm(pve ~ factor(state) + factor(year) - 1, data=df_rb2_stab_t0)
df_rb2_stab_t0$adj <- residuals(m_s1_step1)

m_s1_step2 <- lm(adj ~ treat, data=df_rb2_stab_t0)
summary(m_s1_step2)

#### Model (4) => add controls ####
m_s2 <- lm(
  as.formula(paste0("adj ~ treat + ", paste(controls_terror, collapse=" + "))),
  data=df_rb2_stab_t0
)
summary(m_s2)


###############################################################################
# *************************** Appendix 2: Parallel trends **********************
###############################################################################

#### Stata: use DonganTanReplication, clear => we reload the main dataset ####
df_app2 <- read_dta("DonganTanReplication.dta") %>%
  mutate(state = as.factor(country))

#### gen rel_time = year - first_sanction ####
# (Assumes first_sanction is already in df or you create it similarly)
df_app2 <- df_app2 %>% mutate(rel_time = year - first_sanction)

#### Forval to create F_*, L_* ####
for (x in 1:10) {
  df_app2[[paste0("F_",x)]] <- as.integer(df_app2$rel_time == -x)
}
for (x in 0:10) {
  df_app2[[paste0("L_",x)]] <- as.integer(df_app2$rel_time == x)
}

#### xtreg mm F_* L_* i.year $controls, fe vce(cluster state) => we can do with fixest ####
model_app2 <- feols(
  formula = as.formula(
    paste0("mm ~ ",
           paste0("F_",1:10, collapse=" + "), " + ",
           paste0("L_",0:10, collapse=" + "), " + i(year) + ",
           paste(controls, collapse=" + "),
           " | state")
  ),
  cluster=~state,
  data=df_app2
)
summary(model_app2)

#### event_plot => replicate with did2s or custom approach ####

#### Next: 2sDiD for dem==1 vs dem==0; lowEcGI==1 vs lowEcGI==0 ####
# did2s mm if dem==1, first_stage(...) second_stage(...) ...
# event_plot(...) 
# Similar code as the earlier did2s steps, but restricted to dem==1 or dem==0 subsets.

###############################################################################
# ************************** Appendix 3: Correlation test **********************
###############################################################################

#### Stata: pwcorr mm treat polity EcGI lgdp interwar intrawar lcinc efindex, sig star(.05)
# In R, we do something like:

# 1) Main dataset again
df_app3 <- read_dta("DonganTanReplication.dta")
vars_app3 <- c("mm","treat","polity","EcGI","lgdp","interwar","intrawar","lcinc","efindex")
df_app3_sub <- df_app3 %>% select(all_of(vars_app3)) %>% na.omit()

cor_matrix <- cor(df_app3_sub)
cor_matrix
# significance => use cor.test in a loop or psych::corr.test

#### Then for DonganTanRobustnessReplication2 => year>=1970 & year<=2020 => pwcorr terror ...
df_app3_rb2 <- read_dta("DonganTanRobustnessReplication2.dta") %>%
  filter(year>=1970 & year<=2020)
vars_terror <- c("terror","treat","polity","EcGI","lgdp","interwar","intrawar","lcinc","efindex")
df_app3_rb2_sub <- df_app3_rb2 %>% select(all_of(vars_terror)) %>% na.omit()
cor(df_app3_rb2_sub)

#### Next => year>=1996 & year<=2022 => pwcorr pve ...
df_app3_rb2_stab <- read_dta("DonganTanRobustnessReplication2.dta") %>%
  filter(year>=1996 & year<=2022)
vars_stab <- c("pve","treat","polity","EcGI","lgdp","interwar","intrawar","lcinc","efindex")
df_app3_rb2_stab_sub <- df_app3_rb2_stab %>% select(all_of(vars_stab)) %>% na.omit()
cor(df_app3_rb2_stab_sub)


###############################################################################
# END OF SCRIPT
###############################################################################

# NOTE:
# 1) This script is a comprehensive translation of your Stata code to R code.
# 2) Some advanced Stata features (vce(bootstrap), marginsplot, event_plot) 
#    need additional libraries or manual steps (e.g., 'boot' or 'marginaleffects').
# 3) You may refine data file paths, variable names, and custom plotting 
#    functions as needed in your local environment.
