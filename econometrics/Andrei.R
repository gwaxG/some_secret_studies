library("tidyverse")
library("wooldridge")
library(lmtest)
library(plm)
library(stargazer)

data('jtrain')

# Convert relevant categorical variables to factors
data$union <- as.factor(data$union)
data$d88 <- as.factor(data$d88)
data$d89 <- as.factor(data$d89)

# Creating treatment variable
data <- data %>%
  mutate(Post_Treatment = ifelse((grant == 1 & ((year >= 1988 & grant_1 == 0) | (year >= 1989 & grant_1 == 1))), 1, 0),
         DiD = grant * Post_Treatment)

# Panel data
pdata <- pdata.frame(data, index = c("fcode", "year"))

# Modèle 1 : Effet de la subvention sur les heures de formation par employé
# hrsempit = α+ β1 grantit + β2 Periodet + β3 (grantit ×Periodet )+ β4 ×lemployi + β5 ×unioni +β6×d88i + β8 ×d89i +δi + εit
model_1 <- plm(hrsemp ~ grant + Post_Treatment + DiD + lemploy + union + d88 + d89,
               data = pdata, model = "within", effect = "individual")
summary(model_1)
stargazer(model_1, type = "text", title = "DID Regression Results Model 1")


# Modèle 2 : Effet de la subvention sur le logarithme du taux de rebuts​
# lscrapit = α+ β1​grantit​+ β2​Periodet​+ β3​(grantit​×Periodet​)+ β4​×lemployi ​+ β5​×unioni​ ​+β6×d88i ​+ β7​×d89i +β8​×hrsempi​ +δi​+ εit​
model_2 <- plm(lscrap ~ grant + Post_Treatment + DiD + lemploy + union + d88 + d89 + hrsemp,
               data = pdata, model = "within", effect = "individual")
stargazer(model_2, type = "text", title = "DID Regression Results Model 2")

# Modèle 3 : Effet de la subvention sur le logarithme des ventes
model_3 <- plm(lsales ~ grant + Post_Treatment + DiD + lemploy + lavgsal + union + d88 + d89,
               data = pdata, model = "within", effect = "individual")
stargazer(model_3, type = "text", title = "DID Regression Results Model 3")

# Modèle 4 : Effet de la subvention sur le salaire moyen des employés
# lavgsali​t= α+ β1​granti​+ β2​Periodet​+ β3​(grantit​×Periodet​)+ β4​×lsalesi ​+ β5​×unioni + ​+β6×d88i ​+ β7​×d89i +δi​+ εit​
model_4 <- plm(lavgsal ~ grant + Post_Treatment + DiD + lsales + union + d88 + d89,
               data = pdata, model = "within", effect = "individual")
stargazer(model_4, type = "text", title = "DID Regression Results  Model 4")


#######################################################
################## Robustness checks ##################
#######################################################
# This code could be rerun for every model.

# I Robustness
# Step 1.1: Model Without Control Variables
# Check the effect of grant, Post_Treatment, and DiD without controls:
model <- model_1
model_1_no_controls <- plm(hrsemp ~ grant + Post_Treatment + DiD,
                           data = pdata, model = "within", effect = "individual")
summary(model_1_no_controls)
stargazer(model_1_no_controls, type = "text", title = "Model 1 Without Controls")

# Step 1.2: Model With Additional Controls
# If you have more variables that could influence training hours, add them:
model_1_extra_controls <- plm(hrsemp ~ grant + Post_Treatment + DiD + lemploy + union + d88 + d89 + lsales + lavgsal,
                              data = pdata, model = "within", effect = "individual")
summary(model_1_extra_controls)
stargazer(model_1_extra_controls, type = "text", title = "Model 1 With Additional Controls")

# Compare results across models.
# If coefficients remain stable, it suggests robust estimates.


# II Selection Bias Check
# Selection bias occurs if firms that received the grant are systematically different from those that didn’t.
# Step 2.1: Pre-treatment Trends
# Check if treated and control firms had similar training hours before treatment.
# Estimate a placebo regression using only pre-treatment years (1987):

pre_treatment_data <- pdata %>% filter(year == 1987)

placebo_model <- lm(hrsemp ~ grant + lemploy + union, data = pre_treatment_data)
summary(placebo_model)
stargazer(placebo_model, type = "text", title = "Placebo Test (1987 Only)")

# If grant is significant, it suggests selection bias (treated firms were already different before treatment).

# III Robust Standard Errors
# Heteroskedasticity or autocorrelation can bias standard errors. Use clustered robust standard errors:

coeftest(model_1, vcovHC(model_1, type = "HC0", cluster = "group"))
# This corrects for heteroskedasticity and within-group correlation.

# IV Fixed Effects vs. Random Effects
# To determine if fixed effects (FE) or random effects (RE) are more appropriate, run the Hausman test:

model_1_fe <- plm(hrsemp ~ grant + Post_Treatment + DiD + lemploy + union + d88 + d89,
                  data = pdata, model = "within")  # Fixed Effects

# F-Test for Fixed Effects
# If you suspect firm-specific effects, an F-test can determine whether Fixed Effects (FE) are needed.
pFtest(model_1_fe, plm(hrsemp ~ grant + Post_Treatment + DiD + lemploy + union + d88 + d89,
                       data = pdata, model = "pooling"))  # Compare FE vs OLS
# P-value < 0.05 → Use Fixed Effects (FE) (Firm effects are significant).
# P-value > 0.05 → OLS might be sufficient.
