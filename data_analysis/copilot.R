# Load necessary libraries
library(haven)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(tidyverse)
library(stargazer)

# Load the dataset
data <- read_dta("C:/dev/statistics/some_secret_studies/data_analysis/DonganTanRobustnessReplication.dta")

# Drop countries sanctioned in the first year of the dataset
data <- data %>% filter(!(year == 1949 & treat == 1))

# Encode country as a factor
data <- data %>% mutate(state = as.factor(country))

# Set panel data structure
pdata <- pdata.frame(data, index = c("state", "year"))

# Define controls
controls <- c("polity", "EcGI", "lgdp", "interwar", "intrawar", "lcinc", "efindex")

# Model (1): Fixed effects regression without controls
model1 <- plm(mm ~ treat + year, data = pdata, model = "within", effect = "individual")
coeftest(model1, vcov = vcovHC(model1, type = "HC1", cluster = "group"))

# Model (2): Fixed effects regression with controls
model2 <- plm(mm ~ treat + year + polity + EcGI + lgdp + interwar + intrawar + lcinc + efindex, 
              data = pdata, model = "within", effect = "individual")
coeftest(model2, vcov = vcovHC(model2, type = "HC1", cluster = "group"))

# Model (3): Regression for non-treated countries and residuals adjustment
data_filtered <- data %>% filter(treat == 0) %>% drop_na(mm)
model3 <- lm(mm ~ state + year, data = data_filtered)
stargazer(model3, type = "text", title = "Table 2 Model 3 itself")

data_filtered <- data_filtered %>% mutate(adj = residuals(model3))
data <- left_join(data, data_filtered %>% select(state, year, adj), by = c("state", "year"))

model3_adj <- lm(adj ~ treat, data = data)
stargazer(model3_adj, type = "text", title = "Table 2 Model 3 Constant")

# With controls to produce the final table result
model3_adj_controls <- lm(adj ~ treat + polity + log(gdp) + interwar + intrawar + efindex + log(cinc) + EcGI, data = data)
stargazer(model3_adj_controls, type = "text", title = "Table 2 Model 3 with Controls")

I do not know what I am really doing... hell... but Copilot seems to work fine.