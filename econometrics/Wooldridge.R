# 1. wooldridge
install.packages("wooldridge")
install.packages("estimatr")
library(wooldridge)
library(dplyr)
library(estimatr)
data(package = "wooldridge")

data("jtrain")
head(jtrain)
str(jtrain)
View(jtrain)

data <- jtrain %>%
filter(!is.na(hrsemp), !is.na(grant), !is.na(employ), !is.na(avgsal))

OLS_formula <- hrsemp ~ grant + lemploy + lavgsal + union

#Erreurs standard robustes
model <- lm_robust(hrsemp ~ grant + lemploy + lavgsal + union, data = data)
summary(model)



# Chargement des packages nécessaires
library(dplyr)
library(lmtest)
library(sandwich)

# Préparation des données
data <- jtrain %>%
  filter(!is.na(hrsemp), !is.na(grant), !is.na(employ), !is.na(avgsal), !is.na(scrap), !is.na(sales))

# Transformation des variables
data <- data %>%
  mutate(
    lemploy = log(employ),
    lavgsal = log(avgsal),
    lsales = log(sales),
    lscrap = log(scrap)
  )

# Modèle 1 : Effet sur hrsemp
model_hrsemp <- lm(hrsemp ~ grant + lemploy + lavgsal + union + d88 + d89, data = data)
summary(model_hrsemp)

# Modèle 2 : Effet sur lscrap
model_lscrap <- lm(lscrap ~ grant + lemploy + lavgsal + union + d88 + d89, data = data)
summary(model_lscrap)
