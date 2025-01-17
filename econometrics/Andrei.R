library("wooldridge")
library(plm)
library(stargazer)

data('jtrain')

# Convert relevant categorical variables to factors
data$union <- as.factor(data$union)
data$d88 <- as.factor(data$d88)
data$d89 <- as.factor(data$d89)

# Modèle 1 : Effet de la subvention sur les heures de formation par employé
# hrsempit = α+ β1 grantit + β2 Periodet + β3 (grantit ×Periodet )+ β4 ×lemployi + β5 ×unioni +β6×d88i + β8 ×d89i +δi + εit
did_model_1 <- plm(hrsemp ~ grant + d88 + d89 + grant:d88 + grant:d89 + lemploy + union,
   data = data,
   index = c("fcode", "year"),
   model = "within",
   effect = "individual")

stargazer(did_model_1, type = "text", title = "DID Regression Results Model 1")


# Modèle 2 : Effet de la subvention sur le logarithme du taux de rebuts​
# lscrapit = α+ β1​grantit​+ β2​Periodet​+ β3​(grantit​×Periodet​)+ β4​×lemployi ​+ β5​×unioni​ ​+β6×d88i ​+ β7​×d89i +β8​×hrsempi​ +δi​+ εit​
did_model_2 <- plm(lscrap ~ grant + d88 + d89 + grant:d88 + grant:d89 + lemploy + union,
                 data = data,
                 index = c("fcode", "year"),
                 model = "within",
                 effect = "individual")

stargazer(did_model_2, type = "text", title = "DID Regression Results Model 2")

# Modèle 3 : Effet de la subvention sur le logarithme des ventes
# lsalesi​t = α+ β1​granti​+ β2​Periodet​+ β3​(grantit​×Periodet​)+ β4​×lemployi ​+ β5​×lavgsali​ + β6​×unioni​ ​+β7×d88i ​+ β8​×d89i +δi​+ εit​
did_model_3 <- plm(lsales ~ grant + d88 + d89 + grant:d88 + grant:d89 + lemploy + lavgsal + union,
  data = data,
  index = c("fcode", "year"), # Panel structure: firm-level over years
  model = "within", # Fixed effects
  effect = "individual") # Firm-level fixed effects

stargazer(did_model_3, type = "text", title = "DID Regression Results Model 3")

# Modèle 4 : Effet de la subvention sur le salaire moyen des employés
# lavgsali​t= α+ β1​granti​+ β2​Periodet​+ β3​(grantit​×Periodet​)+ β4​×lsalesi ​+ β5​×unioni + ​+β6×d88i ​+ β7​×d89i +δi​+ εit​
did_model_4 <- plm(lavgsal ~ grant + d88 + d89 + grant:d88 + grant:d89 + lsales + union,
  data = data,
  index = c("fcode", "year"), # Panel structure: firm-level over years
  model = "within", # Fixed effects
  effect = "individual") # Firm-level fixed effects

stargazer(did_model_4, type = "text", title = "DID Regression Results  Model 4")