library(StMoMo)
library(demography)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(gbm)
library(randomForest)

#ITALYdataRaw <- hmd.mx(country="ITA", username = "lstake@hs-koblenz.de", password = "1650394322")
load("data/raw/ITALYdataRaw.RDA")
load("data/processed/HMD_df.RDA")

#Ages and Years for Fitting and Forecasting
ITALYdata <- StMoMoData(ITALYdataRaw, series = "male")
ages_fit <- 30:100
years_fit <- 1981:2019
#prep for forecast 2009-2019
years_subset_forecast <- 1981:2009

#Base Models
LCModel <- lc(link = "log")
APCModel <- apc(link = "log")

#Base Model Fits
LCFit <- fit(LCModel, data = ITALYdata, ages.fit = ages_fit, years.fit = years_fit)
LCFit.subset_forecast <- fit(LCModel, data = ITALYdata, ages.fit = ages_fit, years.fit = years_subset_forecast)

APCFit <- fit(APCModel, data = ITALYdata, ages.fit = ages_fit, years.fit = years_fit)
APCFit.subset_forecast <- fit(APCModel, data = ITALYdata, ages.fit = ages_fit, years.fit = years_subset_forecast)

### Plot Fit 
plot(LCFit)
plot(APCFit)

#Residuals and Residual Heatmaps
LCRes <- residuals(LCFit)
APCRes <- residuals(APCFit)

plot(LCRes, type="colourmap", reslim = c(-5, 5))
plot(APCRes, type="colourmap", reslim = c(-5, 5))

#Forecasting 10 years from 2009 on Base Models and Plots before ML
LCFor <- forecast(LCFit.subset_forecast, h=10)
plot(LCFor, parametricbx = FALSE)

APCFor <- forecast(APCFit.subset_forecast, h=10)
plot(APCFor, parametricbx = FALSE)

#Preparation of data
choosen_Model <- APCFit

dx_mdl <- choosen_Model$Ext * exp(choosen_Model$ax + choosen_Model$bx %*% choosen_Model$kt)

HMD_df_filtered <- filter(HMD_df, Year >= years_fit[1] & Year <= years_fit[length(years_fit)] & Age >= ages_fit[1] & Age <= ages_fit[length(ages_fit)] & Gender == "Male")

HMD_df_filtered$dx <- as.vector(t(dx_mdl))
HMD_df_filtered <- HMD_df_filtered %>%
  mutate("psi" = Deaths/dx, "Cohort" = Year - Age)

### DT ###
dt <- rpart(psi ~ Age + Year + Cohort,
            data = HMD_df_filtered, method = "poisson",
            cp = 0.003)

prp(dt)

### RF ###
rf <- randomForest(formula = Age + Year + Cohort~., data = HMD_df_filtered, ntree=200)
plot(rf, type="l")
rf1 <- randomForest(formula = Age + Year~., data = HMD_df_filtered, ntree=300)
plot(rf1, type="l")

### GB ###
gbm1 <- gbm(psi ~ Year + Age + Cohort,  "gaussian", data = HMD_df_filtered, n.trees = 5000, interaction.depth = 6, 
            shrinkage = 0.001,  cv.folds = 5)
tree_1 <- pretty.gbm.tree(gbm1, i.tree = 1)
plot.gbm(gbm1,1:2,5)
plot.gbm(gbm1,1:2,50)
plot.gbm(gbm1,1:2,500)
plot.gbm(gbm1,1:2,5000)
summary.gbm(gbm1, n.trees = 1)


