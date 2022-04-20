library(StMoMo)
library(demography)
library(tidyverse)
library(rpart)
library(rpart.plot)
install.packages("gbm")
libraray(gbm)


#ITALYdataRaw <- hmd.mx(country="ITA", username = "lstake@hs-koblenz.de", password = "1650394322")
load("data/raw/ITALYdataRaw.RDA")
load("data/processed/HMD_df.RDA")

#Ages and gender for fitting
ITALYdata <- StMoMoData(ITALYdataRaw, series = "male")
ages.fit <- 0:100
years.fit <- 1981:2019


LCModel <- lc(link = "logit")
RHModel <- rh(link = "logit", cohortAgeFun = "1")

LCFit <- fit(LCModel, data = ITALYdata, ages.fit = ages.fit, years.fit = years.fit)
RHFit <- fit(RHModel, data = ITALYdata, ages.fit = ages.fit, years.fit = years.fit)

LCRes <- residuals(LCFit)
RHRes <- residuals(RHFit)


### Plot Fit and Res
plot(LCFit)
plot(RHFit)

plot(LCRes)
plot(RHRes)




#Preparation of data
dx_mdl <- LCFit$Ext * exp(LCFit$ax + LCFit$bx %*% LCFit$kt)

HMD_df <- filter(HMD_df, Year >= years.fit[1] & Year <= years.fit[length(years.fit)] & Gender == "Male")

HMD_df$dx <- as.vector(t(dx_mdl))
HMD_df <- HMD_df %>%
  mutate("psi" = Deaths/dx)

### DT ###
dt <- rpart(psi ~ Age + Year,
            data = HMD_df, method = "poisson",
            cp = 0.003)

prp(dt)


### GB ###
gbm1 <- gbm(psi ~ Age + Year,  "gaussian", data = HMD_df, n.trees = 5000, interaction.depth = 6, 
            shrinkage = 0.001,  cv.folds = 5)

tree_100 <- pretty(gbm1, i.tree = 1)

