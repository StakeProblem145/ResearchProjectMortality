library(StMoMo)
library(demography)
library(rpart)



ITALYdataRaw <- hmd.mx(country="ITA", username = "lstake@hs-koblenz.de", password = "1650394322")
ITALYdata
plot(ITALYdata, series="male")

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




### DT ###
dx_mdl <- LCFit$Ext * exp(LCFit$ax + LCFit$bx %*% LCFit$kt)

year <- LCFit$years
age <- LCFit$ages
cohort <- LCFit$cohorts




dt <- rpart(LCFit$Dxt/dx_mdl ~ Age + Year,
            data = HMD_df, method = "poisson",
            cp = 0.003)
