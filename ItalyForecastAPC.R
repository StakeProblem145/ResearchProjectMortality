# Initialize libraries----
library(StMoMo)
library(demography)  
library(tidyverse)

# Import data----
#setwd("C:/Eigene Dateien/Hochschule/Kanada Projekt/1. Projektphase/15")

setwd("C:/Users/Captain/Dropbox/Uni/ResearchProject/ResearchProjectMortality/data/raw")

CAN <- read.demogdata("Italy_Death_Rates_1x1.txt",
                      "Italy_Exposures_1x1.txt",
                      type="mortality",
                      label="Italy")

# Set data ranges----
ageRange <- 40:100
yearRange <- 1950:2005
filterSeries = 'female'

##### M1 #####
M1_mle <- function(ageRange,
                   yearRange,
                   filterSeries,
                   demogData = CAN) {
  fit(
    lc(link = "log"),
    data = StMoMoData(demogData,
                      series = filterSeries),
    ages.fit = ageRange,
    years.fit =  yearRange,
    verbose = FALSE,
    options(warn= -1)
  )
}

##### M2 #####
M2_mle <- function(M1,
                   cohortAgeFun = "1",
                   clip = 5,
                   ageRange,
                   yearRange,
                   filterSeries,
                   demogData = CAN) {
  
  
  fit(
    rh(
      link = "log",
      cohortAgeFun = cohortAgeFun, #"NP"
      approxConst = T
    ),
    wxt = genWeightMat(
      ages = ageRange,
      years =  yearRange,
      clip = clip
    )
    ,
    data = StMoMoData(demogData,
                      series = filterSeries),
    ages.fit = ageRange,
    years.fit =  yearRange,
    start.ax = M1$ax,
    start.bx = M1$bx,
    start.kt = M1$kt,
    verbose = FALSE,
    options(warn= -1)
  )
}


##### M3 #####
M3_mle <- function(M1,
                   clip = 5,
                   ageRange,
                   yearRange,
                   filterSeries,
                   demogData = CAN) {
  
  
  fit(
    apc(link = "log"),
    data = StMoMoData(demogData,
                      series = filterSeries),
    ages.fit = ageRange,
    years.fit =  yearRange,
    start.ax = M1$ax,
    start.kt = M1$kt,
    verbose = FALSE,
    options(warn= -1),
    wxt = genWeightMat(
      ages = ageRange,
      years =  yearRange,
      clip = clip
    )
  )
}


##### Fit der Modelle #####
M1fit=M1_mle(ageRange,yearRange,filterSeries, CAN)
#M2fit=M2_mle(M1=M1fit,cohortAgeFun = "1",clip = 5,ageRange,yearRange,filterSeries,demogData = CAN)
M3fit=M3_mle(M1=M1fit,clip = 5,ageRange,yearRange,filterSeries,demogData = CAN)

#plot(M2fit, parametricbx = FALSE)
plot(M3fit, parametricbx = FALSE)

#######################################################

# (2,0,2), (2,1,2), (3,0,0) f?r gamma f?r M2
# (0,1,0), (0,2,1), (2,1,0), (3,0,0) f?r kappa M3
# Plots und AIC's
# Parameterwerte 
# f?r Male und Female

##### Forecasting #####

# kt.method -> "iarima" oder "mrwd"

# kt.order -> Angabe eines Arima Modells f?r kappa
# gc.order -> Angabe einer Arima Modells f?r gamma

##### M2 #####
# M2for=forecast(M2fit,h=30,level=c(80,95),kt.method="iarima",kt.order=c(0,2,1),gc.order=c(1,1,1))
# 
# #plot(M2fit, parametricbx = FALSE)
# # Plots des forecastings f?r kappa und gamma
# plot(M2for,cex.lab=1.5,cex.main=1.5,cex.axis=1.5,only.kt=TRUE)
# plot(M2for,cex.lab=1.5,cex.main=1.5,cex.axis=1.5,only.gc=TRUE)
# 
# M2for[["kt.f"]][["model"]][["models"]][[1]][["aic"]]   # AIC Wert f?r kappa  >> kt.order=c(0,2,1): AIC=71.5
# M2for[["gc.f"]][["model"]][["aic"]]                    # AIC Wert f?r gamma >> gc.order=c(1,1,1); AIC=-406.3 | gc.order=c(1,0,0): AIC=-367.9
# 
# ##### Speichern der Parameterwerte #####
# M2_kt_mean     = M2for[["kt.f"]][["mean"]][1,]
# M2_kt_lower_95 = M2for[["kt.f"]][["lower"]][1,,2]
# M2_kt_lower_80 = M2for[["kt.f"]][["lower"]][1,,1]
# M2_kt_upper_95 = M2for[["kt.f"]][["upper"]][1,,2]
# M2_kt_upper_80 = M2for[["kt.f"]][["upper"]][1,,1]
# 
# M2_gc_mean     = M2for[["gc.f"]]$mean
# M2_gc_lower_95 = M2for[["gc.f"]]$lower[,2]
# M2_gc_lower_80 = M2for[["gc.f"]]$lower[,1]
# M2_gc_upper_95 = M2for[["gc.f"]]$upper[,2]
# M2_gc_upper_80 = M2for[["gc.f"]]$upper[,1]




##### M3 #####
M3for=forecast(M3fit,h=50,level=c(80,95),kt.method="iarima",kt.order=c(0,1,0),gc.order=c(1,0,0))

# Plots des forecastings f?r kappa und gamma
plot(M3for,cex.lab=1.5,cex.main=1.5,cex.axis=1.5,only.kt=TRUE)
plot(M3for, parametricbx = FALSE, nCol = 2)
M3for[["kt.f"]][["model"]][["models"]][[1]][["aic"]]   # AIC Wert f?r kappa

plot(M3for,cex.lab=1.5,cex.main=1.5,cex.axis=1.5,only.kt=TRUE)
plot(M3for,cex.lab=1.5,cex.main=1.5,cex.axis=1.5,only.gc=TRUE)

M3for[["gc.f"]][["model"]][["aic"]]                    # AIC Wert f?r gamma



library(viridis)

M3for_df <- as.data.frame(as.table(M3for$rates))
colnames(M3for_df) <- c("Age", "Year", "mx")

M3for_df <- dplyr::filter(M3for_df,Year%in%2006:2016)
M3for_df <- M3for_df[order(M3for_df$Age),]

real_data <- filter(pred_raw, Gender == "Female")


real_data$mx_for <- M3for_df$mx

library(viridis)

real_data <- real_data %>%
  mutate(diff_abs = mortality-mx_for, diff_p = (mortality/mx_for)-1)


ggplot(real_data, aes(Age, Year, fill = diff_abs)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)

ggplot(real_data, aes(Age, Year, fill = diff_p)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)
