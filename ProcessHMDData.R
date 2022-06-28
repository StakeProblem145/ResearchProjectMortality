# Open libraries ####

library(data.table)
library(lubridate)
library(tidyverse)


##### Exposure ####
HMD_E <- fread("data/raw/Japan_Exposure_1x1.txt")
head(HMD_E)
colnames(HMD_E)
which(HMD_E$Age=="110+")

HMD_E[,Age:=as.numeric(Age)]
HMD_E[,Age:=replace_na(Age,110)]
HMD_E=data.table::melt(HMD_E, id.vars = c("Year","Age"))
colnames(HMD_E)[3:4]<-c("Gender","Exposure")
HMD_E=filter(HMD_E, !Gender=="Total")
head(HMD_E)
summary(HMD_E)

##### Deaths ####
HMD_D <- fread("data/raw/Japan_Deaths_1x1.txt")
head(HMD_D)
HMD_D[,Age:=as.numeric(Age)]

HMD_D[,Age:=replace_na(Age,110)]
HMD_D=melt(HMD_D,
             id.vars =c("Year", "Age"))
colnames(HMD_D)[3:4]<-c("Gender","Deaths")
HMD_D<-filter(HMD_D, !Gender=="Total")
head(HMD_D)
summary(HMD_D)

##### Combined Data Tables ####
HMD_df<-merge.data.table(HMD_E, HMD_D,
                         by = c("Age", "Gender","Year"))
head(HMD_df)
summary(HMD_df)
HMD_df$Gender<-factor(HMD_df$Gender)

# Find which ages we do not have exposures/deaths data
HMD_df$Age[which(is.na(as.numeric(HMD_df$Exposure)))]
HMD_df$Age[which(is.na(as.numeric(HMD_df$Deaths)))]
# Filter Age
HMD_df<-filter(HMD_df,  Age <= 100)
HMD_df$Exposure<-as.numeric(HMD_df$Exposure)
HMD_df$Deaths<-as.numeric(HMD_df$Deaths)
summary(HMD_df)

Japan_HMD_df <- HMD_df

remove(HMD_E)
remove(HMD_D)
save(HMD_df, file = "data/processed/Japan_HMD_df.RDA")
