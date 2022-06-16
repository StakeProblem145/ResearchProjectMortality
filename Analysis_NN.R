library(tidyverse)
library(viridis)

NN_prediction_female <- filter(testForecastData, Gender == "Female" & Age<90)
NN_prediction_male <- filter(testForecastData, Gender == "Male")


heatmapAgeYear <- function(dataSet, fillParameter, gender, limitFillParameter) {
  if(hasArg(limitFillParameter)) {
    dataSet <- dataSet %>%
      mutate(across(!!fillParameter, ~ ifelse(. <= limitFillParameter[1], limitFillParameter[1], .))) %>%
      mutate(across(!!fillParameter, ~ ifelse(. >= limitFillParameter[2], limitFillParameter[2], .)))
  }

  ggplot(filter(dataSet, Gender == gender), aes_string("Year", "Age", fill = fillParameter)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue")
}

heatmapAgeYear(testTrainingData, "Res_Deaths", "Female", c(-10,10))

ggplot(testTrainingData, aes(Age, Year, fill = Res_mortality)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)

ggplot(NN_prediction_female, aes(Age, Year, fill = NN_diff_p)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)



ggplot(NN_prediction_male, aes(Age, Year, fill = NN_diff_abs)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)

ggplot(NN_prediction_male, aes(Age, Year, fill = NN_diff_p)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)

predProcessed <- filter(NN_prediction_female, Year == 2006)
ggplot(predProcessed)+
  geom_line(aes(x = Age, y = log_mortality), color = "blue") +
  geom_line(aes(x = Age, y = log(NN_mortality)), color = "red")

ggplot(predProcessed, aes(x = Age, y = mortality/NN_mortality-1, ymin=-0.25, ymax=0.25)) +
  geom_line()

comparisonDF_NN_CLA <- left_join(NN_prediction_female, classicModelForcast) %>%
  select(-c("V1"))



sum(abs(comparisonDF_NN_CLA$NN_diff_p))
sum(abs(comparisonDF_NN_CLA$CLA_diff_p))

#O <- train_raw$De
#E <- rates * d$e
#dev <- sum(2 * (ifelse(O == 0, 0, O * log(O/E)) - (O - E)))
#res <- sign(O - E) * sqrt(2 * (ifelse(O == 0, 0, O * log(O/E)) - (O - E)))



### Training Comparison

#Fixed Year Comparison

year <- 1980
filter_training_year <- filter(test_NN, Year == year, Gender == "Female")
ggplot(filter_training_year) +
  geom_point(aes(x = Age, y = log(mortality), color = "real")) +
  geom_line(aes(x = Age, y = log(NN_mortality), color = "NN")) 


ggplot(filter_test) +
  geom_point(aes(Age, y = mortality/NN_diff_abs-1, color = "NN")) 




#Fixed Age Comparison

age <- 80


filter_training_age <- filter(test_NN, Age == age, Gender == "Female")
ggplot(filter_training_age) +
  geom_point(aes(x = Year, y = log(mortality), color = "real")) +
  geom_line(aes(x = Year, y = log(NN_mortality), color = "NN")) 

ggplot(filter(test_NN, Age == age)) +
  geom_point(aes(Year, y = mortality/NN_diff_abs-1, color = "NN"))

 

### Forecast Comparison

# Fixed Year Comparison

year <- 2008

ggplot(filter(comparisonDF_NN_CLA, Year == year)) +
  geom_line(aes(x = Age, y = log(mortality), color = "real")) +
  geom_line(aes(x = Age, y = log(NN_mortality), color = "NN")) +
  geom_line(aes(x = Age, y = log(CLA_mortality), color = "CLA"))


ggplot(filter(comparisonDF_NN_CLA, Year == year)) +
  geom_point(aes(Age, y = mortality/NN_diff_abs-1, color = "NN")) +
  geom_point(aes(Age, y = mortality/CLA_diff_abs-1, color = "CLA")) 


# Fixed Ages Comparison

age <- 70

ggplot(filter(comparisonDF_NN_CLA, Age == age )) +
  geom_line(aes(x = Year, y = log(mortality), color = "real")) +
  geom_line(aes(x = Year, y = log(NN_mortality), color = "NN")) +
  geom_line(aes(x = Year, y = log(CLA_mortality), color = "CLA"))

ggplot(filter(comparisonDF_NN_CLA, Age == age)) +
  geom_point(aes(Year, y = mortality/NN_diff_abs-1, color = "NN")) +
  geom_point(aes(Year, y = mortality/CLA_diff_abs-1, color = "CLA")) 


