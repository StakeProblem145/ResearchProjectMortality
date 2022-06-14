library(tidyverse)

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



