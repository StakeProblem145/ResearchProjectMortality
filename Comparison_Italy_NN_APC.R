library(tidyverse)

comparisonDF_NN_CLA <- left_join(NN_prediction_female, classicModelForcast) %>%
  select(-c("V1"))



sum(abs(comparisonDF_NN_CLA$NN_diff_p))
sum(abs(comparisonDF_NN_CLA$CLA_diff_p))


### Training Comparison

#Fixed Year Comparison

year <- 1975

ggplot(filter(test_NN, Year == year)) +
  geom_line(aes(x = Age, y = log(mortality), color = "real")) +
  geom_line(aes(x = Age, y = log(NN_mortality), color = "NN")) 


ggplot(filter(test_NN, Year == year)) +
  geom_point(aes(Age, y = mortality/NN_diff_abs-1, color = "NN")) 


#Fixed Age Comparison

age <- 40

ggplot(filter(test_NN, Age == age)) +
  geom_line(aes(x = Year, y = log(mortality), color = "real")) +
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

age <- 90

ggplot(filter(comparisonDF_NN_CLA, Age == age)) +
  geom_line(aes(x = Year, y = log(mortality), color = "real")) +
  geom_line(aes(x = Year, y = log(NN_mortality), color = "NN")) +
  geom_line(aes(x = Year, y = log(CLA_mortality), color = "CLA"))

ggplot(filter(comparisonDF_NN_CLA, Age == age)) +
  geom_point(aes(Year, y = mortality/NN_diff_abs-1, color = "NN")) +
  geom_point(aes(Year, y = mortality/CLA_diff_abs-1, color = "CLA")) 



