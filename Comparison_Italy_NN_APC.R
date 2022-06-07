library(tidyverse)

comparisonDF_NN_CLA <- left_join(NN_prediction_female, classicModelForcast) %>%
  select(-c("V1"))


sum(abs(comparisonDF_NN_CLA$NN_diff_p))
sum(abs(comparisonDF_NN_CLA$CLA_diff_p))


year <- 2016

ggplot(filter(comparisonDF_NN_CLA, Year == year)) +
  geom_line(aes(x = Age, y = log(mortality), color = "real")) +
  geom_line(aes(x = Age, y = log(NN_mortality), color = "NN")) +
  geom_line(aes(x = Age, y = log(CLA_mortality), color = "CLA"))


ggplot(filter(comparisonDF_NN_CLA, Year == year)) +
  geom_point(aes(Age, y = mortality/NN_diff_abs-1, color = "NN")) +
  geom_point(aes(Age, y = mortality/CLA_diff_abs-1, color = "CLA")) 