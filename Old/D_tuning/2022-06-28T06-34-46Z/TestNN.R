library(tidyverse)
library(data.table)
library(keras)
library(viridis)
library(tfruns)
library(scales)

# Prediction Dataset for Forecast
# Training Dataset to train the NN, split into
# Fitting Dataset
# Validation Dataset

load("data/processed/Italy_HMD_df.RDA")

HMD_df <- HMD_df %>%
  mutate("mortality" = Deaths / Exposure) %>%
  mutate("log_mortality" = log(mortality)) %>%
  filter(Age >= 40)


yearPredRange <- c(2006:2016)
yeraTrainRange <- c(1950:2005)

predRaw <- dplyr::filter(HMD_df,Year%in%yearPredRange)
trainRaw <- dplyr::filter(HMD_df,Year%in%yeraTrainRange)


hmdProcessed <- HMD_df %>%
  mutate("Gender_cat" = factor(Gender), "Age_cat" = factor(Age)) %>%
  mutate(Gender = as.integer(Gender), Age = as.integer(Age))

# Convert the integer or index starting from 0
hmdProcessed$Gender <- hmdProcessed$Gender-1
# Convert the datatype of the feature Year into numeric.
hmdProcessed$Year <- as.numeric(as.character(hmdProcessed$Year))



predProcessed <- dplyr::filter(hmdProcessed,Year%in%yearPredRange)
trainProcessed <- dplyr::filter(hmdProcessed,Year%in%yeraTrainRange)
trainProcessed <- data.table(trainProcessed)

### Bugged? Returns 50% of the Data Set...
validationData <- splitstackshape::stratified(trainProcessed, c('Year','Age'), 0.3)
fittingData <- setdiff(trainProcessed,validationData)

#### prepare the input features for the validation set

selectXParameterList <- function(data) {
  res <- data %>%
    select(Year, Age, Gender)
  return(list(as.matrix(res$Year),as.matrix(res$Age),as.matrix(res$Gender)))
}

selectYParameterList <- function(data) {
  res <- data %>%
    select(log_mortality)
  return(as.matrix(res))
}

# Fitting Data set
X_dev <- selectXParameterList(fittingData)
y_dev <- selectYParameterList(fittingData)

# Validation Data set
X_val <- selectXParameterList(validationData)
y_val <- selectYParameterList(validationData)

# Test Data set (Forecast)
X_test_1st <- selectXParameterList(predProcessed)
y_test_1st <- selectYParameterList(predProcessed)


par <- list( 
  layers = c(3,4,6,8),                 # c(3,6,9),
  dropout = c(0.04),             # c(0.01,0.03,0.05,0.07),
  neurons = c(128,160,200),              # c(128,160,192,224,256)
  epochs = c(300),               # 
  batchsize = c(100, 400),            # c(400,800,1200),
  lr = c(0.12),                  # c(0.05,0.1,0.15),
  patience = c(35),              # c(35,45),
  pats = c(20),                  # c(20,30),
  activation = c("relu")         # c("relu") 
)

# Fit also exists in StMoMo, therefore you have to detach 
# detach("package:StMoMo", unload=TRUE)
runs <- tuning_run('nn_mortality.R', runs_dir = 'D_tuning', sample = 1, flags = par)


#### After the training we rank the performance of all hyperparameter search runs by validation loss in ascending order.

selectBestRun <- function(number) {
  results <- ls_runs(order = metric_val_loss, decreasing = F, runs_dir = 'D_tuning')
  results <- select(results,-c(output))
  return(results[number,])
}

selectLastRun <- function() {
  results <- ls_runs(runs_dir = 'D_tuning')
  results <- select(results,-c(output))
  return(results[1,])
}

#### Load the best performing model
results <- ls_runs(runs_dir = 'D_tuning') %>%
  select(-c(output))

id <- selectBestRun(1)[,1]
path <- file.path(getwd(),id,"model.h5")
model <- load_model_hdf5(path)
summary(model)


#### Prediction on Training Period
X_training <- selectXParameterList(trainProcessed)

trainingLogMortality <- model %>% predict(X_training)

testTrainingData <- trainRaw %>%
  mutate("NN_mortality"= exp(trainingLogMortality[,1])) %>%
  mutate("NN_log_mortality" = trainingLogMortality[,1])


#### Prediction on Forecast Period
predictedLogMortality <- model %>% predict(X_test_1st)

testForecastData <- predRaw %>% 
  mutate("NN_mortality"= exp(predictedLogMortality[,1])) %>%
  mutate("NN_log_mortality" = predictedLogMortality[,1])

sample_n(testForecastData,6)


#### Prediction to infinity and beyond
xToInfinity <- list(Year = 2017:2077, Age = 40:100, Gender = 0:1)
xToInfinity <- expand.grid(xToInfinity)

infinityLogMortality <- model %>% predict(selectXParameterList(xToInfinity))

testInfinityData <- xToInfinity %>% 
  mutate("NN_mortality"= exp(infinityLogMortality[,1])) %>%
  mutate("NN_log_mortality" = infinityLogMortality[,1])

testInfinityData$Gender[testInfinityData$Gender == 0] <- "Female"
testInfinityData$Gender[testInfinityData$Gender == 1] <- "Male"

sample_n(testInfinityData,6)



#### Create Deviances and Residuals
createDevAndRes <- function(data) {
  #dev <- sum(2 * (ifelse(O == 0, 0, O * log(O/E)) - (O - E)))
  #res <- sign(O - E) * sqrt(2 * (ifelse(O == 0, 0, O * log(O/E)) - (O - E)))
  data <- data %>%
    mutate(NN_diff_abs = mortality-NN_mortality, NN_diff_p = (mortality/NN_mortality)-1) %>%
    mutate(Pred_Deaths = Exposure*NN_mortality) %>%
    mutate(Dev_Deaths = 2 * (ifelse(Deaths == 0, 0, Deaths * log(Deaths/Pred_Deaths)) - (Deaths - Pred_Deaths))) %>%
    mutate(Dev_mortality = 2 * (ifelse(mortality == 0, 0, mortality * log(mortality/NN_mortality)) - (mortality - NN_mortality))) %>%
    mutate(Res_Deaths = sign(Deaths - Pred_Deaths) * sqrt(Dev_Deaths)) %>%
    mutate(Res_mortality = sign(mortality - NN_mortality) * sqrt(Dev_mortality))
  return(data)
}

testTrainingData <- createDevAndRes(testTrainingData)
sum(testTrainingData$Dev_Deaths)

testForecastData <- createDevAndRes(testForecastData)
sum(testForecastData$Dev_Deaths)



