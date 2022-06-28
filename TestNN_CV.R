library(tidyverse)
library(caret)
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




#### prepare the input feature sets

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
 

# Test Data set (Forecast)
X_test_1st <- selectXParameterList(predProcessed)
y_test_1st <- selectYParameterList(predProcessed)


# Data Set to infinity and beyond
xToInfinity <- list(Year = 2017:2077, Age = 40:100, Gender = 0:1)
xToInfinity <- expand.grid(xToInfinity)



par <- list( 
  layers = c(4),                 # c(3,6,9),
  dropout = c(0.04),             # c(0.01,0.03,0.05,0.07),
  neurons = c(128),              # c(128,160,192,224,256)
  epochs = c(300),               # 
  batchsize = c(100),            # c(400,800,1200),
  lr = c(0.12),                  # c(0.05,0.1,0.15),
  patience = c(35),              # c(35,45),
  pats = c(20),                  # c(20,30),
  activation = c("relu")         # c("relu") 
)

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

#K-Fold Validation Split. Testing on the mean of the K fold Forecasts
KFOLDS <- 5
ids <- 1:nrow(trainProcessed)
predictedLogMortality_folds <- vector(mode="list", length=KFOLDS)
predictedLogMortality <- vector(mode="list", length=1)

infinityLogMortality_folds <- vector(mode="list", length=KFOLDS)
infinityLogMortality <- vector(mode="list", length=1)

# train and target data
train <- trainProcessed
target <- trainProcessed$log_mortality

# create K-Folds
folds <- createFolds(target, k=KFOLDS, list=TRUE, returnTrain=FALSE)



for(fld in 1:length(folds))
{
  #ids for data split control
  controlID <- ids[folds[[fld]]]
  
  
  dtrain         <- train[folds[[fld]]*-1,]
  dtraintarget   <- data.frame(log_mortality = target[folds[[fld]]*-1])
  
  dcontrol       <- train[folds[[fld]],]
  dcontroltarget <- data.frame(log_mortality = target[folds[[fld]]])
  
  #feautures
  X_dev <- selectXParameterList(dtrain)
  y_dev <- selectYParameterList(dtraintarget)
  
  X_val <- selectXParameterList(dcontrol)
  y_val <- selectYParameterList(dcontroltarget)
  
  # Fit also exists in StMoMo, therefore you have to detach 
  # detach("package:StMoMo", unload=TRUE)
  runs <- tuning_run('nn_mortality.R', runs_dir = 'D_tuning', sample = 1, flags = par)
  
  
  #### Load the last model and save results
  results <- ls_runs(runs_dir = 'D_tuning') %>%
    select(-c(output))
  
  
  id <- selectLastRun()[,1]
  path <- file.path(getwd(),id,"model.h5")
  model <- load_model_hdf5(path)
  
  # Prediction on training period by validation data 
  ValLogMortality <- model %>% predict(X_val)
  
  #use K-folded model for the test prediction
  predictedLogMortality_folds[[fld]] <- model %>% predict(X_test_1st)
  
  #### use K-folded model for the Prediction to infinity and beyond
  infinityLogMortality_folds[[fld]] <- model %>% predict(selectXParameterList(xToInfinity))
  
  
  
  
  #Prediction on training period by id
  if (fld > 1)
  {
    trainingLogMortality <- rbind(trainingLogMortality, data.frame(ids = controlID, NN_log_mortality= ValLogMortality))
  }
  else
  {
    trainingLogMortality <- data.frame(ids = controlID, NN_log_mortality= ValLogMortality)
  }
  
  
}




#### Prediction on Training Period
# restore previous order of training data
trainingLogMortality <- trainingLogMortality[order(trainingLogMortality$ids),]
testTrainingData <- trainRaw %>%
  mutate("NN_mortality"= exp(trainingLogMortality[,2])) %>%
  mutate("NN_log_mortality" = trainingLogMortality[,2])



### Prediction  Forecast mortality as mean over K Folds
y_pred_test <- as.data.frame(do.call(cbind, predictedLogMortality_folds))
predictedLogMortality[[1]] <-  apply(y_pred_test, 1, mean)

testForecastData <- predRaw %>% 
  mutate("NN_mortality"= exp(predictedLogMortality[[1]])) %>%
  mutate("NN_log_mortality" = predictedLogMortality[[1]])

sample_n(testForecastData,6)


#### Prediction to infinity and beyond  as mean over K-Folds
y_pred_infinity <- as.data.frame(do.call(cbind, infinityLogMortality_folds))
infinityLogMortality[[1]] <-  apply(y_pred_infinity, 1, mean)

testInfinityData <- xToInfinity %>% 
  mutate("NN_mortality"= exp(infinityLogMortality[[1]])) %>%
  mutate("NN_log_mortality" = infinityLogMortality[[1]])

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



