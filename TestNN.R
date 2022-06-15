library(tidyverse)
library(data.table)
library(keras)
library(viridis)
library(tfruns)
library(scales)

load("data/processed/Italy_HMD_df.RDA")

HMD_df <- HMD_df %>%
  mutate("mortality" = Deaths / Exposure) %>%
  mutate("log_mortality" = log(mortality)) %>%
  filter(Age >= 40)

pred_raw <- dplyr::filter(HMD_df,Year%in%2006:2016)
train_raw <- dplyr::filter(HMD_df,Year%in%1950:2005)


hmdProcessed <- HMD_df %>%
  mutate("Gender_cat" = factor(Gender), "Age_cat" = factor(Age)) %>%
  mutate(Gender = as.integer(Gender), Age = as.integer(Age))

# Convert the integer or index starting from 0
hmdProcessed$Gender <- hmdProcessed$Gender-1
# Convert the datatype of the feature Year into numeric.
hmdProcessed$Year <- as.numeric(as.character(hmdProcessed$Year))



predProcessed <- dplyr::filter(hmdProcessed,Year%in%2006:2016)
trainProcessed <- dplyr::filter(hmdProcessed,Year%in%1950:2005)
trainProcessed <- data.table(trainProcessed)

### Bugged? Returns 50% of the Data Set...
validationData <- splitstackshape::stratified(trainProcessed, c('Year','Age'), 0.3)
fittingData <- setdiff(trainProcessed,validationData)

#### prepare the input features for the validation set

X_val <- validationData %>%
  select(Year, Age, Gender)
X_val <- list(as.matrix(X_val$Year),as.matrix(X_val$Age),as.matrix(X_val$Gender))

#### Prepare the output feature for the validation set

y_val <- validationData %>%
  select(log_mortality)
y_val <- as.matrix(y_val)

#### Prepare the input features to be fed into the neural nets and convert them into arrays

X_dev <- fittingData %>%
  select(Year, Age, Gender)
X_dev <- list(as.matrix(X_dev$Year),as.matrix(X_dev$Age),as.matrix(X_dev$Gender))

#### Prepare the output feature to be fed into the neural nets and convert it into array

y_dev <- fittingData %>%
  select(log_mortality)
y_dev <- as.matrix(y_dev)

#### Prepare the input features for the test dataset and convert them into arrays

X_test_1st <- predProcessed %>%
  select(Year, Age, Gender)
X_test_1st <- list(as.matrix(X_test_1st$Year),as.matrix(X_test_1st$Age),as.matrix(X_test_1st$Gender))

#### Prepare the output feature for the test dataset and convert them into arrays
y_test_1st <- predProcessed %>%
  select(log_mortality)
y_test_1st <- as.matrix(y_test_1st)



par <- list( 
  layers = c(4),                 # c(3,6,9),
  dropout = c(0.04),             # c(0.01,0.03,0.05,0.07),
  neurons = c(128),              # c(128,160,192,224,256)
  epochs = c(300),               # 
  batchsize = c(400),            # c(400,800,1200),
  lr = c(0.12),                  # c(0.05,0.1,0.15),
  patience = c(35),              # c(35,45),
  pats = c(20),                  # c(20,30),
  activation = c("relu")         # c("relu") 
)

# Fit also exists in StMoMo, therefore you have to detach 
# detach("package:StMoMo", unload=TRUE)
runs <- tuning_run('nn_mortality.R', runs_dir = 'D_tuning', sample = 0.05, flags = par)



#### After the training we rank the performance of all hyperparameter search runs by validation loss in ascending order.

results <- ls_runs(order = metric_val_loss, decreasing = F, runs_dir = 'D_tuning')
results <- select(results,-c(output))


#best result
id <- results[1,1]

#### Load the best performing model

path<-file.path(getwd(),id,"model.h5")
model <- load_model_hdf5(path)
summary(model)


####

X_training <- trainProcessed[,c("Year","Age","Gender")]
X_training <- list(as.matrix(X_training$Year),as.matrix(X_training$Age),as.matrix(X_training$Gender))

#### Prepare the output feature for the validation set

y_training <- trainProcessed[, "log_mortality"]
y_training <- as.matrix(y_training)


test_log_mortality <- model %>% predict(X_training)
test_NN <- cbind(train_raw[,c("Year","Age","Gender")])
test_NN$log_mortality <- y_training

test_NN <- test_NN %>% 
  mutate("mortality"=exp(log_mortality)) %>%
  mutate("NN_mortality"=exp(test_log_mortality[,1]))
test_NN$NN_log_mortality <- test_log_mortality

test_NN <- test_NN %>%
  mutate(NN_diff_abs = mortality-NN_mortality, NN_diff_p = (mortality/NN_mortality)-1)



#### Prediction / Forecast

predicted_log_mortality<- replicate(dim(y_test_1st)[1], 0)


predicted_log_mortality <- model %>% predict(X_test_1st)+predicted_log_mortality
predicted_log_mortality <- predicted_log_mortality/length(id)

NN_prediction<- cbind(pred_raw,predicted_log_mortality)
NN_prediction<-NN_prediction %>% mutate("NN_mortality"=exp(predicted_log_mortality[,1]))

sample_n(NN_prediction,6)



NN_prediction <- NN_prediction %>%
  mutate(NN_diff_abs = mortality-NN_mortality, NN_diff_p = (mortality/NN_mortality)-1)


NN_prediction_female <- filter(NN_prediction, Gender == "Female" & Age<90)
NN_prediction_male <- filter(NN_prediction, Gender == "Male")

library(viridis)

ggplot(NN_prediction_female, aes(Age, Year, fill = NN_diff_abs)) +
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


