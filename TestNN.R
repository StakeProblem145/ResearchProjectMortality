library(tidyverse)
library(data.table)
library(keras)
library(tfruns)

load("data/processed/Italy_HMD_df.RDA")

HMD_df <- HMD_df %>%
  mutate("mortality" = Deaths / Exposure) %>%
  mutate("log_mortality" = log(mortality)) %>%
  select(-c(Exposure, Deaths)) %>%
  filter(., Age%in%30:100)

pred_raw <- dplyr::filter(HMD_df,Year%in%2006:2016)


HMD_df <- HMD_df %>%
  mutate("Gender_cat" = factor(Gender), "Age_cat" = factor(Age)) %>%
  mutate(Gender = as.integer(Gender), Age = as.integer(Age))

### convert the integer or index starting from 0
HMD_df$Gender <- HMD_df$Gender-1

# Convert the datatype of the feature Year into numeric.
HMD_df$Year <- as.numeric(as.character(HMD_df$Year))
HMD_df<-HMD_df %>% select(Gender_cat,Age_cat,Year,Gender,Age, log_mortality, mortality)



training <-dplyr::filter(HMD_df,Year%in%1950:2005)

col_vector <- c("Year","Age","Gender","log_mortality")
Training<- training %>% select(one_of(col_vector))
Training <- data.table(Training)

### Bugged? Returns 50% of the Data Set...
val<-splitstackshape::stratified(Training, c('Year','Age'), 0.3)


#### prepare the input features for the validation set

X_validation <- val[,c("Year","Age","Gender")]
X_val <- list(as.matrix(X_validation$Year),as.matrix(X_validation$Age),as.matrix(X_validation$Gender))

#### Prepare the output feature for the validation set

y_validation <- val[, "log_mortality"]
y_val <- as.matrix(y_validation)


train<-setdiff(Training,val)

#### Prepare the input features to be fed into the neural nets and convert them into arrays

X_training <- train[,c("Year","Age","Gender")]
X_dev <- list(as.matrix(X_training$Year),as.matrix(X_training$Age),as.matrix(X_training$Gender))


#### Prepare the output feature to be fed into the neural nets and convert it into array

y_training <- train[, "log_mortality"]
y_dev <- as.matrix(y_training)


#### Select the test set (2006 to 2016)
test <- dplyr::filter(HMD_df,Year%in%2006:2016)

#### Prepare the input features for the test dataset and convert them into arrays

X_test <- test[,c("Year","Age","Gender")]
X_test_1st <- list(as.matrix(X_test$Year),as.matrix(X_test$Age),as.matrix(X_test$Gender))


#### Prepare the output feature for the test dataset and convert them into arrays
y_test <- test[, "log_mortality"]
y_test_1st <- as.matrix(y_test)



par <- list( 
  layers = c(3,6,9),                 # c(3,6,9),
  dropout = c(0.01,0.03,0.05,0.07),             # c(0.01,0.03,0.05,0.07),
  neurons = c(128,160,192,224,256),              # c(128,160,192,224,256)
  epochs = c(250),               # 
  batchsize = c(400,800,1200),            # c(400,800,1200),
  lr = c(0.05,0.1,0.15),                  # c(0.05,0.1,0.15),
  patience = c(35,45),              # c(35,45),
  pats = c(20,30),                  # c(20,30),
  activation = c("relu")         # c("relu") 
)

par <- list( 
  layers = c(9),                 # c(3,6,9),
  dropout = c(0.05),             # c(0.01,0.03,0.05,0.07),
  neurons = c(192),              # c(128,160,192,224,256)
  epochs = c(250),               # 
  batchsize = c(400),            # c(400,800,1200),
  lr = c(0.05),                  # c(0.05,0.1,0.15),
  patience = c(35),              # c(35,45),
  pats = c(30),                  # c(20,30),
  activation = c("relu")         # c("relu") 
)

runs <- tuning_run('nn_mortality.R', runs_dir = 'D_tuning', sample = 0.5, flags = par)



#### After the training we rank the performance of all hyperparameter search runs by validation loss in ascending order.

results <- ls_runs(order = metric_val_loss, decreasing= F, runs_dir = 'D_tuning')
results <- select(results,-c(output))




id<-results[1,1]
path<-file.path(getwd(),id,"model.h5")

#### Load the best performing model

model <- load_model_hdf5(path)
summary(model)

predicted_log_mortality<- replicate(dim(y_test_1st)[1], 0)


predicted_log_mortality <- model %>% predict(X_test_1st)+predicted_log_mortality
predicted_log_mortality <- predicted_log_mortality/length(id)

NN_prediction<- cbind(pred_raw,predicted_log_mortality)
NN_prediction<-NN_prediction %>% mutate("NN_mortality"=exp(predicted_log_mortality[,1]))

sample_n(NN_prediction,6)


NN_prediction <- NN_prediction %>%
  mutate(diff_a = mortality-NN_mortality, diff_p = abs((mortality/NN_mortality)-1))

library(viridis)
NN_prediction_female <- filter(NN_prediction, Gender == "Female")
ggplot(NN_prediction_female, aes(Age, Year, fill = diff_a)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)
ggplot(NN_prediction_female, aes(Age, Year, fill = diff_p)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)


NN_prediction_male <- filter(NN_prediction, Gender == "Male")
ggplot(NN_prediction_male, aes(Age, Year, fill = diff_a)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)
ggplot(NN_prediction_male, aes(Age, Year, fill = diff_p)) +
  geom_tile() +
  scale_fill_viridis(discrete=FALSE)
