library(tidyverse)
library(data.table)
library(keras)
library(viridis)
library(tfruns)

load("data/processed/Italy_HMD_df.RDA")

HMD_df <- HMD_df %>%
  mutate("mortality" = Deaths / Exposure) %>%
  mutate("log_mortality" = log(mortality)) %>%
  select(-c(Exposure, Deaths)) %>%
  filter(Age >= 40)

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

par2 <- list( 
  layers = c(2,4,8),                 # c(3,6,9),
  dropout = c(0.02,0.04,0.08),             # c(0.01,0.03,0.05,0.07),
  neurons = c(64,84,164,184,256),              # c(128,160,192,224,256)
  epochs = c(150.300),               # 
  batchsize = c(400,800,1200),            # c(400,800,1200),
  lr = c(0.02,0.04,0.08,0.12),                  # c(0.05,0.1,0.15),
  patience = c(35,50),              # c(35,45),
  pats = c(20,30),                  # c(20,30),
  activation = c("relu", "tanh")         # c("relu") 
)

par <- list( 
  layers = c(4),                 # c(3,6,9),
  dropout = c(0.02),             # c(0.01,0.03,0.05,0.07),
  neurons = c(184),              # c(128,160,192,224,256)
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

results <- ls_runs(order = metric_val_loss, decreasing= F, runs_dir = 'D_tuning')
results <- select(results,-c(output))

### saved results

# original result
original  <-   "D_tuning/2022-06-05T22-05-03Z"

# original with different epochs
original_epoch300  <-  "D_tuning/2022-06-06T16-42-02Z"
original_epoch600  <-  "D_tuning/2022-06-06T21-10-23Z"

#original with different layers
original_layers2  <-  "D_tuning/2022-06-06T21-15-32Z"
original_layers6  <-  "D_tuning/2022-06-06T21-18-55Z"

#original with different dropouts
original_dropout0.01  <-  "D_tuning/2022-06-07T08-00-22Z"
original_dropout0.04  <-  "D_tuning/2022-06-07T08-04-15Z"

#original with different neurons
original_neurons124  <-   "D_tuning/2022-06-07T08-08-57Z"
original_neurons200  <-   "D_tuning/2022-06-07T08-11-38Z"

#original with different batchsizes
original_batchsize200  <-   "D_tuning/2022-06-07T08-43-58Z"
original_batchsize800  <-  "D_tuning/2022-06-07T08-46-09Z"

#original with different lr
original_lr0.06  <-  "D_tuning/2022-06-07T08-48-52Z"
original_lr0.24  <-  "D_tuning/2022-06-07T08-53-05Z"

#original with different patience
original_patience25  <-  "D_tuning/2022-06-07T09-13-49Z"
original_patience45  <-  "D_tuning/2022-06-07T09-16-20Z"

#original with different pats
original_pats20  <-  "D_tuning/2022-06-07T09-18-27Z" #better model
original_pats40  <-  "D_tuning/2022-06-07T09-25-19Z"

#original with different activation functions
original_activisionlinear  <-  "D_tuning/2022-06-07T09-27-52Z"
original_activationtanh  <-  "D_tuning/2022-06-07T09-30-27Z"

#best result
id <- results[1,1]


#### Load the best performing model

path<-file.path(getwd(),id,"model.h5")
model <- load_model_hdf5(path)
summary(model)


predicted_log_mortality<- replicate(dim(y_test_1st)[1], 0)


predicted_log_mortality <- model %>% predict(X_test_1st)+predicted_log_mortality
predicted_log_mortality <- predicted_log_mortality/length(id)

NN_prediction<- cbind(pred_raw,predicted_log_mortality)
NN_prediction<-NN_prediction %>% mutate("NN_mortality"=exp(predicted_log_mortality[,1]))

sample_n(NN_prediction,6)



<<<<<<< HEAD
=======


>>>>>>> 5667214e0581f8e2e65d3db1917ad06f57f83743
NN_prediction <- NN_prediction %>%
  mutate(NN_diff_abs = mortality-NN_mortality, NN_diff_p = (mortality/NN_mortality)-1)


NN_prediction_female <- filter(NN_prediction, Gender == "Female")
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

test <- filter(NN_prediction_female, Year == 2006)
ggplot(test)+
  geom_line(aes(x = Age, y = log_mortality), color = "blue") +
  geom_line(aes(x = Age, y = log(NN_mortality)), color = "red")

ggplot(test, aes(x = Age, y = mortality/NN_mortality-1, ymin=-0.25, ymax=0.25)) +
  geom_line()

id
