library(keras)
library(tfdatasets)
library(tidyverse)
library(scales)
library(splitstackshape)
library(data.table)

load("data/processed/Italy_HMD_df.RDA")

HMD_df <- HMD_df %>%
  mutate("mortality" = Deaths / Exposure) %>%
  mutate("log_mortality" = log(mortality)) %>%
  select(-c(Exposure, Deaths))

pred_raw <- dplyr::filter(HMD_df,Year%in%2006:2016)


HMD_df <- HMD_df %>%
  mutate("Gender_cat" = factor(Gender), "Age_cat" = factor(Age)) %>%
  mutate(Gender = as.integer(Gender), Age = as.integer(Age))

### convert the integer or index starting from 0
HMD_df$Gender <- HMD_df$Gender-1

# Convert the datatype of the feature Year into numeric.
HMD_df$Year <- as.numeric(as.character(HMD_df$Year))
HMD_df<-HMD_df %>% select(Gender_cat,Age_cat,Year,Gender,Age, log_mortality, mortality)



training <-dplyr::filter(HMD_df,Year%in%1956:2005)

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


build_model <- function() {
  
  Year <- layer_input(shape=c(1),dtype='float32',name='Year')
  Age <- layer_input(shape=c(1),dtype='int32',name='Age')
  Gender <- layer_input(shape=c(1),dtype='int32',name='Gender')
  
  ##### set up the embedding layer of the neural nets
  
  
  Age_embed <- Age %>% 
    layer_embedding(input_dim = 101,output_dim=5,input_length=1,name='Age_embed') %>% 
    keras::layer_flatten()
  
  Gender_embed <- Gender %>% 
    layer_embedding(input_dim=2,output_dim=5,input_length = 1,name='Gender_embed') %>% 
    keras::layer_flatten()
  
  
  ##### merge all the feature vectors 
  
  features <- layer_concatenate(list(Year,Age_embed,Gender_embed)) 
  
  middle<-features 
  
  for (i in (1: FLAGS$layers)){  
    middle <- middle %>% 
      layer_dense(units=FLAGS$neurons,activation=FLAGS$activation) %>% 
      layer_batch_normalization() %>% 
      layer_dropout(FLAGS$dropout)      
  }  
  
  
  ##### set up the output layer
  
  main_output <- layer_concatenate(list(features,middle)) %>% 
    layer_dense(units=FLAGS$neurons,activation=FLAGS$activation) %>%
    layer_batch_normalization() %>% 
    layer_dropout(FLAGS$dropout)%>% 
    layer_dense(units=1,name='main_output')
  
  #### set up the model combining input layers and output layer
  
  model <- keras_model(inputs=c(Year,Age,Gender),outputs=c(main_output))
  
  
  model %>% compile(
    optimizer_adam(learning_rate = FLAGS$learning_rate),
    loss='mse',
    metrics=c("accuracy", 'mae')
  )
  model
}


FLAGS <- list( 
  layers = c(3),
  dropout = c(0.01),
  neurons = c(128),
  batchsize = c(400),
  learning_rate = c(0.05),
  patience = c(35),
  pats = c(20),
  activation = c("relu")  
)



model <- build_model()

early_stop <- callback_early_stopping(monitor = 'val_loss', patience = FLAGS$patience)

lr_reducer <- callback_reduce_lr_on_plateau(monitor = 'val_loss', factor = 0.1,
                                            patience = FLAGS$pats, verbose = 0, mode = 'min',
                                            min_delta = 1e-04, cooldown = 0, min_lr = 0)


history <- model %>% fit(
  x = X_dev,
  y = y_dev,
  batch_size = FLAGS$batchsize,
  epoch = 50,
  validation_data = list(X_val, y_val),
  verbose = 1,
  callbacks = list(early_stop,lr_reducer)
)

score <- model %>% evaluate(X_test_1st, y_test_1st, verbose = 0)

#### After the training we rank the performance of all hyperparameter search runs by validation loss in ascending order.

results <- ls_runs(order = metric_val_loss, decreasing= F, runs_dir = 'D_tuning')

results <- select(results,-c(output))