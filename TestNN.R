library(keras)
library(tfdatasets)
library(tidyverse)
library(scales)

Year <- layer_input(shape = c(1),
                    dtype = "float32",
                    name = "Year")
Age <- layer_input(shape = c(1),
                   dtype = "int32",
                   name = "Age")

Age_embed = Age %>% 
  layer_embedding(input_dim = 101, output_dim = 5, input_length = 1, name = "Age_embed") %>%
  keras::layer_flatten()


features <- layer_concatenate(list(Year, Age_embed))
      
middle = features %>%
  layer_dense(units = 128, activation = "tanh") %>%
  layer_batch_normalization() %>%
  layer_dropout (0.05) %>%
  
  layer_dense(units = 128, activation = "tanh") %>%
  layer_batch_normalization() %>%
  layer_dropout (0.05) %>%
  
  layer_dense(units = 128, activation = "tanh") %>%
  layer_batch_normalization() %>%
  layer_dropout (0.05) %>%
  
  layer_dense(units = 128, activation = "tanh") %>%
  layer_batch_normalization() %>%
  layer_dropout (0.05)


main_output = layer_concatenate(list(features, middle)) %>%
  layer_dense(units = 128, activation = "tanh") %>%
  layer_batch_normalization() %>% 
  layer_dropout (0.05) %>%
  layer_dense(
    units = 1,
    activation = "sigmoid",
    name ="main_output"
  )

model <- keras_model(inputs = c(Year, Age), outputs = c(main_output))

model %>% compile(loss = "mean_squared_error",
                  optimizer = "adam",
                  metrics = c("accuracy"))

history <- model %>% fit(
  x_train,
  y_train,
  epoch = 40,
  validation_split = 0.05
)

model_predict <- model %>%
  predict(x_test)

x_train <- HMD_df %>%
  filter(Gender == "Female", Year <= 2009, Year >= 1950) %>%
  select(-c(Gender, Exposure, Deaths))

test <- feature_spec(x_train, label ~ . ) %>% 
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard())

x_train_year <- rescale(x_train$Year, to = c(-1,1))

x_train$Year <- x_train_year

y_train <- HMD_df %>%
  filter(Gender == "Female", Year <= 2009, Year >= 1950) %>%
  mutate("main_output" = Deaths / Exposure) %>%
  select(main_output)

x_test <- HMD_df %>%
  filter(Gender == "Female", Year > 2009, Year >= 1950) %>%
  select(-c(Gender, Exposure, Deaths))

y_test <- HMD_df %>%
  filter(Gender == "Female", Year > 2009, Year >= 1950) %>%
  mutate("main_output" = Deaths / Exposure) %>%
  select(main_output)
