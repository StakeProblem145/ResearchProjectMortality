FLAGS <- flags(
  flag_integer('layers', 3),
  flag_numeric('dropout', 0.05),
  flag_integer('neurons', 128),
  flag_integer('epochs', 50),
  flag_numeric('lr', 0.01),
  flag_integer('patience', 35),
  flag_integer('pats', 20),
  flag_integer('batchsize', 1200),
  flag_string('activation', 'relu')   
)

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
  
  middle <- features 
  
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
    optimizer_adam(learning_rate = FLAGS$lr),
    loss='mse',
    metrics=c('mae')
  )
  model
}

model <- build_model()


##### the folowing two callback functions are used to control the training process by monitoring the validation loss

early_stop <- callback_early_stopping(monitor = 'val_loss', patience = FLAGS$patience)


lr_reducer <- callback_reduce_lr_on_plateau(monitor = 'val_loss', factor = 0.1,
                                            patience = FLAGS$pats, verbose = 0, mode = 'min',
                                            min_delta = 1e-04, cooldown = 0, min_lr = 0)


### Fit the neural network specified as above

history <- model %>% fit(
  x  = X_dev, 
  y  = y_dev,
  batch_size = FLAGS$batchsize, 
  epochs = FLAGS$epochs,
  validation_data =list(X_val, y_val),
  verbose = 1,
  callbacks = list(early_stop,lr_reducer)
)


# plot(history)

score <- model %>% evaluate(X_test_1st, y_test_1st, verbose = 0)

save_model_hdf5(model, 'model.h5')

save(results, file = "results_nn.RDA")

# cat('Test loss:', score$loss)