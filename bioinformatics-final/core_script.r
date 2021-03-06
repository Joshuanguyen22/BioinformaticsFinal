# https://keras.rstudio.com/
# https://machinelearningmastery.com/tutorial-first-neural-network-python-keras/
# Install commands
# devtools::install_github("rstudio/keras")

library(keras)

# Here I will work through the digit recognition example

# Load in the data
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape the data from a matrix into an array
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale the values from [0, 255] to [0,1]
x_train <- x_train / 255
x_test <- x_test / 255

# https://www.quora.com/What-is-one-hot-encoding-and-when-is-it-used-in-data-science
# Now we one-hot encode the y data as it is currently an integer from 0-9
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# Create the model
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

model %>% evaluate(x_test, y_test)

model %>% predict_classes(x_test)