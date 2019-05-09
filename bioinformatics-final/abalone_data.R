  # install.packages(c("tidyr", "devtools"))
  
  library(keras)
  library(tidyr)
  
  abalone <- read.csv(file='abalone.csv', header=TRUE, sep=',')
  
  x <- as.factor(abalone$Sex)
  levels(x) <- 1:length(levels(x))
  x <- as.numeric(x)
  x <- x - 1
  x <- to_categorical(x, 3)
  s
  abalone$Male=x[,3]
  abalone$Female=x[,1]
  abalone$Infant=x[,2]
  
  abalone$Sex <- NULL
  
  attach(abalone)
  abalone <- data.frame(Male, Female, Infant, Length, Diameter, Height, Whole.weight, Shucked.weight, Viscera.weight, Shell.weight, Rings)
  detach(abalone)
  
  x <- abalone$Rings
  for (i in 1:length(x)) x[i] <- if(x[i] < 9) {0} else if (x[i] < 11) {1} else {2}
  abalone$Rings <- x
  
  # https://stackoverflow.com/questions/36068963/r-how-to-split-a-data-frame-into-training-validation-and-test-sets
  spec = c(train = .6, test = .2, validate = .2)
  
  g = sample(cut(
    seq(nrow(abalone)), 
    nrow(abalone)*cumsum(c(0,spec)),
    labels = names(spec)
  ))
  
  res = split(abalone, g)
  
  x_train <- data.matrix(res$train[,1:10])
  x_test <- data.matrix(res$test[,1:10])
  x_validate <- data.matrix(res$validate[,1:10])
  
  
  y_train <- res$train$Rings
  y_test <- res$test$Rings
  y_validate <- res$validate$Rings
  
  y_train <- to_categorical(y_train, 3)
  y_test <- to_categorical(y_test, 3)
  y_validate <- to_categorical(y_validate, 3)
  
  # Create the model
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = 256, activation = 'relu', input_shape = c(10)) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 3, activation = 'softmax')
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  history <- model %>% fit(
    x_train, y_train, 
    epochs = 100, batch_size = 128, 
    validation_split = 0.2
  )
  
  model %>% evaluate(x_test, y_test)
  model %>% evaluate(x_validate, y_validate)
  
  model %>% predict_classes(x_test)