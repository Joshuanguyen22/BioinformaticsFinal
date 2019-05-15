  # install.packages(c("tidyr", "devtools"))
  
  library(keras)
  library(tidyr)
  library(caret)
  
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
  
  y_test_values <- y_test
  y_validate_values <- y_validate
  
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
  
  predicted <- model %>% predict_classes(x_validate)
  reference <- y_validate_values
  
# https://ragrawal.wordpress.com/2011/05/16/visualizing-confusion-matrix-in-r/

actual = as.data.frame(table(reference))
names(actual) = c("Actual","ActualFreq")

confusion = as.data.frame(table(reference, predicted))
names(confusion) = c("Actual","Predicted","Freq")

#calculate percentage of test cases based on actual frequency
confusion = merge(confusion, actual, by=c("Actual"))
confusion$Percent = confusion$Freq/confusion$ActualFreq*100

#render plot
# we use three different layers
# first we draw tiles and fill color based on percentage of test cases
tile <- ggplot() +
  geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) +
  labs(x="Actual",y="Predicted")
tile = tile + 
  geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
  scale_fill_gradient(low="grey",high="red")

# lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
tile = tile + 
  geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.3, fill="black", alpha=0) 

#render
tile
