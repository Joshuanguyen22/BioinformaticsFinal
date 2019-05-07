# install.packages(c("tidyr", "devtools"))

library(keras)
library(tidyr)

abalone <- read.csv(file='abalone.csv', header=TRUE, sep=',')

x <- as.factor(abalone$Sex)
levels(x) <- 1:length(levels(x))
x <- as.numeric(x)
x <- x - 1
x <- to_categorical(x, 3)

abalone$Male=x[,3]
abalone$Female=x[,1]
abalone$Infant=x[,2]

abalone$Sex <- NULL
attach(abalone)

abalone <- data.frame(Male, Female, Infant, Length, Diameter, Height, Whole.weight, Shucked.weight, Viscera.weight, Shell.weight, Rings)
detach(abalone)
head(abalone)