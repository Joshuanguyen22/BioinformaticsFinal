#We Need these packages!
install.packages("gclus")
install.packages("rpart")
install.packages("rpart.plot")

#Load the libraries we need, and set the seed.
library(rpart.plot)
library(rpart)
library(gclus)
set.seed(1)

#Read in our abalone data
abaloneData = read.csv(file="abalone.csv", header = TRUE, sep=",")
attach(abaloneData)

#Sanitize the data by removing non-quantitative rows and splitting the data into a testing and training set.
PCData = subset(abaloneData,select=-c(Sex))
splitter = sample(1:2,size=nrow(PCData),replace=TRUE,prob=c(0.6,0.4))
train = PCData[splitter==1,]
test = PCData[splitter==2,]

#Run the principal compononet analysis on the training data.
princomp_fit = princomp(train, cor=TRUE)
plot(princomp_fit, type="lines")  #Here is a scree plot.
PCData.r = abs(cor(PCData))
PCData.col = dmat.color(PCData.r)
cpairs(PCData,order=NULL,panel.colors = PCData.col,gap=0.5,main="Abalone Data PCA")  #Here is a series of scatter plots comparing variables.

#Prepare the training data set by attaching the best PCA fit values to the associated outputs.
train.data = data.frame(Rings = train$Rings, princomp_fit$scores)
train.data = train.data[,1:2]

#Create a decision tree based on the training data and the PCA fit values.
abaloneRPart = rpart(Rings ~ ., data = train.data, method = "anova", cp = 0.01)
rpart.plot(abaloneRPart)  #Here is a cool plot of the decision tree!

#Prepare the test data by predicting PC values for the data based on the previous PCA.
test.data = predict(princomp_fit,test)
test.data = as.data.frame(test.data)
test.data = test.data[,1]
test.data = data.frame(Comp.1 = test.data)

#Predict "Rings" based on the test data set and the decision tree!
abalonePredictions=predict(abaloneRPart,test.data)

#Compare the predicted values to the actual values.
ringsData = test[,"Rings"]
comparison = data.frame(Predicted_Rings = abalonePredictions, Actual_Rings = ringsData, Difference = abalonePredictions-ringsData)
attach(comparison)
summary(abs(Difference))

#Determine the distribution of "Rings" values.
hist(Rings,breaks = 30, col = "green")   #A nice histogram of Ring values
ringCategories = table(cut(Rings,1:30,right=FALSE))
ringFrequencies = cumsum(ringCategories)
ringFrequencies = ringFrequencies/4177
plot(ringFrequencies, col = "blue", xlab = "Rings", ylab = "Cumulative Frequency")  #A nice line graph of cumulative ring frequencies
lines(1:29,ringFrequencies, col = "blue")