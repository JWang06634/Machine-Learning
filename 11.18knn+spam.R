library(mlbench) ## houses the dataset for this example
data("PimaIndiansDiabetes2")

library(caret)
library(dplyr)
library(ggplot2)

PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)

###Inspect the data
class(PimaIndiansDiabetes2$diabetes)

###Visualize data
ggplot(PimaIndiansDiabetes2, aes(glucose, mass, color=diabetes)) + geom_point()

###Set seed
set.seed(123)
divideData <- createDataPartition(PimaIndiansDiabetes2$diabetes, p=.8, list = FALSE)
train <- PimaIndiansDiabetes2[divideData,]
test <- PimaIndiansDiabetes2[-divideData,]

###Make the model on the training data
knnfit <- train(diabetes~., data = train, method="knn", preProcess = c("center", "scale"))

###Find best # of K
knnfit$bestTune
plot(knnfit)

###Make predictions
knnclass <- predict(knnfit, newdata = test)
head(knnclass)

###Confusion matrix
confusionMatrix(knnclass, test$diabetes) 
#see accuracy rate, CI(smaller, more narrow) -- accuracy should in CI  
#and sensitivity-balanced accuracy
mean(knnclass==test$diabetes)
A <- 42; B <- 12; C <- 10; D <- 14
##how well we did at predicting positives
sensitivity <- A/(A+C); sensitivity
##how well we did at prediction negatives
specificity <- D/(B+D); specificity


####Spam problem
install.packages("kernlab")
library(kernlab)
data(spam)

###Seperate the data
set.seed(123)
divideData <- createDataPartition(spam$type, p = .3, list = FALSE)
train <- spam[divideData,]
test <- spam[-divideData,]

###Make our model
spamfit <- train(type~., data = train, method = "knn", preProcess = c("center", "scale"))

###Make predictions
predknn <- predict(spamfit, newdata = test)

###Create confusion matrix
confusionMatrix(predknn, test$type)
#accuracy rate 0.8944

###logistic regression
spamlogisticmodel <- glm(type~., data = train, family = binomial)

###Set up predictions and calculate probabilities
spamprob <- predict(spamlogisticmodel, test, type = "response")
spampred <- ifelse(spamprob > .5, "spam", "nonspam")

mean(spampred==test$type)
###Logistic regression accuracy rate 0.9307453
table(spampred, test$type)

summary(spamlogisticmodel)
exp(spamlogisticmodel$coefficients)
