####
library(caret)
library(dplyr)

mower <- read.csv("LawnMowers.csv", stringsAsFactors = TRUE)
class(mower$Ownership)

###Split my dataset
set.seed(123)
divideData <- createDataPartition(mower$Ownership, p = .5, list = FALSE)
train <- mower[divideData,]
test <- mower[-divideData,]

###Visualize
plot(Lot_Size~Income, data = train, pch=ifelse(train$Ownership=="Owner",1,3))

###Make my model
knnfit <- train(Ownership~., data = train, method = "knn", preProces=c("center","scale"))
plot(knnfit)
knnfit$bestTune

###Make predictions
knnclass <- predict(knnfit, newdata = test)
head(knnclass)

###calculate accuracy rate
table(knnclass, test$Ownership) 
# knnclass   Nonowner Owner <- actual
# Nonowner        6     1
# Owner           6    11
# Predicted
1-7/24 #accuracy rate
7/24 #error rate
mean(knnclass==test$Ownership)

confusionMatrix(knnclass, test$Ownership)
