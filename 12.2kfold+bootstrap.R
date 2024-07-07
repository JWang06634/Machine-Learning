library(caret)
library(dplyr)

data("iris")
str(iris)

## Split
set.seed(123)
divideData <- createDataPartition(iris$Species, p = .75, list = FALSE)
train <- iris[divideData,]
test <- iris[-divideData,]


## build a model
set.seed(123)
knnmodel <- train(Species~., method = "knn", data = train, tuneLength = 7)
knnmodel
knnmodel$bestTune

set.seed(123)
knnmodel <- train(Species~., method = "knn", data = train, tuneGrid=expand.grid(k=c(5,11,21,25)))
knnmodel
knnmodel$bestTune


## LOOCV sample
trainControl <- trainControl(method = "LOOCV")
set.seed(123)
LOOCVmodel <- train(Species~., method = "knn", data = train, trControl = trainControl)
LOOCVmodel$bestTune
plot(LOOCVmodel)
LOOCVmodel
newpred <- predict(LOOCVmodel, newdata = test)
mean(newpred==test$Species) #=1, means perfect
confusionMatrix(newpred, test$Species)


## kfold model
trainControl <- trainControl(method = "cv", number = 10)
set.seed(123)
ldamodel <- train(Species ~., methods = "lda", data = train, trControl = trainControl)
ldamodel
newpred <- predict(ldamodel, mewdata = test)
mean(newpred==test$Species)
confusionMatrix(newpred, test$Species)






#### bootstrap sample
library(ISLR)
data("Auto")

trainControl <- trainControl(method = "boot", number = 100)
set.seed(1)
bootmodel <- train(mpg~horsepower, data =Auto, method = "lm", trControl = trainControl)
bootmodel


#install.packages("boot")
library(boot)
bootfn <- function(data, index){
  return(coef(lm(mpg~horsepower, data = data, subset = index)))
}
bootfn(Auto, 1:392)
bootcorr <- boot(Auto, bootfn, 1000)
summary(bootcorr)
plot(bootcorr)

summary(lm(mpg~horsepower, data = Auto))

#You can note that using the standard lm() function gives a slightly different 
#standard errors, because the linear model makes some assumptions about the data. 
#The bootstrap approach does not rely on any of these assumptions made by the 
#linear model, and so it is likely giving a more accurate estimate of the 
#coefficients standard errors than using the summary() function
