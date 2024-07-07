####Spam
install.packages("kernlab")
library(kernlab)
library(caret)
data(spam)

class(spam$type)

##divide the data
set.seed(111)
divideData <- createDataPartition(spam$type, p=.3, list = FALSE)
train <- spam[divideData,]
test <- spam[-divideData,]

### make our knn model
spamfit <- train(type~., data=train, method = "knn", preProcess=c("center", "scale"))
spamfit$bestTune #what k is the most approparate k
plot(spamfit) #5,7,9 ##expecting testing group lower than training group

## make our predictions
predknn <- predict(spamfit, newdata = test)

## produce accuracy rates and other accuracy info
confusionMatrix(predknn, test$type)

A <- 1837; B <- 279; C <- 114; D <- 990
sensitivity <- A/(A+C); sensitivity # pretty well
specificity <- D/(B+D); specificity # not well
Precision <- A/(A+B); Precision #=Pos pred value, predict well
NegpredValue <- D/(C+D); NegpredValue 
Prevalence <- (A+C)/(A+B+C+D); Prevalence
detectionRate <- A/(A+B+C+D); detectionRate
DetectionPrevalence <- (A+B)/(A+B+C+D); DetectionPrevalence
balanceAccuracy <- (sensitivity+specificity)/2; balanceAccuracy


##logistic regression
## make my model
spamlogisticmodel <- glm(type~., data = train, family = binomial)

## make my predictions
spamprob <- predict(spamlogisticmodel, test, type="response")
spampred <- ifelse(spamprob > .5, "spam", "nonspam")

## produce accuracy rates
mean(spampred == test$type) #0.9214286

##interpreting coeff
summary(spamlogisticmodel)
exp(spamlogisticmodel$coefficients)


options(scipen = 9)
## interpreting coeff
summary(spamlogisticmodel)
exp(spamlogisticmodel$coefficients)
