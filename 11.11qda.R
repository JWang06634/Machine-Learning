library(rattle)
library(MASS)
library(tidyverse)
library(caret)
data(wine)
attach(wine)

###Separate the data
set.seed(1000)
divideData <- Type %>% createDataPartition(p=.8, list = FALSE)
train <- wine[divideData,]
test <- wine[-divideData,]

###Centering and scale
preprocessing <- train %>% preProcess(method=c("center","scale"))
traintransformed <- preprocessing %>% predict(train)
testtransformed <- preprocessing %>% predict(test)

###Make a model --- qda
model <- qda(Type~., data = traintransformed)
model

###Make predictions
predictions <- model %>% predict(testtransformed)
mean(predictions$class == testtransformed$Type)

##
phoneme <- read.csv("phoneme.csv", stringsAsFactors = TRUE)

phoneme <- phoneme[,-c(1,259)]
str(phoneme)
class(phoneme$g)

attach(phoneme)

###Dividing Data
set.seed(100)
divideData <- createDataPartition(g, p=.8, list = FALSE)
train <- phoneme[divideData,]
test <- phoneme[-divideData,]

###Center and Scale
preprocessing <- train %>% preProcess(method = c("center", "scale"))
traintransformed <- preprocessing %>% predict(train)
testtransformed <- preprocessing %>% predict(test)

###Make a lda model 
ldamodel <- lda(g~., data = traintransformed)
ldamodel

###Calculate accuracy rate
predictions <- ldamodel %>% predict(testtransformed)
mean(predictions$class == testtransformed$g)
##Given a proportion of .2 and a set seed of 100, our accuracy rate is 0.9098474
##Given a proportion of .8 and a set seed of 100, our accuracy rate is 0.9344444
table(predictions$class, testtransformed$g)

###Graph this data
library(ggplot2)
ldaforgraph <- cbind(traintransformed, predict(ldamodel)$x)

ggplot(ldaforgraph, aes(LD1,LD2)) + geom_point(aes(color = g, shape = g))
ggplot(ldaforgraph, aes(LD1,LD3)) + geom_point(aes(color = g, shape = g))
ggplot(ldaforgraph, aes(LD1,LD4)) + geom_point(aes(color = g, shape = g))
ggplot(ldaforgraph, aes(LD2,LD3)) + geom_point(aes(color = g, shape = g))
ggplot(ldaforgraph, aes(LD2,LD4)) + geom_point(aes(color = g, shape = g))
ggplot(ldaforgraph, aes(LD3,LD4)) + geom_point(aes(color = g, shape = g))

###qda
qdamodel <- qda(g~., data = traintransformed)
qdamodel

###Make predictions and calculate accuracy
predictions <- qdamodel %>% predict(testtransformed)
mean(predictions$class == testtransformed$g)
##  p = .8 set.seed(100) - test accuracy [1] 0.8622222

table(predictions$class, testtransformed$g)
