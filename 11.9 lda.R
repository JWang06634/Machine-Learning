#install.packages("rattle")
library(rattle)

data(wine)
str(wine)
attach(wine)

library(MASS) #lda need load every time
wine.lda <- lda(Type~., data = wine)
wine.lda
# linear discrimination function --- LD1 -0.403399781 * Alcohol + 0.165254596 * Malic ...

## Making predictions
options(scipen=999)
wine.lda.values <- predict(wine.lda)
wine.lda.values # Class, Posterior probabilities

## Create histograms
ldahist(data = wine.lda.values$x[,1], g = Type)
ldahist(data = wine.lda.values$x[,2], g = Type)

library(ggplot2)
ggplot(wine, aes(wine.lda.values$x[,1],wine.lda.values$x[,2])) + geom_point(aes(color = Type))

       
## Split the data into test and train
library(caret)
library(dplyr)
set.seed(1)
divideData <- createDataPartition(Type, p=.8, list = FALSE)
train <- wine[divideData,]
test <- wine[-divideData,]       

## Centering and Scaling
preprocessing <- train %>% preProcess(method = c("center", "scale"))
traintransformed <- preprocessing %>% predict(train)
testtransformed <- preprocessing %>% predict(test)

## Make our model
model <- lda(Type~., data = traintransformed)
plot(model)

## Make prediction
predictions <- model %>% predict(testtransformed)
names(predictions)
mean(predictions$class==testtransformed$Type)
table(predictions$class==testtransformed$Type)

###
ldaforgraph <- cbind(traintransformed, predict(model)$x)
ggplot(ldaforgraph, aes(LD1, LD2)) + geom_point(aes(color=Type))


### No centering and scaling
model <- lda(Type~., data = train)

predictions <- model %>% predict(test)
mean(predictions$class==test$Type)
#when the prediction is low, centering and scaling usually helps