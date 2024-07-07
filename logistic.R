library(ISLR)
data("Default")
str(Default)
library(ggplot2)

ggplot(Default, aes(balance, income, group = default)) + geom_point(aes(shape = default, color = default))
attach(Default)

table(Default$student, Default$default)


#install.packages("caret")
library(caret)
set.seed(1)
divideData <- createDataPartition(Default$default,p = .8, list = FALSE)
train <- Default[divideData,]
test <- Default[-divideData,]

logisticreg <- glm(default~balance, family = binomial, data = train)
options(scipen = 999)
summary(logisticreg)

#Moves log odds to odds
exp(coef(logisticreg))

#Move odds to probability
b0 <- logisticreg$coefficients[1]; b0
b1 <- logisticreg$coefficients[2]; b1
probdefault <- exp(b0 + b1)/(1+exp(b0+b1)); probdefault

log(probdefault/(1-probdefault))

b0+b1

prob <- ifelse(Default$default=="Yes",1,0)
ggplot(Default, aes(balance, prob)) + geom_point(alpha = 1) + geom_smooth(method = "glm", method.args=list(family=binomial))


#Make prediction
x <- 1500 #setting value for balance
predict(logisticreg, data.frame(balance=x), type = "response")
exp(b0+b1*x)/(1+exp(b0+b1*x))


####11.4####
## Calculate test error and accuracy rate
probs <- predict(logisticreg, test, type = "response")
pred <- ifelse(probs > .5, "Yes","No")
table(pred, test$default)

mean(pred  != test$default) ###test error rate 0.02901451
(6+52)/1999
mean(pred==test$default) #0.9709855 test accuracy rate
##1-test error rate


### Training accuracy and error rate
probs <- predict(logisticreg, train, type = "response")
pred <- ifelse(probs > .5, "Yes","No")
table(pred, train$default)
mean(pred != train$default) #training error rate 0.02712161
mean(pred == train$default) #training accuracy rate 0.9728784

### multiple logistic regression example
multilog <- glm(default~student + balance, family = binomial, data = train)
summary(multilog)

exp(multilog$coefficients)

probs <- predict(multilog, test, type = "response")
pred <- ifelse(probs > .5, "Yes", "No")
table(pred, test$default)

mean(pred!=test$default) #testing error rate 0.02751376
mean(pred==test$default) #testing accuracy rate 0.9724862

### assumptions for logistic regression
# assumption 1 linearity of the logit (assumption 1 violates as non-linearity)
attach(Default)
plot(balance, log(balance))

interaction <- balance*log(balance)
checkinteract <- glm(default~interaction, family = binomial, data = default)
summary(checkinteract)

# assumption 2 absence of multicollinearity (scores are low, no multi)
car::vif(multilog)

# assumption 3 lack of strong influencial of outliers
library(broom)
library(tidyverse)
library(ggplot2)
modelResults <- augment(multilog) %>% mutate(index = 1:n())
ggplot(modelResults, aes(index, .std.resid)) + geom_point(aes(color = default))
ggplot(modelResults, aes(index, .cooksd)) + geom_point(aes(color = default))

### Breast Cancer
#install.packages("mlbench")
library(mlbench)
data("BreastCancer")
str(BreastCancer)

BreastCancer <- BreastCancer[,-1]
BreastCancer <- na.omit(BreastCancer)

set.seed(123)
divideData <- createDataPartition(BreastCancer$Class, p = .7, list = FALSE)
train <- BreastCancer[divideData,]
test <- BreastCancer[-divideData,]

model <- glm(Class~., data = train, family = binomial)
summary(model)

#Test accuracy and error rate
prob <- predict(model, test, type = "response")
pred <- ifelse(prob > .5, "malignant", "benign")
mean(pred == test$Class) # test accuracy rate 0.9264706
table(pred, test$Class)
mean(pred!=test$Class) # test error rate 0.07352941
