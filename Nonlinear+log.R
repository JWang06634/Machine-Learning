####Nonlinear
beverages <- read.csv("BeverageSales.csv")
attach(beverages)

str(beverages) #both numerical 
summary(beverages)

plot(Sales~Temperature)

library(ggplot2)
ggplot(beverages, aes(Temperature, Sales)) + geom_point() + geom_smooth(method = 'lm') + geom_smooth(method = 'loess', color = 'red')
#first geom_smooth - straight line; second geom_smooth - curve

hist(Sales) #Positive skewed (right-skewed)
hist(Temperature, breaks = 5)


lmBev <- lm(Sales~Temperature)     
summary(lmBev) #Pretty good, due to high Adjusted R-squared and significant direct effect
# Residual standard error: 1013 on 40 degrees of freedom
# Multiple R-squared:  0.8523,	Adjusted R-squared:  0.8486 
## Ctrl + Shift + C: comments a couple of line at once
cor(lmBev$fitted.values, Sales)^2 #0.8523335

# Wants to see RSE goes down when we add a term

lmBevQuad <- lm(Sales~Temperature + I(Temperature^2))
summary(lmBevQuad)
# Residual standard error: 618 on 39 degrees of freedom
# Multiple R-squared:  0.9464,	Adjusted R-squared:  0.9436 
##Better

lmBevCUbic <- lm(Sales~poly(Temperature,3))
summary(lmBevCUbic)
# Residual standard error: 607.3 on 38 degrees of freedom
# Multiple R-squared:  0.9496,	Adjusted R-squared:  0.9456 
##Cubic is not a good model


anova(lmBev, lmBevQuad) #P-value is significant. Insight that quadratic effects was significantly different than just the linear model
anova(lmBevQuad, lmBevCUbic) #Not significant. F-stat is close to 0 and P-value is quite high. Confirm!


####Log Transformations

lmBev <- lm(Sales~Temperature)     
summary(lmBev)
# Residual standard error: 1013 on 40 degrees of freedom
# Multiple R-squared:  0.8523,	Adjusted R-squared:  0.8486 
cor(lmBev$fitted.values, Sales)^2
## [1] 0.8523335 

lmBevlogX <- lm(Sales~log(Temperature))
summary(lmBevlogX)
# Residual standard error: 1080 on 40 degrees of freedom
# Multiple R-squared:  0.8322,	Adjusted R-squared:  0.828 
##Not really help from what we do earlier
plot(Sales~log(Temperature)) #Didn't linearlize like we wanted to
b0 <- lmBevlogX$coefficients[1]; b0
## (Intercept) 
##   -153792.3
b1 <- lmBevlogX$coefficients[2]; b1
## log(Temperature) 
##         35134.23
yhat <- (b0 + b1*log(Temperature))
logXcor <- cor(yhat, Sales)^2; logXcor
## [1] 0.8321751


lmBevloglog <- lm(log(Sales)~log(Temperature))
summary(lmBevloglog)
plot(log(Sales)~log(Temperature)) #Make it as straight line
## It helps
Sig <- sigma(lmBevloglog); Sig ##rse
## [1] 0.2107096
b0 <- lmBevloglog$coefficients[1]; b0
## (Intercept) 
##    -59.0117
b1 <- lmBevloglog$coefficients[2]; b1
## log(Temperature) 
##     14.9195
yhat <- exp(b0 + b1*log(Temperature) + Sig^2/2)
loglogcor <- cor(yhat, Sales)^2; loglogcor
## [1] 0.9435362 #not change much


lmBevlogY <- lm(log(Sales)~Temperature)
summary(lmBevlogY)
plot(log(Sales)~Temperature)
##Same as log-log

##Which one is better? Depends on the which one is easy to explain
#For logY model, 0.17*100 means 17% change as X increases by 1 unit

##Between quadratic and logY, prefer logY since quadratic is more difficult to interpret
#Explaining about the same amount of variance, but have an additional term that you're estimating that additional slope parameter

anova(lmBevQuad,lmBevlogY) #Cannot being compared, needs Y to Y / logY to logY

cor(lmBevlogY$fitted.values, Sales)^2
## [1] 0.8523335 
Sig <- sigma(lmBevloglog); Sig ##rse
## [1] 0.2107096
b0 <- lmBevlogY$coefficients[1]; b0
## (Intercept) 
##   -7.342357
b1 <- lmBevlogY$coefficients[2]; b1
## Temperature 
##   0.1715675
yhat <- exp(b0 + b1*Temperature + Sig^2/2)
logycor <- cor(yhat, Sales)^2; logycor
## [1] 0.9373075




