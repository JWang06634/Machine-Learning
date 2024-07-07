## Lesson 5 Sep 30

setwd("C:/Users/DELL/DESKTOP/WM/Machine Learning")

m <- matrix(1:9,3,3,byrow = TRUE)
m[1,]
m[,2]
m[-c(1:3),]
m[,-c(1:2)]

install.packages("MASS")
library(MASS)
data("Insurance")
head(Insurance)

first5rows <- Insurance[1:5,]
first5rows
first2cols <- Insurance[,1:2]
first2cols
AllButRow7 <- Insurance[-7,]
AllButRow7

l <- list(1234, "Machine Learning", 'BUAD5972')
l

l2 <- list(Employee = c("Sally", "Sue","Sybil"), Salary = c(4000,5000,6000))
l2
str(l2)
summary(l2)
class(l2)

str(Insurance)
summary(Insurance)
class(Insurance)

Auto <- read.table("Auto.data", header = TRUE, stringsAsFactors = TRUE)
fix(Auto)
str(Auto)

Auto2 <- read.csv("Auto.csv")
str(Auto2)
head(Auto2)
install.packages("readxl")
library(readxl)
Auto3 <- read_excel("Auto.xlsx")

fileurl <- "https://www.statlearning.com/s/Auto.csv"
download.file(fileurl, destfile = "./Auto.csv")
list.files("./")

## Lesson 6 Oct 1

install.packages("dplyr")
library(dplyr)
summary(Auto)
summarize(Auto, avg = mean(mpg))
Auto2 <- read.csv("Auto.csv")
class(Auto2$horsepower)
Question <- filter(Auto2, horsepower == '?')

Auto[,4] <- as.numeric(Auto2[,4])
Summary(Auto2)
Summary(Auto2$horsepower)

newdata <- select(Auto2, mpg, cylinders, displacement)
head(newdata)
newdata <- select(Auto2, 1:3)
head(newdata)

secondhalf <- select(Auto2, 4:6)

mergedata <- cbind(newdata, secondhalf)
head(mergedata)

install.packages("mltools")
library(mltools)

actuals <- c(1.0, 2.0, 9.5)
pred <- c(0.9, 2.1, 10.0)

sumsquares <- (1-.9)^2 + (2-2.1)^2 + (9.5-10)^2
mse <- sumsquares/length(pred)
mse

## Lesson 7 Oct 5
data('cars')
str(cars)
summary(cars)

lmcar <- lm(dist~speed, data = cars) 
summary(lmcar)
plot(dist~speed, data = cars)
abline(lmcar, col = 'red')

##make prediction
## regression equation: -17.5791 + 3.9324 * speed

speed <- 10
-17.5791 + 3.9324 * speed ##distance would be 21.7449 when speed is 10 

speed <- 15
-17.5791 + 3.9324 * speed

##construct a confidence level interval around our model
confint(lmcar, level = .95)
speed <- 10
-31.167850 +3.096964 * speed # lower bound of the CI
-3.990340 + 4.767853 * speed # upper bound of the CI
## CI [-.19821, 43.68819]
## so we are 95% confident that distance will be between -.198 and 43.688 when speed is 10

plot(lmcar)

par(mfrow = c(2,2))
plot(lmcar)


##Lesson 8 Oct. 7
Ad <- read.csv('Advertising.csv')

str(Ad)
summary(Ad)
pairs(Ad)

#make regression object
options(scipen = 999)
attach(Ad)
lmAd <- lm(sales~TV, data = Ad)
summary(lmAd)
plot(sales~TV, data = Ad)
abline(lmAd, col = 'purple')

coef(lmAd)

tv <- 150
7.03259355 + 0.04753664 * tv

confint(lmAd)
6.12971927 + 0.04223072 * tv # lower bound
7.93546783 + 0.05284256 * tv # upper bound
#Giving tv at 150, we estimate sales at an upper bound of 15861.85
## [12.46433, 15.86185] 95% CI given tv at 150

predict(lmAd, data.frame(TV = 150))

## normality of 
hist(lmAd$residuals)
mean(lmAd$residuals)
par(mfrow = c(2,2))

plot(lmAd)
# heteroskedasticity test 
install.packages('lmtest')

par(mfrow = c(1,1))
lmtest::bptest(lmAd) # Violation of homoskedasticity assumption
#The p-value more close 0, the more heteroskedasticity it is


##Corolla
corolla <- read.csv('corolla.csv')
summary(corolla)

#clean the dataset
corolla$Fuel_Type <- as.factor(corolla$Fuel_Type)
class(corolla$Fuel_Type)
attach(corolla)
Model <- as.factor(Model)
class(Model)

hist(Price)
plot(Price~Age)
plot(Price~KM)
plot(Price~Fuel_Type)

plot(Price~cc)
fix(corolla)
summary(corolla)

##

##Lesson 9 Oct 12

setwd("C:/Users/DELL/DESKTOP/WM/Machine Learning")
rm(list = ls())

Advertising <- read.csv("Advertising.csv")
str(Advertising)
Advertising <- Advertising[,-1]
cor(Advertising)

#Create lm
lmAdvert <- lm(sales~TV + radio + newspaper, data = Advertising)
lmAdvert <- lm(sales~., data = Advertising) # . same as the rest
summary(lmAdvert)

##Check Assumptions
install.packages("car")
car::vif(lmAdvert) # multicollinearity? any scores above 5
#no multicollinearity 

hist(lmAdvert$residuals) # Showing signs of left skewness, most likely violated the assumptions of normality of errors
mean(lmAdvert$residuals) # the mean is about 0, which means the skewness is not that bad
plot(lmAdvert) # first graph test linearity and homoskedasticity, we have 131 as an outlier

install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(Advertising[,c(1,2,4)], color = "hot pink", angle = 99, type = "h")

lmtest::bptest(lmAdvert) #pass-data is homoskedastic

library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("broom")
library(broom)

modelResults <- augment(lmAdvert) %>% mutate(index = 1:n()) # look at the 3 sd
ggplot(modelResults, aes(index, .std.resid)) + geom_point()
sum(abs(modelResults$.std.resid) > 3)
subset(modelResults$index, abs(modelResults$.std.resid) >3)





##Class 10 Oct 14
Advertising <- read.csv("Advertising.csv") 
corolla <- read.csv("Corolla.csv", stringsAsFactors = TRUE)
corolla <- corolla[, -c(1,2,6)]
str(corolla)
summary(corolla)
lmcorolla <- lm(Price~., data = corolla)
summary(lmcorolla)


lmcorollaforw <- step(lmcorolla, direction = "forward")
summary(lmcorollaforw)  #All significant

lmcorollaback <- step(lmcorolla, direction = "backward")
summary(lmcorollaback)

lmcorollastep <- step(lmcorolla, direction = "both")
summary(lmcorollastep)


car::vif(lmcorollaback)
lmtest::bptest(lmcorollaback)
hist(lmcorollaback$residuals)
mean(lmcorollaback$residuals)
plot(lmcorollaback)

library(broom)
library(tidyverse)

corolla <- corolla[-222, -c(1,2,6)] #Delete 222 then run bptest, hist(still left-skewed), mean, and plot
lmcorollaback <- step(lmcorolla, direction = "backward")
summary(lmcorollaback)

modelResults <- augment(lmcorollaback) %>% mutate(index = 1:n())
sum(abs(modelResults$.std))




#Class 11 Oct 21
####Midterm Review
#check str class summary works in advance
Advertising <- read.csv("Advertising.csv")
str(Advertising)
Advertising <- Advertising[,-1]

cor(Advertising)
options(scipen=999)
lmAdvert <- lm(sales~TV*radio, data = Advertising) #significant increase from add interaction(R^2 89->96)
summary(lmAdvert)

coef(lmAdvert)
#6.750220203 + 0.019101074*TV + 0.028860340*Radio + 0.001086495*TV*Radio

lmAdvert2 <- lm(sales~TV*newspaper, data = Advertising)
summary(lmAdvert2)

car::vif(lmAdvert)

hist(lmAdvert$residuals, breaks = 20)
lmtest::bptest(lmAdvert) #heterosdacestic
plot(lmAdvert) #QQ plot: 131 outlier


##########Corolla##########
corolla <- read.csv('corolla.csv')
str(corolla)
lmCorolla <- lm(Price~Age*HP, data = corolla)
summary(lmCorolla)

car::vif(lmCorolla)
hist(lmCorolla$residuals, breaks= 20) #slight right-skewed
plot(lmCorolla)
lmtest::bptest(lmCorolla)#heterosdacestic

cancer <- read.csv('cancer.csv')
str(cancer)
cancer <- cancer[,-c(9,13)]
#check on nas
#cancer <- na.omit(cancer)
cor(cancer)

lmCancer <- lm(TARGET_deathRate ~ ., data = cancer)
summary(lmCancer)

lmCancer <- step(lm(TARGET_deathRate ~ . -PctBachDeg18_24-PctPrivateCoverageAlone-avgDeathsPerYear, data = cancer), direction = "forward")
summary(lmCancer)
car::vif(lmCancer) # PctBachDeg18_24 high, so remove

cancer <- read.csv('cancer.csv')
lmCancerInter <- lm(TARGET_deathRate~incidenceRate*PercentMarried, data = cancer)
summary(lmCancerInter)
