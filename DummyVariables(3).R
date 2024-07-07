##This R file shows instructions on how to code dummy variables to calculate the correlation given a model where you have transformed Y using the log function
#and used a categorical variable. 

##setwd("C:/Users/PSG/Desktop/5072F21/DataSets")

##Here, I read in the dataset. 
Perceptions <- read.csv("worldPerceptions.csv", stringsAsFactors = TRUE)

##I processed one categorical variable with a log(Y) model
##This is the way we learned in class with the exponential log model (log Y) as regressed on Region (X)
lmEasyWay <- lm(log(LifeExpectancy)~Region, data=Perceptions)
str(Perceptions)
summary(lmEasyWay) ##Adjusted R-squared:  0.7392 

###########next I want to show you how to code dummy variables manually - in the step above, they are coded automatically for us. 

##But first, let's look up the names of the categories of the X variable
table(Perceptions$Region)

##Next, manually make the dummy variables using a series of ifelse statements
##Be sure to use the category name exactly as it is written in the column. And we need k-1 number of dummy variables.
##Therefore, we don't need a dummy for the first category (Central and Eastern Europe) because it will be the referent category. 
CatCIS <- ifelse(Perceptions$Region=="Commonwealth of Independent States", 1,0)
CatEA <- ifelse(Perceptions$Region=="East Asia",1,0)
CatLAC <- ifelse(Perceptions$Region=="Latin America and Caribbean",1,0)
CatENA <- ifelse(Perceptions$Region=="Middle East and North Africa", 1,0)
CatNA <- ifelse(Perceptions$Region=="North America and ANZ", 1,0)
CatSA <- ifelse(Perceptions$Region=="South Asia",1,0)
CatSEA <- ifelse(Perceptions$Region=="Southeast Asia",1,0)








CatAf <- ifelse(Perceptions$Region=="Sub-Saharan Africa", 1,0)
CatWE <- ifelse(Perceptions$Region=="Western Europe", 1,0)
##note - there are a lot of categories - so this is not the ideal way to do this - time consuming! There are functions to manually do this for you that you can find, 
##but I taught you the ifelse statement in class so I used it here. 


##After you code the variables, use the cbind function to bring the dummy variables into the dataset 
##This is not required, but extremely helpful in making sure you have consistent lengths of the vectors, which can be tricky when also deleting observations. 
Perceptions <- cbind(Perceptions, CatCIS, CatEA, CatLAC, CatENA,CatNA,CatSA,CatSEA,CatAf,CatWE)

##make a comparitative model using the dummy variables as opposed to the one variable Region.
##This is a duplicate model using manually coded dummy variables directly compared to the linear regression model above - note the same adjusted R-squared value  
lmCat <- lm(log(LifeExpectancy)~CatCIS + CatEA + CatLAC + CatENA + CatNA + CatSA + CatSEA + CatAf + CatWE, data=Case3)
summary(lmCat) ##Adjusted R-squared:  0.7392 

##k number of categories means k-1 dummy variables, also means lots of coefficients. 
Sig <- sigma(lmCat); Sig ##rse
b0 <- lmCat$coefficients[1]; b0 ##intercept --- remember Central and Eastern Europe (category 1) is in the intercept - known as the referent category. 
b1 <- lmCat$coefficients[2]; b1 ## RegionCommonwealth of Independent States
b2 <- lmCat$coefficients[3];b2 ##RegionEast Asia
b3 <- lmCat$coefficients[4];b3 #RegionLatin America and Caribbean  
b4 <- lmCat$coefficients[5];b4 #RegionMiddle East and North Africa
b5 <- lmCat$coefficients[6];b5  ##RegionNorth America and ANZ 
b6 <- lmCat$coefficients[7];b6  ##RegionSouth Asia
b7 <- lmCat$coefficients[8];b7 ##RegionSub-Saharan Africa 
b8 <- lmCat$coefficients[9];b8 ##RegionSub-Saharan Africa 
b9 <- lmCat$coefficients[10];b9 ##RegionWestern Europe

##Calculating y-hats because we took the log of Y. 
yhat <- exp(b0 + b1*CatCIS + b2*CatEA + b3*CatLAC + b4*CatENA + b5*CatNA + b6*CatSA + b7*CatSEA + b8*CatAf + b9*CatWE +Sig^2/2)
head(yhat) ##just doing a quick check for values - they are the same because the region had the same value for the first 6 observations 
##and we only had one predictor variable X in our model. 

##Finally, calculate the updated correlation and square it to get a comparative R-squared
cor(yhat, Perceptions$LifeExpectancy)^2 ##0.7644627
#adjusted R-Square adjusted from 0.7392 to 0.7644627 - both good


##I hope you enjoyed my tutorial on making and using dummy variables. Best of luck on Case 3 and 
##remember, you only need to manually code dummy variables when you take the log of Y and have a categorical X variable. 
