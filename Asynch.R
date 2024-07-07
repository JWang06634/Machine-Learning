####Modle 1 - Life Expectancy Model####

Perceptions <- read.csv("worldPerceptions.csv", stringsAsFactors = T) #Country doesn't need to be factor, but Region does.
Perceptions <- Perceptions[,-1]
str(Perceptions)

plot(Perceptions$Region) #not very visible
plot(LifeExpectancy~Region, data=Perceptions) #Dot is outlier


##Make lm for qualitative variable
categorical <- lm(LifeExpectancy~Region, data = Perceptions)
summary(categorical)
#Reference category is central and eastern europe

categorical$coefficients[1]+categorical$coefficients[3] # 71.09485 

##Make a lm for one qualitative variable andone continuous variable
catcont <- lm(LifeExpectancy~GDPpercapita + Region, data = Perceptions)
summary(catcont)
#higher R, less significant

GDPEastAsia <- 12
catcont$coefficients[1]
catcont$coefficients[2]
GDPEastAsia
catcont$coefficients[4]

catcont$coefficients[1] + catcont$coefficients[2]*GDPEastAsia + catcont$coefficients[4] # 75.53947 





####Modle 2 - Salaries####
salaries <- read.csv("Salaries.csv", stringsAsFactors = T)
str(salaries)

lmAge <- lm(Salary~Age, data = salaries)
summary(lmAge)
plot(Salary~Age, data = salaries)
abline(lmAge)

lmMBA <- lm(Salary~MBA, data = salaries)
summary(lmMBA) #Not significant, don;t care R
plot(Salary~MBA, data = salaries)

lmSalary <- lm(Salary~Age*MBA, data = salaries)
summary(lmSalary)

library(ggplot2)
MBA <- salaries$MBA
ggplot(salaries, aes(Age, Salary, color=MBA)) + geom_point() + geom_smooth(method = "lm")



####Model 3 - ML Life Expectancy####

lmML <- lm(LifeExpectancy~.-Generosity-FreedomToMakeLifeChoices, data = Perceptions) #Region is strong predictor by seeing the R-squared without it
summary(lmML)
coefficients(lmML)
cor(Perceptions[,2:7]) 
#Generosity quite low, so take out
#Remove FreedomToMakeLifeChoices, since not significant


catcont <- lm(LifeExpectancy~GDPpercapita + Region, data = Perceptions)
car::vif(catcont)
lmtest::bptest(catcont) #hetero
#Both have no issue in VIF, consistent, no multicollinearity
car::vif(lmML)
lmtest::bptest(lmML) #homoskedastic
hist(lmML$residuals) #fairly normal

hist(Perceptions$LifeExpectancy) #Left/negative - skewed
hist(Perceptions$GDPpercapita) #Left-skewed

catcont <- lm(log(LifeExpectancy)~log(GDPpercapita) + Region, data = Perceptions) #still hetero


LifeExpectancy = 45.1522336 - 2.1452918*RegionCommonwealth of Independent States + 1.7530185*RegionEast Asia -0.1310318*RegionLatin America and Caribbean
-2.1244059*RegionMiddle East and North Africa + 0.4417622*RegionNorth America and ANZ -1.9099625*RegionSouth Asia -2.4311070*RegionSoutheast Asia 
-7.8213982*RegionSub-Saharan Africa + 1.7022135*RegionWestern Europe + 2.0474798*GDPpercapita + 6.8351268*Socialsupport -4.0030945*PerceptionsOfCorruption




