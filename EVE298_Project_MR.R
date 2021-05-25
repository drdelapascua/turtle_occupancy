#### EVE 298: Group Project ####
# Authors: Melissa, Sidney, Christofer, Danielle
# Date: Spring Quarter 2021

# set working directory and read in the data
library(nlme)
library(MASS)
library(lme4)
library(AED)
library(glmmML)
library(pscl)
setwd("C:/Users/mkril/Desktop/EVE298")

wpt <- read.csv("data/WPT_edited.csv")

# Note that this data has been modified to better fit the skills we learned in class.  
# Date is replaced with the month sampling took place. Values for air temperature, wind speed, water temperature, salinity, bank cover, and  flow status are averaged for the three visits.  Number is now the sum of the turtles across all three visits.  In the column "present" there is a one if turtles were observed on any of the three visits and a zero if no turtles were observed.

head(wpt)
str(wpt)

wpt$fhabitat <- as.factor(wpt$habitat)
wpt$fmgmt <- as.factor(wpt$mgmt)
wpt$fmonth <- as.factor(wpt$month)
wpt$logsalinity <- log(wpt$salinity)
str(wpt)

# > data exploration ----

# response variable - number of wpt

hist(wpt$number,
     main="Histogram of Western Pond Turtles Observed", 
     xlab="Number of Turtles",
     col="green")
dotchart(wpt$number, color = wpt$fhabitat)

wpt$lognumber <- log(wpt$number)
hist(wpt$lognumber,
     main="Histogram of Log Western Pond Turtles Observed", 
     xlab="Log Number of Turtles",
     col="green")

# predictor variables - logsalinity, habitat type, air temp, water temperature, wind speed, flow status, mgmt, basking

plot(salinity ~ number, data = wpt)
hist(wpt$salinity,
     main="Histogram of Salinities", 
     xlab="Salinity (ppt)",
     col="green") #more data points at lower (?) salinities, only a few at higher salinities - log transform? Will stretch out the big ones
dotchart(wpt$salinity)

hist(wpt$logsalinity,
     main="Histogram of Log Salinities", 
     xlab="Log Salinity",
     col="green") #huzzah! we should use the log transformed salinity

plot(airtemp ~ number, data = wpt) #funnel shaped - is this good??
hist(wpt$airtemp) # probably okay?
dotchart(wpt$airtemp) 

plot(watertemp ~ number, data = wpt) # funneled
hist(wpt$watertemp) #slight skew, but mostly normal. a few outliers that have high temps
dotchart(wpt$watertemp)

plot(avgwind ~ number , data = wpt) #funneled
hist(wpt$avgwind)#skewed, log trans
dotchart(wpt$avgwind)
wpt$logwind <- log(wpt$avgwind)
hist(wpt$logwind)
plot(logwind ~ number, data = wpt) #do not use the log scale, its worse

hist(wpt$flowstatus) #up in the corner
dotchart(wpt$flowstatus)

hist(wpt$baskingarea)
dotchart(wpt$baskingarea)

# autocorrelation
plot(watertemp ~ logsalinity, data = wpt) #uncorrelated!
plot(watertemp ~ airtemp, data = wpt) #correlation, worth a ggplot 
plot(watertemp ~ maxwind, data = wpt) #uncorr?
plot(logsalinity ~ airtemp, data = wpt)#uncorr

boxplot(wpt$number ~ wpt$fhabitat)
boxplot(wpt$logsalinity ~ wpt$fhabitat) 

# > preliminary linear model, gaussian distribution ----

mod1 <- lm(number ~ logsalinity, data = wpt)
summary(mod1)
plot(mod1) # we have a trumpet which is bad

#mod2 <- lm(lognumber ~ logsalinity, data = wpt) #lognumber doesn't work for some reason
#summary(mod2)
#plot(mod2) # it looks worse

mod3 <- lm(number ~ logsalinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, data = wpt)
summary(mod3)
plot(mod3) #the trumpet is still problematic




# > random structure ----

mod.random <- lme(number ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, random = ~1|surveypoint, method = "ML",data = wpt)
summary(mod.random)

# > poisson distribution ----
mod.poisson1 <- glm(number ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, family = poisson, data = wpt)

summary(mod.poisson1) # residual deviance is way higher than the DF, so our model is over dispersed
plot(mod.poisson1)#residuals have a pattern

# > negative binomial ----

mod.nb <- glm.nb(number ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, link = "log", data = wpt)
plot(mod.nb, 1)

# > binomial with presence data ----

mod.bino <- glm(present ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, family = binomial, data = wpt)
summary(mod.bino)
plot(mod.bino)

# > adding a random effect for site, using binomial ----

mod.lmer <- glmer(present ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt + (1|surveypoint), family = binomial, data = wpt)


summary(mod.lmer)
plot(mod.lmer)
str(wpt)

# > model validation ----

AIC(mod2, mod.poisson)

# > ZIP model ----
f1 <- formula(number ~ salinity + fhabitat + fmgmt) 
f2 <- formula(lognumber ~ logsalinity + fhabitat + fmgmt)
f3 <- formula(number ~ logsalinity + fhabitat + fmgmt)
f4 <- formula(lognumber ~ salinity + fhabitat + fmgmt)
Zip1 <- zeroinfl(f1, dist = "poisson", data = wpt)
Zip2 <- zeroinfl(f2, dist = "poisson", data = wpt)
Zip3 <- zeroinfl(f3, dist = "poisson", data = wpt)
Zip4 <- zeroinfl(f4, dist = "poisson", data = wpt)

AIC(Zip1, Zip3) # salinity should be log transformed - better AIC

summary()

summary(Zip3)

summary(Zip2)
#### Presence/Absence Data ####

# > logistic regression ----

# > model diagnostics ----

# 1) variance homogeneity
plot(mod3)

# the first plot shows fitted vs. residuals, want to see a "starry night", in this case we got a trumpet plot, variance high at high values, this indicates a log linear relationship

# 2) normality of errors
plot(mod3, 2) # creates a QQ plot, want all data points to fall on theline, indicates normality
hist(resid(mod3)) # ours is skewed in this case

# 3) variance homogeneity
plot(resid(mod3) ~ wpt$salinity) #the weirdness comes from length
boxplot(resid(mod3) ~ wpt$salinity)

# 4) plot predicted values

plot(number ~ salinity, data = wpt)

# now add predicted lines for each month

#plot(AFD ~ LENGTH, data = clams, type = "n")
# now adding in raw data for each month
#points(AFD[fMONTH == "2"] ~ LENGTH[fMONTH == "2"], data = clams, col = "red")

#### Questions ####

# how to deal with uneven sampling
# how to deal with repeat visits
# some survey points were visited twice at different times of the year
# type of model - gaussian vs. poisson distribution
# what should be a random effect
# what predictors should be included, is there colinearity between predictors
# managed vs. tidal, do we care about habitat types within these major categories?
