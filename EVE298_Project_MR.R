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

wpt1 <- read.csv("data/WPT_Occupancy_R.csv")

head(wpt1)
str(wpt1)
wpt1$fhabitat <- as.factor(wpt1$habitat)
str(wpt1)

# > lets do some data exploration! ----

plot(number ~ salinity, data = wpt1)

# response variable - number of wpt

hist(wpt1$number)
dotchart(wpt1$number, color = wpt1$fhabitat)

# predictor variables - salinity, habitat type, air temp, water temperature, wind speed, flow status

hist(wpt1$salinity)
dotchart(wpt1$salinity)

hist(wpt1$airtemp)
dotchart(wpt1$airtemp) 

hist(wpt1$watertemp)
dotchart(wpt1$watertemp)

hist(wpt1$avgwind)
dotchart(wpt1$avgwind)

hist(wpt1$flowstatus)
dotchart(wpt1$flowstatus)

#this is an observational data set, we should check to see if anything is co-linear
#pairs(wpt)
#pairs(loyn[,2:5]) #subsetting dataframes [rows, columns]
#pairs(loyn[,c(2,4,6)])
#pairs(loyn[,c("AREA", "DIST", "ABUND", "GRAZE")])


#looking at the effect of habitat, but also looking at the spread at different habitat levels, may have to check homogeneity of variance closely
boxplot(wpt1$number ~ wpt1$fhabitat)
boxplot(wpt1$salinity ~ wpt1$fhabitat) 

#### Data that is Summed ####

wpt <- read.csv("data/WPT_edited.csv")

head(wpt)
str(wpt)

hist(wpt$salinity)
hist(wpt$logsalinity)
wpt$fhabitat <- as.factor(wpt$habitat)
wpt$fmgmt <- as.factor(wpt$mgmt)
wpt$fmonth <- as.factor(wpt$month)
wpt$logsalinity <- log(wpt$salinity)
str(wpt)

# response variable - number of wpt

hist(wpt$number)
wpt$lognumber <- log(wpt$number)
hist(wpt$lognumber)
dotchart(wpt$number, color = wpt$fhabitat)

# predictor variables - salinity, habitat type, air temp, water temperature, wind speed, flow status, mgmt, basking

hist(wpt$salinity)
dotchart(wpt$salinity)

hist(wpt$airtemp)
dotchart(wpt$airtemp) 

hist(wpt$watertemp)
dotchart(wpt$watertemp)

hist(wpt$avgwind)
dotchart(wpt$avgwind)

hist(wpt$flowstatus)
dotchart(wpt$flowstatus)

hist(wpt$baskingarea)
dotchart(wpt$baskingarea)

boxplot(wpt$number ~ wpt$fhabitat)
boxplot(wpt$salinity ~ wpt$fhabitat) 

# > preliminary linear model, gaussian distribution ----

mod1 <- lm(number ~ salinity, data = wpt)
summary(mod1)
plot(mod1) # we have a trumpet which is bad

mod2 <- lm((sqrt(number)) ~ salinity, data = wpt)
summary(mod2)
plot(mod2) # it looks worse

mod3 <- lm(number ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, data = wpt)
summary(mod3)
plot(mod3)


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

summary(Zip3)

summary(Zip2)
#### Presence/Absence Data ####

# > logistic regression ----


#### Questions ####

# how to deal with uneven sampling
# how to deal with repeat visits
# some survey points were visited twice at different times of the year
# type of model - gaussian vs. poisson distribution
# what should be a random effect
# what predictors should be included, is there colinearity between predictors
# managed vs. tidal, do we care about habitat types within these major categories?
