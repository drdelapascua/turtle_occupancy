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
     col="green")
dotchart(wpt$salinity)

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
boxplot(wpt$salinity ~ wpt$fhabitat) 

# > preliminary linear model, gaussian distribution ----

#we know this is count data, but we can try to use a linear model to investigate our question and see if this type of model explains our data
mod1 <- lm(number ~ salinity, data = wpt)
summary(mod1)
plot(mod1) # we have a trumpet which is bad

#mod2 <- lm(lognumber ~ salinity, data = wpt) #lognumber doesn't work for some reason
#summary(mod2)
#plot(mod2) # it looks worse if you do a sqrt transformation on the number of turtles

mod3 <- lm(number ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, data = wpt)
summary(mod3)
plot(mod3) #the trumpet is still problematic, and adding all the predictors doesn't make the model any better

# > random structure ----

#maybe adding random structure will help improve the model

mod.random <- lme(number ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, random = ~1|surveypoint, method = "ML",data = wpt)
summary(mod.random)

AIC(mod1, mod3, mod.random) # adding in in a random effect improves the model, but the poisson distribution will still be better at fitting this type of data

# > poisson distribution ----
mod.poisson1 <- glm(number ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, family = poisson, data = wpt)

summary(mod.poisson1) # the residual deviance is way higher than the DF, so our model is over dispersed

plot(mod.poisson1) #residuals still have a trumpet pattern

#can we add in a random effect, while also using the poisson distribution?
mod.poisson2 <- glmer(number ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt + (1|surveypoint), family = poisson, data = wpt) #this model failed to converge

mod.poisson3 <- glmer(number ~ salinity + (1|surveypoint), family = poisson, data = wpt)
summary(mod.poisson3) #this worked, but the deviance is still really high 
plot(mod.poisson3) #fitted vs. residuals does not look great

#maybe the negative bionomial distribution will work out better...

# > negative binomial ----

mod.nb <- glm.nb(number ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, link = "log", data = wpt)
plot(mod.nb, 1) #this plot still looks awful, so something is going on

# > ZIP model ----

# we did some exploration of ZIP models

f1 <- formula(number ~ salinity + fhabitat + fmgmt) 
#f2 <- formula(lognumber ~ salinity + fhabitat + fmgmt) #log transforming the data doesn't work for these models
f3 <- formula(number ~ salinity + fhabitat + fmgmt)
#f4 <- formula(lognumber ~ salinity + fhabitat + fmgmt) #log transforming the data doesn't work for these models
Zip1 <- zeroinfl(f1, dist = "poisson", data = wpt)
#Zip2 <- zeroinfl(f2, dist = "poisson", data = wpt)
Zip3 <- zeroinfl(f3, dist = "poisson", data = wpt)
#Zip4 <- zeroinfl(f4, dist = "poisson", data = wpt)

AIC(Zip1, Zip3) # salinity should be log transformed - better AIC

# trying a ZIP model with all of our predictor variables, and taking the different predictors out to see which explanatory variables can be dropped

f5 <- formula(number ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt)
Zip5 <- zeroinfl(f5, dist = "poisson", data = wpt) # I get an error

f6 <- formula(number ~ salinity + maxwind + watertemp + fhabitat + fmgmt)
Zip6 <- zeroinfl(f6, dist = "poisson", data = wpt) # taking out month and airtemp fixes the above error????

f7 <- formula(number ~ salinity + watertemp + fhabitat + fmgmt)
Zip7 <- zeroinfl(f7, dist = "poisson", data = wpt) 

f8 <- formula(number ~ salinity + fhabitat)
Zip8 <- zeroinfl(f8, dist = "poisson", data = wpt)

f9 <- formula(number ~ salinity + fhabitat + maxwind + fmgmt)
Zip9 <- zeroinfl(f9, dist = "poisson", data = wpt)

f10 <- formula(number ~ salinity + fmgmt)
Zip10 <- zeroinfl(f10, dist = "poisson", data = wpt)

f11 <- formula(number ~ salinity)
Zip11 <- zeroinfl(f11, dist = "poisson", data = wpt)

f12 <- formula(number ~ fmgmt)
Zip12 <- zeroinfl(f12, dist = "poisson", data = wpt)

AIC(Zip1, Zip3, Zip6, Zip7, Zip8, Zip9, Zip10, Zip11, Zip12) #Zip 3, 8, and 9 are the best

summary(Zip8) # I'm finding it hard to interpret the summary table...I'm not sure if the result for salinity makes sense


# > ZIP vs. ZINB ----
# we need to see if the ZIP model properly took care of the overdispersion, to do this we'll compare it to a ZINB model

nb8 <- zeroinfl(f8, dist = "negbin", link = "logit", data = wpt)


library(lmtest)
lrtest(Zip8, nb8)

#log-likelihood for nb8 is better than Zip8, showing evidence that the ZINB model is a better fit to the data
AIC(nb8, Zip8)
summary(nb8) # I still think this interpretation is problematic, and I don't know why the NaNs are int he output...

plot(resid(nb8) ~ wpt$salinity)


####need to decide what should be reported####

# > model validation ----

EP <- residuals(nb3, type = "pearson")
plot(EP)

# > model diagnostics ----

# we have been doing some of this all along (looking at variance homogeneity and fitted vs. residual plots), but there are a few other things that can be done, I have copied the code from class if we want to explore anything else

# 1) variance homogeneity
#plot(Zip8)

# the first plot shows fitted vs. residuals, want to see a "starry night", in this case we got a trumpet plot, variance high at high values, this indicates a log linear relationship

# 2) normality of errors
#plot(mod3, 2) # creates a QQ plot, want all data points to fall on theline, indicates normality
#hist(resid(mod3)) # ours is skewed in this case

# 3) variance homogeneity
plot(resid(Zip8) ~ wpt$salinity) 
boxplot(resid(Zip8) ~ wpt$salinity)

# 4) plot predicted values

#plot(number ~ salinity, data = wpt)

# now add predicted lines for each month

#plot(AFD ~ LENGTH, data = clams, type = "n")
# now adding in raw data for each month
#points(AFD[fMONTH == "2"] ~ LENGTH[fMONTH == "2"], data = clams, col = "red")

# > binomial with presence data ----

#what happens if we just do the presence/absence data?
mod.bino <- glm(present ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt, family = binomial, data = wpt)
summary(mod.bino)
plot(mod.bino) #weird pattern in residuals...

# > adding a random effect for site, using binomial with presence data----

mod.lmer <- glmer(present ~ salinity + fmonth + airtemp + maxwind + watertemp + fhabitat + fmgmt + (1|surveypoint), family = binomial, data = wpt)

summary(mod.lmer)
plot(mod.lmer)

# > logistic regression ----


# > Questions and Challenges ----

# how to deal with uneven sampling
# how to deal with repeat visits
# some survey points were visited twice at different times of the year
# type of model - gaussian vs. poisson distribution
# what should be a random effect
# what predictors should be included, is there colinearity between predictors
# managed vs. tidal, do we care about habitat types within these major categories?
