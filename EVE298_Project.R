#### EVE 298: Group Project ####
# Authors: Melissa, Sidney, Christofer, Danielle
# Date: Spring Quarter 2021

# set working directory and read in the data

setwd("~/GitHub/turtle_occurance")
wpt1 <- read.csv("data/WPT_Occupancy_R.csv")

#code

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


wpt <- read.csv("WPT_Occupancy_R.csv")

head(wpt)
str(wpt)
wpt$fhabitat <- as.factor(wpt$habitat)
str(wpt)

# response variable - number of wpt

hist(wpt$number)
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

### Salinity
plot(salinity ~ number, data = wpt) #funnel plot - not great
hist(wpt$salinity) #more data points at lower (?) salinities, only a few at higher salinities - log transform? Will stretch out the big ones
wpt$logsalinity <- log(wpt$salinity)
hist(wpt$logsalinity) #huzzah! we should use the log transformed pH scale

### Air temp
plot(airtemp ~ number, data = wpt) #funnel shaped - is this good??
hist(wpt$airtemp) # probably okay?

### Avg wind
plot(avgwind ~ number , data = wpt) #funneled
hist(wpt$avgwind) #skewed, log trans
wpt$logwind <- log(wpt$avgwind)
hist(wpt$logwind)
plot(logwind ~ number, data = wpt) #do not use the log scale, its worse

### max wind
plot(maxwind ~ number, data = wpt) #funneled - perhaps autocorrelation?
hist(wpt$maxwind) #nice distribution - normal

### water temp

plot(watertemp ~ number, data = wpt) # funneled
hist(wpt$watertemp) #slight skew, but mostly normal. a few outliers that have high temps

### bank cover
plot(bankcover ~ number, data = wpt) #up in the corner, amaybe the cover is not cont?
hist(wpt$bankcover) #skewed to the right, lots of data points at end of scale a few in the front. How to deal? sqrt?
hist((wpt$bankcover))

### flow status
plot(flowstatus ~ number, data = wpt) #up in the corner
hist(wpt$flowstatus)

### autocorrelation
plot(watertemp ~ logsalinity, data = wpt) #uncorrelated!
plot(watertemp ~ airtemp, data = wpt) #correlation, worth a ggplot 
plot(watertemp ~ maxwind, data = wpt) #uncorr?
plot(logsalinity ~ airtemp, data = wpt)#uncorr

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

# > poisson distribution ----
mod.poisson <- glm(number ~ salinity, family = poisson, data = wpt)

summary(mod.poisson) # residual deviance is way higher than the DF, so our model is over dispersed
plot(mod.poisson)#residuals have a pattern

AIC(mod2, mod.poisson, test = "Chi")
# > adding a random effect for site ----
# > model validation ----

AIC(mod2, mod.poisson)

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
