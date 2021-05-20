#### EVE 298: Group Project ####
# Authors: Melissa, Sidney, Christofer, Danielle
# Date: Spring Quarter 2021

# set working directory and read in the data

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
