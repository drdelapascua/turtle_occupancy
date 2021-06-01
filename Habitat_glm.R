# set working directory and read in the data
library(nlme)
library(MASS)
library(lme4)

# this line of code below doesn't need to be changed because it's relative to my documents
wpt <- read.csv("../../../../Documents/GitHub/turtle_occupancy/WPT_edited.csv")

# Note that this data has been modified to better fit the skills we learned in class.  
# Date is replaced with the month sampling took place. Values for air temperature, wind speed, water temperature, salinity, bank cover, and  flow status are averaged for the three visits.  Number is now the sum of the turtles across all three visits.  In the column "present" there is a one if turtles were observed on any of the three visits and a zero if no turtles were observed.

head(wpt)
str(wpt)

wpt$fhabitat <- as.factor(wpt$habitat)
wpt$fmgmt <- as.factor(wpt$mgmt)
wpt$fmonth <- as.factor(wpt$month)
wpt$fbaskingarea <- as.factor(wpt$baskingarea)
str(wpt)




# habitat - managed habitat types
wpt.new <- wpt

'subsetting management type'
managed <- subset(wpt.new, wpt.new$fmgmt == "managed")
table(managed$fhabitat)
tidal <- subset(wpt.new, wpt.new$fmgmt == "tidal")


'glm, negative binomial, without zeroes' #winner
managed.pos <- subset(managed, managed$number > 0)
mod.glmnb <- glm.nb(number ~ salinity + fhabitat + fmonth + airtemp + maxwind + watertemp, data = managed.pos)
summary(mod.glmnb)
plot(mod.glmnb)


AIC(mod.bd, mod.md, mod.pond)
plot(resid(mod.glmnb) ~ managed.pos$number) #many zeros, a couple large numbers
plot(resid(mod.glmnb) ~ managed.pos$salinity) # starry night, skewed left
plot(resid(mod.glmnb) ~ managed.pos$airtemp) # starry night
plot(resid(mod.glmnb) ~ managed.pos$maxwind) # starry night
plot(resid(mod.glmnb) ~ managed.pos$watertemp) # starry night
boxplot(resid(mod.glmnb) ~ managed.pos$fhabitat) # the 3 habitat types do not center great around the mean
boxplot(resid(mod.glmnb) ~ managed.pos$fmonth) # June's mean separate from the mean

coplot(number ~ salinity|fhabitat, data = managed.pos) # used in presentation
coplot(number ~ fmonth|fhabitat, data = managed.pos)

plot(number ~ fmonth, data = managed.pos)




# Following code shows that we really need more samples to be able to compute the models
bd <- subset(managed.pos, fhabitat == "Borrow Ditch")
mod.bd <- lme(number ~ salinity + fmonth + airtemp + maxwind + watertemp, data = bd)
summary(mod.bd)

md <- subset(managed.pos, fhabitat == "Major Ditch")
mod.md <- lme(number ~ salinity + fmonth + airtemp + maxwind + watertemp, data = md)
summary(mod.md)

pond <- subset(managed.pos, fhabitat == "Pond")
mod.pond <- lme(number ~ salinity + fmonth + airtemp + maxwind + watertemp, data = pond)
summary(mod.pond)
plot(mod.pond)
