# EVE 298: Group Project
# Authors: Melissa, Sidney, Christofer, Danielle
# Date: Spring Quarter 2021

# set working directory and read in the data

setwd("C:/Users/mkril/Desktop/EVE298")

wpt <- read.csv("data/WPT_Occupancy_R.csv")

head(wpt)
str(wpt)
wpt$fhabitat <- as.factor(wpt$habitat)
str(wpt)

'lets do some data exploration!'

plot(number ~ salinity, data = wpt)

# response variable - number of wpt

hist(wpt$number)
dotchart(wpt$number, color = wpt$fhabitat)

# predictor variables - salinity, habitat type, air temp, water temperature, wind speed, flow status

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

#this is an observational data set, we should check to see if anything is co-linear
#pairs(wpt)
#pairs(loyn[,2:5]) #subsetting dataframes [rows, columns]
#pairs(loyn[,c(2,4,6)])
#pairs(loyn[,c("AREA", "DIST", "ABUND", "GRAZE")])


#looking at the effect of habitat, but also looking at the spread at different habitat levels, may have to check homogeneity of variance closely
boxplot(wpt$number ~ wpt$fhabitat)
boxplot(wpt$salinity ~ wpt$fhabitat) 




#### Questions ####

# how to deal with uneven sampling
# how to deal with repeat visits
# some survey points were visited twice at different times of the year
# type of model - gaussian vs. poisson distribution
# what should be a random effect
# what predictors should be included, is their colinearity between predictors
# managed vs. tidal, do we care about habitat types within these major categories?
