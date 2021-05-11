# EVE 298: Group Project
# Authors: Melissa, Sidney, Cnristofer, Danielle
# Date: Spring Quarter 2021

# set working directory and read in the data

setwd("C:/Users/mkril/Desktop/EVE298")

wpt <- read.csv("data/WPT_Occupancy_R.csv")

head(wpt)
str(wpt)

# response variable - number of wpt
# predictor variables - salinity, habitat type, water temperature?, air temperature?, wind speed?


plot(salinity ~ number, data = wpt)
hist(wpt$salinity)


#### Questions ####

# how to deal with uneven sampling
# how to deal with repeat visits
# some survey points were visited twice at different times of the year
# type of model - gaussian vs. poisson distribution
# what should be a random effect
# what predictors should be included, is their colinearity between predictors
# managed vs. tidal, do we care about habitat types within these major categories?
