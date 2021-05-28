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



'glm, without zeroes' #winner
managed.pos <- subset(managed, managed$number > 0)
mod.managed.pos.glmnb <- glm.nb(number ~ salinity + fhabitat + fmonth + airtemp + maxwind + watertemp, data = managed.pos)
summary(mod.managed.pos.glmnb)
summary(mod.managed.pos)
plot(mod.managed.pos, 1)
