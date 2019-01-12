# Doing stuff on Boston data
# Predicting percapita crime rate
library(MASS)
bostondata = Boston

## a)
# Best subset selection
# Lasso
# Ridge
# PCR

## a.1)
# Performing best subset selection
names(bostondata)
str(bostondata)
summary(bostondata)
# No NA's in the data
sum(is.na(bostondata))

# Importing regsusbset from leaps library
library(leaps)

reg_model = regsubsets(crim~.,data = bostondata)