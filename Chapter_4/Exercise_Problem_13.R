## Using the boston dataset
library(ISLR)
library(MASS)

bostdata = Boston
names(bostdata)
summary(bostdata)

pairs(bostdata)


# So our crime rate which is "crim" variable has to be detected
# Lets add another variable which is 1 if crime is greater than its median and 0 if not
bostdata$crim0 = bostdata$crim
bostdata$crim0 = ifelse( bostdata$crim > median(bostdata$crim),1,0)

pairs(bostdata)

cor(bostdata$crim0,bostdata)

# The correlations are higher with nox,rad,tax,age,dis,indus,lstat
#
