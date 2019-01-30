# In the wage dataset we have a number of features that are not explroed
# We can explore the relationship between some of these other predictors and wage
#   And then use non linear fitting techniques in order to fit fliexibel models to data
#
# And then plot the results obtained and then finally write a summary of the findings
library(ISLR)
wagedata = Wage

names(wagedata)
str(wagedata)

# Lets plot a scatterplot matrix of each predictor with respect to another
pairs(wagedata)

# seeing how many number of times each of the following factor typed predictor value is being
#   present in the dataset.
table(wagedata$maritl)
table(wagedata$jobclass)
table(wagedata$race)

par(mfrow = c(1,3))
plot(wagedata$maritl , wagedata$wage)
plot(wagedata$jobclass, wagedata$age)
plot(wagedata$race, wagedata$age)

# We can use gams to predict as a non liner relationship with smooth splines as we have shown
#   in the lab chapter 7

# Loading the gam library
library(gam)

attach(wagedata)

gam_model1 = gam(wage~maritl, data = wagedata)

summary(gam_model1)

# s() function is used to indicate the SMOOTHING spline type line in the gam function.
# We can also use ns() to inidicate the natural spline and  bs() to indicate the cubic spline
#   which is the normal type of spline without natural spline constraints, which is to have
#   boundary constraints at the start and end of the curve.

plot(gam_model1,se = TRUE,col="green")


gam_model2 = gam(wage~jobclass, data = wagedata)
summary(gam_model2)
plot(gam_model2,se=TRUE,col = "green")









































































































































