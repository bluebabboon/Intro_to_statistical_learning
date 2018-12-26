# We are going to use LOOCV method in the following models
#
## a)
# Fitting a logistic regression model on Weekly data to predict Direction using Lag1 and Lag2
library(ISLR)
weekdata = Weekly
weekdata
nrow(weekdata)
names(weekdata)
attach(weekdata)

# logistic regression -> glm on Direction with Lag1 and Lag2
lmfit = glm(formula = Direction~Lag1+Lag2,family = binomial,data = weekdata)
lmfit


## b)
# Using all the data except for the first element
lmfit_not1 = glm(formula = Direction~Lag1+Lag2,family = binomial,data = weekdata[-1,])
lmfit_not1




































































































