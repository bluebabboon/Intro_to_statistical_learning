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


## c)
# Now using the model from b to predict the direction of rist observation
firstpred = predict(object = lmfit_not1,newdata = weekdata[1,],type = "response")
firstpred
firstpred_categorical = ifelse(firstpred > 0.5,"Up","Down")
firstpred_categorical
# Now checking if it is correct by comparing it with the actual correct observation
Direction[1] == firstpred_categorical
# It was not correctly classified

## d)
# Now creating a function that does the following
# Fits a logistic regression in a forloop from 1 to nrow(data)
# Where using all but ith observation in that ith loop
# Creating a empty array of same dimesion as weekdata to store the errors, if occured

error_array = rep(x = 0,nrow(weekdata))
error_array

for (i in 1:nrow(weekdata)) {
  glmmodel = glm(formula = Direction~Lag1+Lag2,family = binomial,data = weekdata[-i,])
  predhere = predict(object = glmmodel,newdata = weekdata[i,],type = "response")
  predhere_categorical = ifelse(predhere > 0.5,"Up","Down")
  if (Direction[i] == predhere_categorical) {
    NULL
  }else{
    error_array[i]=1
  }
}

error_array
sum(error_array)
mean(error_array)

# So total 44.9% of the observations are misclassified if using LOOCV estimation methodology

