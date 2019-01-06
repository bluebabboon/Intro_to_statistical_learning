# In this problem we will generate simulated data and use that to perform the best susbset selection.

## a)
# Use the rnorm() to generate predictor X of length n=100 and also noise of e of length 100
x = rnorm(n = 100)
eps = rnorm(n = 100)
length(x)
length(eps)

## b)
# Generate response vector Y of length n = 100 with the model below
y = 1 + 2*x + 3*x^2 + 4*x^3 + eps

## c)
# Use the regsubsets to perform the ebst subset selection with predictors from x^1 to x^10
# What is best model obtained according to Cp, BIC and adjsuted R^2

# Adding regsubsets library to this, which is inside the library called leaps
library(leaps)

# We have to create a dataframe from x^1 to x^10
xfull = poly(x = x,degree = 10)

# making a dataframe of y and x
datahere = data.frame(y,xfull)

# Fitting the regsubsets model
reg_model = regsubsets(y~.,data = datahere,nvmax = 10)
reg_model
plot(reg_model)
?regsubsets

reg_summary = summary(reg_model)
reg_summary
reg_summary$rsq

# Creating a grid of 2x2
par(mfrow = c(2,2))

# Plotting for each of the type of indicators we consider to select model
plot(reg_summary$rsq)
plot(reg_summary$bic)
plot(reg_summary$cp)
plot(reg_summary$adjr2)

maxpointnumber_adjrsq = which.max(reg_summary$adjr2)
maxpointnumber_adjrsq
minpointnumber_cp = which.min(reg_summary$cp)
minpointnumber_cp
minpointnumber_bic = which.min(reg_summary$bic)
minpointnumber_bic
points(maxpointnumber_adjrsq, reg_summary$adjr2[maxpointnumber_adjrsq], col="red", cex=2, pch=20)

# According to bic and cp indexes we have these at the lowest for index equal to 3
# So lets get the coefficients of the model where we have 3 predictors,which is meant by 3 above.
coef(reg_model,3)
coef(reg_model,minpointnumber_cp)
coef(reg_model,maxpointnumber_adjrsq)

## d)
# Now using the backward and forward selection for selecting the model predictors
# For specifying the backward and forward selection we just hve to add another argument in regsubsets
?regsubsets

reg_model_forward = regsubsets(y~.,data = datahere,nvmax = 10,method = "forward")
reg_sumamry_forward = summary(reg_model_forward)

maxpointnumber_adjrsq = which.max(reg_sumamry_forward$adjr2)
maxpointnumber_adjrsq

minpointnumber_bic = which.min(reg_sumamry_forward$bic)
minpointnumber_bic

minpointnumber_cp = which.min(reg_sumamry_forward$cp)
minpointnumber_cp

coef(reg_model_forward,maxpointnumber_adjrsq)
coef(reg_model_forward,minpointnumber_bic)
coef(reg_model_forward,minpointnumber_cp)

# Now using the backward selection method to fit the model and doing the same process again.
reg_model_backward = regsubsets(y~.,data = datahere,nvmax = 10,method = "backward")
reg_summary_backward = summary(reg_model_backward)

maxpointnumber_adjrsq = which.max(reg_summary_backward$adjr2)
maxpointnumber_adjrsq

minpointnumber_bic = which.min(reg_summary_backward$bic)
minpointnumber_bic

minpointnumber_cp = which.min(reg_summary_backward$cp)
minpointnumber_cp

coef(reg_model_backward,maxpointnumber_adjrsq)
coef(reg_model_backward,minpointnumber_bic)
coef(reg_model_backward,minpointnumber_cp)

## As we can observe all the three method resulted in the same value and number of coefficietns that
##  we can select. So the way of seleciton of predictors have no infleunce in the selection of
##  predcitors and the numbe r of predictors

## e)
# Fitting the lasso model to the simualated model. using the same data.
# Using the cross validation now , not the validation model.










































































