# Using the colleged dataset.
# loading the islr library which has the college dataset
library(ISLR)

collegedata = College

str(collegedata)

names(collegedata)

# No, NA's , since sum of is.na is 0
sum(is.na(collegedata))


## a)
# To split the data in to training set and a test set. Using the OutState predictor as the response
#   and other variables as predictors,lets perform and forward stepwise selection on the training
#   set , to contian only those predictors which will gives us a satisfactory model

# loading library leaps which has the regsubsets function
library(leaps)

# First splitting the data into test and train

set.seed(42)

train = sample(1:nrow(collegedata),0.7*nrow(collegedata))

dim(collegedata)

test = -(train)

?regsubsets

regfit = regsubsets(Outstate~.,data = collegedata[train,], nvmax = ncol(collegedata)-1)

regfit

coef(regfit,5)

# Plotting the fit, which gives us which variabels are selected for a particular bic (bayesian
#   information criteria)j

par(mfrow = c(2,2))

plot(regfit)

regsummary = summary(regfit)

# cp , rss and adjr2 which are part of summary of regfit

plot(regsummary$rss,type = 'l')

plot(regsummary$cp,type = 'l')

plot(regsummary$bic,type = 'l')


# Now we have fit the model on the training set , then we have to predict the response on the
#   test set and see for which number of predictors it is the lowest. And then select those
#   predictors

# Creating a test prediction matrix so that we can multiply each model with its coefficients

test_matrix = model.matrix(Outstate~.,data = collegedata[test,])

val_errors = rep(0,ncol(collegedata)-1)

for (i in 1:17) {
  coef_current = coef(regfit,i)
  testpred = test_matrix[,names(coef_current)]%*%coef_current
  val_errors[i] = mean((collegedata$Outstate[test] - testpred)^2)
}

val_errors









































































































