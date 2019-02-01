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

which.min(regsummary$rss)

plot(regsummary$cp,type = 'l')

which.min(regsummary$cp)

plot(regsummary$bic,type = 'l')

which.min(regsummary$bic)

plot(regsummary$adjr2, type = 'l')

which.max(regsummary$adjr2)


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


# Rather than predicting th errors directly like above we can write a predict function that
#   we can add it to the regsubsets library and then we can use the predict like we use
#   it on any other model fit like we have used earlier


# Copy pasted our prediction code over reg subsets from the lab chapter 6, since the regsubsets
#   doesn't have a inbuilt prediction function.
predict.regsubsets = function(object,newdata,id,...){
  # The following line of code will extract the formula that we have typed in our given object
  # Here the object is the regsusbset function that we are going to pass
  # then for every regsusbset output we have a call attribute which is of type "language" and
  #   inside that attribute we have to entire formula and the variables stored
  # Of that stored call, its [[2]] index will give us formula character
  # But we want that to be stored as formula so we shall convert that string to forumula using
  #   as.formula
  formulahere = as.formula(object$call[[2]])
  matrixhere = model.matrix(object = formulahere,data = newdata)

  # id is for selecting the coefficients of which number of variable model, like do we want model
  #   which is best with 10 predictors or model with 11 or whatever.
  coeffi = coef(object = object,id = id)
  xvariables = names(coeffi)
  matrixhere[,xvariables]%*%coeffi
}

# Using the predict function that we have created over the regsusbsets and then using that
#   to predict the errors
val_errors = rep(0,ncol(collegedata)-1)

for (i in 1:(ncol(collegedata)-1)) {
  predshere = predict(regfit,collegedata[test,],i)
  val_errors[i] = mean((collegedata$Outstate[test] - predshere)^2)
}

val_errors

which.min(val_errors)

# These are the predictors that we have selected that gives the minimum test error
coef(regfit,11)

# If we observe we got the same errors whether we calculated directly or created another function
#   and then used that to predict the mean erros for each model (where every model has different
#   number of predictors)






## b)
# Fit a GAM on the training data and , OutState as the response and the features selected in the
#   previous step as the predictors
# Plot the results and explaing the findings

# Now that we have the predictors that we are considering with 11 coefficients, we can use
#   smooth splines for each with a nominal degreee of 3 and then fit a gam

str(collegedata)

library(gam)

# Fitting the gam model only on the training set

gam_fit = gam(Outstate~ Private + s(Apps,3)+s(Accept,3)+s(Enroll,3)+s(Top10perc,3)
                                + s(Room.Board,3)+s(Personal,3)+s(PhD,3)
                                + s(perc.alumni,3)+s(Expend,3)+s(Grad.Rate,3),
              data = collegedata[train,])

gam_fit

summary(gam_fit)

# Setting the graphical parameters, such as numbe rof rows in grid of plot and
#   setting the margins for each plot and also the outer margins, for the full graphical plot
#   at the same time.
par(mfrow = c(2,6),mar = c(4.5,4.5,1,1),oma = c(0,0,4,0))

plot(gam_fit,se = TRUE, col = "red")




## c)

# Now evaluating the gam model that we have fit using the training set on thetest dataset

preds_gam = predict(gam_fit,newdata = collegedata[test,])

# The mean squared error of the gam fit over the test dataset, is as follows
gam_mse = mean((preds_gam - collegedata$Outstate[test])^2)



## d)
# For which variables we have the evidence of non-linear relationship with the response.

summary(gam_fit)

# If we check the F values and the p-values associated with it then we see that
#  For Expend we have the greatest p value and most observed non linear relationship with the
#   response.
#  It is followed by Accept and then by Personal, Gradrate and finally Applications


