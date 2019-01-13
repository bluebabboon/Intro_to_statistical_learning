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
dim(bostondata)
names(bostondata)
str(bostondata)
summary(bostondata)
# No NA's in the data
sum(is.na(bostondata))


####################
## REGSUBSETS SELECTION
####################
# Importing regsusbset from leaps library
library(leaps)

# Splitting data and creating parts of dataframes as well as matrices
set.seed(42)

# Creating 70% of indices to sample out from
train = sample(1:nrow(bostondata),0.7*nrow(bostondata))
test = -train

trainframe = bostondata[train,]
testframe = bostondata[test,]

trainmatrix = model.matrix(crim~.,data = trainframe)[,-1]
testmatrix = model.matrix(crim~.,data = testframe)[,-1]

# Lets use validation set error to get the error of test set
reg_model = regsubsets(crim~.,data = trainframe,nvmax = 13)
summary(reg_model)

# Creating a predict function over regsubsets
predict.regsubsets = function(object,newdata,id){
  formulahere = as.formula(object$call[[2]])
  matrixhere = model.matrix(formulahere,data = newdata)
  coeffi = coef(object,id)

  # Since the coefficients of the ith fit might not contain all the predictors we have to use
  #   only those that are present , so we have to select columns whose names match with the
  #   current coefficients
  nameshere = names(coeffi)
  preds = matrixhere[,nameshere] %*% coeffi
  preds
}

# Now that we have created this prediction function on the regsubsets , we can use it to get
#   error on the test dataset
# Lets create a array of 0's to store the error for each best model until it used all the predictors
reg_test_err = rep(0,ncol(bostondata)-1)
reg_test_err

for (i in 1:(ncol(bostondata)-1)) {
  predhere = predict(reg_model,testframe,i)
  reg_test_err[i] = mean((predhere - testframe$crim)^2)
}

# getting the lowest error
which.min(reg_test_err)
reg_error = reg_test_err[10]

# Plotting all the errors for eeach best model we have found
plot(reg_test_err,type = "b")



############
## RIDGE REGRESSION
###########
# Importing glmnet library which has lasso and ridge
library(glmnet)

#Doing cross validation
ridge_model = cv.glmnet(x = trainmatrix,y = trainframe$crim,alpha = 0)
summary(ridge_model)
ridge_model
bestlambda_ridge = ridge_model$lambda.min

ridge_preds = predict(object = ridge_model,newx = testmatrix,s = bestlambda_ridge)
ridge_error = mean((ridge_preds - testframe$crim)^2)

predict(object = ridge_model,s = bestlambda_ridge,type = "coefficients")


############
## LASSO REGRESSION
###########
lasso_model = cv.glmnet(x = trainmatrix,y = trainframe$crim,alpha = 1)
summary(lasso_model)
lasso_model

bestlambda_lasso = lasso_model$lambda.min

lasso_preds = predict(object = lasso_model,newx = testmatrix,s = bestlambda_lasso)
lasso_error = mean((lasso_preds - testframe$crim)^2)


which.min(c(ridge_error,reg_error,lasso_error))

# Lasso got the lowest error of them all.
lasso_error


