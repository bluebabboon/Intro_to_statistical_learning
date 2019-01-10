# Using the college dataseet which is part of ISLR library
library(ISLR)

collegedata = College

nrow(collegedata)
dim(collegedata)

names(collegedata)
fix(collegedata)

str(collegedata)
summary(collegedata)

# The goal of this problem is to predict the number of applications receive using the other variables
#   in the dataset


## a)
# Split the dataset in to a training set and a test set
# Lets split into 70 30 ratio
set.seed(1)
train = sample(1:nrow(collegedata),0.7*nrow(collegedata),replace = T)
test = -train
traindata = collegedata[train,]
testdata = collegedata[test,]
sum(is.na(traindata))
sum(is.na(testdata))


## b)
# Fitting a linear model using least suares on training set and calcualte the test error
lm_model = lm(formula = Apps~.,data = traindata)
lm_model
summary(lm_model)

lm_pred = predict(object = lm_model,newdata = testdata)
lm_pred

# Mean squared error is
lmsquarederror = mean((lm_pred - testdata$Apps)^2)


## c)
# Using ridge regression model on training set, and with chosen lambda with cross validtaion
# And then calculate the test error
library(glmnet)

# This will not work, because cv.glmnet expects a model.matrix not a data.frame
ridge_model = cv.glmnet(x = traindata[,-1],y = traindata$Apps,alpha = 0)

# So we have to convert our training data of X, into a model.matrix
?model.matrix
?as.matrix

# So creating two matrices, first is for traiing and second is for testing
# Also keeping the [,-1] at the end because we dont want the first column which is nothing but an
#   intercept column
xmatrix = model.matrix(Apps~.,data = traindata)[,-1]
xmatrix_test = model.matrix(Apps~.,data = testdata)[,-1]

# Now this will work because we have used matrix type of x instead of just dataframe
ridge_model = cv.glmnet(x = xmatrix,y = traindata$Apps,alpha=0)

ridge_model

# Getting the coefficients of the best model , best model is for where we have the lambda.min
#   which means we had the lowest error for that particular lambda
ridge_coeffs =predict(ridge_model,s = ridge_model$lambda.min,type = "coefficients")
ridge_coeffs[-1,]

# Getting the predictions of the best model, we have to give the x test data to predict the new y's
# So we have created the model.matrix for the test data too.
ridge_preds = predict(ridge_model,s = ridge_model$lambda.min,newx =xmatrix_test )
ridgesquarederror = mean((ridge_preds - testdata$Apps)^2)



## d)
# Using lasso model on training set and doing the same thing again
# We just have to change the aplha value to 1
lasso_model = cv.glmnet(x = xmatrix,y = traindata$Apps,alpha = 1)
lasso_model

lasso_preds = predict(lasso_model,newx = xmatrix_test,s = lasso_model$lambda.min)
lassosquarederror = mean((lasso_preds - testdata$Apps)^2)

lasso_coeffs = predict(lasso_model,s = lasso_model$lambda.min,type = "coefficients")
lasso_coeffs[-1,]

# Out of total 17 predictors , to predict Apps , we have 3 of them equal to 0, So in total
#   14 coefficients are non zero
#



## e)
# Fitting a pcr model with M chosen by cross validation and then do a test error with that selected
#   M
library(pls)
set.seed(42)

# We dont need x and y in model.matrix format now
# First argument is the Formula , with the lhs on ~ begin the response and rhs being everything else
# Second argument is from which dataframe we want the principal components from
# Third arguemnt is scale which standardizes all the components in normal distribution format
# Fourth argument is validation type, we want to the cross validaiton so we keep "CV" for this
pcr_fit = pcr(Apps~.,data = traindata,scale = TRUE,validation = "CV")
pcr_fit

# summary of this fit will gives us the Cross validation errors for different number of principal
#   components.We can see that the lowest value of cross validation error is when the components are
#   17
summary(pcr_fit)


plot(pcr_fit)

# Doing a validation plot with the given model and the argument val.type is for MSEP
# MSEP stands for mean squared error predictions
# By default the validaiotn plot gives us RMSEP but we want the MSEP , although it doens't actually
#   make a difference
validationplot(pcr_fit,val.type = "MSEP")

pcr_pred = predict(pcr_fit,newdata = testdata,ncomp = 17)
pcrsquarederror = mean((pcr_pred - testdata$Apps)^2)


## f)
# Doing the same thing as in PCR but now the model is pls instead of pcr
set.seed(1)
?plsr
pls_fit = plsr(Apps~.,data = traindata,scale = TRUE,validation = "CV")
pls_fit
summary(pls_fit)

# With 15 components alone we had reached the lowest cross validation error
plot(pls_fit)

validationplot(pls_fit,val.type = "MSEP")

pls_pred = predict(pls_fit,ncomp = 15,newdata = testdata)
plssquarederror = mean((pls_pred - testdata$Apps)^2)

error_vector = c(lmsquarederror,ridgesquarederror,lassosquarederror,pcrsquarederror,plssquarederror)
error_vector

plot(error_vector,type = "h")

# The lowest error we got is for lasso error.Also the errors are very close to each other.


