# In this lab we will explore some resampling techniques

# Validation set approach
# Importing islr library
library(ISLR)
# setting the seed to get same random numbers everytime
set.seed(42)

# Using sample function
# Sample takes 2 arguments , from where the data has to be taken from
# Second argument is how much we want to select
# Here below we want to select 192 random numbers from the number 1 to 392
?sample
train = sample(392,196)
train
# Now train contains indexes we can say sort of from 1 to 392 where the total number of
# indexes are equal to 196

# Using linear regression to fit mpg and horsepower of Auto dataset
# And giving the subset as train. which makes our data to select only those observations whose indexes match or exists in the train.
lm.fit = lm(mpg~horsepower,data = Auto,subset = train)
lm.fit

# Using the predict funcntion to estimate response for all observations
lm.preds = predict(object = lm.fit,newdata = Auto)
lm.preds

attach(Auto)


# Squared errors of all predictions other than train data
sqerr_array = (mpg-lm.preds)[-train]^2

# Mean of squared array is 25.1
# Which is pretty large
mean(sqerr_array)

# This means that estimated test MSE (Mean squared error) that means , mean of sum of squared errors.
# MSE using linear regression fit is 25.1

# Lets add more features with poly function, that will just make polynomial feature
lm.fit2 = lm(mpg~poly(horsepower,2),data = Auto,subset = train)
lm.preds2 = predict(object = lm.fit2,newdata = Auto)
sqerr_array2 = (mpg-lm.preds2)[-train]^2
# MSE for this is only 20.2
mean(sqerr_array2)

# What is effect if we add polynomial 3 of given horsepower
lm.fit3 = lm(mpg~poly(horsepower,3),data = Auto,subset = train)
lm.preds3 = predict(object = lm.fit3,newdata = Auto)
sqerr_array3 = (mpg-lm.preds3)[-train]^2
# MSe for this is 20.39
mean(sqerr_array3)



# Selecting another sample set by calling sample function again, but with now seeding as another
# If we use sample function without changing seed , then we are going to get same set of selections
set.seed(3)
train = sample(392,196)
train
lm.fit = lm(mpg~horsepower,data = Auto,subset = train)
lm.preds = predict(object = lm.fit,newdata = Auto)
sqerr = (mpg-lm.preds)[-train]^2
mean(sqerr)
# MSE here is 26.29


lm.fit2 = lm(mpg~poly(horsepower,2),data = Auto,subset = train)
lm.preds2 = predict(object = lm.fit2,newdata = Auto)
sqerr2 = (mpg-lm.preds2)[-train]^2
mean(sqerr2)
# MSE here is 21.50

lm.fit3 = lm(mpg~poly(horsepower,3),data = Auto,subset = train)
lm.preds3 = predict(object = lm.fit3,newdata = Auto)
sqerr3 = (mpg-lm.preds3)[-train]^2
mean(sqerr3)
# MSE here is also 21.50






###########
### Leave One Out Cross Validation [LOOCV]
###########

# The LOOCV estimate can be automatically computed for any generalized linear model.
# This can be acheived using glm() and cv.glm() functions

# We can acheive linear regression using glm(), we used glm() and passing family argument as binomial to get logistic regression
# But in the same time if we dont use family option then we can acheive linear regression

# For example
glm.fit = glm(mpg~horsepower,data = Auto)
# TO get the coefficients
coef(glm.fit)

lm.fit = lm(mpg~horsepower,data = Auto)
# To get the coefficients
coef(lm.fit)

# We can see that both models gave same results


# To acheive cross validation we use another certain function called cv.glm()
# Its part of library called "boot"
library(boot)
glm.fit = glm(mpg~horsepower,data = Auto)
cv.err  = cv.glm(data = Auto,glmfit = glm.fit)

# Our cv.glm function produces a list with several components.
# calling names(cv.err)
names(cv.err)

# It has "call", "K" , "delta" and "seed"
#
#     call
# The original call to cv.glm.
#     K
# The value of K used for the K-fold cross validation. If we dont specify k as something , then it will be set to number of observations which is nothing but LOOCV
#     delta
# A vector of length two. The first component is the raw cross-validation estimate of prediction error. The second component is the adjusted cross-validation estimate. The adjustment is designed to compensate for the bias introduced by not using leave-one-out cross-validation.
#     seed
# The value of .Random.seed when cv.glm was called.

# Generally LOOCV means we exclude each observations then fit the model , calculate MSE and then take average of all the MSE's , for each observation that is excluded.
# Refer 5.1 formula in ISLR.
# Now this delta value in cv.err gives that value.
# It has 2 values, first one is cv estimate from LOOCV and second one is adjusted cv estimate to compensate bias if we dont do LOOCV

# LOOCV cv estimate is, this is nothing but average of all MSE's
cv.err$delta

# The delta has 2 values that has been discussed above.
# The first value matches with second one in case we do LOOCV and second one is accurate only if we do linear regression, REFER 5.2 formula in ISLR

cv.error = rep(0,5)


# Calculating the crossvalidation error, here LOOCV unless we specify k value
# We are calculating cv error for each polynomial fit we are doing

for(i in 1:5){
  glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
  cv.error[i] = cv.glm(data = Auto,glmfit = glm.fit)$delta[1]
}

# CV error tends to lower as we keep on adding more polynomials
# But there is no considerable gain in improvement
cv.error



##############
####### K Fold Cross Validation
#############
set.seed(17)

# To do k fold , we just have to add k argument's value in the cv.glm function

cv.error.10 = rep(0,10)

for(i in 1:10){

  # Using glm function and performing linear regression
  glm.fit = glm(mpg~poly(horsepower,i),data = Auto)

  # We are adding k = 10 , that means we are doing 10 fold cross validation
  # Then we will get that cv estimate in the delta and its first element is what we want
  cv.error.10[i] = cv.glm(data = Auto,glmfit = glm.fit,K = 10)$delta[1]
}

cv.error.10

# The delta of the cv.glm which has 2 values, will differ in this case when we use Kfold
# Only when we use LOOCV these will match. The second value is a bias corrected version and the first one is what we got from the actual fit.





##########
#### BOOTSTRAP
#########

















































