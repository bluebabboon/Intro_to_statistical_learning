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
# Boot strap means nothing but repeatedly sampling the data from the existing one
# including replacement
# Using bootstrap in R is pretty straightforward , we use boot() function
# We are going to use the Portfolio dataset in ISLR package , to illustrate the use of the bootstrap  on this data we must first create a function alpha.fn()
#
library(ISLR)
alpha_fn = function(data,index){
 X = data$X[index]
 Y = data$Y[index]
 return ((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
Portfolio
names(Portfolio)
nrow(Portfolio)

# Taking the Portfolio dataset as first argument and the 1:100 index as second one
# Then we are predicting the alpha return as it is described in section 5.2 and Formula 5.7
alpha_fn(Portfolio,1:100)

# Next we are going to repeatedly select the indexes using sample with replacement as true( this is the basic of bootstrapping)
set.seed(42)
# Using replacement of samples we are selecting 100 observations but these samples can have duplicate observations now
alpha_fn(Portfolio,sample(x = 100,size = 100,replace = TRUE))

# We can do bootstrap analysis by repeatedly using the above command and then taking the standard deviation of all the repeated results
# Or we can use the boot function that automates this appraoch
# It takes 3 arguments, first one is data that asks on which data we are going to do the computations
# Second is the statistic or moreover we can say the function that returns a value for which we are going to compute the standard deviation
# Third one is R ,or number of replicates that we want the statistic function to be implemented and the result to be computed on

# Also boot function is part of boot library ,so we are going to import that too
library(boot)
boot(data = Portfolio,statistic = alpha_fn,R = 1000)

# The output of boot shows us the standard error on our return estimate that is calculated using the bootstrap method
#



##########
## Estimating the Accuracy of a Linear Regression Model
##########
# This approach can be used to assess the variability of the coefficients estimates and predictions from a statistical learning method(Means linear regression and logistic etc). Here we are going to use bootstrap to assess the variabiliity of the estimates beta0 and beta1

# Creating a function that calculates the coefficients and outputs them

lm_fn = function(data,index){
  return(coef(lm(mpg~horsepower,data = data,subset = index)))
}

# Now using boot function to do this 1000 times
set.seed(42)
library(ISLR)
library(boot)
boot(data = Auto,statistic = lm_fn,R = 1000)

# Now lets see what are the standard erros using the summary on the lm
summary(lm(Auto$mpg~Auto$horsepower,data = Auto))$coef

# We can observe that we have difference in the standard errors that we got from the bootstrap and what we got from the summary of the model
# This is because in the summary calculation we are inherently assuming some assumptions regarding the calculation of variance.
# They depend on the unknown parameter sigma^2 which is estimated from the RSS
# And also the standard error formula assume that all x's are fixed and the variability comes from the variation in the erros.
# The bootstrap approach doesn't make any assumptions regarding the data type and behaviour. So it is more likely to give correct estimate of beta0 and beta1 than the summary() function

# Lets do the bootstrap standard error estimate for the quadratic model for the same mpg and horsepower relation above. Since the model gives good fit for these coefficients , lets calculate the standard error.

lm_quadfn = function(data,index){
 return(coef(lm(Auto$mpg~Auto$horsepower+I(Auto$horsepower^2),data = Auto,subset = index)))
}

boot(data = Auto,statistic = lm_quadfn,R = 1000)

summary(lm(Auto$mpg~Auto$horsepower+I(Auto$horsepower^2),data = Auto))

# We can see that the difference in the estimates is reduced as compared to above.
# In the bootstrap estimate the std error for intercept is 2.09 and in summary function the estimate is 1.8 . As we keep on adding more fit model we will be closer to the bootstrap estimate.





















