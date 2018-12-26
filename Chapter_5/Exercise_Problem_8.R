# Now we are going to perform a cross validation on a simulated dataset

## a)
# Generating dataset as follows
set.seed(1)
y = rnorm(100)
x = rnorm(100)

y = x-2*x^2+rnorm(100)
# Here n is 100 , number of observations and
# p is number of predictors, and p=2

## b)
plot(x,y,xlab = "X",ylab = "Y")
# It resembles a parabola

## c)
# setting a random seed
set.seed(42)
# loading the boot library which has cv.glm() function
library(boot)
?data.frame
datahere = data.frame(x,x^2,x^3,x^4,y)
names(datahere)
attach(datahere)

# Using different fits as specified in the example we are going to fit each model
# i)
# We are using linear regression here insted of logistic, this can be acheived by neglecting the binomial argument in the family.
glmfit1 = glm(formula = y~x,data = datahere)
glmfit1
cverror1 = cv.glm(data = datahere,glmfit = glmfit1)
names(cverror1)
# The LOOCV error is in the delta part of this cverror
cverror1$delta

# ii)
glmfit2 = glm(formula = y~x+x.2,data = datahere)
glmfit2
cverror2 = cv.glm(data = datahere,glmfit = glmfit2)
cverror2$delta

# iii)
glmfit3 = glm(formula = y~x+x.2+x.3,data = datahere)
glmfit3
cverror3 = cv.glm(data = datahere,glmfit = glmfit3)
cverror3$delta

# iv)
glmfit4 = glm(formula = y~x+x.2+x.3+x.4,data = datahere)
glmfit3
cverror4 = cv.glm(data = datahere,glmfit = glmfit4)
cverror4$delta

## d)
# Using another random seed
set.seed(27)

glmfit1 = glm(formula = y~x,data = datahere)
glmfit1
cverror1 = cv.glm(data = datahere,glmfit = glmfit1)
names(cverror1)
# The LOOCV error is in the delta part of this cverror
cverror1$delta

# ii)
glmfit2 = glm(formula = y~x+x.2,data = datahere)
glmfit2
cverror2 = cv.glm(data = datahere,glmfit = glmfit2)
cverror2$delta

# iii)
glmfit3 = glm(formula = y~x+x.2+x.3,data = datahere)
glmfit3
cverror3 = cv.glm(data = datahere,glmfit = glmfit3)
cverror3$delta

# iv)
glmfit4 = glm(formula = y~x+x.2+x.3+x.4,data = datahere)
glmfit3
cverror4 = cv.glm(data = datahere,glmfit = glmfit4)
cverror4$delta

# The results that we got in c are same as what we got in d
# the setting of seed affects whenever we are doing any random operation invovled
# Here we are using the randomness when we have generated the data.
# But after generating it we are using it to compute LOOCV
# in LOOCV we always use all the observations unlike K-fold wheere we split the observations randomly
# So the seed preferences doesn't matter when we are computing LOOCV


## e)
# In the models of c) we observed the lowest error for model ii)
# It is what we expect bccause that is what we have acutally given when we generated the y data from the x
# Although we have added another rnorm() in the y, it is for generating the errors that cannot be predicted by the model.


## f)
# Now using lm function to fit each of these models from i) to iv)
# i)
lmfit1 = lm(formula = y~x,data = datahere)
lmfit1
summary(lmfit1)

lmfit2 = lm(formula = y~x+x.2,data = datahere)
summary(lmfit2)

lmfit3 = lm(formula = y~x+x.2+x.3,data = datahere)
summary(lmfit3)

lmfit4 = lm(formula = y~x+x.2+x.3+x.4, data = datahere)
summary(lmfit4)

# In all the cases we have seen above only the coefficients of x and x.2 are seems to be significant with its pvalues being lesser than 0.05
# This agrees with our cross validation results which says that the model ii) has the least LOOCV error which resembles our true model.


