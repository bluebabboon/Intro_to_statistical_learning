# We are going to further analyze the wage dataset that has been considered throughout this
#   chapter. To do the following on this dataset
#

## a)
# To perform the polynomial regression to predcit wage using age
# Using the cross validation to select the optimal degree d of polynomial
# And then checking which degree was chosen and how does this compare to result of hypothesis
#   testing using anova.
# Plot this resulting polynomial fit to the data

library(ISLR)
wagedata = Wage

names(wagedata)

str(wagedata)

summary(wagedata)

attach(wagedata)

### Predicting using the cross validation approach
# As we have seen in the chapter 5 , of the section 5.3.3 of its lab , there we have used
#   glm , without using binomial argument inside the function which will give us linear regression
#   instead of logistic regression. Then we can use cv.glm() on this model and calculate the
#   cross validation error for each polynomial that we can fit

# Importing library boot which contains the cv.glm() method
library(boot)

# Creating a sample vector to store the cross validation errors for each polynomial we have
#   chosen

cv_errors = rep(0,10)

# Now for each number of polynomila fit we have chosen we are performing cross validation
#   with 10 , folds and, the cv.glm object has delta attribute and its first element gives
#   us the error for that particular cross validation fit
for (i in 1:10) {
  glm_model = glm(wage~poly(age,i),data = wagedata)
  cv_errors[i] = cv.glm(glmfit = glm_model,data = wagedata,K = 10)$delta[1]
}

# Plotting all the cross validation errors and listing them in the consoel
cv_errors

# We see that among all the cross validation errors we have the 5th one as lowest
which.min(cv_errors)

# Plotting all the errors
plot(cv_errors,type = 'b')
title("Cross validation errors")


### Now fitting those models using anova and then comparing them in anova fit

model_fit_list = list()
model_fit_names = paste0("fit",1:10)

for (i in 1:10) {
  assign(paste("fit",i,sep = ""),i)
}


arbit = paste0("a",1:10)
















































































































































