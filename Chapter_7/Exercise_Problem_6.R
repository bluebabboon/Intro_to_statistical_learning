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

  # Creating a current linear lm model with lm function
  current_model = lm(wage~poly(age,i),data = wagedata)

  # Now we have to assign this current model to the variable name that we have created in the
  #   model fit names and for that current name we have use assign function

  # Here we are assigning the values for those there in the first argument of assign to the seoncd
  #   argument in the assign function
  # In the first argument we have the fit0, or fiti based on the ith variable that we are currently in
  #   the loop, And in the second argument we should have the value that we want to assign to the first
  #   argument as the variable name
  # assign(paste("fit",i,sep = ""),i^^)

  assign(model_fit_names[i],current_model)

}


# Now that we have fit all the 10 models in fit1, fit2 ... fit10 respectievely ,now we have to do the
#   anvoa on this
?anova
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10)

# Anova says that adding the 2nd degree to the fit will incerase the F value , and then adding another
#   3rd variable have nice significance. But adding the 4th one is not giving any value addition




### b)
# Now to fit a step function to predict the wage using age and then perform cross validation  to choose
#   the optimal number of cuts. Also then to obtain the plot
# We can use the cut function to fit the categorical type of variable to our data
# Cut function will basically conver the numeric data into factor variable.
?cut
table(cut(age,4))

# We are plotting from the table , for the cut(age,4) which will give the grouped type of thing
#   where each factor will be shown how many number of times it is appeared in the data after
#   it has been converted in to the factor type.
step_fit = lm(wage~cut(age,4),data = wagedata)
coef(step_fit)
summary(step_fit  )

# From the above sumamry it says that the 4th cut is not that significant.

# Now we want to choose the optimal number of cuts , but we are going to iterate from 1 to 10 number
#   of cuts and for each cut we are going to save the error and see which error is the lowest

step_cv_errors = rep(0,10)

for (i in 2:10) {
  step_glm_fit =  glm(wage~cut(age,i),data = wagedata)
  step_cv_errors[i] = cv.glm(glmfit = step_glm_fit,data = wagedata,K = 10)$delta[1]
}































































































































