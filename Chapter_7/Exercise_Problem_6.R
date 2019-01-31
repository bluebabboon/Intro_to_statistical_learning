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

# Some information on assign and how it works
?assign



#########################
##### USE THIS URL FOR THE NEW CONCEPT OR ERROR WE HAVE LEARNT
## http://r.789695.n4.nabble.com/Predict-polynomial-problem-td1017140.html

# We have learnt a new concept of creating models multiple times and then saving them inside a
#   list, Where we can call the different models in the list and use predictions on them
#   whenever we want
#

# bquote is a function that will help us in letting our predict function take correct i value
#   and then give correct predictions , rather than relying on the global i value that is stored
#   in the environment.
?bquote

for (i in 1:10) {

  # Creating a current linear lm model with lm function
  # The following formula will not be able to give predictions if we use predictions other
  #   than fit10 in the predict function. Because the current i inside the predict function will
  #   10 at the end of loop and the formula will try to use 10, but the model we are supplying
  #   if it is other than fit10, then it will throw the error
  #
  ####  current_model = lm(wage~poly(age,i),data = wagedata)

  # So we have to use the following formula instead of the formula we have above
  current_model = lm(bquote(wage~poly(age,.(i))),data = wagedata)

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

# Now we have to plot these fit that we have found useful which is 3rd fit for us
age_limits = range(wagedata$age)

age_grid = seq(age_limits[1],age_limits[2])

# Just calling preds4 in this example without modifying the formula we are specifying in the
#   loop of model we have saved will not work
# Here in the below predictions we are doing, the predict function first argument is model
#   And the model we are giving it is fit4, And by common sense it has to use fit4 and use the
#   poly(age,4) inside the formula, But what happens is that the formula we have given is
#   poly(age,i) and the current i will be the last i in the loop we have given and it is 10
#   So poly is doing poly(age,10) and the model only has 4, So it will throw this error
# TO fix this error we have to do this
#
# Instead of using lm(wage~poly(age,i)) in the formula in the for loop above we have to do this
# lm(wage~bquote(wage~poly(age,.(i))))
#
# To learn why is this happening is because as per bquote , bquote quotes its argument except that
#   the terms wrapped in the ".()" are evaluated in specified "where" environment and that is
#   its parent.frame by default.
preds4 = predict(fit4,newdata = list(age=age_grid),se.fit = TRUE)

# Creating the bands of standard errors with +- 2 times the preds4$fit
# Binding the plus and minus errors from its main , in columnwise
standarderror_bands_4 = cbind(preds4$fit + 2*preds4$se.fit , preds4$fit - 2*preds4$se.fit)

plot(age,wage,xlim = age_limits,cex = 0.5,col = "darkgrey")

# plotting the lines for each value of grid as x value in age_grid and corresponding predicted
#   value on y. With lwd giving the line width , col giving colur
?lines
lines(age_grid,preds4$fit,lwd = 3, col = "green")

# matlines to plot one column as x and keeping the same x but different columns of y of another
#   dataframe, or list or whatever. And then plotting the lines, And lwd with line width
#   and lty giving the line type. lty 3 will be dashed line type
?matlines
matlines(age_grid, standarderror_bands_4, lwd = 0.5, lty = 3, col = "red")

# Finally giving the title to the plot
title("4th Degree polynomial fit")











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

step_cv_errors = rep(0,9)

# Here we cannot use a single cut, so we have to keep the range of i from 2 to 10, or else the cut
#   function will not work here.

for (i in 2:10) {
  # We have to add the new factor values of age that is produced by the cut function over the age
  #   to the wagedata dataframe, then we have to keep on updating it , or else it doesn't consider
  #   the new cut ages that we have specified
  #
  # We are adding a new column of values to our dataframe wagedata called cut_age and then using that
  #   cut_age in the glm function formula, poly() used to work without adding anything like this
  # But i dont understand why is this not working. Have to check this.
  wagedata$cut_age = cut(age,i)
  step_glm_fit =  glm(wage~cut_age,data = wagedata)
  step_cv_errors[i-1] = cv.glm(glmfit = step_glm_fit,data = wagedata,K = 10)$delta[1]
}

step_cv_errors
which.min(step_cv_errors)

# So using 8 cuts of age seems to give us the lowest cross validation error, because the 7th index
#   is for 8 cuts

plot(2:10,step_cv_errors,type = "b")

# So finally in our data frame we have cut_age column with 10 age as factors, Here the cut will
#   automatically decides where to give the cut in the range of age values, if we dont specify
#   anything by default.
table(wagedata$cut_age)


# Plotting the step_glm_fit for 8 number of cuts, which is 7th index
step_glm_fit_8 = glm(wage~cut(age,8),data = wagedata)

preds8 = predict(object = step_glm_fit_8,newdata = list(age = age_grid),se.fit = TRUE)

standarderror_bands_8 = cbind(preds8$fit + 2*preds8$se.fit , preds8$fit - 2*preds8$se.fit)

plot(age,wage,col = "darkgrey", xlim = age_limits,cex = 0.5)

lines(age_grid,preds8$fit,lwd = 2, col = "green")

matlines(age_grid,standarderror_bands_8,lty = 3, lwd = 0.5, col = "red")

title("Step fit for 8 number of cuts")

