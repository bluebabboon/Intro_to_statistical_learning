# Using the same techniques that we have shown in the lab chapter 7 we want to fit those
#   non linear models on the Auto dataset

library(ISLR)
autodata = Auto

names(autodata)

pairs(autodata)

# We want to predict the mileage per gallon or known as mpg here with different other predictors
#   that our data has. After taking a look at the paired scatter plot we see that
# cylinders, displacement, horsepower and acceleration


# Using gam's to predict the relationship
library(gam)

gam_model1 = gam(mpg~s(displacement,4), data = autodata)
gam_model1

summary(gam_model1)

plot(gam_model1, se = TRUE, col = "green")


# For selecting the number of degrees that will be acceptable in our non linear fits we have
#   following methods to see


## First method
## Fitting using the smooth spline function and then seeing the number of degrees of freedom
## Although this will not give us any degrees of polynomial it will give us DOF , which can be
##  calculated using the cv method.
fit1 = smooth.spline(autodata$displacement, autodata$mpg,cv = TRUE)
fit1
summary(fit1)
fit1$df


## Second Method
## Fitting linear models with poly function with different levels of polynomial degrees and then
##  we use anova to check which one is significant and adding value. Then we can chose that
##  particular degree value which will give us significant p value
lmfit1 = lm(mpg~displacement, data = autodata)
lmfit2 = lm(mpg~poly(displacement,2),data = autodata)
lmfit3 = lm(mpg~poly(displacement,3),data = autodata)
lmfit4 = lm(mpg~poly(displacement,4),data = autodata)
lmfit5 = lm(mpg~poly(displacement,5),data = autodata)

anova(lmfit1,lmfit2,lmfit3,lmfit4,lmfit5)

# Choosing displacement to a degree of 2 seems reasonable and there is no considerable improvement
#   considering all the other degrees for this predictor


## Third Method
## Fitting the glm model without using the binomial argument which makes it equal to linear model
##  fit. Then we loop from 1 to number of degrees we want to see and then fit the glm's
##  using poly and we will check with cross validation using cv.glm, to see which model is
##  giving us the lowest error. Then we can select that particular degree for further analysis

# loading the boot libary which has the cv.glm() function
library(boot)

cv_errors = rep(0,10)

for (i in 1:10) {
  glm_fit = glm(mpg~poly(displacement,i),data = autodata)
  cv_errors[i] = cv.glm(glmfit = glm_fit,data = autodata,K = 10)$delta[i]
}

which.min(cv_errors)

# This method suggests that we use the 2 degrees of freedom same as what we have got from the
#   anova method from above.



####
### We can use one of above three methods on all the predictors that we would like to fit a
###   non linear relationship
# To acheive that , lets create a function that will take a method and will reproduce all the
#   above steps and then gives us the degree that will be most suitable for further calculation



#########################################
#### ANOVA FUNCTION WITH MULTIPLE MODELS
########################################



# Creating function for fitting lm models from 1 to 10 coeffs and then doing anova on all the
#   models at once


## After some google search i found what i want to call multiple models in anova when the
##  number of models might vary
##
##  do.call function to the resuce
#####################
### EXTRA KNOWLEDGE
#####################
?do.call

# What do.call is simple , it constructs and executes a function call from a name or a
#   function and list of arguments that needs to be passed to it
# For example do.call(what , args) takes the what as first argument that is the function we want
#   to execute
#   Then it takes the args as list as input that means if we have list as list(a,b,c) and we want
#     to pass these a,b,c as arguments in the function above.
#
# Then do.call will execute what(a,b,c) where args are list of list(a,b,c)

# Creating the
model_create_fun = function(datahere,response,predictor,degree){

  # Creating an empty list to save the models temporarily in a list
  templist = list()

  for (i in 1:degree)
  {
    # For each degree we are saving the current lm fit model that particular i
    #   inside the list , Here we have used the paste0 function to combine all the
    #   characters inside and we are using the poly function to create the polynomials
    #   Also here we have to give all the arguments of response and predictors in characters
    templist[[i]] = lm(as.formula(paste0(response,"~","poly(", predictor, "," ,i, ")")),
                                   data = datahere)
  }

  # After fitting the models and saving them in the list , now we have to apply the anova
  #   but we have to give all the models at the same time , as we are increasing the model
  #   polynomials one by one
  #
  # REFER THIS LINK FOR MORE DETAILS
  # https://stackoverflow.com/questions/47104744/how-to-use-the-function-anova-to-multiple-models-saved-in-a-list

  do.call(anova,templist)

}


# Now that we have created the function to fit any response with any predictors and see the
#   anova model of ,it we can determine for each one what is the degree that will be
#   pretty good.


# So , for the variables that we haven't calculated on , like cylinders, horsepower and
#   acceleration. We are going to use this function and seeing the summary of it or just by
#   typing it in console we can see what is the degree we have to consider for each of these
#   predictors
horsepower_anova = model_create_fun(datahere = autodata,response = "mpg",predictor = "horsepower",
                            degree = 4)

# Calling the horsepower_anova
horsepower_anova


acceleration_anova = model_create_fun(autodata, "mpg","acceleration",4)

# calling the acceleration_anova
acceleration_anova


# So from the above considerations we see that its better to take
# horsepower until degree 2
# cylidners is factor , so no degree is needed
# acceleration until degree 4

# And we have already considered displacement until degree 2


# Now that we decided on the degrees and the variables we are going to use
#   lets just fit a gam function with smooth splines or natural splines or just lm
#   but with the given number of degrees is from the above

gam_final = gam(mpg~poly(horsepower,2)+cylinders+
                  poly(acceleration,4)+poly(displacement,2),data = autodata)
gam_final

summary(gam_final)

# Splitting the graph in to 1x4
par(mfrow = c(1,4))

# Plotting all the different predictors with its influence along with standard errors
plot(gam_final,se = TRUE, col ="red")


