# Doing some fun on Boston dataset
# Loadint the mass library which has the boston dataset
library(MASS)
bostondata = Boston

str(bostondata)

# We have dis , column which is the weighted mean of distances to five Boston Employment centers
#   and nox , which is nitrogen oxides concentrations in parts per 10 million
# dis is predictor and nox is response

##a)
# Using the poly() function to fit a cubic polynomial regression to predict nox using dis
#   TO then see the regression output and plot the resulting data and polynomial fits

noxmodel = lm(nox~poly(dis,3),data = bostondata)
noxmodel
summary(noxmodel)

par(mfrow = c(2,2))

# Plotting the model as we call it below will give us four plots that we have observed in
#   chapter 3. Residucals , qq plot, scale location  and residuals vs leverage plots
plot(noxmodel)

dislimits = range(bostondata$dis)
dislimits

dis_grid = seq(dislimits[1],dislimits[2],by = 0.1)

preds_model1 = predict(object = noxmodel,se.fit = TRUE,newdata = list(dis = dis_grid))

se_bands_model1 = cbind(preds_model1$fit + 2*preds_model1$se.fit ,
                                preds_model1$fit - 2*preds_model1$se.fit)

plot(bostondata$dis,bostondata$nox,xlim = dislimits,col = "darkgrey")

lines(dis_grid,preds_model1$fit,lwd = 2, col = "red")

matlines(dis_grid,se_bands_model1, lwd = 1 , col = "blue", lty = 3)



### b)
# Plotting polynomial fits ranging from degree 1 to 10 and reporting the residual sum of squares

res_error = rep(0,10)

for (i in 1:10) {
  lm_fit = lm(nox~poly(dis,i),data = bostondata)
  res_error[i] = sum(lm_fit$residuals^2)
}

res_error
which.min(res_error)

plot(res_error)
# The 10th model gives us the lowest residual errors


### c)
## To perform cross validaiton or another approach to selec the optimal degree for the polynomial
##  And then seeing the residuals again
cv_errors = rep(0,10)

for (i in 1:10) {
  glm_fit = glm(nox~poly(dis,i),data = bostondata)
  cv_errors[i] = cv.glm(glmfit = glm_fit,data = bostondata,K = 10)$delta[1]
}

cv_errors
which.min(cv_errors)

plot(cv_errors)
# By using the cross validation approach and fitting the linear model using glm , we foundthat
#   having 3 polynomials is giving us the lowest cross validaiton error.


## d)
# Now using the bs() function to fit a regression spline to predic the nox and using dis.
#   TO report this output for the fit using four degree of freedom.
#   Also have to explain about the knots we have chosen. Then plot the resulting fit

# A cubic spline is what we get in the bs and it already have 3 degrees of freedom, so we can
#   keep one extra knot,
# Another way is to specify the df argument inside the bs function

bs_fit = lm(nox~bs(dis,df = 4),data = bostondata)
bs_fit

# To see the knots attribute , we can use the following function that we have seen in the lab
#   chapter 7.
attr(bs(bostondata$dis,df=4),"knots")

# Fitting the predictions like we did in ## a) and then plotting the fit and standard errors
preds_bs_fit = predict(bs_fit,se.fit = TRUE,newdata = list(dis = dis_grid))

se_bands_bsfit = cbind(preds_bs_fit$fit + 2*preds_bs_fit$se.fit ,
                          preds_bs_fit$fit - 2*preds_bs_fit$se.fit)

plot(bostondata$dis,bostondata$nox , col = "darkgrey")

lines(dis_grid, preds_bs_fit$fit , lwd =2 , col = "red")

matlines(dis_grid, se_bands_bsfit, lwd = 1, lty =3 , col = "green")




### e)
# Now we are fitting a regression spline for a range of degrees of freddom and plot the resulting
#   fits and report the resulting RSS. And then using plots to describe the results

# We have to vary the degrees of freedom, lets vary the dof from 1 to 10 and then select the
#   best one out of it which has the lowest amount of RSS which can be calculated from the
#   residuals which we can get from the model that we fit

lm_errors = rep(0,10)

# When  we called this varying the degrees of freedom from 1 to 10 it says that it usees
#   for df=1 and df=2 as df=3 , it says that df is small and it considers 3 by default
#
# We can consider the i from 1 to 10 but it will consider the i from 3, because the b spline
#   by default has 3 degrees of freedom because it is part of cubic spline. Every spline has
#   minimum of 3 degrees of freedom if it is derived from cubic spline.
for (i in 1:10) {
  lm_fit = lm(nox~bs(dis,df = i),data = bostondata)
  lm_errors[i] = sum(lm_fit$residuals^2)
}

lm_errors

which.min(lm_errors)

plot(lm_errors,type = 'b')



## f)
# Now it is time to perform cross validation, or any approach to select the best degrees of
#   freedom. For a regression spline on this data.

# As we have used cv.glm in c) above we can use the same , but instead of using poly() we can
#   use bs() or ns() to use splines here. Lets use ns()
cv_spline_errors = rep(0,7)

for (i in 4:10) {
  glm_fit = glm(nox~ns(dis,df = i),data = bostondata)

  # delta 1 will give us the cross validaiton error and delta[2] is the loocv error something
  #   like that, Have to check chapter 5 to confirm , what these delta[1] and delta[2] mean
  cv_spline_errors[i] = cv.glm(data = bostondata,glmfit = glm_fit,K = 10)$delta[1]
}

cv_spline_errors

which.min(cv_spline_errors[-(1:3)])

# 5th index of this cv_spline_errors is the lowest , it emans with i as 8.

plot(cv_spline_errors[-(1:3)],type = 'b')

# We can see that 5th index of errors after removing the first 3 elements gives us lowest error
#   So 5+3 is the 8th i value and for degree of freedom of 8 of natural spline we have the lowest
#   error


