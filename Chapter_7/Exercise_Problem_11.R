# We have stated in the labchapter earlier that we use the gams to use a backfitting approach.
#  The idea of backfitting is simple. We shall explore the backfitting in context of multiple
#  linear regression.

# Suppose we want to perform multiple linear regression, but we are only able to do a simple
#   linear regression. Then even in that situation we can do multiple linear regerssion
# The process is iterative and like this.We repeatedly hold all but one coefficient estimate
#   fixed at its current value, and update only that coefficient estimate using a simple
#   linear regression.
# WE are going to continue this process until convergence, that is until the coefficient estimates
#   stop changing

## a)
# Lets to an example

# Generate Y response based on two predictors X1 and X2 with arbitrary beta0 beta1 and beta2

X1 = rnorm(100)
X2 = rnorm(100)

beta0 = 3
beta1 = 4
beta2 = 1

Y = beta0 + beta1*X1 + beta2*X2 + rnorm(100,sd = 1)

par(mfrow = c(1,3))

# Creating three plots that can be used to see the plots of x's and their relationshipt with y
plot(X1,Y)
plot(X2,Y)
plot(Y)


## b)
# setting a beta1 estimate , lets call it estb1
estb1 = 1

## c)
# fitting the Y-estb1*X1 = beta0+beta2*X2 + error

a = Y - estb1*X1

estb2 = lm(a~X2)$coef[2]

estb2

## d)
# Now doing the same thing as above but using reversing the roles of estb1 and estb2

a = Y - estb2*X2

estb1 = lm(a~X1)$coef[2]

estb1


## e)
# Now to do the both above steps in c and d repeatedly which will converge the values of beta1
#   and beta2

# creating empty vectors to store beta0, beta1 and beta2 values for each for loop

beta1_arr = rep(0,1000)
beta2_arr = rep(0,1000)

beta0_arr = rep(0,1000)

for (i in 1:1000) {

  a = Y - estb1*X1
  estb2 = lm(a~X2)$coef[2]

  beta2_arr[i] = estb2

  a = Y - estb2*X2
  estb0 = lm(a~X1)$coef[1]
  estb1 = lm(a~X1)$coef[2]

  beta0_arr[i] = estb0
  beta1_arr[i] = estb1

}

plot(beta0_arr, type = "l",col = "red",ylim = c(-10,10)
              , xlab = "Iterations", ylab = "Coefficients")
title("Estimates vs Iterations")
lines(beta1_arr,type = 'l',col = "green")
lines(beta2_arr,type = 'l',col = "grey")


## f)
# Performing the multiple linear regression on X1 and X2

reg_fit = lm(Y~X1+X2)
reg_fit
summary(reg_fit)

regbeta0 = reg_fit$coefficients[1]
reg_fit$coefficients
regbeta1 = reg_fit$coefficients[2]
regbeta2 = reg_fit$coefficients[3]

?abline
# abline adds straight line to the plots. a, b are intercepts and slopes respectiviely
#   It also has, h which means the y valuefor the horizontal lines

??lty
abline(h = regbeta0,lty = 2,col = "red" , lwd = 4)
abline(h = regbeta1,lty = 2,col = "green" , lwd = 4)
abline(h = regbeta2,lty = 2,col = "gray" , lwd = 4)


## g)
# The number of iterations that are needed to converge can be seen by observing the beta arrays
beta0_arr[1:5]
beta1_arr[1:5]
beta2_arr[1:5]

# It seems like there is no significant change in betas even after the first iteration. Also
#   we can see the graph and it appears to be straight line almost. So we can say that one
#   iteration is all it needs to converge


