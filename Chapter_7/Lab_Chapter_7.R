# In this lab we will reanalyze the Wage data which is thoroughly discussed in this chapter
# ISLR library which contains our wage dataset
library(ISLR)
wagedata = Wage
attach(wagedata)

##########################
# Polynomial Regression and Step Funtions
##########################
# fitting the polynomial model using the poly function
fit = lm(wage~poly(x = age,degree = 4),data = wagedata)
fit
summary(fit)
coef(summary(fit))
?poly

# The poly function will let us avoid wriing the fourth degree polynomial with that long formula
#   This funciton matinly returns a matrix whose columns are basisi of ORTHOGONAL POLYNOMIALS
# Orthogonal polynomials means that the covariance of two columns of x and x^2 , will be made
#   zero automatically and the poly funcction will scale them in such a way.
# Orthogonality is set to be on by default and we can turn it off by specifying the argument
#   "raw = True", this means that x, x^2... x^n will be exactly kept there
# In orthogonality for each column it will create a linear combination of given x to x^n
# Then it will create n such polynomials such that the covariance between any two columns is zero.
# This will give us uncorrelated features but are still n degree polynomials

# Lets fit another model where raw=True is kept in the poly function and see the difference in
#   coefficients estimate
fit2 = lm(wage~poly(x = age,degree = 4,raw = T),data = wagedata)
coef(summary(fit2))

# This doesn't affect the fitted values for the prediction but actual coefficient values are
#   differing in both the cases. It also changes the pvalue of each coefficients.REMEMEBER that
#   the fitted values will not change but the actual coefficient values change.

# There are also other equivalejnt ways of fitting this model. For example using the I() function
#   It is a wrapper function and the symbol ^ has different meaning when used inside the formula
fit2a = lm(wage~age+I(age^2)+I(age^3)+I(age^4),data = wagedata)
coef(summary(fit2a))

# We observe that we got the same coefficients and its standard errors are also matching
# So indirectly using the argument "raw = T" means using the I() function for each of the degree
#   until the given number of degrees are specified.

# Another type of fit is using the age , age^2 .. age^n , by creating them and then binding
#   them using cbind
fit2b = lm(wage~cbind(age, age^2, age^3, age^4),data = wagedata)
summary(fit2b)
coef(summary(fit2b))

# Both using the I(age) and just using the cbind is the same way of fitting the data and the
#   coefficients we got are same form both the different models.


# We shall now create a grid of values for which we want to predict for different values of
#   ages. We shall then use the predict() function to call

?range
# Range returns the minimum and maximum of all the given arguments iniside it
agelims = range(age)

age_grid = seq(agelims[1],agelims[2])

# Predicting for the grid values we have created, here in the new data we have given the data
#   input as list() and in the list we are saying that the column name to be age and takes
#   all the age_grid values.
# Also we have another argument called se.fit which will let us output all the standard errors
#   that we need for each value of age_grid data that we see.
#
preds = predict(object = fit,newdata = list(age = age_grid),se.fit = TRUE)

# Now you can see that preds is object like structure with different columns that it has
#   One of that is se.fit which holds all the standard errors
preds

# Now  we are creating the standard error bands with + or - two times the standard errors
#   to create a band in which our predictions lie for each grid value in the age_grid
se.bands = cbind(preds$fit + 2*preds$se.fit , preds$fit - 2*preds$fit)

# Lets plot those standard errors along with the predictions that it has made
?par
# par is used to set or query graphical parameters. They can also be set
#   mfrow will split the graph display portion in to number of splits ,where each graph is one
#     split.
#   mar is used to set margin c(bottom, left, top,right) , a vector of these 4 values to be
#     given to set the margin space
#   oma is outer margin, this is for the entire grpah displaying portion itself while the previous
#     one is for each plot alone. follows the same c(bottom, left, top , right)
par(mfrow = c(1,2),mar = c(4.5,4.5,1,1),oma = c(0,0,4,0))

?plot
# xlims for setting range i think ,i have to search this
# cex is for modifying the relative magnification of the text part that displays in the graph
#   area, it will be scaled up or down with respect to default value
# col as you know is to set the color of the plot between the x and y axis
plot(age,wage,xlim = agelims,cex = .5, col = "darkgrey")

?title
# this sets the title for the plot. If the outer is set to true the title will be placed in the
#   outer margins of the plot
title("Degree -4 Polynomial",outer = T)

?lines
lines(age_grid, preds$fit, lwd =2 , col = "blue")
































