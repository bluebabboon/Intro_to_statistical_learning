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
# lines will give us a line on the currently activated plot, here we are plotting all the fitted
#   values on the existing plot
lines(age_grid, preds$fit, lwd =2 , col = "blue")

?matlines
# This will plot the columns of one matrix against the columns of another.
#   So here if we give matline(x,y) then the first column of x will be plotted against the first
#     column of y, if one matrix has fewer columns plotting wil lcycle back thrgough the columns
#     again.
matlines(age_grid,se.bands,lwd = 1, col = "blue",lty = 3)

# We have earlier discussed that whether or not we use orthogonal set of basis functions our
#   predicitons wont be affected, lets check that
preds2 = predict(fit2,newdata = list(age = age_grid),se.fit = TRUE)

# The maximum value among the differences in 0 , which means that both the models have given
#   us same output.This means that the way we use the argument raw=True or not will not affect
#   the output predictions
max(abs(preds2$fit - preds2$fit))


# Whenever we are doing a polynomial regression first we have to decide on the number of
#   polynomials we are going to use. We can use hypothesis testing to fit models ranging from
#   linear to a degree of n polynomial and seek to determine the best SIMPLEST model wihch is
#   sufficient to explain the relationship
#
# One way to do this using ANOVA function which performns an ANALYSIS OF VARIANCE using Ftest
#   inorder to test the null hypothesis that a model M1 is sufficient enough to explain the
#   data against the alternative hypothesis that a more complex model M2 is required.
#
# Inorder to use this anova function M1 and M2 must be nested models that means that predictors
#   of M1 will be subset of M2 and M3 will have all the predictors of M2 and so on

# Lets create models ranging from M1 to M5 and then check anova for each

fit1 = lm(wage~age, data = wagedata)
fit2 = lm(wage~poly(age,2),data = wagedata)
fit3 = lm(wage~poly(age,3),data = wagedata)
fit4 = lm(wage~poly(age,4),data = wagedata)
fit5 = lm(wage~poly(age,5),data = wagedata)

# Lets use anova to check the F value for each of this model and see the p value for that F
?anova
anova(fit1,fit2,fit3,fit4,fit5)

# Anova says that p value comparing the linear Model M1 to quadratic model M2 is basically zero
# This says that linear fit is not sufficient
#
# Similarily the p value comparing the quadratic model M2 to the cubic model M3 is also very low
#   So the quadratic fit is also not sufficient
# The p value is 5% when we compare model 3 with model4 and model 5 seems unnecessary

# In this case instead of using hte anova funciton we could have obtained these p values
#   more accurately we can just output the coefficients using the coef on the summary of this fit
coef(summary(fit5))

# When there are only two groups where group can be thought of as predictor, then one way
# Anova test becomes t student with Fvalue = t^2 ,where t is student statistic

# See that the t value for the second predictor that is age^2 is -11.98 , when we square it
#   it is equal to teh F value of the 2nd fit , that is fit with age and age^2
(-11.983)^2


# Another thing to note is that anova works whether we used orthogonal polynomials or not.
#   This means using the raw= True or not this will work.
# And also having another predictor will not change the behaviour of F value in anova
fit1 = lm(wage~education+age,data = wagedata)
fit2 = lm(wage~education+poly(age,2),data = wagedata)
fit3 = lm(wage~education+poly(age,3),data = wagedata)

anova(fit1,fit2,fit3)

# As an alternative approach we can also choose the polynomial degree using the cross validation
#   as we have discussed in chapter 5


# Lets consider the task of predicting whether an individual earn more than 250k$ per year
# Here the response vector has to be created and then we can apply glm() function to fit
#   logistic regression over it
fit = glm(I(wage>250)~poly(age,4),data = wagedata,family = binomial)
# Here note that we have created the binary variable response on the fly using wrapper function
#   I() . The expression I>250 will produce TRUE where it is satisfied for that row and False
#   viceversa. Then glm()  COERCES BOOLEAN TO BINARY by setting TRUE to 1 and FALSE to 0
fit
summary(fit)

preds = predict(object = fit,newdata = list(age = age_grid),se.fit = TRUE)

# However there is slight problem in the predictions here. The default prediction type for
#   glm() is type="link" which is what we use here. This means that we get predictions for LOGIT
# That is log( p(y|x) / (1 - p(y|x))) = X*beta
# So the predictions we get are X*beta , but we need p(y|x) . So we have to transform the response
#   to match the actual y

# Transforming the logits to actual predictions
pfit_actual = exp(preds$fit) / (1+exp(preds$fit))

# Now creating the standard errors band ( Standard error = variance / number of observation)
se.bands.logit = cbind(preds$fit + 2*preds$se.fit , preds$fit - 2*preds$se.fit)

# We have to transform these standard errors also , because they are standard errors of logits
#   We need standard errors of predictions
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))


# We could have predicted these actual fits rather than logits and then transforming them again
#   by simply giving another argument " type = "response" " in the predict function
preds = predict(object = fit,newdata = list(age = age_grid),type = "response",se.fit = TRUE)

# But however we would have ended up with negative probabilities

# Plotting the plots , the beautiful type ones that we have seen before, here as we know some
#   of the arguments, here type = "n" will not plot anything.
plot(age,I(wage>250),xlim = agelims,type = "n",ylim = c(0,0.2))

?jitter
# jitter adds a small amount of noise to a numeric vector
?points
# cex, is for character or symbol , this will add "l" type of points on the given points of age on x
#   and I(wage>250) on y axis
points(jitter(age),I((wage>250)/5),cex = 0.5,pch="l",col = "darkgrey")

# Now we are plotting the line plot on the current plot, with age_grid values on x axis and
#   and y axis values are pfit_actual which are predictions we have got from the logistic regression
lines(age_grid,pfit_actual,lwd=2,col = "blue")

?matlines
# plots columns of matrices of one columns with another column.
#   x axis has age_grid values and y axis has sebands[,1] and sebands[,2] with same x axis values
matlines(age_grid,se.bands,lwd = 1,col = "blue",lty = 3)






