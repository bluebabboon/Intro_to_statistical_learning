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

# The plot that we have plotted above is called RUG PLOT

#### Now fitting the step function
## We use cut() function
?table
# As we have already used table to get number of false positives and negatives in 4th chapther
# Here we are using table to cross-classify factors to build a contingency table of counts at each
#   combination of factor levels
table(cut(age,4))
?table

?cut
# Cut divides the range of x in to intervals and codes the values in x according to which interval
#   they fall. The left most is level 1, after that is level 2 and so on.

cut(age,4)
# So the age has 3000 rows and then it will take each age value and gives it value among these 4
#   (17.9,33.5] (33.5,49] (49,64.5] (64.5,80.1], The levels we have given are 4 , so it takes
#   the max and min of age and then divides in to 4 regions and gives each age a factor based
#   on where it is lying
length(age)

fit = lm(wage~cut(age,4),data = wagedata)
coef(summary(fit))

# In the above cut function it picks the levels automaticly, we can also specify our own cut
#   limits by using the breaks argument in the cut function. The fucntion cut returns an
#   ordered categorical variable.
# The lm function then creates a set of dummy variables for use in the regression
# If we see here the age < 35 is not there in the coefficients because we already have intercept
#   and we can interpret that  intercept coefficeitn is the average salary for those age<35 and
#   here it is 94160$
# Other coefficients can be interpreted as the average increase in sslary that each age group gets
#   apart from getting 94160$



######################
#### SPLINES
######################

# In order to use regression splines in R we use the splines library.
# We saw that regression splines can be fit by constructing an appropriate matrix of basis function
#   The bs() function generates matrix of basis functions for spliens with specified knots
# We are using cubic splines by default
library(splines)
?bs
# bs genretes the b splines basis matrix for a polynomial splines
bsplines = bs(age,knots = c(25,40,60))

# For splines basis representation we have y = beta0 + beta1* x + beta2*x^2 + beta3*x^3 +
#   beta4* h1(x,eta1) + beta5* h2(x,eta2) + beta6*h3(x,eta3)
#   Where h1(x,eta1) is (x-eta1)^3 if x>eta1 or 0 otherwise, Here eta1, eta2, eta3 are cutoff points
#   or knots where the polynomial changes
#
# The bs function generates matrix will all the predictors from x to h3(x,eta3) ,Now we use that
#   as input to the linear model and fit for predictors
#
# Here in this formula if instead of writing wage~bs(age,knots=c(eta1,eta2,eta3))
#   and we assign that bs(...) to some variable and write it like wage~bsplines, the predict function
#   will not work, it will predict for all the data instead of the new data we have passed.
#   REMEBER THIS
fit = lm(wage~bs(age,knots = c(25,40,60)),data = wagedata)

# RECALL THAT CUBIC SPLINE WITH 3 KNOTS HAVE 7 DEGREES OF FREEDOM, Cubic spline with k knots have
#   k+4 degrees of freedom
summary(fit)

# Summary says that intercept , x^2, x^3, h1(x,eta1), h2(x,eta2) are significant
pred = predict(fit,list(age=age_grid),se = T)

plot(age,wage,col = "gray")

# For this lines to be plotted we have to give the formula as formula itself , we cannot assign it
#   and then use it inside the lm.
lines(age_grid,pred$fit,lwd = 2)

# After fitting the line plot of the prediction we have to plot the standard errors for each age
#   value. So we are plotting those standard errors with dashed lines
?lines
# lty is graphical paramter which specify the line type, it is present in the matlines function
?matlines
lines(age_grid,pred$fit+2*preds$se.fit,lty = "dashed")
lines(age_grid,pred$fit-2*pred$se.fit,lty = "dashed")

# Here we are using spline with 7 degrees of freedom, And we are specifying the knots postion here
#   Insted of specifying we can use df to get uniform knots based on the quantiles and percentages
dim(bs(age,knots = c(25,40,60)))
dim(bs(age,df = 6))
# In above having 3 knots means having 6 degrees of freedom, 3 for the knots itself, knot ceoffi-
#   cients and another 3 for the cubic spline
?attr
# This function gets specific attributes of the object that we specify in the which argumetn
#   Here bs() returns an object and inside that we are specifying the knots attribute we want to
#   output
#   Apart from knots we have another attributes sucnh as "degree" , "Boundary.knots" and
#   "intercept" as well
attr(bs(age,df=6),"knots")
bs(age,df = 6)

###########
### NATURAL SPLINE
############
#
# Instead of using the cubic spline we can use natural spline. Natural spline is nothing but having
#   another constraint that the spline has additional boundary constraints. THe fucntion is
#   required to be linear at the boundary (in the region where X is smaller than the smallest
#   knot or larger thatn the largest knot)

###### Natural Spline Degrees of FREEDOM EXPLANATION
# A natural spline with same as cubic nature has 4 degrees of freedom (with 3 knots)
# Cubic has 7 degrees of Freedom
# Natural Spline has boundary knots as well , So this adds another 2 degrees of Freedom
# Now total is 9, But we impose two constraints at each boundary, So 9 - 4 = 5 DOF
# Out of 5 DOF , we already have intercept which takes care of one boundary constraint
# So 5-1 = 4 DOF in TOTAL. DEGREES OF FREEDOM are nothing but here they are coefficients that we
#   determine

?ns
dim(ns(age,df = 4))

# Instead of giving df option in ns() we can also use knots to directly give points of age
# Fitting the new model with natural spline , with four degrees of freedom
fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2 ,newdata=list(age=age_grid),se=T)

# Givin red colur to natural spline fit
lines(age_grid,pred2$fit,col = "red",lwd =2)

################
## SMOOTH SPLINE
#################
#
# For smoothing spline unlike the normal cubic spline we dont need to give any formula inside
#   the lm function , we can directly use the inbuilt function called smooth.spline() for the
#   model to be generated.
plot(age,wage,xlim = agelims,cex = 0.5,col = "darkgrey")
title("Smooting Splines")

?smooth.spline

# Fits a cubic smoothing spline to the supplied data
# Smooth spline is where it has knots at each unique value of X data, so knots = unique(x)
#
# Smooth spline has combination of loss+penalty term, loss is for RSS and penalty is with
#   lambda which is tunining parameter. THis lambda controls the degrees of freedom
# Smooth splines has lambda and this determines effective degrees of freedom
# In first fit we specified the degrees of freedom and for that particular number of DOF
#   it calculates the lambda and uses that.
# In second fit we specified cross validation is true, so it uses the CV method and determines
#   lambda according to that
#
# Smooth.spline needs no formula as the first argument
#   First argument will be the predictor from which we want to predict the response from
#   Second argument is the predictor value , No need to give any ~ and create a formula
#   Third argument is either df, or cv, Df will give the number of degrees of freedom
#     that we want the spline to have. Then it will calculate the lambda value for which those
#     df are generated and will use that. Or
#     We can use the cv option which will determine the lambda value for which the best on
#     is generated and uses that which will reduce the cross validation error accordingly
fit = smooth.spline(age,wage,df = 16)
fit
summary(fit)
fit2 = smooth.spline(age,wage,cv = TRUE)
fit2

# Here we see that after using cv method DOF are 6.8
fit2$df

# Plotting those two smooting splines with different colour
# In previous method ,
lines(fit,col = "red",lwd =2)
lines(fit2,col = "blue",lwd =2)

# To plot the legend on the top right corner
#   We have to provide additional two arguments , first is legend which will tell us what text
#   to write and then another argument called col which will assign colour to the respecitive
#   lines that are plotted consecutively, So if you plot line 1 first and line 2 second
#   then if we want to give line 1 blue and line 2 red colour then we have to give arguments
#   in respective manner.
legend("topright",legend = c("16 DF","6.8 DF"),col = c("red","blue"),
        lty = 1 , lwd =2 , cex = 0.8)




######################
#### LOCAL REGRESSION
######################

# Inorder to perform the local regression we use the loess() function

# Plotting the basic plot
plot(age,wage,xlim = agelims,cex = 0.5, col = "darkgrey")

# Giving the title
title("LOCAL Regression")

# Fitting the models
?loess

# Fitting a polynomial surface determined by one or more predictors using local fitting
#   It takes the following arguments. First one is the formula in shape of y~x and the seoncd one
#   is "Span" which will let us select the number of local x values that we can use to predict
#   the next y or the y in that region.

# Fitting with wage as Y and age as x, with span as 0.2 and data is wagedata
fit = loess(wage~age,span = 0.2,data = wagedata)

# Fitting with span of 0.5 and everything else remains the same
fit2 = loess(wage~age,span = 0.5,data = wagedata)

# Predicting based on the first model
preds1 = predict(fit,data.frame(age=age_grid))

# Predicting based on the second model
preds2 = predict(fit2, data.frame(age= age_grid))

?lines
# x on first argument , y on second,

# Predicitons for the age grid values with fit1, where span is 0.2
lines(age_grid,preds1,col = "red",lwd =2)

# Predictions for the age grid values with fit2 ,where span is 0.5
lines(age_grid,preds2,col = "blue",lwd =2)

# Now plotting the legend to indicate the fitted values for which span they belong to
?legend

# legend needs a argument legend to write what we want to write in legend'
#   This will be a vector of characters that we are going to represent based on the colour we
#   have assitgned to our plots earlier
# Then it needs another argument col to give the colour to the legends we have written
# lwd to indicate the line width
# lty to indicate the line type
# cex stands for the character expression , here we are using 0.8
legend("topright",legend = c("Span = 0.2","Span = 0.5"),col = c("red","blue"),
       lty = 1,lwd = 2,cex = 0.8)


# We have seen that the larger the span value we are using the smoother the fit is
#   When we have kept span as 1 then it means that we are using the full data then it no longer
#   differs from the normal regression that we use

# Also we can use the locfit library to fit the local regression
?locfit # Strange it says that this library is not there. Why are you doing this to me ISLR?



###################################
####### GENERALIZED ADDITIVE MODELS
###################################

# We can use GAM to predict wage using natural spline functions of year and age, and then
#   treating the education as qualitative predictor

# GAM's are nothing but big linear regression models where we have natural splines of each
#   predictor with respect to another.So we can just use two natural splines to fit the model
#   And then jsut adding them in the formula.
gam1 = lm(wage~ns(age,5)+ns(year,4)+education , data = wagedata)
gam1
summary(gam1)


# We have used natural splines above, we nwo are going to use smoothing spliesn instead of
#   natural. But inorder to use smoothing splines we cannot assume this to be simple combination
#   of basis functions and express it in linear regression using lm function
#
# Rather to use smoothing splines we have to use the gam library

library(gam)
?gam

# gam library is used to fit generalized additive models by giving formula , just like we did
#   as above for lm, everything remains same as above but only thing that changes is
#   gam , instead of lm
#
# There is s() function which is part of gam library used to indicate whether we want to use
#   the smoothing spline. We say to this function that whether we want to have 4 degrees of
#   freedom or 5 degrees of freedom.
# Since education variable is qualitative we can leave that as it is, and it will automatically
#   converted in to dummy variable.
?s

# This s function which is a part of gam library is a symbolic wrapper to indicate a smooth
#  spline term in a formula argument in gam.
# If we want to use the smoothing spline in this gam function we cannot use lm so we are going
#   to use this s() function to generate a smoothing spline : Smoothing spline is nothing but
#     natural spline with knots at each distinct value of x values
gam_m3 = gam(wage~s(year,4)+s(age,5)+education, data = wagedata)
gam_m3
summary(gam_m3)

# Inorder to plot the gam plots we just have to call the model inside the plot like this
par(mfrow = c(1,3))
# Saying the se = TRUE will plot the standard errors for each value of the point of the x
#   with +- 2 times the standard error. This wont have any effect on the qualitative variable
#   though
plot(gam_m3,se = TRUE,col = "blue")

# The generic plot function recognizes that gam_m3 object is a class of gam and then invokes
#   plot.gam() method.
#
# But conveniently even though our previous gam1 model is not actually generaed from the gam
#   library, but still it can be used as part of plot.gam() function eventhough its generated
#   using the lm() function.
plot.Gam(gam1,se = TRUE,col = "red")

# In this following plots we see that year variable is very linear in the starting and then
#   it curves a little bit , but still it is very linear after that too. So we want to check
#   whether splining is giving any effect on the model or not
#
# ANOVA can be used to check by changing the predictor from model to model2 by only changing
#   single predictor and keeping all the rest as same. So this lets us to compare models and
#   evaulates the predictors that we consider.
#
# So lets consider two more models apart from the one we have already created.
#
# In Model 1 we have education + age smoothed with 5 DOF
# In Model 2 we have education + age smoothed with 5 DOF + year single
# In Model 3 we have education + age smoothed with 5 DOF + year smoothed with 4 DOF
#
# Now we are going to fit anova comparing these 3 models inside anova function
gam_m1 = gam(wage~s(age,5)+education, data = wagedata)
gam_m2 = gam(wage~s(age,5)+education+year , data = wagedata)
gam_m3 = gam(wage~s(year,4)+s(age,5)+education,data = wagedata)

anova(gam_m1,gam_m2,gam_m3,test = "F")

# We find evidence according to F statistic that adding year as single variable without any
#   polynomial stuff is creating valuable contribution to the model prediction unlike adding
#   smooth spline of year. This says that having linear function of year is better than a
#   GAM that doesnot include year at all
#
# And also having more non linear consideration of year is not helping in prediction that much
#   as we expected. So in conclusion it is better to consider model M2

summary(gam_m3)

# The below summary has lot of information to grab on to , It first give us AIC information
#   AIC stands for Akaike information criteria
# And then it gives us Anova for Parametric effects. I HAVE TO SEARCH WHAT THIS MEANS
# And then it gives us another table for anova for non parametric effects
# I have to see what is the difference between the anova for parametric and non parametric effects
#
# As it is considered in ISLR book Anova for non parametric effects :
#   According to that our s(age,5) term is clearly significant.
#   Here p values correspond to NULL HYPOTHESIS of a linear relationship versus that ALTERNATIVE
#     HYPOTHESIS
#   As per that large p value for s(year,4) says that this particular variable is not that useful
#   However there is very celar evidence that a non-linear term of age is required and it
#     contributes to the prediciton very well and might not occur by chance.

###
## Predicitons on the GAM objects.
# We can make predictions from gam objects just like we did on the lm objects using the predict()
#   method for the class of gam. Here we make the predcitions on the trainging set.

preds_gam = predict(gam_m2,newdata = wagedata)

# Here preds_gam are just predictions of y values for each value of data and it doesn't contain
#   any extra information.


#########
### LO FUNCTION
#########

# We can use local regression fits as building blocks in a GAM insted of using smoothing splines
#   or using natural splines, We can achieve this by using lo function.

?lo

# This lo function specify a loess fit in GAM formula
#
# Here we have used lo function to use local regression on the age variable with a span of 0.7
gam_lo = gam(wage~s(year,4)+education+lo(age,span = 0.7), data = wagedata)

# plot.GAM function is activated by passing the gam_lo object to this and se = TRUE
#   argument will let the standard errors to be plotted in the plot.
plot(gam_lo,se = TRUE, col = "green")























