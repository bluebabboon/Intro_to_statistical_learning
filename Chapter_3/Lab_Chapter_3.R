####### Linear Regression ########
####### Lab-Chapter 3     ########


# let us load the libraries for our dataset Boston(contained in MASS)
library(MASS)
library(ISLR)

# Help for boston
?Boston
# This data frame contains the following columns:
#   
# crim -    per capita crime rate by town.
# zn -      proportion of residential land zoned for lots over 25,000 sq.ft.
# indus -   proportion of non-retail business acres per town.
# chas -    Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox -     nitrogen oxides concentration (parts per 10 million).
# rm -      average number of rooms per dwelling.
# age -     proportion of owner-occupied units built prior to 1940.
# dis -     weighted mean of distances to five Boston employment centres.
# rad -     index of accessibility to radial highways.
# tax -     full-value property-tax rate per \$10,000.
# ptratio - pupil-teacher ratio by town.
# black -   1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# lstat -   lower status of the population (percent).
# medv -    median value of owner-occupied homes in \$1000s

# fix in dataeditor,see the column names and finally attach names of column names in Boston so we can use it directly
fix(Boston)
names(Boston)
nrow(Boston)
ncol(Boston)
dim(Boston)
attach(Boston)

# lm is to fit linear models(lm),To carry out linear regression

?lm

# class of lm gives function in return
class(lm)

# lets consdier response of medv with lstat (with X as lstat, Y as medv)
# medv - median value of owner occupied homes in $1000 
# lstat - lower status of the population (percentage)

plot(lstat,medv)

# lm(formula) -- here formula argument takes the form of Y~X where we try to predict Y using X
# General syntax lm(y~x,  data=givendataframe,  subset="we can use just part of data",  
# na.action="what to do if we got NA in data(use na.omit or na.fail)",etc....)
# lets assign lm(Y~X) to a variable,so that it will contain a list of all important things that are predicted by lm() function


lm_fit_model=lm(medv~lstat)
class(lm_fit_model)                                                                       # This gives class as lm,but it is a list though


# After we fit the data to a model and calling it lets view the summary
# It gives basic data of what our y and x are,residuals min max(error from actual y to predicted y)
# coefficients of intercept and our x's ((each coefficients, estimate, std error or deviation or sqrt(variance), 
# tstatistic,and p value for that t statistic))

summary(lm_fit_model)

# After fitting the model we want to get data like,residuals,sigma,Rsquared,Fstatistic etc
# We can see all the data that is present in summary()

names(summary(lm_fit_model))


# calling str() will give you an overview of each of the elements inside that model list,you can see that each element is 
# probably of different shape as well as different datatype too
str(lm_fit_model)

# we can see that lm_fit_model is list of 12,where each element itself is again list or list of lists ...
names(lm_fit_model)

# although we can get coefficients of the model by typing lm_fit_model$coefficients its safer to use
# extractor functions like coef()
coef(lm_fit_model)

# To get cofident intervals for the coefficients of the model
confint(lm_fit_model,level = 0.95)

# To predict for some x's we will write it this way
# predict(modelwhereitisstored, x's written as dataframe , interval for predicted y's = confidence or prediction)
# Difference between confidence and prediction interval is confidence gives interval on an average 
# while prediction gives interval on that particular case

predict(lm_fit_model,data.frame((lstat=c(5,10,15))),
        interval = "confidence")

predict(lm_fit_model,data.frame((lstat=c(5,10,15))),
        interval = "prediction")

# Plot between x and y
plot(Boston$lstat,medv)

# abline(a,b) function will draw a line with intercept a and slope b
# abline(ourmodel) will take coefficients and draws the line on plot
abline(lm_fit_model)

# Various arguments can be given to the abline() function
abline(lm_fit_model,lwd=5,col="red")

# while plotting we can use various symbols that is given by pch argument
# pch can take "symbol here" character argument(pch = "*") or pch = 4 number argument
plot(Boston$lstat,medv,col="red",pch=5)

# this plot shows all the symbols that are stored in pch
plot(1:20,1:20,pch=1:20)

########## Diagnostic plots ###########
par(mfrow=c(2,2))

# There are four plots used to check 
#   Residuals vs Fitted values 
#                              -- To check whether there is nonlinearity in data,a strong pattern suggest there is a nonlinearity 
#                              -- Also funnel shape i.e., variance(fluctuation) in residuals increases as fitted value increase means there is non constant variance in error
#                              -- Non constant variance known as heteroscedasticity,its presence poses problem for underlying assumption that 
#                                 all error termns has constant variance (Var(erro)=sigma^2) 
#   Standardized residuals(studentizised) vs Fitted values
#                              -- To check outliers ,although residuals can be used to plot outliers(means very large diff in y and prediction)
#                                 we dont know which absolute value of residual to consider,so we divide them by std.dev of residuals to get standardized residuals
#                              -- High value of this indicates the point is outlier
#   Std.Residuals vs Leverage
#                              -- To check whether a point is high leverage,this points have unusual value of x's
#                              -- Obtained by a formula (Euclidean distance of point x from mean(x))/(sum(Euclidean distance of all points from mean of x))
#                              -- Points that lie in top right corner indicates those points are both high leverage and outliers
#
#   Std.Residuals vs Theoritical Quantiles(QQ plots)
#
############ Quantiles and QQ Plot ##########
# Quantiles -- These are basically percentiles,These are points in data below which certain proportion of your data fall
# Consider Normal distribution(default theoritical one) 0.5 quantile or 50th percentile or 50% point is 0, i.e., 50% of all data points lie below 0
# 0.95 quantile or 95th percentile or 95% is 1.64; 95% of data points lie below 1.64
# So these are the values that are given on X axis
# Similarly we will record 1st,2nd,...100th percentile for our data to compare.Say [10th,50th,60th,90th] percentile of our data is [1,1.4,2,6]
# We then plot quantile points of theoritical normal distribution vs The data points (in above [1,1.4,2,6]) to see how much our data is close to normal distribution
# Getting a linear plot means our data is close to normal distribution.
#
# More Info   ----    http://data.library.virginia.edu/understanding-q-q-plots/
#
#                              -- QQ plot is scatter plot created by plotting two sets of quantiles against one another
#                              -- On X axis generally we take quantiles of normal distribution called as theoritical quantiles
#
# So all the four diagnostic plots mentioned above can be plotted just by calling plot(model where we fit)
plot(lm_fit_model)

# If we want only first plot, residuals vs fitted values
plot(predict(lm_fit_model),residuals(lm_fit_model))                                       # Residuals can be called using residuals(model_fitted)
plot(fitted.values(lm_fit_model),residuals(lm_fit_model))                                 # Both predict(model_fitted) and fitted.values(model_fitted) gives same result    

# Plotting standardized or studentized residuals vs fitted values
plot(fitted.values(lm_fit_model),rstudent(lm_fit_model))

# LEVERAGE can be computed for any number of predictors using hatvalues() function
# As said earlier it is a measure of how far away independent variable's(feature's) value of an observation from all other observations
# More info ---- https://onlinecourses.science.psu.edu/stat501/node/337
#           ---- https://en.wikipedia.org/wiki/Leverage_(statistics)
?hatvalues
hatvalues(lm_fit_model)
length(hatvalues(lm_fit_model))
# Leverage plot with index
plot(hatvalues(lm_fit_model))
# Standardized residuals vs Leverage
plot(hatvalues(lm_fit_model),rstudent(lm_fit_model))
# which.max() function returns the index location of first maximum value in an array
# Here we can see which observations have the highest leverage that poses problem to data
which.max(hatvalues(lm_fit_model))



################## Multiple Linear Regression ################


####### Only two variables(lstat,age) predicting medv #######

# it is pretty straight forward to fit multiple linear regression
# we just have to change the formula argument in syntax of lm()

# If we attached boston no need to give data=Boston argument
lm_multi_model1=lm(medv~lstat+age,data=Boston)

# We took y=intercept + beta1*lstat + beta2*age as our model
# It has p=2 ,so F statistic will be for p x (n-p-1) ; 2 x (506-2-1) = 2 x 503 DegofFreedom
# The p value is very small so atleast one of the predictors coeff is non zero
# Also individual p values shows everything is < 0.01,so the effects are significant
summary(lm_multi_model1)
str(lm_multi_model1)
names(lm_multi_model1)

coef(lm_multi_model1)
confint(lm_multi_model1,level = 0.95)

par(mfrow=c(2,2))
plot(lm_multi_model1)

hatvalues(lm_multi_model1)
which.max(hatvalues(lm_multi_model1))
plot(hatvalues(lm_multi_model1))

# Summary can also give various insights too
?summary.lm
# To get r squared of a model
summary(lm_multi_model1)$r.squared
# To get RSE (residual standard error)
summary(lm_multi_model1)$sigma
# To get coefficients,estimate,SE(of coefficient),t value,p value
summary(lm_multi_model1)$coefficients



######## All the variables predicting medv i.e., 13 variables ########

# Syntax for writing formula to include all variables is just to keep "." after ~

lm_multi_model_all=lm(medv~.,data=Boston)

summary(lm_multi_model_all)
coef(lm_multi_model_all)
confint(lm_multi_model_all)
summary(lm_multi_model_all)$r.squared
summary(lm_multi_model_all)$sigma

par(mfrow=c(2,2))
plot(lm_multi_model_all)
plot(fitted.values(lm_multi_model_all),
     residuals(lm_multi_model_all),pch=10,col="red")

# Leverages of each observation
hatvalues(lm_multi_model_all)
# Standardized residuals vs Leverage
plot(hatvalues(lm_multi_model_all),residuals(lm_multi_model_all))
# Standardized residuals vs Fitted values
plot(fitted.values(lm_multi_model_all),
     rstudent(lm_multi_model_all))


###### Variablity inflation factor (VIF) ######
# Importing library(car) which contains function for calculating VIF
# VIF tells you how collinear a feature is to any other of them
# VIF --> high means Rsquared of a feature fit as function of rest of features is high --> That feature is redundant
# Have to neglect VIF which is high ,VIF(for a feature)= 1/(1-R_squared_feature) ,where R_squared_feature is derived from 
# fitting that feature as y and rest of them as x's

vif(lm_multi_model_all)
vif(lm_multi_model1)
vif(lm_fit_model)                                                       # this shows an error because there is only one x,so it cant fit it as func of anyother x


######### All variables predicting medv except age(and how to do it) #########
lm_multi_model_noage = lm(medv~.-age,data=Boston)
summary(lm_multi_model_noage)
summary(lm_multi_model_noage)$r.squared

# We can also update model after creating it by the following function
lm_multi_model1=update(lm_multi_model1,~lstat+age+indus)


######### Interaction Terms and how to add them in model #############

# We have to specify in formula what is the interaction we want
# If we give lstat*age,it automatically includes the effects of lstat and age although pvalue of anyone of them is high
# This is fundamental property of interaction is that we cannot just add interaction term and ignore the effects by individual ones
lm_multi_interaction = lm(medv ~ lstat*age,data=Boston)
summary(lm_multi_interaction)

# We can see that interaction is shown as lstat:age,insted of lstat*age,Both are same in terms of synatx

######### Non linear Transformation of Predictors,adding squared and polynomials #########
# We cannot just say lstat+lstat^2,we have to use I() ,I() is needed because ^ has special meaning in formula
?I
lm_nonlinear= lm(medv~lstat+I(lstat^2),data = Boston)
summary(lm_nonlinear)                                                   # P value associated with ^2 term is very less suggesting that it is very significant

######### ANOVA ##########
# We use anova to check whether two models make any difference in prediciton accuracy
# We will compare linear model and non linear model to see the significance of is there any difference in sample means obtained by those two models
?anova
anova(lm_fit_model,lm_nonlinear)                                        # Null hypothesis is that two models fit equally well                                                        
                                                                        # Alternative hypothesis is that second model is superior
# We observe that p value for second model is very low indicating that it is more accurate thus rejecting Null hypothesis

par(mfrow=c(2,2))
# We can observe from plot fittedvalues vs Residuals show no pattern,(only horizontal st line) indicating that our model is accurate
plot(lm_nonlinear)

######## Non linear Polynomial Fit,upto a given degree ########
# If we want to fit a model having not only squared term but also ^3,^4,... we use poly() function
?poly
# poly() takes object as first argument and ,degree to which it has to compute as second argument
lm_nonlinear5th = lm(medv~poly(lstat,5),data=Boston)
summary(lm_nonlinear5th)

######## Non linear logarithimic fit,for a predictor ########
lm_nonlinear_log = lm(medv~log(lstat),data = Boston)
summary(lm_nonlinear_log)



############## Qualitative Predictors ################
# let us work on CarSeats data which is a part of ISLR library

cardata = Carseats
names(cardata)
dim(cardata)
attach(cardata)
# In str(cardata) we see that we have three factor variables aka known as qualitative variables
# They are ShelveLoc - Good,Medium,Bad ; Urban - Yes,No ; US - Yes,No
str(cardata)
summary(cardata)
class(cardata)
fix(cardata)

# We can use linear regression lm() function to even fit factor variables
# It will create a dummy variable to indicate the level,generally dummy variables are lesser than factor(exactly one less than factor levels)

# This model includes interaction terms between Income and Advertising as well as Price and Age
# Remember Income:Advertising is same as Income*Advertising inside lm() ; That syntax A:B tells R to include interaction between A and B

lm_qualitative = lm(Sales ~ .+ Income:Advertising + Price:Age,data = cardata )
summary(lm_qualitative)
names(lm_qualitative)
coef(lm_qualitative)
confint(lm_qualitative,level = 0.97)

# DOF = n-p-1 = 400 - (10 + 2 (for interaction) + 1 (for dummy variable) - 1 (for intercept)) = 400-13-1 = 386
df.residual(lm_qualitative)                                              # Tells you the Degrees of Freedom the model has and considered in F statistic

# Contrasts are a way to see what are the dummy variables created and how is it related to Actual data for factor predictors
?contrasts
# What this means is that R created dummy variables ShelveLocGood and ShelveLocMedium (3 levels ,so only 2 dummy variables are needed)
# So when Actual value of Shelveloc is Good ,S..Good takes 1 and S..Medium takes 0; If ShelveLoc is Medium S..Good takes 0 and S..Medium takes 1
# What happens when ShelveLoc is Bad ,well Both S..Good and S..Medium takes 0 and 0 , and the extra burden of fitting the data will go to Intercept
# So if the factor is only two levels there is no need for a dummy variable , Intercept will take care of it
contrasts(ShelveLoc)                                                     
contrasts(Urban)
contrasts(US)
