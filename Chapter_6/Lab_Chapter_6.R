# Using our famous library ISLR
library(ISLR)

# We are going to use a new data here ,its called Hitters data
# We want to predict a baseball players salary on the basis of various statistics associated
#   with performance in the previous year

hitterdata = Hitters

# From this fix we can see the data.and we can observe that some of the salary data is missing
#   in the data
fix(hitterdata)

# The following function is.na will return a bool vector on this salary with true or false
# If the particular row has salary as NA it will be shown as true and false if not
is.na(x = hitterdata$Salary)

# To get the number of rows where salary data is missing or it is NA , we can use the sum over it
sum(is.na(x = hitterdata$Salary))

dim(hitterdata)

# So from above sum we see that salary is missing ofr 59 players
#
# We then can use the na.omit() function to remove all the rows that have missing values in any variable
#
?na.omit
hitterdata = na.omit(hitterdata)

dim(hitterdata)

# Now we can see that we have only 263 rows while the previous 59 rows are deleted
#
sum(is.na(x = hitterdata))




###
## Lets use the library called leaps which does the best subset selection by identifying the best
##  model that contaisn a given number of predictors , where best is quantiied using RSS
install.packages("leaps")
??leaps
library(leaps)
?regsubsets

# We are going to use regsubsets function for model selection.
# It scans through all the subsets of variables using best subset selection model method
# And then gives output if we call summary for each number of predictors which are relevant
# That means for each number of predictors that we want to fit the model with, it will tell what
#   are best predictors that we can use to predict in best way using RSS

regfit = leaps::regsubsets(hitterdata$Salary~.,hitterdata)

summary(regfit)
# In summary we can see that it keeps a star and we have column of numbers
# This numbers indicates the number of variables that the function is considering
# By default regsubset only include model upto 8 variables.
# But we can add more by providing an argument called "nvmax"

# Now using all the variables in the data
dim(hitterdata)

# We have 20 dimensions and we are fitting salary, so we need to use the rest 19 variables
regfitfull = regsubsets(x = hitterdata$Salary~. , data = hitterdata,nvmax = 19)
summary(regfitfull)

regfitfullsummary = summary(regfitfull)

names(regfitfullsummary)

# We can see that inside the summary, we have also R^2, RSS , adjusted R^2, Cp and BIC
# We can use these to try and select the best overall model
# Lets see what is the Rsquared value for each subset we have calculated
regfitfullsummary$rsq

# As expected R^2 statisti increaes as we include more variables.
# Its better we plot these R^2 statistic instead of just verifying in th tcommand line

# Splitting the Gui in to 2x2 gui scheme
par(mfrow=c(2,2))

?plot

# Using the "l" inside plot function with type will let us plot a line graph instead of
#   dotted scatter plot
# Plotting the RSS with number of variables
plot(regfitfullsummary$rss,xlab = "Number of variables",ylab = "RSS",type = "l")

# Plotting the Adjustted R^2 with number of variables
plot(regfitfullsummary$adjr2,xlab = "Number of variables",ylab = "Adjusted R^2",type = "l")


# Another function to the rescue is point function which works like the plot command ,
#   except that it puts points on a plot that has already been created, insted of creating a new plot
# which.max() is another function which can be used to identify the locaiton of maximum point of a
#    vector.We will see which is the max point in the vector of values we  have plotted and then
#    use that index to use it inisde the points() function.
#
which.max(regfitfullsummary$adjr2)

# It says that 11th index has the maximum value

# Now using the points function to plot the red dot on the existing plot
?points

# The arguments inside the points function are
# First argument is the index where the modification of datapoint's apperance takes place to be
# Second argument is the dataset that we are using and on which the indexes value has to be
#   derived from
# Actually the first 2 arguments here means that x and y coordinate on the plot which we want
#   to plot on
# And this function will plot on the current plot that is activated. Now the second one is
#   activated and it will use that as reference until we use another plot function
# col is just argument that gives colour to the point
# cex is the argument that increases the dot size that we are plotting.
#   Giving 2 value is the best one.
# pch is the plotting character, here 20 stands for circle, we can also have square
#   or various other things.
points(11,regfitfullsummary$adjr2[11],col="red",cex=2,pch=20)


# In a similar fashion we can plot the Cp, and BIC and indicate the models which has the
#   smallest statistic using which.min()
# Using line plot
plot(regfitfullsummary$cp,xlab = "Number of predictors",ylab = "Cp value",type = 'l')
# Finding the minimum of the cp values
which.min(regfitfullsummary$cp)
# 10th one is the minimum index, Now using points function to plot this point on
#   the currently activated grpah that is nothing but the cp graph
points(10,regfitfullsummary$cp[10],col="green",cex=2,pch=20)


# Now plotting the Bic the sameway we have dont for the Cp
plot(regfitfullsummary$bic,xlab = "Number of predictors",ylab = "Bic value",type = "l")
# Finding the minimum of the bic
which.min(regfitfullsummary$bic)
# Minimum one is 6, so using this 6 index on the current plot to plot the point plot
points(6,regfitfullsummary$bic[6],col="magenta",cex=2,pch=20)



#####
## Until now we are using the summary of the fitted model that is from the regsubsets
## We can use the actual output of the regsubests instead of summary to use the
##  built in plot command which can be used to display the selected variables

par(mfrow=c(2,2))

# Help to know more about the regsubsets plot
?plot.regsubsets

# Here it plots a plot which has variables on the x axis and what ever the criteron on the Y axis
# The plot shows a white and black plot
# It can be interpreted in the following way. For a given Y value and if we are checking for a
#    particular R^2 value, we see the model that it gives that R^2
# In that model if we go along the X axis (Draw a line which is parellel to X axis at that
#   particular Y value), then if a particular variable is present in that model we see a
#   "BLACK" colored square on the plot.
# If that particular variable is not present , then it will be "WHITE" colored square
plot(regfitfull,scale = "r2")
plot(regfitfull,scale = "adjr2")
plot(regfitfull,scale = "Cp")
plot(regfitfull,scale = "bic")

# Check the above plot and in that for bic plot , we see that we have several models share a
#   BIC close to -15-, However the model with lowest BIC (Lowest means higher negative value),
#   has only 6 variables
# They are AtBat,Hits,Walks,CRBI,DivisionW,PutOuts
# We can use the coef() function to see the coefficeint estimates associated with this model
# Coef function when used on this regsubset needs another argument to let know how many
#   predictors are to be there.
coef(regfitfull,19)



######
## Forward and Backward Stepwise Selection
## We can use the same regsubsets() function to perform forward stepwise or backward
##  stepwise selection
## We just have to add another argument "method = "forward"" or "method="backward""
?regsubsets
# The default method is exhaustive search method, which means looking through all the
#   possible combination of models

# Using the forward selection method, means going from 0 predictors to full predictors

regfitforward = regsubsets(x = hitterdata$Salary ~ .,data = hitterdata,nvmax = 19,method = "forward")
regfitforward
summary(regfitforward)

regfitbackward = regsubsets(x = hitterdata$Salary ~ .,data = hitterdata,nvmax = 19,method = "backward")
regfitbackward
summary(regfitbackward)


# Observing the coefficients of the said models for both forward and bakcward
coef(regfitfull,7)
coef(regfitforward,7)
coef(regfitbackward,7)

# We can see that using forward ,backward and exhaustive might result in different coefficeints
#   and also different predictors.
# For this particular data the best one variable through six variable models are each identical
# But for the best seven-variable models is different from subset selection, forward and backward



########
### Choosing Among models using the Validation set approach and Cross Validation
##
## We already saw how to choose models using Cp, Bic and adjusted R^2
## We can use the summary over the regsubsets and use that to plot the differnt type of plots
#
# But we will now consider how to select the best model using the validation set and
#   cross-validation approaches.
# In order to use this approach we have first split our test and training set with indexes

########################
####### USING VALIDATION SET APPROACH
########################


set.seed(1)

# Here in this type of train and splitting method
# We are creating a random true and false index vectors with the size of number of rows
#   of hitter data
# We are randomly splitting the testing and trainign set with replacement and we have no
#  control over the size of trian and test.
train = sample(c(TRUE,FALSE),nrow(hitterdata),rep = TRUE)
train_type2 = sample(1:nrow(hitterdata),0.7*nrow(hitterdata))
# Inversing all the indexes apart from the train to test
test = (!train)
# By observing the mean we can say that this much percentage of observations are true
#   and else are false
mean(train)

# Now applying the regsubsets over the data only considering the training set
# OK THIS DUMB MISTAKE CAN BE MADE - Dont put in the formula with $,
# like dont put "hitterdata$Salary ~." only keep "Salary ~." , No need to specify the name
#   and column of the formula by referencing it fomr the dataset even if you didn't attach the data
# I always thought if we dont attach column names we have to refer the formula with $ for
#   the y we want to predict
regfit_best = regsubsets(x = Salary~.,data = hitterdata[train,],nvmax = 19)
#####
## Another way we wont get this error "variable lengths differ (found for 'AtBat')"
## regfit_best = regsubsets(x= hitterdata$Salary[train]~.,data=hitterdata[train,],nvmax=19)
## Use the above and we wont get that error. Basically why that is happening is that
## in previous when we fit backward and forward we are not using any subset of data
## So hitterdata$Salary is taking all the indexes while we only want the indexes that are
##  given by train.
regfit_best
summary(regfit_best)

# Now computing the validation set error fomr the test set
# But we have to predict this validation error for each of the model that we have fit
# So we need to make a matrix which will be the same size of the dataset

test_matrix = model.matrix(object = Salary~.,data = hitterdata[test,])
# This model.matrix function is used in many times for building an "X" matrix fron the data.
?model.matrix()

# model.matrix takes its first argument as follows - it takes a formula
# In that formula if you write x~a+b it will include the a and b columsn but will not
#   include x, as x is the one we are calculating
# if you want everything just keep ~.
# And the second argument is the data from which given predictors have to be chosen

# Another way of getting all the columns except the salary column is liek this
# This type of method is already explored but i forgot in which chapter and which problem
names(hitterdata)
# We want all columns but "Salary" column
names(hitterdata) %in% c("Salary")
# From the above command we get a boolean array which has false in all except where we have salary
# We are just going to negate that
!(names(hitterdata) %in% c("Salary"))

# Now lets use this following data
testmatrix2 = hitterdata[test,!(names(hitterdata) %in% c("Salary"))]

dim(test_matrix)
dim(testmatrix2)

# We have differences in the dimensions only because we used different methods, in case
#   of using model.matrix funtion, it adds another column called Intercept

typeof(test_matrix)
typeof(testmatrix2)
# Our test_matrix is double while new one is list
?double
is.double(test_matrix)

###
## Now we are going to run a loop for each size i and multiply them into the appropriate
##  columns of the test model matrix to form predictions and compute the MSe

# So we are creating a dummy vector to store the errors for each of the best fit model
#   we get from the regbestfit, (Here we are using )
valerrors = rep(NA,19)

# Here what we are doing is , going from 1st best fit model to model where all 19
#   predictors are selected
# For each given such model , firstly finding the coefficients of the said model
# Then using that model coefficients to predict but on the testmatrix we have created.
## The manner we are doing this is we are using only the names that are present from
##  getting the coefficients.
## Then we are doing a matrix multiplication with the coefficients and testmatrix to get a
##  single column vector of predictons (Essentially we are multiplying matrix of [129x19]x[19x1]
##  to get a single column vector of [129x1] size)
## This %*% operator will act as vectorized form of multiplication
# After doing the matrix multiplication we are then taking the squared error and its mean to get MSE
for (i in 1:19) {
  coeffi = coef(regfit_best,id = i)
  pred = test_matrix[,names(coeffi)]%*%coeffi
  valerrors[i] = mean((hitterdata$Salary[test]-pred)^2)
}

valerrors
coeffi
names(coeffi)

?'%*%'

pred
dim(pred)

##########
### EXTRA KNOWLEDGE
# To see how the multiplication occurs if we dont use matrix multiplication method, we are
#   doing a test type of thing here
# Here we have multiplied the dataframe with the coefficients without using "%*%"
# Here it how it behaves
# Take that our matrix is like this
# 1 2 3
# 2 3 4
# 2 1 4 --> Let this matrix be A , dimension is [3,3]
# We are multiplying with B matrix like this
# 1
# 3 ---> This matrix is B, dimension is [2,1]
# Now when you do A*B , then B is forced to match the dimension of A and it is replicated like this
# 1 1 3
# 3 3 1
# 1 1 3 ---> This will be our new B' , This is created by taking this new B and dragging it
#   along the column and if the column is filled the next element is continued in the next column
# Now A*B will be one to one multiplication
# 1*1 2*1 3*3
# 2*3 3*3 4*1
# 2*1 1*1 4*3 --> This will be our A*B
# And that is what we are getting in this predtest
predtest = test_matrix[,names(coeffi)]*coeffi
predtest
##########



## We have to go through all these complex matrix multiplicaiton and sh*t because we cannot
##    use predict method in regsubsets
# Since we are going to use this function time to time lets create a function that can be reused

predict_regsubsets = function(object,newdata,id,...){
  # The following line of code will extract the formula that we have typed in our given object
  # Here the object is the regsusbset function that we are going to pass
  # then for every regsusbset output we have a call attribute which is of type "language" and
  #   inside that attribute we have to entire formula and the variables stored
  # Of that stored call, its [[2]] index will give us formula character
  # But we want that to be stored as formula so we shall convert that string to forumula using
  #   as.formula
  formulahere = as.formula(object$call[[2]])
  matrixhere = model.matrix(object = formulahere,data = newdata)

  # id is for selecting the coefficients of which number of variable model, like do we want model
  #   which is best with 10 predictors or model with 11 or whatever.
  coeffi = coef(object = object,id = id)
  xvariables = names(coeffi)
  matrixhere[,xvariables]%*%coeffi
}

# Ok now back to model selection
# Among the errors that we have generated and stored in valerrors above we have to find the best
#   one, by selecting which one has the lowest MSE
valerrors
which.min(valerrors)
# As we can see the MSE of the 10th model which has 10 variables has the lowest MSE
#   The coefficients of the MSE are
coef(regfit_best,10)

# Now using the validation approach that we have performed it recommended us 10 variable model
# Lets use the entire data instead of training data and then calculate the coefficients

regfit_best_validation_full = regsubsets(Salary~.,data = hitterdata,nvmax = 19)
# Displaying the output of the best coefficients of the model with 10 predictors
coef(regfit_best_validation_full,10)

# If we observe we have different coefficients when we used all the dataset instead the case when
#    we used only some percent as training data.This is bound to happen but we always has to use
#    by splitting the data into training and testing since this will help us in avoiding overfit.




########################
####### USING CROSS - VALIDATION SET APPROACH
########################
## In validation we took the data and split into training and testing in to two groups.
## In cross validatino we are going to split into 10 parts or say n parts and then we are going
##  to use n-1 parts as trainign and uset the rest 1 part as testing
## Lets do a 10 fold cross validation
## That means lets split our data in to 10 parts

set.seed(42)
k=10
folds = sample(1:k,nrow(hitterdata),replace = TRUE)
folds

# Here folds is a vector which has numebers from 1 to 10 that is distrubuted randomly. The length
#   of folds is same as the number of rows our hitter data has.It will be used to assign each
#   observation of hitterdata to a particular fold and in this way we created 10 different groups
#   of data.

cv_errors = matrix(data = NA,nrow = k,ncol = 19,dimnames = list(NULL,paste(1:19)))
cv_errors

# cv_erros is a matrix we have created to log the cross validation error for each model and also
# for each fold. So for a particular group of data we are using as trainig and testing we are
# going to fit the model with differnet number of predictors, Finally we are going to sum all the

# So an element cv_errors[i,j] means cross validation error for ith fold of data and the model
#   has j number of predictors in it with its besst selected predictors that is obtained from the
#   regsusbsets function

for (i in 1:k) {
  # Here we are going to perform best_fit for each fold and in each fold we are selecting data
  #   all except where the fold is equal to i here. So for 1sst fold all data will be selected
  #   except where ever fold is equal to 1, In a way we are seelcting training data here where
  #   fold is not equal to 1
  best_fit = regsubsets(x = Salary~.,data = hitterdata[folds != i,],nvmax = 19)

  # Now for this current best_fit model for this ith cross valid data, we are doing following
  #   First we make use of the function we have created which can give predictions as an single
  #     column vector of predictions that we can compare with actual one to take MSE. Here we are
  #     doing this by giving the model as first argument , the data from which predictions have
  #     to be generated from.(Selecting the test data where the fold == current i value) and also
  #     lastly we are looping the models from 1 to 19 predictors best models , so for each we
  #     have to have an index to output the coefficients from coef function
  #
  for (j in 1:19) {

    predhere = predict_regsubsets(object = best_fit,newdata = hitterdata[folds == i,],id = j)

    # After getting predictions for jth best model we are getting MSE by subracting the actual
    #   values from the predictions we have made and we are squaring them and taking mean of all
    #   the observations to get the MSE. Then we are going to store in the cv_errors we have
    #   created earlier. Here the importtatnt point to note is that we have select observations
    #   which are considered to be test data, that means where ever the folds index is equal to i
    cv_errors[i,j] = mean((hitterdata$Salary[folds == i] - predhere)^2)

  }

}

# Now you can see that the cv_errors has been populated.
cv_errors

# We are doing all the steps to do one thing and that is selecting a model with x number of
#   predictors where x is where the MSE is lowest.
# So we are going to take average of all the MSE's of a particular x numbered variable model
#   for all the given 10 folds

# This can be acheived by apply() fucntion
?apply

# apply() takes data as first argument, second argumetn is either 1 or 2, 1 means apply
#   the third argument which is function row wise and 2 means apply the third argument
#   which is function in column wise
#
# We want the averages for all the rows (which are folds , k times) for a particular column.
# So we have to use 2 in the second argument of apply
mean_cv_erros = apply(cv_errors, 2, mean)

mean_cv_erros
# Finding the lowest mean of errors is for 11 variabled model.
# So we are gonna select model which has 11 variablesk
which.min(mean_cv_erros)

plot(mean_cv_erros,type = 'b')


# lets use all the data now insted of just data in the training set
regfit_best_crossvalidation_full = regsubsets(x = Salary~.,data = hitterdata,nvmax = 19)
# Coefficients of 11 variabled model is
coef(regfit_best_crossvalidation_full,11)







#######################
########## RIDGE AND LASSO REGRESSION
#######################
# Ridge and lasso regression uses another parameter to reduce the size of coefficients and also
#   reduce the overfit that is produced by normal regression.
#
####
## We use glmnet package in order to preform the ridge regression and lasso
install.packages("glmnet")
?glmnet
??glmnet
# Loading the library glmnet
library(glmnet)
?glmnet

## This function glmnet has different syntax from the normal regression of glm and lm
## The main different is that we must pass in an x matrix as well as y venctor  and we dont use
##    y~x syntax. We will now perform ridge regression and the lasso in order to predict Salary on
##    the Hitters data. As usual we have to remove the NA values to be removed from the data.
##

x = model.matrix(Salary~., data = hitterdata)[,-1]

# Creating a matrix that has all the columns except for the Salary column, Also when we create
#   this the matrix function creates this intercept column where we have 1 throughout all the
#   rows. Inorder to avoid that we are neglecting the first column with [,-1] - before its index.

y = hitterdata$Salary

# This x and y contains no NA rows since we have eliminated them by using na.omit(hitterdata)
sum(is.na(x))
sum(is.na(y))

# We can see that sum of all NA's in x and y is 0

# So here matrix x is our training set containing only predictors and y is the output we want to
#   predict.


######################
##### RIDGE REGRESSION
######################

#####
## The glmnet() funciton has argument called alpha, This alpha indicates whether we are fitting a
##  Ridge regression or lasso regression. If [ alpha =1 then its LASSO] [alpha=0 then its RIDGE]


## Fitting a ridge regression model
library(glmnet)
grid = 10^seq(10,-2,length=100)
grid

?seq

# Sequence function to generate uniformly spaced values , first argument is from which value to start
#   Second arguemnt is at which value to stop. Thrid arguemnt is how many values we want.
#   In the below
#   sequence example we want 100 equally spaced values, from 10 to -2 and they have to be 100 values
seq(10,-2,length = 100)

# And then we are making a grid where we are raising 10 to the power of this sequence
#   generated values.
# ALSO IMPORTANT POINT is that the glmnet function standardizes our data by default , so if one doesnt
#   want that we have to give the value of argument standardize to be FALSE
?glmnet
## The arguments for glmnet are as follows
##  The first argument is the predictors data whcih is in matrix form, it has dimension of
##    numofobservations x numofpredictors.
##  Second argument is the response variable. Should have same number of rows as the x
##  Third argument is alpha which says either lasso or ridge (Lasso when alpah =1 and ridge when its 0)
##  Fourth argument is lambda which can be either vector or single value.This is literally the lambda
##    that we use in the ridge and lasso and by varying this we actually get different models
##    Here in this example we have created a  vector of lambda's ranging from 10^10 to 10^-2
ridge_mod = glmnet(x = x,y = y,alpha = 0,lambda = grid)
dim(coef(ridge_mod))

# We have 100 values of lambda which are nothing but grid values here and also for each lambda
#   we have a ridge regression fit.The fit is with all the coefficients but the dimesion is
#   20x100 because we have another coefficient for intercept.

ridge_mod
# Ridge fit that we have done above has list of 12 elements. and if we call it just a function like
#   that it will gives us 3 column like outputs, these columsn are first is df- which means degrees
#   of freedom and second is % dev which says percentage of deviantion and the third one is lambda
#   which we are supplying externally
#
# IF we want thee coefficients of a particular value of lambda we can use the following
ridge_mod$lambda[50]
# This gives us the value of lambda at the 50th grid value that we have supplied.If we need coefficient
#   at this particular lambda we have to this

# The following line of code will give us the coefficients of our ridge fit for the lambda value of 50
coef(ridge_mod)[,50]
# The following line of code will give us how the first predictor , here "AtBat is varying as we change
#   our lambda from 10^10 to 10^-2
coef(ridge_mod)[2,]

############
#### L2 NORM
#############
# The following is sqrt(c1^2+c2^2....cp^2) where c0, c1 are the coefficients of the predictors
#   This is nothing bu thte l2 norm of the coefficients. Search google for what is l2 nrom
#
# We have larger l2 norm for coefficients as the lambda value keeps on decreasing.
# We generally plot this type of plot for all the coefficients
# On X axis lies the ratio of this (l2norm with ridge of coefficients / l2 norm without ridge for coef)
# On Y axis lies the standardized value of each coefficient, for each coefficients we have a plot
sqrt(sum(coef(ridge_mod)[-1,50]^2))



ridge_mod$lambda[60]
coef(ridge_mod)[,60]
coef(ridge_mod)[-1,60]

# As we can see the lambda value for the 60th value of grid is only 705 but its l2 norm for coefficents
#   is increased to 57, previously it is only 6
sqrt(sum(coef(ridge_mod)[-1,60]^2))


# We can use predict method in this glmnet library, we can use predict to predict for any value of
#   our own lambda
predict(object = ridge_mod,s = 50,type = "coefficients")[1:20,]


####
## Now lets split the dataset in to test and train and do the fitting of ridge again.

set.seed(42)
# Creating indexes for test and train usig sample function
train = sample(1:nrow(x),nrow(x)/2)
test = (-train)

# Splitting the dataset , or more like creating different datasets
y_test = y[test]
x_test = x[test,]
y_train = y[train]
x_train = x[train,]


# Fitting our model of ridge regression on training data, but with thresold until 1e-12 for gradient
#   descent
ridge_mod = glmnet(x = x_train,y = y_train,alpha = 0,lambda = grid,thresh = 1e-12)
ridge_mod$lambda[50]

# As ridge has predict function inbuilt, it has some important arguments
#   First argument is asusual the object is our model, and newx is on the x_test dataset that we want
#   Third argument s, means for which value of lambda we want to predict. s is lambda value here.
?predict.glmnet

# So we are predicting for the value of 4 lambda
ridge_pred = predict(object = ridge_mod,newx = x_test,s = 4)

# Taking the MSE of the errors for TEST dataset
mean((ridge_pred - y_test)^2)




























