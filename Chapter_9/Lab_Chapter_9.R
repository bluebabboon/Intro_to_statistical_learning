
#############################
## SUPPORT VECTOR CLASSIFIER
#############################

# We have to use this weird library called e1071, like wtf, who names library e1071
#   And this has svm() function that can be used to fit support vector classifier
#   when the argument "kernel = "linear" " is used. The default argument for this kernel
#   is linear
#
# Also apart from that we have cost argumetn that allows us to SPECIFY THE COST OF A VIOLATION
#   TO THE MARGIN. When it is SMALL then the margins will be WIDE eand many support vectors
#   will be on the margin or they will violate the margin. When the cost argument is large
#   then the , then the margins will be narrow and there will be few support vaectosr on the
#   margin or violating the margin


# Lets see what svm() function can do

set.seed(1)

x = matrix(rnorm(20*2), ncol = 2)

x

y = c(rep(-1,10), rep(1,10))

y

x[y==1,] = x[y==1,] + 1

x

# Plotting the x data with the colour argument
plot(x, col=(3-y))

# We can see that the classes are not lineary seperable here. Next now we fit the support
#   vetor clasifier. Inorder for the svm() function to perform classification we
#   must encode the response as a factor variable

# Creating a dataframe with the response encoded as factor

dat = data.frame(x=x,y=as.factor(y))

View(dat)

library(e1071)

# The argument scale =FALSE tells smv() function not to scale each feature to have mean zero
#   or standard deviation, depending on one's requirement one might prefer to use scale=TRUE

svmfit = svm(y~., data = dat, kernel = "linear", cost=10, scale = FALSE)

svmfit

summary(svmfit)

svmfit$coefs

# Plotting this svm is pretty easy. It takes two arguments first is the svmfit model where
#   we have fit the svm and also the dataframe that we have supplied to the svmfit
#
# Since we have used a linear kernel we are going yto ge ta linear decision boundary looks like
#   this in the plot. We see that in this case only one observation is misclasisfied
#
# Also observe that the support vectors are plotted in crosses and the remaining observations
#   are plotted in circles. We see that we have total of 7 support vectors here that
#   decides the decision boundary. To see them we have to also specify the limits of x and
#     y axis or they wont be visible
plot(svmfit, dat, xlim=c(-2,3),ylim = c(-2,3))

# To determine the identities of the support vectors we can use the index of the svmfit

svmfit$index
# This will gives us the indexes of the X data points that are forming the support vectors

summary(svmfit)
# This tells us that a linear kernel is used with cost=10 and that there were 7 support
#   vectors , 4 in the first class and 3 in the second class

# What if we insted used a smaller value of the cost parameter

svmfitsmallcost = svm(y~., data =dat, kernel = "linear", cost = 0.1, scale = FALSE)

svmfitsmallcost

summary(svmfitsmallcost)

svmfitsmallcost$index

# We see that support vectors have increased
plot(svmfitsmallcost, dat, xlim = c(-2,3), ylim = c(-2,3))


#####
## TUNING
# The e1071 library has another built in function tune() to perform cross validation
# By default tune() performs a ten fold cross validaiotn on a set of models of interest

# To use this function we pass a relevant information about the set of models that are under
#   consideration. The following command indicates that we want to compare SVMS' with linear
#   kernel , using a range of values of Cost parameter

set.seed(1)

tune_out = tune(svm, y~. , data = dat, kernel = "linear",
                ranges = list(cost=c(0.001, 0.01, 0.1, 1.5, 10, 1000)))

tune_out

summary(tune_out)

# This is a very good method to tune out different parameter settings and get the correct
#   value for less error from cross validation

# We see that for the cost value of 0.01 we have the lowest error (this error is
#   cross validation error)
# The tune() function stores the best model obtained which can be accessed as

bestmodel = tune_out$best.model

bestmodel

summary(bestmodel)
# The best model has 16 support vectors and its cost is 0.1, kernel is linear and gamma is 0.5

# The predict() function can be used to predict the class label on a set of test
#   observations at any given value of cost parameter

xtest = matrix(rnorm(20*2), ncol = 2)

# Select 20 from -1, and 1 with replace is true
ytest = sample(c(-1,1),20 , replace = TRUE)

xtest
ytest

xtest[ytest == 1,] = xtest[ytest==1,]+1

# Increasing the values of xtest for that y=1 and adding +1 to those x values

# Creating the same dataframe using that xtest and ytest and remember that y should be given
#   as FACTOR VARIABLE
testdat = data.frame(x=xtest, y=as.factor(ytest))

# Now we predict the class labels of these test observations . Here we are using the best model
#   we got from the cross validation in order to make the prediction

ypred = predict(bestmodel, testdat)

table(ypred, testdat$y)

# So we have correctly predicted 17 out of 20


# What if we have used cost = 0.01

svmfit = svm(y~., data = dat, kernel = "linear", cost = 0.01, scale = FALSE)
ypred = predict(svmfit, testdat)

table(ypred, testdat$y)

# If we decrease cost we have additional observation that is misclassified


# Now consider where our data is lineary seperable by further seperating the data

x[y==1,] = x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch = 19)

# Now the observations are barely linearly seperable. We fit the support vector classifier and
#   plot the resulting hyperplane using a very large value of cost so that no observations are
#   misclassified

dat = data.frame(x=x, y= as.factor(y))

svmfit = svm(y~. , data = dat, kernel = "linear" , cost = 1e5)
svmfit

summary(svmfit)

# Since we have decreased the cost so much, we have become stricter in choosing the best
#  support vectors so we have only chosen 3, 1 from class1 and 2 from class 2

plot(svmfit, dat)

# We can see that margin is very narrow.

svmfit = svm(y~. , data = dat, kernel = "linear", cost = 1)
summary(svmfit)



#############################
## SUPPORT VECTOR MACHINE
#############################
# Everything remains the same but we will use a different kernel now, using "polynomial" instead
#   of a linear one. Also we have a look at "radial" kernel as well.
#
# In the former case we also use degreee argument to specify a degree for the polynomial
#   kernel , (this is d in the text book) and in latter case we use gamma to specify the
#   value of gamma (this is gamma in textbook) for the radial basis kernel

set.seed(1)

x = matrix(rnorm(200*2), ncol = 2)
x[1:100,] = x[1:100,] +2

x[101:150,] = x[101:150] -2

y = c(rep(1,150), rep(2,50))

dat = data.frame(x=x, y=as.factor(y))

plot(x, col=y)

# Now split the data randomly in to trainging and testing groups
# And then fit the training data using svm() function with a radial kernel and gamma=1

# Select 100 indexes from 1 to 200
train = sample(200,100)

svmfit = svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
summary(svmfit)

# Total of 41 support vectors
# Lets increase the value of cost and we can reduce the number of training errors. However
#   we might have a irregular boundary which might over fit the data

svmfit = svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
summary(svmfit)

plot(svmfit, data = dat[train,])


# We can use cross validation usig tune() to select the best choice of gamma and cost
#   with radial kernel

set.seed(1)

# We are giving ranges argument a list of the parameters that we want to pass to svm
#   but at the same time we want to select multiple values and do cross validation among them
tune_out = tune(svm, y~., data = dat[train,], kernel = "radial",
                ranges = list(
                              cost = c(0.1, 1, 10, 100, 1000),
                              gamma = c(0.5, 1, 2, 3, 4)
                              )
                )


summary(tune_out)

# To get the best parameters
tune_out$best.parameters

# So we are going to use this model which is best model and then predcit using that

bestmodel = tune_out$best.model

ypreds = predict(bestmodel, newdata = dat[-train,])

table(pred = ypreds, true = dat$y[-train])

# Out of 100 observations 11 are misclassified and 89 are correct so 11% is the misclassificaiton
#   rate





#############################
## ROC CURVES
#############################

# The library ROCR can be used to plot the ROC curves ,
library(ROCR)

# We are writing a short function to plot an ROC curve given a vector containing a
#   numerical score for each observation, pred and another vector contianing the class label
#   for each observations, truh
#
# ROC curve is generated by varying the threshold of the predicitons with the truth
#   For different values of threshold we get different false positive rate(fpr) and
#   true positive rates(tpr)


# SVM's however gives us class label predicitons by default
# If we want the actual values (the fitted values) for  a given SVM we have to give another
#   argument called "decision.values = TRUE" when fitting an SVM

rocplot = function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)
}

# SPECIFY decision.values=TRUE in the model creation step
svmfit = svm(y~., data = dat[train,], kernel = "radial", gamma = 2,
             cost = 1, decision.values = TRUE)

svmfit
summary(svmfit)


# SPECIFY decision.values=TRUE in the prediction step
ypreds = predict(svmfit, dat[train,], decision.values = TRUE)

# If we call the ypreds then it will gives us the class predicitons , not the actual
#   fitted values, to get that we have to add another step which will get the attributes
#   of this ypreds
ypreds

?attributes

# Just taking a look at the attributes of this ypreds
attributes(ypreds)

# We only want the decision values and we are storing them in a variable called fitted values
fittedvalues = attributes(ypreds)$decision.values

fittedvalues


# Now we can produce the plot
par(mfrow = c(1,2))

# Passing the fitted values in first argument, actual in second argument and extra
#   arguments in form of "..." , this "..." is to be specified in the function argument
#   declaration and then use these same "..." on whereever you want them to given as input.
rocplot(fittedvalues, dat$y[train], main= "Training data")

# By increasing the gamma we can produce a more flexible fit and generate further improvements
#   on the accuray

svmfit_flex = svm(y~., data = dat[train,] , kernel = "radial", gamma = 50,
                  cost = 1, decision.values = TRUE)

summary(svmfit_flex)

ypreds = predict(svmfit_flex, dat[train,], decision.values = TRUE)

fittedvalues = attributes(ypreds)$decision.values

# By giving argument add=T we are saying that we want both the plots in the same on
# And the col gives colour argument
rocplot(fittedvalues, dat[train,"y"], add = T, col = "red")


# However all these ROC's are on the training data. We are really more interested in the level
#   of prediciton accuracy on the test data. When we compute the ROC curves on the test dtaa
#   the modle with gamma = 2 appears to provide the most accurate results

fittedtest = attributes(predict(svmfit, dat[-train,], decision.values = TRUE))$decision.values
rocplot(fittedtest, dat[-train,"y"], main = "Test data")

fittedtest = attributes(predict(svmfit_flex, dat[-train,],
                                decision.values = TRUE))$decision.values
rocplot(fittedtest, dat[-train,"y"], add = TRUE, col = "red")




#############################
## SVM WITH MULTIPLE CLASSES
#############################

set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0,50))

x[y==0,2] = x[y==0,2]+2
dat = data.frame(x=x,y=as.factor(y))

x
y

View(dat)

# So intotal we have 250 observations

par(mfrow = c(1,1))

# Giving col as a input based on y value, col can also be stated by 1, 2,3, instead of calling
#   them "red" etc, each value has a colour value as well
plot(x, col=(y+1))

svmfit = svm(y~., data = dat, kernel = "radial", cost = 10,
             gamma = 1)

# This plot will automatically plots the decision boundary and fills it nicely
#   So no need to worry in classifying more than 2 classes.
plot(svmfit, dat)





######################################
## APPLICATION TO GENE EXPRESSION DATA
######################################

# Lets examine the Khan dataset, which has number of tissue samples corresponding to four
#   distinct types of small round blue cell tumors
# For each tissue sample gene expression measurements are available
# The dataset consisits of training data xtrain and ytrain and testing data, xtest and ytest


library(ISLR)
names(Khan)

View(Khan)

dim(Khan$xtrain)

# So total 63 rows and 2308 columns or predictors

length(Khan$ytest)

# And 20 examples on test

table(Khan$ytrain)

table(Khan$ytest)


# Here y is 4 class variable and x is 2308 predictors
# Lets use support vector approach to predict the cancer subtype using gene expression
#   measurements. In this dataset there are very large number of features relative to the
#   number of obsrvations, This suggests that we should use a linaer kernel
#   because the additional flexibilty that will result from using a polynomial or
#   radial kernel is not needed

dat = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))

svmfit = svm(y~. , data = dat, kernel = "linear", cost = 10)

svmfit

summary(svmfit)

# Out of the 63 observations we have 58 of them as support vectors

# This gives us the classes that are fitted using the svm approach
svmfit$fitted

table(svmfit$fitted, dat$y)

# We see that there are no training errors Because there are large number of variables relative
#   to the number of observations
#
#   Which means that it is easy to find hyperplanes that fully separate the classes
#   We are interested not in the performance of the classifier on traning set, but we are
#     more interested in test set performance.

# Making test data set with same dimension as the input dataframe

testdat = data.frame(x= Khan$xtest, y = as.factor(Khan$ytest))

testpreds = predict(svmfit, newdata = testdat)

table(testpreds, Khan$ytest)

# There are only 2 errors from this and the cost is 10, for this setting. We can use
#   the cross validation to select the best cost using the tune function if we want.

