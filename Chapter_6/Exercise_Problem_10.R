# Creating a simulated dataset to check this. To see that as the number of features used in a model
#   increases, the training error will necessarily decrease but the test error my not
#
# To explore that in this simulated data set


## a)
# Generate a dataset with p = 20 features and n = 1000 observations and associate a quantitative
#   response vector generated according to this model Y = X* beta + eps
#   Where beta has some elements exactly equal to zero

# We are going to use the replicate function to create a 20 vectors each of size 1000 and then assign
#   it to a variable. The replicate is kind of like a wrapper using sapply . It takes two arguments
#   mainly
#   First argument is the vector or list over whyich the second argument function is to be applied for
#   The second argument is the funciton that is being applied to each element of the first argument
#     elements
#  As per the documentation we had that replicate is a wrapper for the common use of sapply for
#   repeated evaluation of expression ( which will usually involve random number generation)
#   replicate(x, expression, simplify = "array")

?replicate

# Here we want to create a 20 column matrix with each column having 1000 numbers sampled from
#   normal distribution
xdata = replicate(20,rnorm(n = 1000))
# This above command creates a matrix , to check whether we actually got a matrix
is.matrix(xdata)

# Another way of generating data in matrix format, Creating all the 20000 elements and then saying that
#   we need only 20 columns will let the matrix sort out how to cut them in to columns
xnew = matrix(rnorm(20000),ncol = 20)

?data.frame
# Creating a dataframe from the matrix above, using this simple data.frame() function
xframe = data.frame(xdata)
is.data.frame(xframe)

# Now we have to create the Y , or we can say response for this particular X value
# For that we have to first create coefficients array and then assign some of them to zero
#
#   Creating random coefficients sampling from 1:20, and we need 20 elements and also having that
#     replace is true
#   And then assiging the elements fo 3rd 5th 13 and 15 to zero
coeffs = sample(1:20,20,replace = T)
coeffs[c(3,5,13,15)] = 0

# Checking the coeffs at last
coeffs


# Now generating the response vector Y, along with error
eps = rnorm(1000)

# Doing the matrix multiplicaiton to get the response vector
# Y = xdata * coeffs + eps ==> [1000x20] x [20x1] , gives us 1000x1 matrix which is our Y
responseY = xdata %*% coeffs + eps



## b)
# Splitting the dataset in to training set of 100 observations and test set of 900 observations
# creating a trian indcies by sampling them from sample function
train = sample(x = 1:1000,size = 900)
test = -(train)

xtrain = xdata[train,]
xtest = xdata[test,]

ytrain = responseY[train]
ytest = responseY[test]


# Giving column name y to the response , or else it will just create a random name
# Combining the matrix and vector to create dataframe that we are going to use further
trainframe = data.frame(y = ytrain,xtrain)
testframe = data.frame(y = ytest,xtest)


# Creating model.matrices for both training and testing to use them further in prediction
# We need the intercept here because our matrix multiplication with the coefficients will have
#   an intercept value that needs to be multiplied by 1, so dont keep [,-1] at the end
trainmatrix = model.matrix(y~.,data = trainframe)
testmatrix = model.matrix(y~.,data = testframe)


## c)
##
##
# Performing best subset selection on training set and then plot the training set MSE with the
#   associated best model of each size
library(leaps)
reg_model = regsubsets(y~.,data = trainframe,nvmax = 20)
reg_model

summary(reg_model)


## The training errors for each best model from i=1 to i=20 and storing them in trainerrors vector
trainerrors = rep(0,20)

# As regsubsets doesn't have predict method, we can either create one or just get the coefficients
#   of a particular model size and then multiply with testdataset and then get predictions
# Getting the coefficients of the ith best model with coef() function
# Multiplying the coefficients which has intercept with the model.matrix we have created on the
#   training set. Here we are doing matrix multiplication (Do not forget intercept)
# Then getting the Mean of the squared error with the ytrain and then storing in the trainerrors
#   vector in the current ith element
for (i in 1:20) {
  coeffi = coef(reg_model,i)
  predsi = trainmatrix[,names(coeffi)] %*% coeffi
  trainerrors[i] = mean((predsi - ytrain)^2)
}

# Plotting the MSE for training set on all different models we have created
plot(trainerrors,type = "s")
# The minimum error for training is on 20th one. Because we are fitting the model on training set
#   So the error on training set should be lowest for the model which has maximum number of predictors
which.min(trainerrors)



## d)
# Doing the same thing as above but using the model to get the test mse
testerrors = rep(0,20)

for (i in 1:20) {
  coeffi = coef(reg_model,i)
  predsi = testmatrix[,names(coeffi)] %*% coeffi
  testerrors[i] = mean((predsi - ytest)^2)
}

# Plotting the MSE for testing set on all different models we have created
plot(testerrors,type = "b")

# So here instead of 20 variable model we have model which is saying that only taking 16 predictors
#   is enough. Recall that we have assigned 4 predictors to 0 . So indirectly our best model selector
#   is saying that neglect that predictors which gives zero influence in the response.
which.min(testerrors)


coef_best = coef(reg_model,16)
coef_best



## f)
# Comparing the coefficients values at the best model vs actual coefficients

# These are actual coefficients we have assigned
coeffs

# These are the best coefficients we got from the model
coef_best

# Our coeffs that we have actually created doesn't have names associated with it, So we are
#   creating a vector of names and then assigning them to the names of the coeffs
#
# Paste will work as string concentation but the normal paste function will use a space " " as
#   seperator , so we have to use paste0 function to remove that space
names(coeffs) = paste0("X",1:20)
coeffs

names(coeffs)

# The best coefficients that we have extracted from teh model already has the names so we are just
#   matching them with the names we are creating newly to the actual coefficients
names(coef_best)


# Now we are creating dataframes including another column called namecolumn which is same for both
#   given coeffs and best coeffs, It will act as key to sort out or merge those two dataframes
givencoeff_frame = data.frame(namecolumn = names(coeffs),coeffs)
bestcoeff_frame = data.frame(namecolumn = names(coef_best),coef_best)

# Apparently merge is pretty good way of joining two different dataframes with different number of
#   rows and columns too.
# REFERE THIS LINK FOR NICE INFO http://www.datasciencemadesimple.com/join-in-r-merge-in-r/
#
# We can merge these two dataframes and there are different ways of merging
#   We have to give a key value column by which it will merge the colummns together
#   This is specified by the "by" argument. All the columns which has same column name are matched and
#     grouped in to single one. THen using that as reference the rest of columns are added
#   We also have another important argument called "all" which will let us which rows to be kep
#   If we keep all.x=T, then all the rows of x dataframe are kept and those that are not there in y
#     which are there in x are filled with NA's
#   We have to keep all the given coefficients and the best ones which has lesser coeffs than actual
#     should be assigned NA's , so we have to keep all.x=T ; THen which evr coefficient that is not
#     represented by the current best model will be given NA's
merged_coeffs = merge(x = givencoeff_frame,y = bestcoeff_frame,all.x = T)

View(merged_coeffs)

# Here if we view the coefficients dataframe after merging we can see that they are combined
#   and where ever the coeffcieint is not present in the best coeff array it is assigned NA

# Now we have to assign 0 to all the coeffs which are NA's to perform the squared error sum of coeffs
# Getting bool array where the NA's are present
is.na(merged_coeffs)

# Assiging the index of those which has true to zero
merged_coeffs[is.na(merged_coeffs)] = 0

# All the NA's been given 0 value now
merged_coeffs

# Calculating the MSE of actual coeffs and predicted ones
mean((merged_coeffs$coeffs - merged_coeffs$coef_best)^2)
merged_coeffs$coef_best - merged_coeffs$coeffs





## g)
#
# Creating a beta array which plots MSE of betas with the real to predicted ones from 1:20 models

beta_vector = rep(0,20)
beta_vector

for (i in 1:20) {
  coef_best = coef(reg_model,i)
  names(coeffs) = paste0("X",1:20)
  givencoeff_frame = data.frame(namecolumn = names(coeffs),coeffs)
  bestcoeff_frame = data.frame(namecolumn = names(coef_best),coef_best)
  merged_coeffs = merge(x=givencoeff_frame,y = bestcoeff_frame,all.x = T)
  merged_coeffs[is.na(merged_coeffs)] = 0
  beta_vector[i] = mean(( merged_coeffs$coeffs - merged_coeffs$coef_best )^2)
}

beta_vector
which.min(beta_vector)

# So for the 16th point we have the lowest value for this MSE of beta vector which it should be
#   because we have originally 16 betas and made 4 out of 20 to zero randomly

plot(beta_vector,type = "b")
points(which.min(beta_vector),beta_vector[which.min(beta_vector)],col = "red",cex=2,pch=20)


