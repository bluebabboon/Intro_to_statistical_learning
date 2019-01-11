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
trainframe = data.frame(y = ytrain,xtrain)
testframe = data.frame(y = ytest,xtest)


























































































