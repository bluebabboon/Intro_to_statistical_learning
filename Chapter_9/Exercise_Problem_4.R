# Generate a simulated two class data set with 100 obnservations and two features
# It should have a visible but non linear speration between the tow classes.

# To show that in this setting a SVM with a polynomial kernel with degree greater than 1 or a
#   radial kernel will outperform a support vector classifier on the training dta.
#
# Which technique performs best on the test data? Make plots and report the training and test
#   error rates inorder to backup whatever the claims that we made


# Generating a non linear class boundary as acoording to the lab chapter
set.seed(1)

x = matrix(rnorm(100*2),ncol =2)
dim(x)

# Here the first 30 index of X's are increased by 2 and the rest are decreased by 2
#   But the y values are kept 1 for first 50 and 2 for second 50 elements
x[1:30,] = x[1:30,] + 2
x[30:100,] = x[30:100,] - 2

y = c(rep(1,50),rep(2,50))

# We need to keep y as the factor variable
dat = data.frame(x=x,y=as.factor(y))

View(dat)

# We can see from the plot that the decision boundary has to be non lieanr to separate the
#   classes based on the x values
plot(x, col=(y))



####
## Using only support vector classifier , this means we are going to use only linaer kernel

# First we have to create indices to split the training and testing dataset
#   Creating a trian array using the sample and we are going to select this as subset

# Selecting 70 values from the 100 values and using that as the training set
train = sample(100,70)
train

trainset = dat[train,]
testset = dat[-train,]

# Importing the weird library to use the svm function
library(e1071)

svm_classifierfit = svm(y~., data = trainset, kernel = "linear", cost = 0.1, scale = F)

summary(svm_classifierfit)

# Using the tune function to tune the cost value to use the best cost value

tune_classifierfit = tune(svm, y~., data = trainset, kernel = "linear",
                          ranges = list(cost=c(0.001, 0.01, 0.1, 1, 1.5, 10,1000)))

tune_classifierfit

# This says that the cost we have chosen earlier with 0.1 is the best one

summary(tune_classifierfit)


# Predicting the trining set error
svm_classifier_trainpreds= predict(svm_classifierfit, trainset)
table(svm_classifier_trainpreds, trainset$y)
# The training set error is 13/70

# So our classifier model is remaining the same

# Predicting the error rate using predict function and just calling the model inside it

svm_classifier_preds= predict(svm_classifierfit, testset)

table(svm_classifier_preds, testset$y)

# The test error is 7/30 which is pretty high

# Plotting this svm
# As you can see that the linear boundary is not enough to seperate them clearly
#   Lets try another kernel which will eb nothing but support vector machines
plot(svm_classifierfit, trainset)



#####
### Using support vector machines now, which is nothing but changing the kernel from linear to
# polynomial as stated here and we are also tune the best polynomial degree value using the tune

svm_machinefit = svm(y~., data = trainset, kernel = "polynomial", degree = 2,
                     cost = 0.1, scale = F)
svm_machinefit

summary(svm_machinefit)

# Out of the 70 test observations that we have used it is using almost 63 of them as
#   support vectors which is like too much, These can be reduced using the cost parameter
#   We can increase the cost parameter and this will force the algorithm to take lesser
#   values for these support vectors but the downside is that we will get less accuracy from
#   the model.

# The non linear boundary is most of it successful in atleast identifying the regions and
#   then classify them accordingly
plot(svm_machinefit, trainset)

# Now calculating the training and testing set errors
svm_machinefit_trainpreds = predict(svm_machinefit, trainset)
table(svm_machinefit_trainpreds, trainset$y)


# Using both the cost argument and the degree argument as the ranges arguemnt inside the
#   tuning model
tune_machinefit = tune(svm, y~., data = trainset, kernel = "polynomial",
                       ranges = list(cost= c(0.001, 0.01, 0.1, 1, 1.5, 1000),
                                     degree = c(5, 4, 3, 2, 1, 0.5, 0.1, 0.01)))

# We see that the degree of 1 and cost of 1 gives the best results using the cross validation
tune_machinefit

summary(tune_machinefit)

svm_machinefitbest = tune_machinefit$best.model

svm_machinefitbest

# Now calculating the test error on this best model

svm_machinefitbestpreds = predict(svm_machinefitbest, testset)

table(svm_machinefitbestpreds, testset$y)

svm_machinefitpreds = predict(svm_machinefit, testset)
table(svm_machinefitpreds, testset$y)


