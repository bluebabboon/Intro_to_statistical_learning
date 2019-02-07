# In the lab we applied random forests to the Boston data usign mtry=6, using ntree=25
#   and ntree=500,
# Create a plot displaying the test error resulting from random forests on this dataset
#   for a more comprehensive range of values for mtry and ntree
#
# Model your plot after figure 8.10. And then describe results


# mtry indicates the number of predictors that we have to chose for random forest classification
#   Choosign all the predictors in mtry makes is equal to the bagging model.
# Also we can vary the tree size

# Loading the bostondata which is part of mass library
library(MASS)
bostondata = Boston

# loading the random forest libraryj
library(randomForest)

set.seed(42)

train = sample(1:nrow(bostondata),nrow(bostondata)/2)


# Number of predictors that we are using from all the columns ,here , The max size is
predictors = ncol(bostondata)-1


# Since we have been asked to report the TEST ERROR , we can use the inbuilt test
#   argument inside the randomForest function to give xtest and ytest as arguments
# If we give xtest and ytest the mse is calculated on that
#
# Also we have to give the xtest and ytest  with only test indices,
# And also the xtest should not contain the response variable in it, ytest should be of
#   same dataframe format as x and it should only have response
# Since our medv is 14th predictor
#   We can say xtest = bostondata[-train,-14] which indicates that take all observations
#     except it has train index in it and all predictors except it is 14th predcitor
#   Similarily our ytest becomes the last column and all indices except the train indices
#     So it can be denoted as bostondata[-train,14]
rf_boston_model_full = randomForest(medv~. ,
                               data = bostondata[train,],
                               xtest = bostondata[-train,-14],
                               ytest = bostondata[-train,14],
                               mtry = predictors,
                               importance = TRUE
                               )
rf_boston_model_full


# Now what we want is to create a graph of test classification error and how it varies with
#   number of trees in the random forest. And also a seperate graph for each of these
#   different number of predictors chosen.

# All the mse for each size of tree is calculated and stored in mse
rf_boston_model_full$mse

# In same way as mean squared error we also have rsquared which is calculated for
#   the number of trees , like it takes 1 tree in first and increaset them later?
rf_boston_model_full$rsq

# To plot the error vs number of trees just call the plot function and it will do it
#   from 1 tree size to 500 or max tree size that we have given
plot(rf_boston_model_full)



# For a random forest in regression which is what we are doing here, optimal thumb rule
#   for predictor size is p/3

# optimal size is 13/3 rounded which is 4
round(predictors/3)

rf_boston_model_pby3 = randomForest(medv~.,
                                    data = bostondata[train,],
                                    mtry = round(predictors/3),
                                    xtest = bostondata[-train,-14],
                                    ytest = bostondata[-train,14],
                                    importance = TRUE
                                    )

plot(rf_boston_model_pby3)


# Using the number of predictors as sqrt(p)
round(sqrt(predictors))

# p/3 and sqrt(p) gave us same number of  predictor sizes
# So lets just plot the actual model with all predictors and with 4 predictors in a plot

?plot

# Since the mse for each number of trees is contained in the rfmodel$mse ,we are just plotting
#   that information
plot(rf_boston_model_full$mse, type = 'l',
     xlab = "Tree size", ylab = "Mean squared error",
     col = "green",
     ylim = c(10,35),
     main = "MSE comparison with predictor size")

lines(1:500 , rf_boston_model_pby3$mse, col = "red")

# plotting the legend
?legend

# Have to lty or else we wont see the lines with color in the legend box
legend("topright",legend = c("Mse with Full","Mse with p/3"),
       col = c("green","red"), lty =1)


# Keeping lesser predictors is better than including all because it will give a chance for
#   such a tree to be developed by an inferior predictor in the first split that might give
#   more reduction in rss , since we follow greedy split at each split.So the dominating
#   predictor will not be chosen in some trees and this will help while averaging the trees
