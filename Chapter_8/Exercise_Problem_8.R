# In lab a classification tree was applied to the carseats dataset while converting Sales into
#   a qualitative response variale.Now we will predict Sales using regression tree instead of
#   classification tree.


## a)
# Split the dataset into a training set and a test set
# Creating a sample and then creating train and test datasets

library(MASS)

cardata = Carseats

set.seed(42)
train = sample(1:nrow(cardata), nrow(cardata)/2)

train_data = cardata[train,]
test_data = cardata[-train,]

dim(cardata)

names(cardata)

# Sales variable that we are targeting is of column index1

library(tree)

?tree


## b
reg_tree_car = tree(Sales~., data = train_data)

summary(reg_tree_car)

reg_tree_car

plot(reg_tree_car)

text(reg_tree_car)

# getting the predictions for test data
preds_reg_tree = predict(reg_tree_car, newdata = cardata[-train,])

# calculating the test mse

# The mean squared error is 4.09
mean((preds_reg_tree - cardata$Sales[-train])^2)



## c)
# Using the cross validation now in order to determine the optimal level of tree complexity
#   Have to check the MSE after pruning the tree
cv_fit_carseats = cv.tree(object = reg_tree_car, FUN = prune.tree)

cv_fit_carseats

# From here we know that 10th index has the lowest devianec, Here the deviance is nothing but
#   Rss
which.min(cv_fit_carseats$dev)

# The tree size for that particular rss is 5, terminal nodes
cv_fit_carseats$size[10]

# And the k value for that particular index of 10 is 46.26
cv_fit_carseats$k[10]

# Since we get the best tree size , we can now use the size and prune the existing tree
#   using the prune.tree function
pruned_tree = prune.tree(tree = reg_tree_car, best = 5)

pruned_tree

plot(pruned_tree)
text(pruned_tree, pretty = 0)

# Now lets use this tree and calculate the test MSe
# Predicting using the pruned tree

pruned_preds = predict(object = pruned_tree, newdata = cardata[-train,])

# Mean squared error is 4.99, THe mse has increased
mean((pruned_preds - cardata$Sales[-train])^2)



## d)
## Using the bagging appraoch to analyze the data. And also calcualate the test mse here.
## Apart from that use the importance() function to decide which variables are important

# To use the bagging approach we can use the randomforest library
library(randomForest)

# TO use the bagging model we have to use all the predictors, which is a special case of
#   randomforest.
# So we use all the columns in the mtry argument except for the one column we want to do the
#   predictions for
bag_tree_carseat = randomForest(Sales~.,
                                data = cardata[train,],
                                mtry = ncol(cardata)-1,
                                importance = TRUE)

bag_tree_carseat

plot(bag_tree_carseat)

# TO get the test MSe , now we have to use this model and then predict on the test dataset
preds_bag_tree = predict(bag_tree_carseat, newdata = cardata[-train,])

# The test mse over this bagged model is 2.73, which is pretty low as compared to earlier
#   4.something
mean((preds_bag_tree - cardata$Sales[-train])^2)


# Getting the importance of the variables by using this importance function
importance(bag_tree_carseat)

# Plotting the variable importance with each different measure, % inc in Mse if that predictor
# is not included     and Node purity
?varImpPlot
varImpPlot(bag_tree_carseat)





## e)
# Now using the randomforests and instead of using all the predictors we are now only
#   going to use only some of variables

# Lets test for two predictor sizes, sqrt(p) and p/2
round(sqrt(ncol(cardata)-1)) # this is 3

round((ncol(cardata)-1)/2)   # this is 5



rf_tree_carseat_sqrtp = randomForest(Sales~.,
                               data = cardata[train,],
                               mtry = 3,
                               importance = TRUE)

rf_tree_carseat_pby2 = randomForest(Sales~.,
                               data = cardata[train,],
                               mtry = 5,
                               importance = TRUE)

preds_rf_treesqrtp = predict(rf_tree_carseat_sqrtp, newdata = cardata[-train,])
preds_rf_treepby2 = predict(rf_tree_carseat_pby2, newdata = cardata[-train,])


# MSE of different predictor sizes are

# Mean squared error is 3.42
mean((preds_rf_treesqrtp - cardata$Sales[-train])^2)

# Mean squared error is 2.93
mean((preds_rf_treepby2 - cardata$Sales[-train])^2)


importance(rf_tree_carseat_pby2)
importance(rf_tree_carseat_sqrtp)

# We can see that randomforest doesn't necessarily decreases the mse, and the least mse
#   we got is from bagging method in this case

