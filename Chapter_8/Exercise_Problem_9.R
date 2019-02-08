## a)
# This problem involves the OJ dataset which is a part of the ISLR package
rm(list = ls())
library(ISLR)
ojdata = OJ

dim(ojdata)

names(ojdata)

str(ojdata)

sum(is.na(ojdata))

summary(ojdata)

fix(ojdata)

# Creating a training and testing set with a random sample of 800 observations and
#   rest in the test set


# Setting the seed as 42 which is my favourite number

set.seed(42)
train = sample(1:nrow(ojdata), 800)

traindata = ojdata[train,]
testdata = ojdata[-train,]

## b)
# Fit a tree to the training data , with purchase as response and other variabels except
#   for Buy as predictors.
# Then use the summary() function to produce the summary statistics about tree and
#   explaing the resutls. What is the training error rate and how many terminal nodes are
#   there in the tree

library(tree)
?tree

names(ojdata)

# There is no variabel named Buy, lets prodcue a boolean array to double check that
names(ojdata) %in% "Purchase"
# This give TRUE where the purchase is located
# Lets try the same thing but instead of keeping "Purchase" lets keep "Buy" in that place instead
names(ojdata) %in% "Buy"
# Everything is False, (Lol sounds like a movie quote) and we see that Buy predictor is not
#   actually present


base_tree_oj = tree(formula = Purchase~. , data = traindata)
summary(base_tree_oj)

# The variables that are actually used in constructing the tree are "loyalCH" and "PriceDiff"
#  And the Residual mean devicance , which is nothing but MSE here is 0.784
#  And the number of terminal nodes are 8

plot(base_tree_oj)
text(base_tree_oj, pretty = 0)




## c)
# Type the name of the tree object to get a detailed text output and Then pick one
#   terminal node and interpret the information displayed
base_tree_oj
# Lets take the 15th one of this and explaing that

# It says that
# When LoyalCH is greater than 0.5036 and if for sure it is greater than 0.705 then if
#   Price diff is greater than -0.39 then 99.85% chance that "Purchase" is CH


## d)
# Create a plot, This is already done above. Please refer to that plot


## e)
# Predict the response on the test dat anow and then print out a confusion matrix
#   with the test labels to the predicted test labels. Then what is the error rate
# Since we want factor based predictions , we can just use the class type to get the
#   class based predictions, or else we will get probabilites of 270x2 matrix
#   where each row has probability for CH or MM which will add up to one row
preds_base = predict(base_tree_oj, newdata = testdata, type = "class")

table(preds_base, testdata$Purchase)

# Test error rate is nothing but incorrect predictions of labels divided by total
#   observations in the test dataset

(12+38)/270
# 18.5% is the test error rate


## f)
# Apply the cv.tree() function on the training set in order to determine the optimal size of
#   the tree
# Since this is a classification tree we have to use the prune.misclass in the FUN argument
#   inside the cv.tree function
cv_tree_base = cv.tree(object = base_tree_oj, FUN = prune.misclass)
cv_tree_base


## g)

plot(cv_tree_base$size, cv_tree_base$dev)

# Seems like taking the tree size of 8 and 5 gives us the same results . So the optimal
#   tree size is 5


## i)
# Now choosing the pruned tree with the size as 5
pruned_tree_base = prune.misclass(tree = base_tree_oj, best = 5)
pruned_tree_base

plot(pruned_tree_base)
text(pruned_tree_base, pretty = 0)

## j)
# Compare the training error rate
# The training error rate for this new tree is
preds_pruned = predict(object = pruned_tree_base, newdata = testdata, type = "class")

# The error rate for the pruned tree is
table(preds_pruned, testdata$Purchase)
# The error is 18.5%
(12+38)/270


## k)
# It seems like the test error rates are same fo both the pruned and unpruned trees


