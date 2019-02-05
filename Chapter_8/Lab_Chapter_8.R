
###################################
##### FITTING CLASSIFICATION TREE
###################################


# We can use the tree library to construct the classification and regression tree
library(tree)

# We first use the classification trees to analyze the Carseats data set. In this data Sales is
#   a continous variable , and we begin by recording it as a binary variable.
# We can use the ifelse() fucntion to create a variable called High which takes Yes if the
#   Sales variable exceed a threshold and here it is 8, and takes No if otherwise

library(ISLR)

attach(Carseats)

# We have created the carseats data and now we create High variable
High = ifelse(Sales > 8 , "Yes","No")

# Creating a new dataframe attaching the Carseats and the newly created variable High to it
cardata = data.frame(Carseats,High)

# We can now use the tree funciton to fit a classification tree in order to predict High using
#   all the variabels but Sales. THe syntax of the tree() function is quite similar to the lm()
#   function


?tree
# Some knowledge about trees, Tree is grown by binary recursive partitioning using the response
#   in the formula and choosing splits from the terms of right hand side in the formula
# Numeric variables are divided in to X<a and X>a , and the levels of unordered factor are
#   divided in to two non empty groups. The split which maximizews the reduction in
#   impurity is chosen. The impurtity can be gini index or just rss.Tree growth is limited to
#   a depth of 31
# The outputs of the tree function is "frame" which is dataframe with a row for each node
#   and row.names gives the node numbers.
#   The column include
#     var, the variabel used at the split for a terminal node n
#     n,(n is the weighted number of cases reaching that node)
#     "yval" , the fitted value at the node ( the mean for the regression tree)
#     "splits", which is a two column matrix of the labels for the left and right split at the
#       nodes. Liek what value the split is happening for that particular label at that branch
#     "yprob", which is a matrix fitted probabililtes for each response level
# The outputs also include some other values apart from the frame, THese are
#   "where" An integer vector giving the row number of the frame detailing th node to which
#     each case is assigned. This means, if we label a terminal node with an integer and
#     for each observation we see at which node it is coming at the end ,if we note down
#     all the nodes for each observation and make a vector that is what we get in "where"
#   "terms" The terms of the formula
#   "call" The matched call to Tree
#   "y" , The response Y that we got from usign this tree, "it is the response"
#
tree_fit_carseats = tree(High~.-Sales, cardata)

# In the above fit we are using all the predictors by giving a . after the ~ and then excluding
#   the variable Sales ,since if we incldue it all the trees will be just showing the Sales data
#   because we derived High data from it.

# In summary we see that we actually used only some of the variables of all of them. Also it
#   gives us the number of terminal nodes, which are also called leaves of the tree
#   And the residual mean deviance: which is derived by a formula
#     DEVIANCE : -2*( Sum,upto m [ Sum,upto k  [n_mk*log(p_mk) ]] ), here n_mk is the number of
#       observations in the mth terminal node that belong to the kth class. And p_mk represent
#       the proportion of observations in the mth node that are from kth class.
#       So this deviance will take zero value if all the values in the mth node actually belong
#       where they should be.
#       Small deviance indicates that the tree that provides a good fit. The "residual mean
#       deviance" is nothing but the Total deviance divided by n-|T_0|, which in this case is
#       400 - 27, that is 373. This means TOtaldeviance / n-|T_0| , n is number of observations
#         and T_0 is number of termianl nodes
summary(tree_fit_carseats)

# Viewing some of the tree data here.
tree_fit_carseats$frame

# The dim is 53,6 , This is what we have explained above, which is a dataframe that we get
#   as output. It has "var", "n", "yval", "splits", "yprob" as column names
dim(tree_fit_carseats$frame)

names(tree_fit_carseats$frame)

# Viewing the first 3 rows of the dataframe that has been generated.
tree_fit_carseats$frame[1:3,]


# Viewing the where for each of the observation
tree_fit_carseats$where

# As it is vector using the length to see that it is same as the data that we have supplied
length(tree_fit_carseats$where)
dim(Carseats)

## PLOTTING TREES
# One of the interesting properties of trees is that they can be graphically displayed. We can
#   use the plot function to display the tree structure and the text() function to display the
#   node labels. The argument, pretty=0 in text function says to R to include the category
#   names for any qualitative predictors , rather than simply displaying a letter for each of
#   that category
plot(tree_fit_carseats)

# Just calling plot gives us empty tree without any text, So we have to give text now
?text
# text draws the stings given in the vector labesls at the coordinates given by the x and y.
text(tree_fit_carseats,pretty = 0)

# Including the text will gives us what is happening at each split, by default the text
#   mentioned above means that the left tree is what is the value with the condition listed
#   on the text and the right is the condition without the tree.

# Also just calling the output of the tree fucntion will give us a output corresponding to
#   each brach , THen it displays the split criterion , the number of observations in that
#   branch , the devicance , the overall prediciton for that branch , and the fraction of
#   the observatrions in that brach that takes on the Value Yes and No.
#   Whatever the branches that leads to the terminal nodes are indicated with a "*", (asterisk)

# Also the output from this is in a nested format which indicates to what parent branches
#   the child or sub branches belong to and this is indicated in a tab format where the spaces
#   are tabbed to indicate the branching
tree_fit_carseats

# In order to properly evaluate the performance of a classification tree on these data, we must
#   estiamt ethe test error rather than simply computing the training error. So we are
#   going to split all the observations in to training and testing and then build the tree
#   using the training set and then compute the error on the testing set.
# The predict() function can be used for this purpose. In case of classification tree
#   the argument type = "class" says R to return the actual class prediction

# Setting the seed
set.seed(42)
train = sample(1:nrow(cardata), nrow(cardata)/2)
test = -(train)

carseat_test = cardata[test,]
carseat_train = cardata[train,]
High_test = High[test]

tree_fit_train = tree(High~.-Sales,data = carseat_train)
# Using the "class" in the type to indicate that predict should return the actual class
#   prediciton, using type = "response" is for glm when we want to predcit the probabilties
#   as we have noticed in chapter 4, In glm function after fitting the model and using that
#   to predict for new data we use type = "response", to get the probabilities
tree_pred = predict(tree_fit_train, newdata = carseat_test, type = "class")

# Taking the table of tree_pred to get the confusion matrix with respect to actual Values of
#   High_test
table(tree_pred, High_test)

# THe total number of correct ones are , 77% , Sum of true postive + true negative / total
(104+50)/200

####################
###### PRUNING
####################
## Now we consider whether pruning the tree might lead to improved results.
#
# The function cv.tree() performs cross validation in order to determine the optimal level of
#   tree complexity; COST COMPLEXITY PRUNING is what we used to select a sequence of trees for
#   consideration.
# We use the argument """FUN=prune.missclass""" in order to indicate that we want the
#   classifiaction error rate to guide teh cross-validation and pruning process,rather than
#   default for the cv.tree() function which is """deviance"""
# The cv.tree() function reports the number of terminal nodes of each tree considered(size)
#   as well as the correspoinding error rate and the value of cost-complexity parameter used
#   This is represented by , k and corresponds to alpha , as given in the  tree pruning formula.
#   As the alpha value increases it indirectly affects the tree size and forces it to become
#     small. As the formula includes the RSS + alpha*Terminal_nodes. So teh number of
#     terminal nodes are forced to be reduced.

set.seed(3)

?cv.tree
# Here in above cv.tree() function we will give the model as the first argument , the model
#   is what we have fit the normal tree as usign just tree function
#   Second argument is FUN which will let which pruning type to use, prune.tree uses just
#     the deviance method and prune.misclas uses the classification error rate.
cv_fit_carseats = cv.tree(object = tree_fit_train,FUN = prune.misclass)
names(cv_fit_carseats)

# The output of this has size, which means the number of terminal nodes
#   And "dev" which means the cross validation error rate in this instance
#   "k" indicates the alpha used
#   "method" indicates which method we have used to do the pruning, here it is misclass method

cv_fit_carseats

# We see that we have lowest "dev" value for 4th index which correspond to the k value of 2
#   This means that for alpha as 2, we have the lowest cv error.

summary(cv_fit_carseats)

# Lets plot the CV error rate as function of both size and k
par(mfrow = c(1,2))
plot(cv_fit_carseats$size, cv_fit_carseats$dev, type = 'b')
plot(cv_fit_carseats$k, cv_fit_carseats$dev, type = 'b')

# So for a tree size of 8 terminal nodes which is obtained by setting the alpha value as 2
#   gives us the lowest cross validation error

# As per ALGORITH 8.1 , now that we have obtained the alpha value using cost complexity pruning
#   with cross validation method, We are going to use this alpha value to get the new tree
#   which has only that nuumber of terminal nodes from which we have the lowest deviance

# For acheiving that we are going to take the tree that we have generated using the training set
#   and then usign that full tree we are going to prune it using the prune.misclass() function
?prune.misclass

# This above function takes tree as its first argument which will use on what tree the operations
#   have to be performed on. Then it can either use k value which is nothing but alpha value
#   that we have obtained from the cross validation method.
#   Or we can use the "best" argument which will tell the fucntion on how many terminal nodes
#     need to be present in the pruned final tree

pruned_cv_tree = prune.misclass(tree = tree_fit_train,best = 8)

# Pltting the pruned tree
plot(pruned_cv_tree)
text(pruned_cv_tree, pretty = 0)

# Now we can test on this pruned tree instead of the whole tree again to see the test error
pruned_preds = predict(pruned_cv_tree, newdata = carseat_test, type = "class")

# Using table to see the error preds
table(pruned_preds,High_test)

# Now the correct predictions are 103+54 / 200
(103+54)/200

# So now the new prediciton rate is 78.5% instead of 77% earlier without pruning the tree.

# We can increase the best value to get more terminal nodes but that will reduce the prediction
#   accuracy, Lets just check that with keeping 15 terminal nodes
pruned_cv_tree_15 = prune.misclass(tree = tree_fit_train, best = 15)
plot(pruned_cv_tree_15)
text(pruned_cv_tree_15, pretty = 0)

# Giving class as the tyep in the predict function, to get th class predcitions since we have
#   qualitative data
pruned_preds_15 = predict(pruned_cv_tree_15, newdata = carseat_test, type = "class")
table(pruned_preds_15, High_test)

# The accuracy is 104+50 / 200 = 77%, which is lower that 78.5% what we got earlier
(104+50)/200



#############################
#### FITTING REGRESSION TREE
#############################

# Clearing all the variables in the environment with this command
rm(list = ls())

# Here we are going to use the Boston data set which is part of MASS library
library(MASS)
set.seed(1)

# Creating the training indices that we can use for training by giving this in the subset
train = sample(1:nrow(Boston), nrow(Boston)/2)

bostondata = Boston

tree_fit_boston = tree(medv~. , data = bostondata, subset = train)

tree_fit_boston

summary(tree_fit_boston)

# Everything remains the same when we compare with the classification tree fit, Only the output
#   in the summary changes
# The summary indicates that only threee of the variabesl have been used in constructing the
#   tree. In context of regression deviance is simply sum of squared errors for the tree
#   And teh mean devicance is deviance / number of observations considered for the fit

plot(tree_fit_boston)
text(tree_fit_boston, pretty = 0)

# Here if we observe the plot of tree it only has 3 variables lstat, rm and dis

# The tree indicates that lower values of lstat correspond to more expernsive houses.
# Also it predicts a median hosue price of 46400$ for larger homes in suburbs in which
#   residents have high socioeconomic status (rm > 7.437 and lstat<9.715 )
# lstat is percentage of individuals with lower socioeconomic status


#####################
###### PRUNING
####################
## Using the same cv.tree fucntion to see whether pruning the tree will improve the performance
?cv.tree

# Here we cannot use FUN = prune.misclass , since this is not classification , so we need to
#   use the default one which is prune.tree
cv_tree_boston = cv.tree(object = tree_fit_boston, FUN = prune.tree)

# We observe that the lowest deviance is found for size of 8, which is likely the most
#   complex tree that we have. As per cross validaiton we got this as the best one
cv_tree_boston

summary(cv_tree_boston)

pruned_cv_tree_boston = prune.tree(tree = tree_fit_boston,best = 8)
plot(pruned_cv_tree_boston)
text(pruned_cv_tree_boston, pretty = 0)

# But this tree is same as the unpruned tree, To get another tree with only say 5 terminal nodes
#   then we have to give best as 5 in the prune.tree() function
pruned_cv_tree_5_boston = prune.tree(tree = tree_fit_boston, best = 5)
plot(pruned_cv_tree_5_boston)
text(pruned_cv_tree_5_boston, pretty = 0)


# To do the testing error , we have to use the best of cross validation tree which is nothing
#   but same as the unpruned tree
# Getting the test error from the observations only on test dataset
pruned_preds_boston = predict(object = pruned_cv_tree_boston, newdata = bostondata[-train,])

# THe mean suared error is 25.05
mean((pruned_preds_boston - bostondata$medv[-train])^2)

plot(pruned_preds_boston , bostondata$medv[-train])
?abline
# Adding a straight linet o the plot using 0 as intercept and 1 as slope
#   abline(a,b) uses a as intercept and b as slope
abline(0,1)






##################################
###### BAGGING AND RANDOM FORESTS
##################################

# In this section we are going to apply bagging and random forests to the Boston data using the
#   randomForest package in R. The exact results obtained in this section may depend on the
#   version of R and the version on randomForest package used.

# Recall that bagging is simply a special case of random forest with m=p
#   So therefore the randomForest() funciton can be used to perform both randomForest and
#   bagging as well

rm(list = ls())
library(randomForest)
set.seed(1)

bostondata = Boston

train = sample(1:nrow(bostondata), nrow(bostondata)/2)

# Performing the bagging which is special case with all the predictors that have been
#   considered. Hwere the number of predictors are 13 and we have to use all of them

bag_boston = randomForest(medv~. , data = bostondata, subset = train
                                , mtry = 13, importance = TRUE)

bag_boston

# The argument mtry=13 indicates taht all 13 predcitors should be considered for each split
#   of the tree - In other words that bagging should be done.
# The arguments of random Forest are as follows
# First argument can be X ,which is a dataframe of matrix of predictors or a formula describing
#   the model to be fitted
# Second arguemtn is data which is optional dataframe contiaiing the variables in the model
#   By default the variables are taken from the environment in which randomForest is called from
# Another argument is "mtry" which indicates the number of variables randomly sampled as
#   candidatest at each split. Note that the defautl valeusa re different for classification
#   Sqrt(p) where p is number of variables in X , and for regression number of variables is p/3
#   Also usign different varlues of p sizes might leads to different improvements.
# Another argument can be subset which is same as what we see in any formula, it can take only
#   subset of observations from the entire full dataset.
# Another argument that can be useful is "importance" which means, it notes the importance
#   of predictors which can be used to display the VARIABLE IMPORTANCE graph. This means nothing
#   but the number of times in the trees the variable is present and highly influencing the
#   graph structure.
#
# VARIABLE IMPORTANCE: Since we have decreased interpretability incase of random forests as
#   compared to decision trees. We need to have some display thingy to have a better chance at
#   explaining the changes in response with the predictors. So what we do here is that we
#   record the total amount of RSS that is decreased due to splits over a given predictors , that
#   is averaged over all the TREES. This gives us variable importance.


?randomForest

# This implements a Breiman's random forest algorithm based on Breiman and Cutler's origianl
#   Fortran code for clasisfication and regression. It can also be used in unsupervised
#   mode for assesing proximities among datapoints

# After creating the bagging model lets test the error of test dataset
# No need to use the type = "class" here , Since this is not a classification
bag_preds = predict(object = bag_boston, newdata = bostondata[-train,])

plot(bag_preds, bostondata$medv[-train])

abline(0,1)

# We have error rate of 13.4%
mean((bag_preds - bostondata$medv[-train])^2)

# TO see hwo the error rate decreases as teh number of trees increases , we can just use the
#   plot function on the bagging model we have created
plot(bag_boston)



##  Bagging model 2
bag_boston_2 = randomForest(medv~. , data = bostondata, subset = train,
                                    mtry = 13, ntree = 25, importance = TRUE)
# This is another type of model where we are restricting the number of treees to only 25
# And also this is bagging model because we have used all the predictors

bag_preds2 = predict(bag_boston_2, newdata = bostondata[-train,])

# THe mean squared error is 14.4% which is increased as ompared to earlier where trees are
#   500 by default
mean((bag_preds2 - bostondata$medv[-train])^2)


#####
### RANDOM FOREST
#####

rf_boston_model1 = randomForest(medv~. , data = bostondata, subset = train,
                                      mtry = 6, importance = TRUE)

# Here generally we have to use 13/3 number of predictors which is nothing but general
#   thumb rule of p/3 predictors for regression model and sqrt(p) number of predictors for
#   classification model

rf_boston_model1

rf_preds = predict(rf_boston_model1, newdata = bostondata[-train,])

# THe meand squared error is 11.5% which is decreased as compared to previous errors which are
#   around 13%
mean((rf_preds - bostondata$medv[-train])^2)

# THe test MSE is 11% wwhich indicates that random forest yielded an improvement over bagging
#   in this case.

# We can use the importance function to view the importance of each variable
#   This is simply nothing but the variable importance that we have discussed earlier

importance(rf_boston_model1)






















































































































































































































































































































































































































































































































































































































































































































































































































































