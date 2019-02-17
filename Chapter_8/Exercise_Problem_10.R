rm(list = ls())

# Using bosoting to predict the Salary in the Hitters dataset

## a)
# Remove the observations for whom the salary informatino is unknown and then log transform the
#   salaries
library(ISLR)
hitterdata = Hitters

names(hitterdata)

sum(is.na(hitterdata))

hitterdata = na.omit(hitterdata)

sum(is.na(hitterdata))

dim(hitterdata)

hitterdata["Salary"] = log(hitterdata$Salary)

dim(hitterdata)

head(hitterdata)


## b)
# Creating a training set with first 200 observations and the test set
#   will be rest of the remaining observations

traindata = hitterdata[1:200,]
testdata = hitterdata[-(1:200),]


## c)
# Now to perform the boosting on the training set with 1000 trees for a range of values of
#   shrinkage parameter lambda. Then give a plot with different shrinkage values on the
#   x axis and the corresponding training set MSE on the y axis



library(gbm)

?gbm

# We need dataset without salaries.So lets create another test and train without salary

set.seed(42)

names(traindata)

boost_hittermodel = gbm(Salary ~ ., data = traindata,
                        distribution = "gaussian", n.trees = 1000, interaction.depth = 4)

summary(boost_hittermodel)

plot(boost_hittermodel)

boost_hittermodel$shrinkage

preds_boost = predict(object = boost_hittermodel,newdata = traindata,n.trees = 1000)

mean((preds_boost - traindata$Salary)^2)

# Lets create a shrinkage vector starting from 1000 to 0.001 or something like that

shrinkage_vecs = 10^seq(-10,-0.1,by = 0.1)

shrinkage_vecs[-20:0]

training_mse = rep(0,length(shrinkage_vecs))

test_mse = rep(0,length(shrinkage_vecs))

# For each of these shrinkage value we have to create a model and then get its predictions and
#   then calculate the MSE of that training set

for (i in 1:length(shrinkage_vecs)) {
  boost_model =  gbm(Salary ~ ., data = traindata,
                        distribution = "gaussian", n.trees = 1000,
                        interaction.depth = 4, shrinkage = shrinkage_vecs[i])

  boost_preds_train = predict(boost_model, newdata = traindata, n.trees = 1000)

  boost_preds_test = predict(boost_model, newdata = testdata, n.trees = 1000)

  training_mse[i] = mean((boost_preds_train - traindata$Salary)^2)

  test_mse[i] = mean((boost_preds_test - testdata$Salary)^2)

}

par(mfrow = c(1,2))

## c)
plot(training_mse)

## d)
plot(test_mse)

which.min(test_mse)

# The test_mse here is 0.255
test_mse[which.min(test_mse)]

shrinkage_vecs[which.min(test_mse)]

## plotting the influence graph
plot(summary(boost_hittermodel))


## The most important predictors are CatBat and CrBI


## g)
# To apply bagging to the training set . And then see the test set MSE
# Bagging is part of randomforest library
library(randomForest)

dim(traindata)

rf_model = randomForest(Salary~.,
                        data = traindata,
                        mtry = 19,
                        importance = TRUE)

preds_rf = predict(rf_model, newdata = testdata)

# The test mse is 0.2291
mean((preds_rf - testdata$Salary)^2)

