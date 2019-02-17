# This problem is using the Caravan dataset
library(ISLR)

set.seed(42)

caravandata = Caravan

?Caravan

## Converting the Yes and No to 1 and 0 on the Purchase variable of this caravandata

caravandata$Purchase = ifelse(caravandata$Purchase == "Yes",1,0)



## a)
# training set consisting of the first 1000 observations and test set consisting of the
#   remaining observations

dim(caravandata)

train = sample(1:nrow(caravandata), 1000)
?train

traindata = caravandata[train,]
testdata = caravandata[-train,]

names(traindata)

## b)
# boosting model iwth training set with Purchase as the response and other varaibles as
#   predictors . Using 1000 trees and shrinkage value of 0.01

library(gbm)

?gbm

boostmodel = gbm(Purchase~.,
                 data = traindata,
                 n.trees = 1000,
                 shrinkage = 0.01,
                 distribution = "gaussian",
                 interaction.depth = 4)

boostmodel

summary(boostmodel)

# As per the relative influence levels we can see that PPERSAUT and MBERHOOG and MOSTYPE
#   are the top 3 most influencing variables


## c)
# Now using the boosting model to predict the response on the test data.

preds_boost = predict(boostmodel, newdata = testdata, n.trees = 1000, type = "response")


testdata$Purchase[1:20]
preds_boost[1:20]
max(preds_boost)

preds_boost = ifelse(preds_boost > 0.2 ,1,0)

table(preds_boost, testdata$Purchase)

# So out of all the 270+28 people who purchased we have predicted 28 of them

91/(91+188)


# Using logistic regression

logitmodel = glm(Purchase~., data = traindata, family = binomial)
summary(logitmodel)

preds_lm = predict(logitmodel, newdata = testdata, type = "response")

preds_lm = ifelse(preds_lm>0.2, 1,0)

table(preds_lm, testdata$Purchase)

# The % of correct predictions who actually bought insurance are
48/(48+231)

