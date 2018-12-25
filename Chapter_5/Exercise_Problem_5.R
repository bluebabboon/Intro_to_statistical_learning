# Using the default dataset which is part of ISLR library
library(ISLR)

## a)
# Fitting a logistic regression model that uses income and balance to predict default
defaultdata = Default
attach(defaultdata)

glmmodel = glm(formula = default~income+balance,family = binomial,data = defaultdata)
glmmodel

summary(glmmodel)


## b)
# Splitting the model into training and testing data
names(defaultdata)
dim(defaultdata)
nrow(defaultdata)


# Using the sample function to select random indexes
?sample
train = sample(x = nrow(defaultdata),size = 0.7*nrow(defaultdata))
train
length(train)

# Now we have selected the indexes that we are going to use for the training the data in the train
# All other data is going to be used for testing the trained algorithm


# Now we are going to fit the default data with other factors that are balance and income using multiple logistic regression model
lm.fit = glm(formula = default~income+balance,family = binomial,data = defaultdata,subset = train)
lm.fit

# Now predicting on the fitted model using predict
?predict
lmpreds = predict(object = lm.fit,newdata = defaultdata[-train,],type = "response")
length(lmpreds)
lmpreds_categorical = ifelse(lmpreds<0.5,"No","Yes")
lmpreds_categorical

# Output of confusion matrix as from table function
table(lmpreds_categorical,defaultdata[-train,]$default)

# % of correct ones are
mean(lmpreds_categorical == defaultdata[-train,]$default)*100
# 97% are correct ones

# Validation error is % of ones that are misclasified, which is nothing but 1-above mean, which is 2.63%
(1-mean(lmpreds_categorical == defaultdata[-train,]$default))*100



## c)
# Using three different splits , that means selecting different subsets, that means selecting different indices in the train. Which leads to setting the seed to another value
# Using a seed of 23
set.seed(23)
train = sample(x = nrow(defaultdata),size = 0.7*nrow(defaultdata))
length(train)
lm.fit1 = glm(formula = default~income+balance,family = binomial,data = defaultdata,subset = train)
lm.fit1
lmpreds1 = predict(object = lm.fit1,newdata = defaultdata[-train,],type = "response")
lmpreds_categorical1 = ifelse(lmpreds1 < 0.5,"No","Yes")
table(lmpreds_categorical1,defaultdata[-train,]$default)
mean(lmpreds_categorical1 == defaultdata[-train,]$default)

# Using a seed of 24
set.seed(24)
train = sample(x = nrow(defaultdata),size = 0.7*nrow(defaultdata))
length(train)
lm.fit2 = glm(formula = default~income+balance,family = binomial,data = defaultdata,subset = train)
lm.fit2
lmpreds2 = predict(object = lm.fit2,newdata = defaultdata[-train,],type = "response")
lmpreds_categorical2 = ifelse(lmpreds2 < 0.5,"No","Yes")
table(lmpreds_categorical2,defaultdata[-train,]$default)
mean(lmpreds_categorical2 == defaultdata[-train,]$default)

# Using a seed of 25
set.seed(25)
train = sample(x = nrow(defaultdata),size = 0.7*nrow(defaultdata))
length(train)
lm.fit3 = glm(formula = default~income+balance,family = binomial,data = defaultdata,subset = train)
lm.fit3
lmpreds3 = predict(object = lm.fit3,newdata = defaultdata[-train,],type = "response")
lmpreds_categorical3 = ifelse(lmpreds3 < 0.5,"No","Yes")
table(lmpreds_categorical3,defaultdata[-train,]$default)
mean(lmpreds_categorical3 == defaultdata[-train,]$default)

# The validation set error is around to be 2.5% and the correct predictions are always around 97.5%. So the selection of subset is not seem to be a influencing factor



## d)
# Now using all the variables including the student variable that we haven't considered eariler.
set.seed(42)
train = sample(x = nrow(defaultdata),size = 0.7*nrow(defaultdata))
length(train)
lm.fit4 = glm(formula = default~student+income+balance,family = binomial,data = defaultdata,subset = train)
lm.fit4
lmpreds4 = predict(object = lm.fit4,newdata = defaultdata[-train,],type = "response")
lmpreds_categorical4 = ifelse(lmpreds4 < 0.5,"No","Yes")
table(lmpreds_categorical4,defaultdata[-train,]$default)
mean(lmpreds_categorical4 == defaultdata[-train,]$default)

# There is no noticable difference in the validation error by including the student variable

























