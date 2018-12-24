# Using the default dataset which is part of ISLR libraryk
library(ISLR)

## a)
# Fitting a logistic regression model that uses income and balance to predict default
defaultdata = Default
attach(defaultdata)

glmmodel = glm(formula = default~income+balance,family = binomial,data = defaultdata)
glmmodel

summary(glmmodel)

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


# Now we are going to fit the default data with other factors that are student balance and income using multiple logistic regression model
lm.fit = glm(formula = default~student+income+balance,family = binomial,data = defaultdata,subset = train)
lm.fit

# Lets predict on all the dataset
