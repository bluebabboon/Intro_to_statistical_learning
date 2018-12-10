# Using the default dataset which is part of ISLR libraryk
library(ISLR)

## a)
# Fitting a logistic regression model that uses income and balance to predict default
defaultdata = Default
attach(defaultdata)

glmmodel = glm(formula = default~income+balance,family = binomial,data = defaultdata)
glmmodel

summary(glmmodel)


