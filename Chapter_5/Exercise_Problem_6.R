# Using the same default dataset
# Now we are going to compute the standard errors using 2 methods
#
# 1st method using botostratp and
# 2nd method is using the standard formula for computing the standard erros in glm() function
#


## a)
# Using summary() and glm() function , we are going to determine the standard errors for the coeffcients associated with "income" and "balance"
library(ISLR)
defaultdata = Default
attach(defaultdata)
names(defaultdata)
# We are not going to split the dataset in to test and train so no need to do any sample
#

lm.fit = glm(formula = default~balance+income,family = binomial,data = defaultdata)
lm.fit
summary(lm.fit)

# The standard errors that we observe that balance is 2.27e-4 and for income its 4.98e-6

## b)
# Now writing a bootfunction which takes dataset and index as arguments and then returing the coefficients
bootfunction = function(dataset,index){
  return(coef(glm(formula = default~balance+income,family = binomial,data = dataset,subset = index)))
}
nrow(defaultdata)

# We are using only 70% of data with replacement and then sampling on them.
# Our bootfunction will be taking this data and will gives us the coefficients as output
set.seed(42)
bootfunction(defaultdata,sample(nrow(defaultdata),0.7*nrow(defaultdata),replace = T))

# Now using the boot() for 1000 times and calculating the bootstrap results of standard errors
# Using the library boot for using the boot function
library(boot)

# Using the full dataset to get the estimates of standard errors
boot(data = defaultdata,statistic = bootfunction,R = 1000)

# From summary standrard error is, income:4.98e-6, from bootstrap income:5.11e-6
# From summary standard error is , balance:2.27e-5, from bootstrap balance:2.34e-4
# Both are pretty close to each other

# Lets use only 70% of data and then repeat the boot again
boot(data = defaultdata[1:7000,],statistic = bootfunction,R = 1000)
# From summary standrard error is, income:4.98e-6, from bootstrap income:2.59e-4
# From summary standard error is , balance:2.27e-5, from bootstrap balance:5.92e-6
# Even when we take 70% of data we still get pretty close standard erros with summary()
# This shows that using less data too we can use bootstrap method to calculate standard errors



