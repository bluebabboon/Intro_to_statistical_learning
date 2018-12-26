# Considering the boston house dataset.
# loading the library MASS which has boston house dataset
library(MASS)
bostondata = Boston
names(bostondata)
fix(bostondata)

## a)
# Estimate for the population mean of medv
mean(bostondata$medv)
# The estimate is 22.5

## b)
# Standard error of medv is obtained by divinding its standard deviation by the square root of number of observations
# sd function to get the standard deviation
sd(bostondata$medv)
stderror = sd(bostondata$medv)/sqrt(nrow(bostondata))

# The standard error is 0.408
stderror
nrow(bostondata)
# Apparently the nrow if we observe on one particular column will give us null
# We can only use it on dataframe
nrow(bostondata$medv)

## c)
# Now computing the standard error of mean using bootstrap
# Loading the boot library
library(boot)

bootfunction = function(data,index){
  return(mean(data[index]))
}
?boot
# While using a boot function the first argument of statistic is always going to be data
# The second argument of the statistic is indices, it can be vector of indices.
stderror_boot = boot(data = bostondata$medv,statistic = bootfunction,R = 1000)

# The standard error from the boot function above is 0.413 which is close to original one which is 0.408


## d)
# From the boot function help we can see that it results in an output of 11 things
# To view them
View(stderror_boot)
# As per the help, the first output t0 is the observed value of statistic applied to data.

# The estimate of the mean of medv from the bootstrap approach is this, the following
stderror_boot$t0

# For a 95% confident interval , the range of the particular estimate should lie in +- 2 times the standard error of itself.

# Read more here https://en.wikipedia.org/wiki/Confidence_interval
# It is actually +- 1.96*standard error , but we generally take 2
# For a 95% confidence interval , assuming the statistic follows a normal distribution then
# 0.95 = P(-1.96 < ((X-mu)/stddev) < 1.96 ), where here (X-mu)/stddev is the statistic that is following normal distribution
# Then the random variable here X which in our case is the mean lies in the following range, for a 95% confidence interval
# X-1.96*stddev and X+1.96*stddev
#
# So the lower end point of mean of medv from bootstrap
# The second one on the list of outputs of the stderror_boot output is the each output that we got by calculating the statistic that we passed inside the boot for the number of times we have asked it to calculate
lowpoint = stderror_boot$t0 - 1.96*sd(stderror_boot$t)

# Higher end point of mean of medv from bootstrap
highpoint = stderror_boot$t0 + 1.96*sd(stderror_boot$t)

lowpoint
stderror_boot$t0
highpoint

# Comparing it with t.test
?t.test
# This function is used to perform one and two sample t-tests on vectors of data
t.test(x = bostondata$medv)

# The confidence interval from bootstrap is 21.72;23.34
# The confidence interval from t.test is 21.72;23.33
# So the confidence intervals match up closely with t.tests



## e)
# To do the same thing as above but now we are going to use it to calculate median
median_medv = median(bostondata$medv)
median_medv


## f)
# Now calculating the standard error of median using bootstrap method
bootfunction = function(data,index){
  return(median(data[index]))
}

stderror_boot = boot(data = bostondata$medv,statistic = bootfunction,R = 1000)
stderror_boot

# Estimate for median is
stderror_boot$t0

# Standard error for median is
sd(stderror_boot$t)



## g)
# Estimate for the tenth percentile of medv
?quantile
median_0.1 = quantile(x = bostondata$medv,probs = 0.1)
median_0.1  # This value is 12.75


## h)
# Estimate the standard error of above 10th percentile median using bootstrap

bootfunction = function(data,index){
  return(quantile(data[index],probs = 0.1))
}

bootmedian0.1 = boot(data = bostondata$medv,statistic = bootfunction,R = 1000)
bootmedian0.1

# Standard error for the 10th percentile is
sd(bootmedian0.1$t)


