# We will attempt to fit data without intercept and do some analysis on that
# We would like to investigate the t-statistic for Null hypothesis H0:beta=0

# Lets set the seed
?set.seed
# se.seed is an integer vector ,containing the Random number generator state
set.seed(1)
x=rnorm(100)
y=2*x + rnorm(100)

# (a)
# lets perform linear regression of y onto x without intercept
# Syntax for not having an intercept is lm(y~x+0)
model1 = lm(y~x+0)
model1_summary = summary(model1)
model1_summary
# We can see that coefficient is 1.9939 which is very close to 2(actual value)

# (b)
# performing linear regression of x onto y without intercept
model2= lm(x~y+0)
model2_summary = summary(model2)
model2_summary
# The tstatistic is 18.73 which is good enough and p value is <2e-16
# So the null hypothesis can be rejected

# (c)
# Both the models gave same tstatistic ,p value and Fstatistic and Rsquared as well
# RSE is different for both

# (d)
# tstatistic = beta/SE(beta)
# By substituting the values given for beta and SE(beta) we can prove algebraically
# lets calculate by formula
# Numerator and Denominators from T statistic formula for (y=beta*x)

Numerator = sqrt(100)*(sum(x*y))
Denominator = sqrt(sum(x^2)*sum(y^2)-(sum(x*y))^2)
Tstatistic_from_formula = Numerator/Denominator
# We got 18.820 from the formula which is very close to what we got from lm() ; it is 18.73


# (e)
# From the formula we can say that if we swap x and y position (like x*y as y*x)
# It doesn't make any difference for T statistic; So regressing y onto x ,or x onto y,will give same t statistic


# (f)
# Now regressing with intercept
model3 = lm(y~x)
model3_summary = summary(model3)
model3_summary

model4 = lm(x~y)
model4_summary = summary(model4)
model4_summary

# We can clearly see that t statistic for both models are same
model3_summary$coefficients[,"t value"]
model4_summary$coefficients[,"t value"]

















