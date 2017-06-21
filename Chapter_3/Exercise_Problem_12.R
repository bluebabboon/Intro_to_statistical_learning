# (a)
# Coefficient estimate for regression is given by (for y=beta*x)
# beta = sum(x*y)/sum(x^2)
# So we want beta to be same for both conditions when regressing Y onto X or X onto Y
# This means sum(x*y)/sum(x^2) = sum(y*x)/sum(y^2)
# so when sum(x^2) = sum(y^2) 
# The coefficients will be same for y=beta*x and x=beta*y

# (b)
# we need to generate 100 observations
# Where coefficients of Y onto X is different from X onto Y
set.seed(2)
x=rnorm(100)
y=3*x + rnorm(100)

model1 = lm(y~x+0)
model2 = lm(x~y+0)
summary(model1)
summary(model2)

# Coefficients for model1 is 2.9522, model2 is 0.3129


# (c)
# we need to generate 100 observations
# where coefficients of Y onto X is same as X onto Y
# sum(x^2) and sum(y^2) are equal
x=seq(1:100)
y=rev(1:100)

model3 = lm(y~x+0)
model4 = lm(x~y+0)
summary(model3)
summary(model4)

# Coefficients for model3 and model4 are same and it is 0.5075

