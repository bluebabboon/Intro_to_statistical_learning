# To focus on the collinearity 

# (a)
set.seed(1)

# uniform distribution,where prob for every element is same
?runif
x1 = runif(100)
plot(x1)
x2 = 0.5*x1 + rnorm(100)/10
y = 2+2*x1+0.3*x2+rnorm(100)

# Linear model is y = 2 + 2*x1 + 0.3*(0.5*x1)

# (b)
cor(x1,x2)                    # gives 0.83,so very close to each other with positive correlation
plot(x1,x2)                   # This plot shows a clear relationship between x1 and x2

# (C)
# Predicting y onto x1 and x2
model1 = lm(y ~x1+x2)
summary(model1)
