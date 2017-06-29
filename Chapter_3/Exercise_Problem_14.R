# To focus on the collinearity 

# (a)
set.seed(1)

# uniform distribution,where prob for every element is same
?runif
x1 = runif(100)
plot(x1)
x2 = 0.5*x1 + rnorm(100)/10
y = 2+2*x1+0.3*x2+rnorm(100)

# Linear model is y = 2 + 2*x1 + 0.3*(0.5*x1) = 2 + 2.15*x1

# (b)
cor(x1,x2)                    # gives 0.83,so very close to each other with positive correlation
plot(x1,x2)                   # This plot shows a clear relationship between x1 and x2

# (C)
# Predicting y onto x1 and x2
model1 = lm(y ~x1+x2)
summary(model1)
# beta0 Estimate = 2.31,Actual = 2 ; beta1 Estimate = 1.43,Actual = 2 ; beta2 Estimate = 1.009,Actual = 0.3
# pvalues of beta2 is very high ,we cannot reject null hypothesis
# pvalue for beta1 is still high (not < 0.01) so we cannot reject null hypothesis for that too

# (d)
model2 = lm(y~x1)
summary(model2)
# estimates are good enough,but Rsquared value is still low
# pvalue is very low so we can reject null hypothesis

# (e)
model3 = lm(y~x2)
summary(model3)
# We can reject null hypothesis,Fstatistic is high too ,and pvalue <0.01

# (f)
# In (c) we fit x1+x2 and in (e) we fit only x2
# But we got a result saying that x2 coeff can be neglected in first case but its reverse in second case
# Because in first case x1 and x2 are highly collinear it is redundant,so pvalues are high; 
# But in second case we donot have that problem

# (g)
# knowingly adding a mis measurement
x1 = c(x1,0.1)
x2 = c(x2,0.8)
y = c(y,6)
# Actual y should be = 2+2*0.1+0.3*0.8 = 2.44 but the observation is 6
# Also x2 = 0.5*0.1 + rnorm() = 0.01+something around zero ~ 0.01 but it is 0.8



# Doing all the steps from c to e
# fitting with x1 and x2
par(mfrow=c(2,2))

model4 = lm(y~x1+x2)
summary(model4)
plot(model4)
# From the above model observation 101 seems to be high leverage as well as outlier
# It includes both the effects explained below ,so it is high leverage and outlier

model5 = lm(y~x1)
summary(model5)
plot(model5)
# From the above model it seems it is outlier but not a high leverage
# Because from the values of x1 and x2 value for y should be around 2.44 but its 6
# Outlier means having unusual measurement of y for a given x; and this observation makes it an outlier


model6 = lm(y~x2)
summary(model6)
plot(model6)
# Here it is high leverage,but not an outlier
# Leverage means having unusual value for x in predictors
# Because the actual value of x2 should be around 0.01 but it was given as 0.8 that makes it as leverage
