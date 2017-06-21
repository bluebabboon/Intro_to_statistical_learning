# (a)
# set.seed(1) for consistent random number generation

set.seed(1)
x=rnorm(100)

# (b)
eps = rnorm(100,mean = 0,sd =0.25)

# (c)
y = -1 + 0.5*x + eps

# length of y,is 100
length(y)
# In the above linear model beta0 = -1 and beta1 = 0.5


# (d)
# scatterplot of x and y
plot(x,y)
# we observe that there are lot of points centered around x=0 and y=-1
# As x is normal distribution(mean = 0 and sd = 1) this means we expect many values of x to be around 0
# So we get lot of points around x=0

# (e)
# fitting a linear model for y onto x (without using eps)

model1 = lm(y~x)
summary(model1)
# beta0estimate = -1.00942,Actual beta0 = -1 ; beta1estimate = 0.49973,Actual beta1 = 0.5
# Rsquared is 0.77 which is low (because we got eps that we didn't consider in the model fitted)

# (f)
?abline
abline(model1,lwd = 1,col = "red")
abline(-1,0.5,lwd = 1,col = "green")
# To add legends; General Syntax
# legend(x="x coordinate where top left corner of legend should be",  y="y coordinate where top left is",
#           legend=c("what you wanna write"), col=c("colors that will be assigned respectievely to what you wrote before"))
?legend
legend(x=-2,y=0.3,legend = c("population line","Fitted line"),text.col = c("green","red"))


# (g)
# fitting y onto x and x^2 as well
model2 = lm(y~x+I(x^2))
summary(model2)
# The p value of x^2 is 0.16 which suggest that we cannot reject null hypothesis of its coefficient being zero
# So quadratic fit is not a good model,and x^2 term is not a good feature


# (h)
# lets have less noise and fit data to that
# So we have to modify eps() variance and set it less
# previously it is 0.25 lets set it to 0.1
eps_less = rnorm(100,mean = 0,sd= 0.1)
y_lessnoise = -1 + 0.5*x + eps_less

plot(x,y_lessnoise)
model_lessnoise = lm(y_lessnoise~x)
summary(model_lessnoise)
# beta0estimate = -0.997,actual = -1 ; beta1estimate = 0.5021,actual = 0.5
# Rsquared is 0.95 which is very good,by decreasing the sd of error in y from 0.25 to 0.1 we increased Rsquared from 0.77 to 0.95


# (i)
# Now with more noise in eps
eps_more = rnorm(100,mean = 0,sd=0.5)
y_morenoise = -1+0.5*x+eps_more

plot(x,y_morenoise)
model_morenoise = lm(y_morenoise~x)
summary(model_morenoise)
# beta0estimate = -0.971,actual = -1 ; beta1estimate = 0.472,actual = 0.5
# Rsquared is 0.42 which is very less,by increasing sd from 0.25 to 0.5 we have decrease in Rsquared from 0.77 to 0.42
# We can say that error on Y is very very important to fit a data

# confident intervals on original
confint(model1)

# confident intervals on less noisy data
confint(model_lessnoise)

# confident interval on more noisy data
confint(model_morenoise)

# The less noisy a dataset is more accurate the confident interval will be



