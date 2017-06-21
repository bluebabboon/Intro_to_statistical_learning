###### Using Auto dataset
library(ISLR)
Autodata=Auto

names(Autodata)
str(Autodata)
summary(Autodata)
attach(Autodata)

# (a)
## Using Linear regression for Mpg as Y and horsepower as X ##
model_mpghorse=lm(mpg~horsepower,data=Autodata)
summary(model_mpghorse)
coef(model_mpghorse)
confint(model_mpghorse)
names(model_mpghorse)

# After performing regression all data like,Rsquared,RSE(sigma),Fstatistic ..etc are stored in summary
# We can see what all the data that we can get by calling names() over that dataobject
names(summary(model_mpghorse))
summary_data = summary(model_mpghorse)
# (i) We can see that F statistic is 599 and p value is very low,so there is relationship between mgp and horsepower

# (ii) To see how strong the relationship between predictor and response,we can use RSE and Rsquared
summary_data["r.squared"]                         # 0.6,only explains 60% of variance in this model
summary_data["sigma"]                             # RSE is 4.9 which is pretty high
# The relationship predicted by this model is not that good

# (iii) Relationship between predictor and response
summary_data["coefficients"]                      # This shows that horsepower coefficient is -0.15,the relationship is negative

# (iv) prediction of mpg with horsepower of 98, and confidence intervals
?predict

# Using confidence interval ,we will get less bounds with this as this averages out on all observationk
predict(model_mpghorse,data.frame(horsepower=c(98)),interval = "confidence")

# Using prediction interval,this will give more bounds,considers only for this particular case
predict(model_mpghorse,data.frame(horsepower=c(98)),interval = "prediction")


# (b)
# Plotting response and predictor
plot(horsepower,mpg,col="green",pch="+")
abline(model_mpghorse,col="red",lwd=5)


# (c)
# Plotting the four diagnostic plots

par(mfrow=c(2,2))
plot(model_mpghorse)

# we can see pattern in residuals vs fitted values which clearly indicates there is some nonlinearity
# QQ plot suggest that theoritical quantiles of normal distribution matches with given residuals ,so it is pretty good approx of Normal distribution
# Standardized residuals vs fitted values shows we got some outliers
# Standardized residuals vs leverage points show that we got some high leverage and outlier points
hatvalues(model_mpghorse)
# The points or observation index with highest leverage is 117 and 116 with leverage of 0.029
# Leverage shows how unusual a datapoint is wrto all other data
which.max(hatvalues(model_mpghorse))
max(hatvalues(model_mpghorse))

# This library contains function for calculating VIF
library(car)
# But this will give error though ,because we only have one predictor in our model
vif(model_mpghorse)

