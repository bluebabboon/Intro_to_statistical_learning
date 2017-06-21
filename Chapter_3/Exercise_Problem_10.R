### Using Carseats data from ISLR ###
library(ISLR)

# (a)
cardata=na.fail(Carseats)
names(cardata)
attach(cardata)
fix(cardata)
str(cardata)
summary(cardata)

# taking Sales as Y , X's are Price,Urban,US (Urban and US are factors)
model1 = lm(Sales~Price+Urban+US,data=cardata)
summary(model1)

# Since Urban and Us are factors it will create a dummy variable
# But since both of them have only two factors ,the intercept will take care of extra one
# So total variables will be 3+Intercept for (price,Urban,US)
# P value of UrbanYes is not significant,it is very high. So its coefficient can be neglected

# (c)
# Model in Equation form
# Sales = Intercept + Beta1*Price + Beta2*X2 + Beta3*X3       {X2:1 if Urban=Yes,0 Otherwise} ; {X3:1 if US=Yes,0 Otherwise}
# We can see how the qualitative variables are set by using contrasts() function
contrasts(Urban)
contrasts(US)

# (d)
model1_summary = summary(model1)

# To get Pvalues of the said model
model_summary$coefficients[,"Pr(>|t|)"]
# Price,US have very low p values ,so we can reject null hypothesis for these predictors

# (e)
# Lets fit model for sales on Price and US
model2 = lm(Sales ~ Price+US,data=cardata)
model2_summary = summary(model2)
names(model2_summary)

# (f)
# Both the models fit the data very poorly
# Although Urban predictor is not helping the first model gave less Rsquared value than the second
model1_summary$r.squared
model2_summary$r.squared
model1_summary$sigma
model2_summary$sigma

# (g)
# Confidence intervals for predictors from model 2
confint(model2)

# (h)
# To do diagnostics
par(mfrow=c(2,2))
plot(model2)
# From Std.residuals vs Fitted values we can see that observations 51,377,69 are mostly outliers
# From Std.residuals vs Leverage plot we can see there are high leverage points

