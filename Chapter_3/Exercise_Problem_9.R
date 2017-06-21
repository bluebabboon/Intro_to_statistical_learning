## Using same Auto dataset
library(ISLR)
# To get VIF function
library(car)
# To plot 3d scatterplot
library(scatterplot3d)

Autodata=Auto
attach(Autodata)
str(Autodata)

# (a)
# Scatter plot matrix to include every predictor,except name
pairs(Autodata)

class(Autodata)

names(Autodata)

# Mutiple ways of selecting all columns except one which we want to exclude

# Selecting all the columns except the 9th one which is "name" using just index of column
temp=Autodata[,-9]
names(temp)

# Selecting all the columns by using which command,it will return all indexes except the index 
# which name coincides with "name"
which(names(Autodata)!="name")
temp=Autodata[,which(names(Autodata)!="name")]
names(temp)

# Selecting all columns by getting the index where its name is "name" and inversing the selection by using "-"
-which(names(Autodata)=="name")
temp=Autodata[,-which(names(Autodata)=="name")]
names(temp)

# We can also use subset function for this purpose
# temp = subset(parent_dataframe,select = "which columns you want to select")

temp=subset(Autodata,select = -c(9))
names(temp)

# (b)
# To find correlation among predictors
newautodata=subset(Autodata,select = -c(9))

# Correlations can be found by using cor() function
# Let us store these in a matrix
correlation_matrix = cor(newautodata)
class(correlation_matrix)
rownames(correlation_matrix)
colnames(correlation_matrix)


# (c)
# Multiple regression using newautodata ,which excludes "name" column
model_multiple=lm(mpg~.,data=newautodata)
summary(model_multiple)
model_summary=summary(model_multiple)
names(model_summary)

# We can see that Fstatistic is 252.4,so there is relationship
# displacement,weight,year,origin are significant
# cylinders,horsepower and acceleration are not that significant
# Coefficient of year variable suggests that as it is increasing,mpg increases by 0.75; very positive correlation


# (d)
# Diagnostic plots
par(mfrow=c(2,2))
plot(model_multiple)

# There is a indication of pattern in Residuals vs Fitted values which suggests non linearity in data
# Observations 323,327,325 have very large standardized residual which indicate they are outliers
# Observation 14,in Leverage vs Std residuals shows that it is high leverage point
# QQ plot indicates our data is similar to normal distribution 

# To calculate Leverage,although we will get this plot from above,just in case remember
# hatvalues() gives leverage
hatvalues(model_multiple)
# Plot of Leverage on X, and standardized residuals ( using rstudent(fitted_model)) on Y axis
plot(hatvalues(model_multiple),rstudent(model_multiple))


# To calculate VIF of our predictors inside the model
# High VIF values are shown for displacement indicating that it is collinear and redundant
vif(model_multiple)


# (e)
# To include interaction effects
# Lets include interaction between cylinders and acceleration ,also interaction between displacement and horsepower
model_interaction = lm(mpg ~ .+ cylinders:acceleration + displacement:horsepower,data = newautodata)
summary(model_interaction)

model_interaction2 = lm(mpg ~.+ cylinders*acceleration + displacement*horsepower,data = newautodata)
summary(model_interaction2)

# Using : or * doesnot make any difference and both are same while specifying interaction
# Effect of cylidners:acceleration is not significant,but displacement:horsepower is very significant
# R^2 is 0.8637 which is pretty good

# (f)
# lets use poly() ,log(),sqrt() to see those type of model fits
# lets take mpg as Y,and X's are year and horsepower
sample_model1 = lm(mpg ~ poly(year,4)+poly(horsepower,3),data = newautodata)
summary(sample_model1)

sample_model2 = lm(mpg ~ poly(year,3)+log(horsepower)+I(horsepower^2))
summary(sample_model2)
# Lets view what is the shape of fitted values is which takes year,horsepower as argument
scatterplot3d(year,horsepower,fitted.values(sample_model2))
