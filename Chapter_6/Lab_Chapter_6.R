# Using our famous library ISLR
library(ISLR)

# We are going to use a new data here ,its called Hitters data
# We want to predict a baseball players salary on the basis of various statistics associated with performance in the previous year

hitterdata = Hitters

# From this fix we can see the data.and we can observe that some of the salary data is missing in the data
fix(hitterdata)

# The following function is.na will return a bool vector on this salary with true or false
# If the particular row has salary as NA it will be shown as true and false if not
is.na(x = hitterdata$Salary)

# To get the number of rows where salary data is missing or it is NA , we can use the sum over it
sum(is.na(x = hitterdata$Salary))

dim(hitterdata)

# So from above sum we see that salary is missing ofr 59 players
#
# We then can use the na.omit() function to remove all the rows that have missing values in any variable
#
?na.omit
hitterdata = na.omit(hitterdata)

dim(hitterdata)

# Now we can see that we have only 263 rows while the previous 59 rows are deleted
#
sum(is.na(x = hitterdata))




###
## Lets use the library called leaps which does the best subset selection by identifying the best model that contaisn a given number of predictors , where best is quantiied using RSS
install.packages("leaps")
??leaps
library(leaps)
?regsubsets

# We are going to use regsubsets function for model selection.
# It scans through all the subsets of variables using best subset selection model method
# And then gives output if we call summary for each number of predictors which are relevant
# That means for each number of predictors that we want to fit the model with, it will tell what are best predictors that we can use to predict in best way using RSS

regfit = leaps::regsubsets(hitterdata$Salary~.,hitterdata)

summary(regfit)
# In summary we can see that it keeps a star and we have column of numbers
# This numbers indicates the number of variables that the function is considering
# By default regsubset only include model upto 8 variables.
# But we can add more by providing an argument called "nvmax"

# Now using all the variables in the data
dim(hitterdata)

# We have 20 dimensions and we are fitting salary, so we need to use the rest 19 variables
regfitfull = regsubsets(x = hitterdata$Salary~. , data = hitterdata,nvmax = 19)
summary(regfitfull)

regfitfullsummary = summary(regfitfull)

names(regfitfullsummary)

# We can see that inside the summary, we have also R^2, RSS , adjusted R^2, Cp and BIC
# We can use these to try and select the best overall model
# Lets see what is the Rsquared value for each subset we have calculated
regfitfullsummary$rsq

# As expected R^2 statisti increaes as we include more variables.
# Its better we plot these R^2 statistic instead of just verifying in th tcommand line

# Splitting the Gui in to 2x2 gui scheme
par(mfrow=c(2,2))

?plot

# Using the "l" inside plot function with type will let us plot a line graph instead of dotted scatter plot
# Plotting the RSS with number of variables
plot(regfitfullsummary$rss,xlab = "Number of variables",ylab = "RSS",type = "l")

# Plotting the Adjustted R^2 with number of variables
plot(regfitfullsummary$adjr2,xlab = "Number of variables",ylab = "Adjusted R^2",type = "l")


# Another function to the rescue is point function which works like the plot command ,except that it puts points on a plot that has already been created, insted of creating a new plot
# which.max() is another function which can be used to identify the locaiton of maximum point of a vector.We will see which is the max point in the vector of values we  have plotted and then use that index to use it inisde the points() function.
#
which.max(regfitfullsummary$adjr2)

# It says that 11th index has the maximum value

# Now using the points function to plot the red dot on the existing plot
?points

# The arguments inside the points function are
# First argument is the index where the modification of datapoint's apperance takes place to be
# Second argument is the dataset that we are using and on which the indexes value has to be derived from
# Actually the first 2 arguments here means that x and y coordinate on the plot which we want to plot on
# And this function will plot on the current plot that is activated. Now the second one is activated and it will use that as reference until we use another plot function
# col is just argument that gives colour to the point
# cex is the argument that increases the dot size that we are plotting. Giving 2 value is the best one.
# pch is the plotting character, here 20 stands for circle, we can also have square or various other things.
points(11,regfitfullsummary$adjr2[11],col="red",cex=2,pch=20)


# In a similar fashion we can plot the Cp, and BIC and indicate the models which has the smallest statistic using which.min()
# Using line plot
plot(regfitfullsummary$cp,xlab = "Number of predictors",ylab = "Cp value",type = 'l')
# Finding the minimum of the cp values
which.min(regfitfullsummary$cp)
# 10th one is the minimum index, Now using points function to plot this point on the currently activated grpah that is nothing but the cp graph
points(10,regfitfullsummary$cp[10],col="green",cex=2,pch=20)


# Now plotting the Bic the sameway we have dont for the Cp
plot(regfitfullsummary$bic,xlab = "Number of predictors",ylab = "Bic value",type = "l")
# Finding the minimum of the bic
which.min(regfitfullsummary$bic)
# Minimum one is 6, so using this 6 index on the current plot to plot the point plot
points(6,regfitfullsummary$bic[6],col="magenta",cex=2,pch=20)



#####
## Until now we are using the summary of the fitted model that is from the regsubsets
## We can use the actual output of the regsubests instead of summary to use the built in plot command which can be used to display the selected variables

par(mfrow=c(2,2))

# Help to know more about the regsubsets plot
?plot.regsubsets

# Here it plots a plot which has variables on the x axis and what ever the criteron on the Y axis
# The plot shows a white and black plot
# It can be interpreted in the following way. For a given Y value and if we are checking for a particular R^2 value, we see the model that it gives that R^2
# In that model if we go along the X axis (Draw a line which is parellel to X axis at that particular Y value), then if a particular variable is present in that model we see a "BLACK" colored square on the plot.
# If that particular variable is not present , then it will be "WHITE" colored square
plot(regfitfull,scale = "r2")
plot(regfitfull,scale = "adjr2")
plot(regfitfull,scale = "Cp")
plot(regfitfull,scale = "bic")

# Check the above plot and in that for bic plot , we see that we have several models share a BIC close to -15-, However the model with lowest BIC (Lowest means higher negative value), has only 6 variables
# They are AtBat,Hits,Walks,CRBI,DivisionW,PutOuts
# We can use the coef() function to see the coefficeint estimates associated with this model

coef(regfitfull,19)
























