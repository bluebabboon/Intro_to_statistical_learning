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



















