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




