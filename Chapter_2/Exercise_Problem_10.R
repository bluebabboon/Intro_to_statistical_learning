# Loading the dataset from MASS library

# This data frame contains the following columns:
#   
# crim -    per capita crime rate by town.
# zn -      proportion of residential land zoned for lots over 25,000 sq.ft.
# indus -   proportion of non-retail business acres per town.
# chas -    Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox -     nitrogen oxides concentration (parts per 10 million).
# rm -      average number of rooms per dwelling.
# age -     proportion of owner-occupied units built prior to 1940.
# dis -     weighted mean of distances to five Boston employment centres.
# rad -     index of accessibility to radial highways.
# tax -     full-value property-tax rate per \$10,000.
# ptratio - pupil-teacher ratio by town.
# black -   1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
# lstat -   lower status of the population (percent).
# medv -    median value of owner-occupied homes in \$1000s


library(MASS)

source("Chapter_2/user_functions.R")                                        # source("path for R file we want to import") will let us import functions stored in another R file

getwd()

boston_data=Boston
?Boston

names(boston_data)

str(boston_data)

summary(boston_data)

dim(boston_data)

fix(boston_data)

class(boston_data)

attach(boston_data)

#### (a)
# number of rows and number of columns
nrow(boston_data)
ncol(boston_data)

#### (b)
# using custom function from user_functions to plot scatter plots between crime and all other features
# we can see negative correlation between crime and distance to employment centers,median value of homes
# positive correlation between age of houses,avg number of rooms
pair_plotter_function(boston_data,crim)


#### (d)
# To get suburbs with high crime rates

mean(crim)
suburbs_more_crime = boston_data[crim > mean(crim),]                                          # selecting all the rows that have crime rate more than mean of crime rate and assigning it to new dataframe                 
range(crim)                           
row.names(suburbs_more_crime)                                                                 # We can see which indexes are collected here 

# lets say we want crime rates where distance to emp < its avg and median value of house < its avg 
# and number of romms > its avg 
suburbs_more_crime = boston_data[dis < mean(dis) & medv<mean(medv) & rm>mean(rm),]

# say that we want only crime and medv and age and rm columns,but other than that we dont want any
suburbs_more_crime = subset(suburbs_more_crime,select = c("crim","medv","rm"))                # subset() is a useful function to select multiple columns only that we want,and assign it to new frame,takes select argument

# Observe with the conditions we considered the mean of crime rates for selected data increased by 3 times
# Just from simple observations from scatter plots and narrowing down the observations will be very helpful
mean(suburbs_more_crime[,"crim"])             


#### (e)
# Number of suburbs bound by charles river
sum(chas)


#### (f)
median(ptratio)


#### (g)
min(medv)
# selecting the suburbs aka Row names where the median value of owner homes is smallest
row.names(boston_data[medv==min(medv),])                                                                    # Remember the syntax for selecting certain row based on given conditions in columns dataframe[dataframe$col1=="some value",]
low_medv_rows=row.names(boston_data[medv==min(medv),])
# other values of features for that particular rows with lowest median house value
boston_data[low_medv_rows,]


#### (h)
morethan7rooms = boston_data[rm > 7,]
# number of rooms with more than 7 rooms
nrow(morethan7rooms)
# crime rate is pretty low eventhough we saw in scatter plot a positive correlation between crime and no.of.rooms
# Maybe because its not at highest number of rooms but say in between ,i.e., at 4-6 rooms not at extremes
mean(morethan7rooms$crim)
range(morethan7rooms$rm)

morethan8rooms = boston_data[rm > 8,]
nrow(morethan8rooms)
# Here also the crime rate is not that high
mean(morethan8rooms$crim)



