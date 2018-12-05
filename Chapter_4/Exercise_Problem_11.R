library(ISLR)
autodata = Auto
autodata
fix(autodata)
names(autodata)
colnames(autodata)
attach(autodata)


## a)
# Calculating the median of mpg
median(mpg)     # its 22.75

# Creating a dummy variable mpg01 with same length as mpg
# Just adding another column to our existing autodata - dataframe
# We are assuming all the values in autodata$mpg01 to 0.
autodata[["mpg01"]] = 0

# Now this function shows us that there is another column named "mpg01"
names(autodata)

# Selecting the column mgp01 and then reselecting the indexes for which the condition
# mpg value > its median and then for those indexes we are changing the value to 1
autodata$mpg01[mpg > median(mpg)] = 1
autodata$mpg01
attach(autodata)


## b)
# Drawing a scatter plot
pairs(autodata)
# Horsepower seems to be giving a good estimate to calculate mpg01, when high horsepower is observed , we have low mpg and vice versa
# displacement,Horsepower, weight and acceleration are showing correlation with mpg

# While selecting some columns we want to select only some of them and dont use the rest
# For example here we dont need names to plot, so we want all columns in the dataframe except for the names. To do that
# First run this command names(autodata) %in% c("horsepower","acceleration").
names(autodata) %in% c("horsepower","acceleration")
# It returns a boolean array with false and true correspondingly
# It has False whereever the name of dataframe column doesn't match with what is there in horsepower and acceleration
# It is true when the name of column is existing in the c() above
# The length of output is equal to the number of columns
# To select first row and first column
autodata[1,1]
# Another way
autodata[1,c("mpg")]
# Another way using boolean
autodata[1,c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)]

# Now to not select the first one and use the rest, just keep "!" before the c
autodata[1,!c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)]
# We can see that except "mpg" everything is selected

# We can see that the following produces that bool array
names(autodata) %in% c("mpg")


# To apply what we have learnt , lets not select "name" in the dataframe and select everything else
auto_noname = autodata[,!(names(autodata) %in% c("name"))]
names(auto_noname)
# Hurray ,there is no name selected in the new dataframe we have created.

# Plotting box plot is not good without standardizing
boxplot.matrix(as.matrix(auto_noname))

# Lets standardize data using scale function
# When you scale something we are returned a vector , instead we want a data.frame
# So we can use as.dataframe on top of scale
scaled.auto_noname = as.data.frame(scale(auto_noname))

# Plotting after scaling.
boxplot.matrix(as.matrix(scaled.auto_noname))


## c)
# To split the data in to training set and testing set
# We can use sample function here to randomly select from the % we are going to select
# Lets select 75% to train and rest 25% to test
set.seed(42)
# Lets see how sample works. First we give from where we want to sample, and then how much we want to sample with size argument
sample(c(1,2,3,4),size = 1)
indices_to_sample = sample(1:nrow(autodata),size = 0.70*nrow(autodata))

train.df = autodata[indices_to_sample,]

# Selecting the rest of data by keeping - sign before the index value
test.df = autodata[-indices_to_sample,]



## d)
# Using lda to predict mpg01 variable with these variables displacement,horsepower,weight and acceleration
names(train.df)
# Importing lda from mass
library(MASS)


lda.fit = lda(mpg01~displacement+horsepower+weight+acceleration,data = train.df)
lda.fit
# Observing the linear discriminants, we have it highest for acceleration

lda_preds = predict(object = lda.fit,newdata = test.df)
lda_preds  # A list of 3

# Now calculating confusion matrix with table()
table(lda_preds[["class"]],test.df$mpg01)
# % of correct are 88%
mean(lda_preds$class == test.df$mpg01)*100



## e)
# Now predicting using qda instead of lda
qda.fit = qda(mpg01~displacement+horsepower+weight+acceleration,data = train.df)
qda.fit
qda_preds = predict(qda.fit,newdata = test.df)
qda_preds   # A list of 2

# Now calculating confusion matrix with table()
table(qda_preds$class,test.df$mpg01)
# % of correct ones are 86%
mean(qda_preds$class == test.df$mpg01)*100
# % of error is 13%
(1 - mean(qda_preds$class == test.df$mpg01))*100



## f)
# using logistic regression, glm
glmfit = glm(mpg01~displacement+horsepower+weight+acceleration,family = binomial,data = train.df)
glmfit

# p values are significant for horsepower and weight
glmpreds = predict(object = glmfit,newdata = test.df,type = "response")

# converting probabilites to 0 and 1 based on threshold of 0.5
glmpreds = ifelse(glmpreds > 0.5,1,0)

# An interesting fact here is that the indexes numbers that have been selected for test.df  are shown in the predictions here. It didn't happen with lda predictions and qda predictions
glmpreds
test.df$mpg01

# confusion matrix with logistic regression on our data
table(glmpreds,test.df$mpg01)

# % of correct ones are 88%
mean(glmpreds == test.df$mpg01)

# % of incorrect ones are 11%
(1 - mean(glmpreds == test.df$mpg01))*100


## g)
# using knn now
# we have to import class library which has knn
library(class)

# Here it will say dimesions wont match if we dont do it this way
knnfit = knn(train = train.df[,c("displacement","horsepower","weight","acceleration")],test = test.df[,c("displacement","horsepower","weight","acceleration")],cl = train.df$mpg01,k = 1)

# Another way is to create data and then use cbind and then use that
trainknn = train.df[,c("displacement","horsepower","weight","acceleration")]
testknn = test.df[,c("displacement","horsepower","weight","acceleration")]

knnfit = knn(train = trainknn,test = testknn,cl = train.df$mpg01,k = 1)

# confusion matrix with knn on our data
table(knnfit,test.df$mpg01)

# % of correct predictions are 85%
mean(knnfit == test.df$mpg01)*100

# % of error , or we can say incorrect predictions are 14%
(1-mean(knnfit == test.df$mpg01))*100
