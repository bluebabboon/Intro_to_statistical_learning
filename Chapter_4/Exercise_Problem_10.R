# Here we are going to use the ISLR package which has Weekly data
library(ISLR)
Weekly

# Weekly data is return of our stockmarket which is recorded weekly for 21 years from beginning of 1990 to 2010.

weekdata = Weekly
dim(weekdata)

# It has total 1089 rows and 9 columns
fix(weekdata)
summary(weekdata)
attach(weekdata)


## a)
summary(weekdata)
?pairs
pairs(weekdata)

# Year and volume are showing positive correlation. Apart from that Today is able to specify some classification of Direction

## b)
## using logistic regression to fit full data with all the five lag variables and volume too.
# Another way of excluding variables all but except 2 , is using the following notation
# To include a variable we will say Y ~ X1+X2...and so on
# Similarily if we have X1,X2,X3 and we want to exclude X3 we can just write Y~ .-X3
# This means that include all except the one with - sign
glm_model = glm(formula = Direction ~ . - Year - Today,data = weekdata,family = binomial)
glm_model
summary(glm_model)

# From the summary we can observe that Lag2 seems to be a statistically significant variable with pvalue less than 0.05


## c)
# Plotting the confusion matrix using table function
glm_preds = predict(object = glm_model,newdata = weekdata,type = "response")
glm_preds
# ifelse function will take dataframe as first argument and then if its passed then it will give second argument as yes and if its false then third argument is applied .
# This will be done vectorically so no worries.
glm_preds = ifelse(glm_preds > 0.5,"Up","Down")
glm_preds

table(glm_preds,Direction)
# % of correct predictions are
mean(glm_preds == Direction) # 56% are correct

# Regarding the type of mistake, Considering Going Up as + and Goind Down as -

# First type error , or Type- I error is when false postivie occurs
# Its also known as (1 - Specificity) , False positive rate is FP/True_N
# True_N = 430+54 = 484
# FP (false postive) = 430; False postivie rate = 430/484 = 88%
# This 88% is Type-I error
# Specificity = 1-88% = 12%

# Second type error, or (1 - Type-II error) is when false negative occurs
# Its also known as sensitivity, True positive rate is TP/True_P
# True_P = 48+557 = 605
# TP (true postive) = 557 ; True positive rate = 557/605 = 92%
# This above 92% is actually 1-Type2 error
# So typeII error = 1-92% = 8%


## d)
# Now fitting the logistic regression using training data from 1990 to 2008 and only Lag2 as predictor

names(weekdata)
Year
dim(weekdata)

# Bool vector of data of year which shows false for year > 2008
YearUntil2008 = (Year <= 2008)

# Creating or more like splitting existing weekdata for until 2008 and after 2008
weekdataUntil2008 = weekdata[YearUntil2008,]
DirectionUntil2008 = Direction[YearUntil2008]

weekdataAfter2008 = weekdata[!YearUntil2008,]
DirectionAfter2008 = Direction[!YearUntil2008]

# Fitting the model only using data until 2008
glmModelUntil2008 = glm(formula = Direction ~ Lag2,family = binomial,data = weekdata,subset = YearUntil2008)

glmModelUntil2008

# Predicting for data apart from 2008
glmPredsAfter2008 = predict(object = glmModelUntil2008,newdata =weekdataAfter2008,type = "response" )

# Converting quantitative variables to qualitative using ifelse function
glmPredsAfter2008 = ifelse(glmPredsAfter2008 > 0.5, "Up","Down")

# Checking with table,
table(glmPredsAfter2008,DirectionAfter2008)
