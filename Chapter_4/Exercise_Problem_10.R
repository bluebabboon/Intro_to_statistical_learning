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
# Here we have to include type = "response" , otherwise instead of probabilities it will return log odds
# log odds are log of ratio of success to failure.
# Say logodds(Going Up) =log( (ProbofUP)/(ProbofDown) )
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

# % of correct predictions are
mean(glmPredsAfter2008 == DirectionAfter2008)*100

## e)
# Now using LDA to predict
# lda is part of mass library, so loading it
library(MASS)

lda_fit = lda(Direction ~ Lag2,data = weekdataUntil2008)
lda_fit

# Our lda predictions as discussed earlier is list of 3, class, posteriorprob and LD1
lda_preds = predict(object = lda_fit,newdata = weekdataAfter2008)

# confusion matrix
table(lda_preds[["class"]],DirectionAfter2008)
# We got same confusion matrix as logistic regression

# % of correct predictions are
mean(lda_preds[["class"]] == DirectionAfter2008)*100

## f)
# Now using QDA to predict
qda_fit = qda(Direction~Lag2,data = weekdataUntil2008)
qda_fit

# predicting on data after 2008
qda_preds = predict(object = qda_fit,newdata = weekdataAfter2008)
qda_preds

# Confusion matrix
table(qda_preds[["class"]],DirectionAfter2008)
# % of correct predictions are
mean(qda_preds[["class"]]==DirectionAfter2008)*100


## g)
# Now using KNN to predict
# KNN is part of class library, so importing that first
library(class)

# If you dont modify the dataframe when using single variable the dimension of it throws error. So its better to always make new dataframe using cbind or as.matrix
# Then use that on the splitted data.
# No need to modify response data in cl, just give that as a column of dataframe
# Column of dataframe has null dimension
# cbind() ones have dimension as like matrix
# Dataframe has dimension, but not column of dataframe.
knn_fit = knn(train = cbind(weekdataUntil2008$Lag2)
              ,test = cbind(weekdataAfter2008$Lag2)
              , cl =weekdataUntil2008$Direction,k = 1)
knn_fit

# Confusion matrix
table(knn_fit,DirectionAfter2008)

# % of correct predicitons are
mean(knn_fit == DirectionAfter2008)*100

## h)
## Of all the models knn-50% , qda-58% ,glm - 62% , lda - 62.5%
# So lda and logistic regression are better to fit.


## i) Now to experiment using different variables, and transformations as well
## Considering Lag1 along with Lag2


glm.fit = glm(Direction ~ Lag2+Lag2^2,family = binomial,data = weekdataUntil2008)
glm.fit
glm.pred = predict(glm.fit,newdata = weekdataAfter2008,type = "response")
glm.pred
glm.pred = ifelse(glm.pred > 0.5, "Up","Down")
mean(glm.pred == DirectionAfter2008) # Gives 62.5% correct



glm.fit = glm(Direction ~ Lag2+log(abs(Lag2)),family = binomial,data = weekdataUntil2008)
glm.fit
glm.pred = predict(glm.fit,newdata = weekdataAfter2008,type = "response")
glm.pred
glm.pred = ifelse(glm.pred > 0.5, "Up","Down")
mean(glm.pred == DirectionAfter2008) # Gives 62.5% correct


glm.fit = glm(Direction ~ Lag2+Lag2^2+Lag^3,family = binomial,data = weekdataUntil2008)
glm.fit
glm.pred = predict(glm.fit,newdata = weekdataAfter2008,type = "response")
glm.pred
glm.pred = ifelse(glm.pred > 0.5, "Up","Down")
mean(glm.pred == DirectionAfter2008) # No improvement, same 62.5%


lda.fit = lda(Direction ~ Lag2+Lag2^2,data = weekdataUntil2008)
lda.fit
lda.pred = predict(lda.fit,newdata = weekdataAfter2008)
lda.pred # list of 3;
lda.classes = lda.pred[["class"]]
mean(lda.classes == DirectionAfter2008)*100 # No improvement, same 62.5%

# Increasing the k neighbours in knn to 10
knn.fit = knn(train = cbind(weekdataUntil2008$Lag2),test = cbind(weekdataAfter2008$Lag2),cl = DirectionUntil2008,k = 10)
knn.fit
mean(knn.fit == DirectionAfter2008)*100   # 55%

# knn neighbours to 30
knn.fit = knn(train = cbind(weekdataUntil2008$Lag2),test = cbind(weekdataAfter2008$Lag2),cl = DirectionUntil2008,k = 30)
knn.fit
mean(knn.fit == DirectionAfter2008)*100 # 51%

# knn neighbours to 25
knn.fit = knn(train = cbind(weekdataUntil2008$Lag2),test = cbind(weekdataAfter2008$Lag2),cl = DirectionUntil2008,k = 25)
knn.fit
mean(knn.fit == DirectionAfter2008)*100 # 53%

# knn neighbours to 20
knn.fit = knn(train = cbind(weekdataUntil2008$Lag2),test = cbind(weekdataAfter2008$Lag2),cl = DirectionUntil2008,k = 20)
knn.fit
mean(knn.fit == DirectionAfter2008)*100 # 58%

# knn neighbours to 17
knn.fit = knn(train = cbind(weekdataUntil2008$Lag2),test = cbind(weekdataAfter2008$Lag2),cl = DirectionUntil2008,k = 17)
knn.fit
mean(knn.fit == DirectionAfter2008)*100 # 59%


# knn neighbours to 16
knn.fit = knn(train = cbind(weekdataUntil2008$Lag2),test = cbind(weekdataAfter2008$Lag2),cl = DirectionUntil2008,k = 16)
knn.fit
mean(knn.fit == DirectionAfter2008)*100 # 56%


# knn neighbours to 15
knn.fit = knn(train = cbind(weekdataUntil2008$Lag2),test = cbind(weekdataAfter2008$Lag2),cl = DirectionUntil2008,k = 15)
knn.fit
mean(knn.fit == DirectionAfter2008)*100 # 58%

# So optimized knn is between 15 to 20 and max lies for around k = 17-18
# Lda and glm with lag^2 as variable gives highest result

