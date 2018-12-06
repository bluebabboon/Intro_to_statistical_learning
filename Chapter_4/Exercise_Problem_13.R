## Using the boston dataset
library(ISLR)
library(MASS)

bostdata = Boston
names(bostdata)
summary(bostdata)

pairs(bostdata)


# So our crime rate which is "crim" variable has to be detected
# Lets add another variable which is 1 if crime is greater than its median and 0 if not
bostdata$crim0 = bostdata$crim
bostdata$crim0 = ifelse( bostdata$crim > median(bostdata$crim),1,0)

pairs(bostdata)

cor(bostdata$crim0,bostdata)

# The correlations are higher with nox,rad,tax,age,dis,indus,lstat
#
selnames = c("nox","rad","age","dis","indus","lstat","crim0")

names(bostdata) %in% selnames

seldata = bostdata[,names(bostdata) %in% selnames]
names(seldata)

# Splitting the seldata - selected data in to 70 and 30% train and test
trainindices = sample(1:nrow(seldata),size = 0.7*nrow(seldata))
trainindices
nrow(seldata)

train.df = seldata[trainindices,]
test.df = seldata[-trainindices,]

## Now using logistic regression to fit the data
# glm is the model
glm.model = glm(train.df$crim0 ~.,family = binomial,data = train.df)
glm.model

glm.preds = predict(object = glm.model,newdata = test.df,type = "response")
glm.preds

glm.preds = ifelse(glm.preds > 0.5,1,0)
glm.preds

# Checking with test.df's crim0
#
table(glm.preds,test.df$crim0)

# % of correct ones are 82%
mean(glm.preds == test.df$crim0)*100


## Using LDA to predict
# lda is part of mass, so importing that
library(MASS)
lda.model = lda(train.df$crim0 ~ .,data = train.df)
lda.model
lda.preds = predict(lda.model,newdata = test.df)
names(lda.preds)

table(lda.preds$class,test.df$crim0)

# % of correct ones are 85%
mean(lda.preds$class == test.df$crim0)*100

## Using KNN to predict
library(class)
knnmodel = knn(train = train.df,test = test.df,cl = train.df$crim0,k = 1)
knnmodel
table(knnmodel,test.df$crim0)

# % of correct ones are 88%
mean(knnmodel == test.df$crim0)

# Creating a function and giving it k value, it returns the % of correct predictions
# while everything else remains same
knnfunction = function(traindf,testdf,targetvar,k_no){
  library(class)
  knnmodel_here = knn(traindf,testdf,traindf[[targetvar]],k_no)
  return(mean(knnmodel_here == testdf[[targetvar]]))
}

knnfunction(train.df,test.df,"crim0",1)
knnfunction(train.df,test.df,"crim0",2)
knnfunction(train.df,test.df,"crim0",3)
knnfunction(train.df,test.df,"crim0",4)
knnfunction(train.df,test.df,"crim0",5)
knnfunction(train.df,test.df,"crim0",6)
knnfunction(train.df,test.df,"crim0",7)

# Using knn = 3 gives best result and increasing it further wont increase any % of correct ones
