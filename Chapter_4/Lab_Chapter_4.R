# importing the ISLR library
library(ISLR)

# To view the Smarket dataset imported from ISLR library
Smarket

# fix will open the data in GUI format to edit or view
fix(Smarket)
# dim to check the number of rows and columns in dataset
dim(Smarket)
# names of the columns in the dataset
names(Smarket)

# A summary of data typically min 1st quartile Median min 3rd quartile and max in each column
summary(Smarket)
# colnames moreover same as names
colnames(Smarket)

# rownames will give what is the name that is given to each row in the dataset
rownames(Smarket)

# Assiging the data to variable named stock data
stockdata=Smarket

# attaching the data to our process so that we can use the column names without always referring the to data
attach(stockdata)

# pairs function to do a pair plot between each column with each other sort of like covariance matrix
pairs(Smarket)

# cor function computes the correlation of the matrix which is called inside it. Each covariance that is divided by its respective columns std deviation is correlation. 
# This will not work,it needs to be numeric
?cor
cor(Smarket)

# Taking all columns except 9th one because it is directional
# And the rest of them are numeric
cor(Smarket[,-9])

#########
# LOGISTIC REGRESSION
#########
# glm is the function to fit logistic regression, glm stands for generalized linear models , a class of models which also included logistic regression
# To use logistic regression we have specify family as binomial inside glm
?glm

glm.fit = glm(Direction~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data = Smarket, family = binomial)

# now getting some details of glm.fit object by calling summary on it
summary(glm.fit)
# this will give results for deviance , Deviance means (I will search it and update)
# Smallest p value is given for Lag1 and its coefficient which is indicated by estimate states that if market had postitivie return yesterdy then it isless likely to go up today

# Using coef function to just get coefficients of the model
coef(glm.fit)

# another way to do this is accessing objects with $ in the inside of glm.fit
glm.fit$coefficients

# another way is to call $ on the summary of glm.fit $ is known as "extractor operator"
summary(glm.fit)$coef

# getting only the coefficient estimates from the dataframe summary(glm.fit)$coef
summary(glm.fit)$coef[,1]


# predict function can be used to predict the probabliity that the market will go up,given the values of predictors in our data
?predict
model_probs = predict(glm.fit, type = "response")
# Now calling the probability predictions for given 1 to 10 rows of our data using our fitted model
model_probs[1:10]
# if we just call model_probs it will give us probabilities that P(Y=1|X) , ie probability that market will go up given our data for all the rows of data.


# We got probabilites but they are in numbers ,but we want a prediction in categorical way saying whether market will go up or down. So we have to convert these numbers to "UP" or "DOWN"
?contrasts
contrasts(Direction)

# Creating a copy of 1250 rows of data of "Down", rep function will help in this
?rep
model_preds = rep("Down",1250)

# Now from this data we will change each element to UP where the probability in model_probs data is > 0.5, R has this speciality to change one array's value based on another array's value just by calling inside it ,sort of like condition
# This means select all the indexes for which model_probs> 0.5 and the values of elements for these indexes in model_preds is then assigned the value "UP"
# We can verify this by checking the lengths 
length(model_preds)                           # This will be 1250
length(model_preds[model_probs>0.5])          # This will be 964
# It means that out of 1250 rows we have 964 rows which are predicted as Up and the rest are denoted as DOWN(which are by default)

# If we check model_preds it only has Down elements
model_preds

model_preds[model_probs>0.5]
model_preds[model_probs>0.5] = "Up"
# If we check model_preds it has both Up and Down at relevant places
model_preds


# Given the predictions we want to see the confusion matrix of these predicted model ,we can do that with table() function
?table
# takes two arguments , checks whether the array of elements of first one matches with second and gives confusion matrix as output 
# table function is not case sensitive , so be cautious in comparing the arrays
table(model_preds,Direction)

# To get the proportion of elements that are same we can use the following
# model_preds == Direction will create a new vector with bool values and give TRUE or FALSE accordingly
# mean will give the average of total true values among them. So total 52% of values are predicted correctly
mean(model_preds == Direction)                

# So total number of values that are correctly predicted are, 652
length(model_preds)*mean(model_preds == Direction)





# Here in the above method we used all the data to train and then we are comparing results with actual data. But we rather want to predict on the data the model has not trained on
# So we are going to split the data or make a copy and then train the model on that
# Lets train the model on the data from year 2001-2004 and then test it on 2005 data

# Creating a vector of bool values which has false if year is > 2005 and true if year < 2005
# This data has TRUE values until 998 and FALSE from there on
train_vector = (Year<2005)

# Smarket[1,] will give ouput as 1st row of this dataframe and all columns
# Smarket[c(TRUE,TRUE,TRUE),] will give output as 1st,2nd and 3rd row of this dataframe and all columns
# Smarket[train_vector,] will give output as all the rows for which its true , i.e., all the rows for which year is < 2005
# Smarket[!train_vector,] will be opposite of the above , i.e., all the data for which year is = 2005 and above. And we are going to save it in the following variable
market_2005_data = Smarket[!train_vector,]
dim(market_2005_data)              # This will be 252 rows and 9 columns

# Similarily saving the response variable data of only 2005 year
# Since Direction itself is a vector no need to add a "," to select any column after that.
Direction_2005_data = Direction[!train_vector]

# Fitting the data to new model
# Here we are using subset of data of the full dataset Smarket ,by using an argument called subset in the glm function 
model_except_2005 = glm(Direction~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                        data = Smarket, family = binomial, subset = train_vector)
summary(model_except_2005)
?predict.glm

# Now predicting using new model
model_except2005_probs = predict(model_except_2005,
                                 newdata = market_2005_data,
                                 type = "response")

model_except2005_probs

# Using the same approach as above we want to see the number of ups and downs
# Since we have to test this on new data of 252 elements
model_except2005_probs_categorical = rep("Down",252)
model_except2005_probs_categorical[model_except2005_probs>0.5] = "Up"

table(model_except2005_probs_categorical, Direction_2005_data)

# Proportion of data that is correctly predicted is 48%, so the test error rate is 52%
mean(model_except2005_probs_categorical == Direction_2005_data)




# Since we got worse results by taking the 2005 data to test and rest to train lets do it in another way
# Lets train data but only considering the features of Lag1 and Lag2 and everything remains same as above

model_except_2005V1 = glm(Direction~Lag1+Lag2,
                          data = Smarket,
                          family = binomial,subset = train_vector)
model_except2005_probsV1 = predict(object = model_except_2005V1,
                                   market_2005_data,
                                   type = "response")

model_except2005_probsV1

model_except2005_probs_categoricalV1 = rep("Down",252)
model_except2005_probs_categoricalV1[model_except2005_probsV1>0.5] = "Up"

table(model_except2005_probs_categoricalV1,Direction_2005_data)

# There appears to be a little improvement compared to earlier and its predicting 56% of values correctly
mean(model_except2005_probs_categoricalV1 == Direction_2005_data)

# Instead of predicting on the full data suppose we want to predict on the particular values of Lag1 and Lag2 (In the above we used market_2005_data to predict)
# say [Lag1,Lag2] is [c(1.2,1.5),c(1.1,-0.8)]
# create a dataframe
data_to_predict = data.frame(Lag1 = c(1.2,1.5), Lag2 = c(1.1,-0.8))

# Then calling the predict function
predict(object = model_except_2005V1,newdata = data_to_predict,type = "response")







###########
# LINEAR DISCRIMINANT ANALYSIS
###########
# lda is part of library called "MASS"
library(MASS)
lda_fit = lda(Direction~ Lag1+Lag2,data = Smarket,subset = train_vector)
lda_fit

# calling above lda_fit model will gives us prior probabilites of our response and then group means for each predictor
# Group means mean that its the average of that predictor in that particular response type 

# And then it gives us coefficients of linear discriminants - These are like indication of each predictor's significance in discriminating or seperating the groups from each other. 

# To know further about what linear discriminants are refer this
# https://sebastianraschka.com/Articles/2014_python_lda.html#principal-component-analysis-vs-linear-discriminant-analysis


summary(lda_fit)

plot(lda_fit)

lda_predictions = predict(lda_fit,newdata = market_2005_data)
lda_predictions

# The predictions of lda will output a list of 3,where the first column is a class which tells which class here "up" or "down" each observation belongs to 
# Second one is named as posterior which is nothing but posterior probability P(Y=k|X=x) , it has 2 columns which is nothing but for a given observation it is giving posterior probability for each of the class, The higher probability class is then shown in the class column which is discussed above
# Third one is column of linear discriminant. Here we have output for First linear discriminant
# Refer the link above which is posted to know more about linear discriminant.In that blog he says that linear discriminant are similar to principal components the only difference is that principal components do a unsupervised discriminantion betweeen our observations to have maximized variance, where as linear discriminants do a supervised discriminantion i.e., discrimination to get maximized groups with more variance between classes

names(lda_predictions)

classes_of_lda = lda_predictions[["class"]]
classes_of_lda

# Now plotting the confusion matrix between our classes and actual data
table(classes_of_lda,Direction_2005_data)
# As we can see positive predicitons or true predictions are 35+106 and false data are 35+76

# To know the percentage of correct predictions we are calculating the mean of correct results

mean(classes_of_lda == Direction_2005_data)
# So in total 55.9% are correct predicitons , Here we are testing this on testing data as 2005 data is unseen on the lda algorithm

# Apart from that if we want to know the number of observations that have probs > 0.5
# So the below lda_predictions$posterior will give 2 columns "Down" and "Up" column with each column's probability
# So if we want the sum of those observations whose Down probability is > 0.5 then use the follwing
lda_predictions$posterior[,1] >= 0.5
# Below command will output 70, so that means on 70 days the probability of market being down is >= 0.5
sum(lda_predictions$posterior[,1] >= 0.5)

# The number of days the market being down probability is < 0.5 is 
# It will output 182 days
sum(lda_predictions$posterior[,1] < 0.5)

# For example if we want the number of days where market being down proabability is more that 90% then
sum(lda_predictions$posterior[,1] > 0.9)
# It will output 0 , that means there is no day at all where the market being down is > 90%
# Infact from the follwing command we know that max probability that the market being down is 52%
max(lda_predictions$posterior[,1])






###########
# QUADRATIC DISCRIMINANT ANALYSIS
###########
# Qda is also part of mass library, we have already called it so no need to call it again
?qda
qda_fit = qda(Direction~Lag1+Lag2, data = Smarket,subset = train_vector)
qda_fit

qda_predictions = predict(qda_fit,newdata = market_2005_data)
qda_predictions
# Similar to lda predictions our qda predictions is list of 2, we have class and posterior columns in qda_predictions.

# Plotting the confusion matrix
# calling the table function between class of qda_predictions and actual data of 2005
table(qda_predictions$class,Direction_2005_data)

# % of correct predictions are
# 59.92 which is pretty good above the lda predictions that we got earlier
mean(qda_predictions$class == Direction_2005_data)





###########
# K- NEAREST NEIGHBORS
###########
# knn() function is part of class library
# This one works differently than previous ones
# We are making a model and then fitting the predictions on that model
# Here we are going to form predictions using single command which requires 4 inputs
library(class)
?knn

# We have make new data for training and testing and also predictions for training and then give them as inputs for our knn

# To make training set that contains observations for years 2000-2004 except 2005
# To do this we are going to club Lag1 and Lag2 columns and then select only the TRUE values of train_2005 vector, basically selecting this will select observations that has true in this
# cbind will column bind Lag1 and Lag2 and then in the resulting dataframe object we are selecting the rows based on train_vector guidance
?cbind()
train_data = cbind(Lag1,Lag2)[train_vector,]
test_data = cbind(Lag1,Lag2)[!train_vector,]
train_Direction = Direction[train_vector]

?set.seed()
set.seed(42)
knn_predictions = knn(train_data,test_data,cl = train_Direction,k = 1)

knn_predictions
table(knn_predictions,Direction_2005_data)

# % of correct ones are 50%
mean(knn_predictions == Direction_2005_data)

# For k=1 only 50% are correctly predicted , lets use k=3

knn_predictions_3 = knn(train = train_data,test = test_data,cl = train_Direction,k = 3)
knn_predictions_3
table(knn_predictions_3,Direction_2005_data)

# % of correct ones are 53%, which is pretty good improvement
mean(knn_predictions_3 == Direction_2005_data)

# By far of all the data we have seen , qda seems to be a better one to fit than all other models to this data










############################
######## An Application to Caravan Insurance Data
############################
Caravan
dim(Caravan)
attach(Caravan)
names(Caravan)
summary(Caravan)

caravan_data = Caravan

# Caravan data is dataset includes 85 predictors that measure demographic characteristics for 5822 individuals. The response variable is Purchase which indicates whether or not a given individual purchases a caravan insurance policy. In this dataset only 6% people purhcased caravan insurance.

# knn predicts the class of given observationo based on identifying the observations nearest to it. So the scale of observations matters a lot. We need to standardize the data befor predicting and training the model on that data.
# scale() function will help us in standardizing the dataset

?scale

# Here in our dataset we have 86 columsn out of which 86th one is qualitative, i.e., factor type, we cant scale a factor type so we have to exclude that column 

scaled_X_caravan = scale(caravan_data[,-86])
caravan_data

# We can quickly check whether the data is scaled or not by checking the standard deviation and mean

var(caravan_data[,1])
var(scaled_X_caravan[,1])
mean(caravan_data[,1])
mean(scaled_X_caravan[,1])


var(caravan_data[,75])
var(scaled_X_caravan[,75])
mean(caravan_data[,75])
mean(scaled_X_caravan[,75])
# So all the columsn of scaled or we can say standardized data has variance of one and mean of 0

# We are now going to split the dataset to test and train
# Setting the first 1000 observations to test set and rest of them to train

test_selection = 1:1000
# This test_selection vector is numeric with values from 1 to 000
# Another way of selecting data instead of using bool values is to use index.
# Here we have created test_selection array of 1 to 1000 , then we will use this as index and call it inside the data like data[test_selection,] will give us all the rows with that particular indexes
train_X = scaled_X_caravan[-test_selection,]
test_X = scaled_X_caravan[test_selection,]
train_Y = Purchase[-test_selection]
test_Y = Purchase[test_selection]

set.seed(42)
knn_predictions_caravan = knn(train = train_X,test = test_X,cl = train_Y,k = 1)

# Total of all the predictions we have 88.7% correct
# That means 11.7% wrong predictions
# This seems good enough but only 6% of our buyers actually bought insurance , So random guessing all as No will give better result than this
mean(knn_predictions_caravan == test_Y)
mean(knn_predictions_caravan != test_Y)
mean(test_Y != "No")

# Here lets suppose that there is cost involving with each customer saying No and company wants to sell insurance to only those who will buy it. So the overall error rate is not interest. Instead we would be interested in in fraction of individuals that are correctly predicted to buy insurance
# To know the correctly predicted ones lets use table() function

table(knn_predictions_caravan, test_Y)

# According to the table out of 74 people whom the algorithm here knn predicted as that will buy insurance 10 people has actually bought it
# So we have 13.5% as correct people who bought insurance which is double the rate that would be obtained by random guessing. Here random guessing is similiar to not trying to guess at all and just following the data. Following the data will give us 6%

# Now guessing with k =3
knn_predictions_caravan3 = knn(train = train_X,test = test_X,cl = train_Y,k = 3)
mean(knn_predictions_caravan3 == test_Y)
mean(knn_predictions_caravan3 != test_Y)

table(knn_predictions_caravan3, test_Y)
# Here using k=3 makes that out of 25 people whom our algorithm has said that will buy insurance 5 people actually bought it.



# Now guessing with k =5
knn_predictions_caravan5 = knn(train = train_X,test = test_X,cl = train_Y,k = 5)
mean(knn_predictions_caravan5 == test_Y)
mean(knn_predictions_caravan5 != test_Y)

table(knn_predictions_caravan5, test_Y)
# Using k=5 makes that out of 15 people whom our algorithm has said that will buy insurance 4 people bought it. The increase is pretty good.


# As a comparison we can fit logistic regression model to the data.

# Fitting the model
lgfit_caravan = glm(formula = Purchase~.,family = binomial,data = caravan_data,subset = -test_selection)

#  Predicting on the fitted model with type as respose and data is test selection data of the caravan
lgfit_probs = predict(object = lgfit_caravan,newdata = caravan_data[test_selection,],type = "response")

# Creating a temp row of all Nos
lgfit_preds = rep("No",1000)

# Changing the probs value to Yes if predicted probability is > 0.5
lgfit_preds[lgfit_probs > 0.5] = "Yes"

# Drawing a comparison table with predicted probs and actual data
table(lgfit_preds, test_Y)
# Apparently our prediction is worse, since out of all predicted 7 people none of them bought insurance. Lets increase the threshold value of probability for 0.25 insted of 0.5
# 
lgfit_preds = rep("No",1000)
lgfit_preds[lgfit_probs > 0.25] = "Yes"

table(lgfit_preds, test_Y)
# This prediction is pretty good , since out of all 33 people 11 are buying insurance.
