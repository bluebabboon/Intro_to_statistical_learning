# using Boston data
library(MASS)
data_boston = Boston

names(Boston)
data_boston = na.fail(data_boston)
str(data_boston)
attach(data_boston)
# To fit one simple linear regression
model1 = lm(crim~zn)
model1_summary = summary(model1)
# pvalue for zn coefficient
model1_summary$coefficients[,"Pr(>|t|)"][2]


# (a)

########### Function for getting a one on one regression for all predictors ############
# lets create a function that takes a dataframe
# giving it two arguments that will take dataframe and which column index should be y

model_creator_simple_lm = function (data_input,index,onlymodel = T)
  {
    
    # attaching the names of given dataframe
    attach(data_input)

    # Creating 4 lists ,for storing model,its names and summary,its names
    modellist = list()
    summarylist = list()
    modellistnames = list()
    summarylistnames = list()
    # creating a newdata named dataframe without our column y and only has all other X's
    newdata = data_input[,-index]
    count = 1
    
    # Starting the loop to record every model
    for(i in 1:length(data_input[,-index]))
      {
      # temporary model for given y(known from index argument) and current X
      #||||# tempmodel = lm(data_input[,index]~newdata[,count])
      
      # Another way of creating a linear regression and in this we will get coefficient names correctly
      # The following works like this
      # paste( colnames(data_input)[index],"~",colnames(newdata)[count],sep = "") --> this gives output as "crim~zn" ; paste will concentate
      # but we used as.formula("crim~zn") which will tell R to consider the string inside it as formula
      # Thats it!
      tempmodel = lm(as.formula(  paste( colnames(data_input)[index],"~",colnames(newdata)[count],sep = "")  )  )
      
      # Inserting that temp model in model list at its current count
      # Similarliy inserting summaries to in another empty list created earlier 
      modellist[[count]] = tempmodel
      summarylist[[count]] = summary(tempmodel)
      
      # Getting the names for model and summaries as well
      # Inserting that current name based on count and using paste function to concentate strings
      modellistnames[[count]] = paste(names(newdata)[count],"_Model",count,sep = "")
      summarylistnames[[count]] = paste(names(newdata)[count],"_Summary",count,sep = "")
      
      count=count+1
      }
    
    # Assigning the collected names from names list to the model's list
    names(modellist) = modellistnames
    names(summarylist) = summarylistnames
    
    # Creating a final big list which will contain All models  and All summaries as well
    # We have given Its name as Allmodels and Allsummaries to access them
    Model_and_summary = list(Allmodels = modellist,Allsummaries = summarylist)
    
    # Added another argument where we dont want summaries but only models lists
    if(onlymodel==T)
      {
      modellist
      }
    else
      {
      Model_and_summary
      }
  }


########## This function gives out a list of summaries #############
# Takes the list created earlier from the function model_creator_simple lm
# Returns a list that contains summary for each model stored that we got as output from previous function

get_summary_model = function(givenmodel)
  {
    summarylist = list()
    summarylistnames = list()
    count = 1
    for(i in 1:length(givenmodel))
      {
      summarylist[[count]] = summary(givenmodel[[count]])
      summarylistnames[[count]] = paste(names(givenmodel)[count],"_summary",sep = "")
      count = count + 1
      }
    names(summarylist) = summarylistnames
    summarylist
  }


arbit_model = model_creator_simple_lm(data_boston,1,onlymodel = T)
names(arbit_model)

# This are applicable when onlymodel argument is false,Then uncomment these  " #|||# "
#||||# arbit_model["Allmodels"]
#||||# summary(arbit_model["Allmodels"][[1]][[1]])

summary_arbit_model = get_summary_model(arbit_model)


# (b)
# To fit multiple regression model now
single_multi_model  = lm(crim~.,data = data_boston)
# From the summary lets see for which predictors p values are greater than 0.01
# zn,indus,chas,nox,rm,age,tax,ptratio,black,lstat can be neglected ,All of the mentioned predictors have pvalue > 0.01
summary(single_multi_model)

# (c)
# To compare coefficients of regression in single regression vs multiple regression
# lets store all the regression coefficients created for single regressions in a vector
# we will use coef function on each element in model list we created earlier

get_coefficients = function(givenmodel)
  {
  count= 1
  coef_array = array()
  for (i in 1:length(givenmodel))
    {
    coef_array[count]=coef(givenmodel[[count]])[2]
    names(coef_array)[count] = names(coef(givenmodel[[count]])[2])
    count = count+1
    }
  coef_array
  }

arbit_coef_array = get_coefficients(arbit_model)

single_multi_coef_array = coef(single_multi_model)

plot(arbit_coef_array,single_multi_coef_array[-1])

# (d)
# We will fit a polynomial function for every predictor

model_creator_poly_lm = function(data_input,index,onlymodel = T,polydegree)
  {
  attach(data_input)
  modellist = list()
  summarylist = list()
  modelnameslist = list()
  summarynameslist = list()
  newdata = data_input[,-index]
  count = 1
  for (i in 1:length(newdata))
    {
    tempmodel = lm( as.formula( paste(colnames(data_input)[index],"~","poly(",colnames(newdata)[count],",",polydegree,")" ,sep = "" ) ))
    modellist[[count]] = tempmodel
    summarylist[[count]] = summary(tempmodel)
    modelnameslist[[count]] = paste(names(newdata)[count],"_Model",count,sep = "")
    summarynameslist[[count]] = paste(names(newdata)[count],"_Summary",count,sep = "")
    count = count +1
    }
  names(modellist) = modelnameslist
  names(summarylist) = summarynameslist
  Model_and_summary = list(AllModels = modellist,AllSummaries = summarylist)
  
  if (onlymodel == T)
    {
    modellist
    }
  else 
    {
    Model_and_summary
    }
  }

poly_all_models = model_creator_poly_lm(data_boston[,-4],1,onlymodel = T,3)

