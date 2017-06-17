# "#" is the one to comment

# following command to clear the console
# apparently this following command sends Ctrl + L 
# to clear the console

cat("\014")

# this one also works as the above

cat("\f")

############ CHAPTER - 1  ############

# INTRODUCTION # overview of statistical learning
# We have supervised learning where we have labeled output y
# with input x
# Unsupervised learning where there is no output y only input x

## We mainly use three datasets in this learning 



######### Wage Data #############
# this is mainly for regression as it gives continous output
# contains 3000 rows with 12 variables to indicate wage,with year,age ,sex ...etc
# library(islr)     -- loads the library
# a <- Wage         -- assigns wage dataset loaded from library to a

library(ISLR)
wage_dataset <- Wage

# shows the shape or dimension of dataset
dim(wage_dataset)

# shows the column names or each variable names as a vector of strings in a dataset
names(wage_dataset)

# shows the summary of dataset for each column,,,, shows this ---> (min,1st qu,median,mean,3rd qu,Max)
summary(wage_dataset)



######### Stock Market Data ##############
# We may often want to predict non - numerical value
# Also called as categorical or qualitative output
# Contains daily movements of S&P 500 Index over a period of 2001 to 2005
# library(islr)            -- loads the library
# stock_data <- Smarket    -- assign Smarket dataset loaded from library to stock_data variable

# This stock dataset only says and we have to predict whether
# the s&p index will go "up" or "down"
#  A classic "Classification Problem" -haha get it :)
# We will learn in detail about this in chapter 4
stock_dataset <- Smarket

dim(stock_dataset)

names(stock_dataset)

summary(stock_dataset)


######### Gene Expression Data ###########
# Often we have datasets with both input and output
# this dataset only has input .This is used for clustering to group similarites
# Contains 6830 rows of gene expressions for each of  64 variables(cancer cells) 
# so basically 64 rows (each row is an observation of cancer) and 6830 columns 
# where each column is gene expression with total 6830 gene expressions
# Instead of predicting a particular output we are interested in determining
# whether there are groups or clusters among cells
# library(islr)              --loads the library
# gene_data <- NCI60         -- assign NCI60 dataset to gene_data

gene_data <- NCI60

dim(gene_data)

names(gene_data)

summary(gene_data)



# Brief History of Statistica Learning
# Method of least squares which we still follow for linear regression
# Linear discriminant analysis became converted by an alternative approach to Logistic Regression
# Nelder and Wedderburn coined Generalized linear models for entire class of statistical learning methods

# Classification and regression trees were later introduced in 80's












