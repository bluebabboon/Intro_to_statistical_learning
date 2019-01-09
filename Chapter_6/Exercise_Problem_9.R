# Using the college dataseet which is part of ISLR library
library(ISLR)

collegedata = College

nrow(collegedata)
dim(collegedata)

names(collegedata)
fix(collegedata)

str(collegedata)
summary(collegedata)

# The goal of this problem is to predict the number of applications receive using the other variables
#   in the dataset


## a)
# Split the dataset in to a training set and a test set
# Lets split into 70 30 ratio
set.seed(1)
train = sample(1:nrow(collegedata),0.7*nrow(collegedata),replace = T)
test = -train






































































































