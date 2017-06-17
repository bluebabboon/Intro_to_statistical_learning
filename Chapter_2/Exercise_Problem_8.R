################ Problem 8 ################

# . Private : Public/private indicator
# . Apps : Number of applications received
# . Accept : Number of applicants accepted
# . Enroll : Number of new students enrolled
# . Top10perc : New students from top 10% of high school class
# . Top25perc : New students from top 25% of high school class
# . F.Undergrad : Number of full-time undergraduates
# . P.Undergrad : Number of part-time undergraduates
# . Outstate : Out-of-state tuition
# . Room.Board : Room and board costs
# . Books : Estimated book costs
# . Personal : Estimated personal spending
# . PhD : Percent of faculty with Ph.D.'s
# . Terminal : Percent of faculty with terminal degree
# . S.F.Ratio : Student/faculty ratio
# . perc.alumni : Percent of alumni who donate
# . Expend : Instructional expenditure per student
# . Grad.Rate : Graduation rate


# (a)
## lets see working directory
getwd()

college_data=read.csv("data/College.csv",header = T)           # header is first row and we are specifying that it exists

names(college_data)                                            # to view the header,or feature names   

# This is important function to check all the features at once to determine whether they are categorical or quantitative
str(college_data)

# (b)
fix(college_data)                                              # Opens the data editor

dim(college_data)                                              # Should give 777 observations and 19 features

attach(college_data)                                           # No need to type College_data$featurename, it loads feature names in to memory,so that we can just type it

# to select first two rows and first three columns

college_data[c(1,2),1:3]

# gives feature names or column names of the dataframe
colnames(college_data)

# gives row names of college_data frame
rownames(college_data)

# Not only knowing the row or column names we can also set those names to particular value or array

rownames(college_data)=college_data[,1]                        # Setting the row names for dataframe to first column of dataframe

dim(college_data)                                              # 777x19; Giving row names will not change the dataframe dimension,it only gives names as pointers to observations

rownames(college_data)

college_data["Abilene Christian University",]                  # selecting only first row by typing rowname,can also use index; college_data[1,]
# Both these above and below gives same output
college_data[1,]

fix(college_data)                                              # You can see that we have new column with row.names,But R will not perform calculations on these.

# We need to eliminate the first column of data where names are there(not row.names,column after that one)
# We can reassign the dataframe excluding first column(can be done easily by using - )

college_data=college_data[,-1]

dim(college_data)                                              # Observe that first column is deleted(first column is names of colleges,not row.names although it appears to be first in data editor)

fix(college_data)

# (c)

summary(college_data)

pairs(college_data[,1:10])                                     # Scatter plots of only first 10 columns

plot(Private,Outstate,col="red",varwidth=T)

# rep replicates a value for a given number of times
?rep                                                           # rep(x,times) replicates x for given number of times,and returns a vector

Elite= rep("No",nrow(college_data))                                                  # nrow(dataframe) returns the number of rows present in that dataframe
                                                               # We are creating another vector Elite,to find Elite colleges where Top10perc > 50 students
Elite[Top10perc>50]="Yes"                                      # Top10perc > 50 creates a array containing TRUE,FALSE ,for each row or say index
                                                               # We then say that whereever we have True assign that value as "Yes",previously all are "No"      
Elite = as.factor(Elite)                                       # Converting the typeof Elite from character to factor ,making it a qualitative variable,with two levels "Yes","No"

college_data=data.frame(college_data,Elite)                    # Reassigning the dataframe by adding a new feature, new_data_frame=data.frame(old_dataframe,feature_column_to_add)

dim(college_data)                                              # 777 x 19 ; Notice that new feature column is added

fix(college_data)

summary(Elite)                                                 # 77- Elite colleges, 699- Non Elite colleges

plot(Elite,Outstate,col="red",varwidth=T,
     xlab="Elite College",ylab="Out of state tution")

summary(college_data)

# lets play with histograms

?hist
hist(Apps,breaks = 10,col="red")

?par
par(mfrow=c(2,2))                                              # par(mfrow=c(x,y)) will divide the plot region into xrows and y column grid
                                                               # So what ever the plot that are given to command ,they will be started filling rows and then nextrow and so on
hist(Apps,breaks=10,col="yellow",                             
     xlab="Applications received",                             # This plot will be in 1x1 grid(1st row and 1st column)
     main = "Applications")

hist(Accept,breaks = 10,col="red",
     xlab="Accepted people",                                   # This plot will be in 1x2 grid
     main = "Acceptance")

hist(Enroll,breaks = 10,col="green",
     xlab="Enrolled people",                                   # This plot will be in 2x1 grid
     main = "Enrollment")

hist(Outstate,breaks =10,col="blue",
     xlab="Out of state tution charges",                       # This plot will be in 2x2 grid
     main = "Tution charge")

pairs(~Apps+Enroll+Accept)                                     # To plot scatter plots in pair,but only for few selected features

pairs(college_data[,c("Enroll","Apps","Accept","Outstate","Private")])             # Works same as above,different notation












