########## Problem 9 ############
# This problem involves selecting Auto data

Auto_data=read.table("data/Auto.data",header = T,na.strings = "?")      # Adding header,saying first row is column names,
                                                                        # also na.strings let us know that missing values are dealt with and substituted with ?

# checking for overview of predictors

str(Auto_data)                                                          # str() function to get overview of all features,says which datatype each column is                                          

names(Auto_data)                                                        # names() function gives all the feature names in a dataframe

summary(Auto_data)                                                      # summary() gives mean,median,1st quartile,3rd quartile,Min,Max for quantitative variables

dim(Auto_data)                                                          # number of rows x number of columns

fix(Auto_data)                                                          # fix() opens data editor

Auto_data=na.omit(Auto_data)                                            # Reassigning dataframe after omitting missing values ; na.omit(dataframe) returns another one,which has to be assigned

nrow(Auto_data)                                                         # nrow() gives number of observations,notice that 5 observations have been deleted

attach(Auto_data)                                                       # attach(dataframe) will let us use feature names directly instead of reffering them as dataframe$feature_name

# Custom function that prints feature type for each feature
feature_type_function=function(data_frame)
  {
    for (i in names(data_frame))      
      {
        feature_name=data_frame[[i]]                                        # class(feature_name) gives type of data,that column has
        class_type = class(feature_name)
        print(paste("Feature type for ",i,"is ",class_type))                # paste function concentates everything inside to single vector of characters(otherwise print will only print first one)
    }
  }  

feature_type_function(Auto_data)

# Custom function to print ranges for each column
Range_function = function(data_frame)
  {
    for (i in names(data_frame))
      {
      data_type=class(data_frame[[i]])
      if(data_type != "factor")                                           # Range will not work on factors ,so excluding that by if condition
        {                                                                 # Also range returns a vector with two elements,min and max
         featurerange=range(data_frame[,i])
         print(paste("Range for ",i,"is ","from",featurerange[1],
                     "to",featurerange[2]))
        }
      }
  }
  
Range_function(Auto_data)

# Custom func for printing mean and standard deviation for each column
Mean_sd_function = function(data_frame)
  {
    for (i in names(data_frame))
    {
      feature_name=data_frame[[i]]
      data_type = class(data_frame[[i]])
      if(data_type != "factor")
        {
          mean_feature=mean(feature_name)                                 # paste() function will concentate the strings to return a single string to be printed by print function    
          sd_feature=sd(feature_name)
          print(paste("Mean for",i," is ",                                                                              
                      round(mean_feature,3)))
          print(paste("Standard deviation for",i," is ",
                      round(sd_feature,3)))
        }
    }
  }

Mean_sd_function(Auto_data)

# removing 10th to 85th rows and assigning dataframe to new one
New_Auto_data = Auto_data[-c(10:85),]

Mean_sd_function(New_Auto_data)

# Scatter plots of pairs of all possible features
pairs(Auto_data)

# Custom function to plot scatter plots only for given feature wrto all other feature
pair_plotter_function = function(dataframe,subframe){
  count=0
  par(mfrow=c(3,3))
  for (i in names(dataframe))
  {
    color_inplot = colors(distinct = T)[count+20]
    plot(subframe,dataframe[[i]],col=color_inplot,
         xlab = deparse(substitute(subframe)),                            # substitue(feature_name),gives only feature_name as output;if we just give feature name,we will get array as output but we dont want that                                          
         ylab = i)                                                        # deparse() function will convert whatever present inside to strings
    count=count+1 
  }
}

pair_plotter_function(Auto_data,displacement)

pair_plotter_function(Auto_data,mpg)

# we can say that horsepower and weight are promising to predict mpg 
# they shows are general downward trend as mpg increases;









