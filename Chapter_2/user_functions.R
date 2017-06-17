feature_type_function=function(data_frame)
{
  for (i in names(data_frame))      
  {
    feature_name=data_frame[[i]]                                        # class(feature_name) gives type of data,that column has
    class_type = class(feature_name)
    print(paste("Feature type for ",i,"is ",class_type))                # paste function concentates everything inside to single vector of characters(otherwise print will only print first one)
  }
} 



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



pair_plotter_function = function(dataframe,subframe){
  count=0
  colcount=ncol(dataframe)
  roundcolcount=round(colcount/3,0)
  par(mfrow=c(roundcolcount,3),mar=c(1,1,1,1))                            # par() splits the plot window to given grid size,we have to add mar=c(1,1,1,1) to set the margins low,other wise we get error
  for (i in names(dataframe))                                             # par(mfrow=c(x,y)) creates x rows and y columns each part for a seperate plot
  {
    color_inplot = colors(distinct = T)[count+20]
    plot(subframe,dataframe[[i]],col=color_inplot,
         xlab = deparse(substitute(subframe)),                            # substitue(feature_name),gives only feature_name as output;if we just give feature name,we will get array as output but we dont want that                                          
         ylab = i)                                                        # deparse() function will convert whatever present inside to strings
    count=count+1 
  }
}


adds_min_max=function(dataframe)
  {
    minrow=rep(0,ncol(dataframe))
    maxrow=rep(0,ncol(dataframe))
    count=1
    for(i in names(dataframe))
    {
      minrow[count] = min(dataframe[[i]])
      maxrow[count] = max(dataframe[[i]])
      count=count+1
    }
    outputframe=rbind(minrow,maxrow)
    colnames(outputframe)=colnames(dataframe)
    return(outputframe)
  }













