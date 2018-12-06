## a)
# to write a function power that prints out results of raising 2 power whatever the number we write.

Powerfunction = function(){
  print(2^3);
}

## b)
# write power2() function that lets us pass any two numbers and then return x^a

Power2 = function(x,a){
  print(x^a)
}


## c)
Power2(10,3)
Power2(8,17)
Power2(131,3)


## d)
Power3 = function(x,a){
  return(x^a)
}

## e)

Power3(2,3)
Xelements = 1:10
Xelements
Yelements = Power3(Xelements,2)
Yelements
# log is another argument available in this plot function
# Whatever we keep for log = "" inside that quote string is converted to log scale
# For example we want to plot x elemetns in log scale just keep log = "x"
# If we want both x axis and y axis in log scale keep log = "xy"
plot(x = Xelements,y = Yelements,xlab = "x",ylab = "y",log = "xy")


## f)

PlotPower = function(x,a){
  Yelem = Power3(x,a)
  plot(x = x,y = Yelem,xlab = "X axis",ylab = "Y axis")
}

PlotPower(1:20,3)