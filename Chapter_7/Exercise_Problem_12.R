# Doing the same thing that we have done in problem 11 but now we are going to do that for 100
#   predictors instead of just two that we have earlier. So we have to create a matrix and then
#   use matrix multiplication to do the operations


# Defining the predictors number as just p
p=100

# Number of examples are 1000 here
n=1000

# Creating a X matrix which has p columns and n number of rows
#   We have added another additional column for the beta0 , and here the entire column is assigned
#   value 1, because it will be multiplied by the intercept
X = matrix(rnorm(n*(p+1)),nrow = n,ncol = p+1)
X[,1] = rep(1,n)

# Lets run the backfitting appraoch for 100 iterations and at each iteration end we are going to
#   save the mean squared error for that particular iteraton
# Creating the same number of betas ,but with random numbers which are distributed, These are the real
#   betas that we want to predict using the backfitting approach
beta_init = rnorm(p+1)*10

Y = beta_init[1]+ X%*%beta_init[-1] + rnorm(n,sd = 1)


# Similarily we are creating 100 mse errors array
mse_arr = rep(0,100)


# Now looping through the iterations and for each iteration we will use back fitting on that predictor
#   column by leaving it and using all other predictors

# Deciding the number of iterations
iter = 100


# Creating a beta array to store betas at each iteration so that we can plot all of them after
beta_arr = matrix(0, nrow = iter, ncol = p)

dim(X)
dim(beta_arr)
dim(Y)


for (i in 1:iter) {
  for (j in 1:100) {
    # Here we are looping with predictors, we have 1 to 100 predictors and for each one we are
    #   looping and creating a linear regression except for that particular jth predictor
    # Also we are multiplying the betas of the ith iteration which is represented by rows and
    #   also we are leaving out the one beta value of kth predictor ,since we are leaving out this
    #   predictor and doing regression on all other
    if(i==1){
      a = Y - X[,-j]%*%beta_arr[i,-j]
      beta_arr[i,j] = lm(a~X[,j])$coef[2]
    } else{
      a = Y - X[,-j]%*%beta_arr[i-1,-j]
      beta_arr[i,j] = lm(a~X[,j])$coef[2]
    }


    # Now we have to ssave the new betas that we can get from regression with "a" as output as we
    #   have done in earlier problem. So here response is a and predictor is X[,-k] without k column
  }
  mse_arr[i] = mean((Y - X%*%beta_arr[i,])^2)
}

# Plotting all the mean squared errors
plot(1:iter, mse_arr)
