# In this lab we will reanalyze the Wage data which is thoroughly discussed in this chapter
# ISLR library which contains our wage dataset
library(ISLR)
wagedata = Wage
attach(wagedata)

##########################
# Polynomial Regression and Step Funtions
##########################
# fitting the polynomial model using the poly function
fit = lm(wage~poly(x = age,degree = 4),data = wagedata)
fit
summary(fit)
coef(summary(fit))
?poly

# The poly function will let us avoid wriing the fourth degree polynomial with that long formula
#   This funciton matinly returns a matrix whose columns are basisi of ORTHOGONAL POLYNOMIALS
# Orthogonal polynomials means that the covariance of two columns of x and x^2 , will be made
#   zero automatically and the poly funcction will scale them in such a way.
# Orthogonality is set to be on by default and we can turn it off by specifying the argument
#   "raw = True", this means that x, x^2... x^n will be exactly kept there
# In orthogonality for each column it will create a linear combination of given x to x^n
# Then it will create n such polynomials such that the covariance between any two columns is zero.
# This will give us uncorrelated features but are still n degree polynomials

# Lets fit another model where raw=True is kept in the poly function and see the difference in
#   coefficients estimate
fit2 = lm(wage~poly(x = age,degree = 4,raw = T),data = wagedata)
coef(summary(fit2))

# This doesn't affect the fitted values for the prediction but actual coefficient values are
#   differing in both the cases. It also changes the pvalue of each coefficients.REMEMEBER that
#   the fitted values will not change but the actual coefficient values change.

# There are also other equivalejnt ways of fitting this model. For example using the I() function
#   It is a wrapper function and the symbol ^ has different meaning when used inside the formula
fit2a = lm(wage~age+I(age^2)+I(age^3)+I(age^4),data = wagedata)
coef(summary(fit2a))

# We observe that we got the same coefficients and its standard errors are also matching
# So indirectly using the argument "raw = T" means using the I() function for each of the degree
#   until the given number of degrees are specified.























































