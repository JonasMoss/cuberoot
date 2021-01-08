# The objective function in two dimensions.



# An attempt at semi-bruteforcing the solution to a three-dimensional problem.

nobs  = 25
xx1   = rnorm(nobs)
xx2   = rnorm(nobs)
beta0 = 1
beta1 = 2
beta2 = 3
yy    = (xx1*beta1 + xx2*beta2 + rnorm(nobs)) >= 0 


values  = c(3,2,9,0,1,5,4,8,7,6)
weights = c(1,1,1,1,1,1,1,1,1,1)
isReds  = c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE,TRUE)