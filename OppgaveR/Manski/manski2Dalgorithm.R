xs     = seq(-15,15,by=0.01)
nobs   = 200
xx1    = rnorm(nobs)
xx2    = rnorm(nobs)
beta0  = 1
beta1  = 6
beta2  = 7
yy     = (beta0 + xx1*beta1 + xx2*beta2 + rnorm(nobs)) >= 0 
slope  = -xx2/xx1

# The "b" is xx2. A line is red if y = 1 and b > 0 or if 
isReds = (xx1 >= 0 & yy) | ( xx1 < 0 & !yy)


res = redBlue2(xx1,xx2,isReds,rep(1,nobs))
res