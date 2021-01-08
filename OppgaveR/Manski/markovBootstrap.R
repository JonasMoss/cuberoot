source("manskiAlgorithm(1d).R")
source("manskiAlgorithm(2d).R")

# Data generation
seed  = 990
set.seed(seed)
nobs  = 300
beta1 = 4
beta2 = 3
xx1   = rnorm(nobs)
xx2   = rnorm(nobs)
yy    = (1+beta1*xx1+beta2*xx2 + rnorm(nobs))>=0

# Attempt to make algorithm

mboot = function(yy,xx1,xx2,Nreps=1000,m=30,replace=FALSE){
  obj = manski2d(yy,xx1,xx2)
  points = obj$points
  index = which.min(apply(points,1,function(z) (z[1]^2+z[2]^2)))
  origin = c(obj$points[index,1], obj$points[index,2])
  XX = rbind(xx1,xx2)
  nobs = length(yy)
  
  # The index variable keeps track of which parameter we subsample on.
  estimates = matrix(NA,Nreps+1,2)
  estimates[1,] = origin
  for (k in 2:(2*Nreps+1)) {
      samples = sample(nobs,M)
      index = k %% 2
      row   = (k %/% 2)+1
      betaHat = estimates[1,-index+2] + runif(1,-1/nobs^(1/3),1/nobs^(1/3))
      xxConst  = 1 + betaHat*XX[-index+2,samples]
      object = manski1d(yy[samples],XX[index+1,samples],xxConst = xxConst,type="pos")
      est = coef(object)
      #print(manski1d(yy[samples],XX[index+1,samples],xxConst = xxConst,type="pos"))
      estimates[row,index+1] = est
  }
  list(betas = m^(1/3)*(estimates - colMeans(estimates)), means = colMeans(estimates),
       medians = apply(estimates,2,median))
}
