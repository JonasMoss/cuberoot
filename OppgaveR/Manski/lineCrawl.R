# Linecrawl crawls along lines in order make estimates better when the starting points are
# reasonably good estimates. This could possibly be used in resampling.

source("manskiAlgorithm(1d).R")
source("manskiAlgorithm(2d).R")

lineCrawl = function(yy,XX,startEstimate,lim=500){
  beta1p = startEstimate[1]
  beta2p = startEstimate[2]
  iter = 0
  newEstimate = c(-Inf,-Inf)
  
  while (any(startEstimate != newEstimate) & iter < lim){
    iter             = iter + 1
    newEstimate      = startEstimate
    
    beta2p           = startEstimate[2] 
    xxConst          = 1 + beta2p*XX[2,]
    object           = manski1d(yy,XX[1,],xxConst = xxConst,type="pos")
    startEstimate[1] = coef(object)

    beta1p           = startEstimate[1] 
    xxConst          = 1 + beta1p*XX[1,]
    object           = manski1d(yy,XX[2,],xxConst = xxConst,type="pos")
    startEstimate[2] = coef(object)
  }
  c(newEstimate,iter)
}

lcstrap = function(yy,xx1,xx2,Nreps=1000,m=30,replace=FALSE){
  obj = manski2d(yy,xx1,xx2)
  points = obj$points
  index = which.min(apply(points,1,function(z) (z[1]^2+z[2]^2)))
  origin = c(obj$points[index,1], obj$points[index,2])
  XX = rbind(xx1,xx2)
  nobs = length(yy)
  
  origin = coef(obj)
  est = origin
  dim(est) = c(1,2)
  for (i in 1:Nreps) {
    sampy = sample(nobs,m)
    #sv = colMeans(points[sample(1:(length(points)/2),sample(1:floor(nobs^(1/2)),1),replace=TRUE),])
    error = 10*runif(2,-1/m^(1/3),1/m^(1/3))
    sv = origin + error
    est = rbind(est,lineCrawl(yy[sampy],rbind(xx1,xx2)[,sampy],sv,lim=500)[c(1,2)])
  }
  
  m^(1/3)*(est - origin)
}

m = 10
res = lcstrap(yy,xx1,xx2,Nreps=5000,m=m)
boots = replicate(5000,bootstrapper(sample(1:nobs,m)))
boots = m^(1/3)*(boots - coef(obj))

plot(res[,2],ylim=c(-10,10))
points(boots[2,],col="red")

plot(res[,1],ylim=c(-10,10))
points(boots[1,],col="red")

plot(boots[1,],boots[2,],ylim=c(-10,10),xlim=c(-10,10),col="red")
points(res[,1],res[,2],col="black")

quantile(boots[1,],c(alpha/2,1-alpha/2))
quantile(res[,1],c(alpha/2,1-alpha/2))
quantile(boots[2,],c(alpha/2,1-alpha/2))
quantile(res[,2],c(alpha/2,1-alpha/2))


`%between%`<- function(x, vec) x>=head(vec,1) & x <= tail(vec,1)

testUgly = function(alpha=0.1,m=10,Nreps=1000) {
  nobs  = 300
  beta1 = 4
  beta2 = -1/2
  xx1   = rnorm(nobs)
  xx2   = rnorm(nobs)
  yy    = (1+beta1*xx1+beta2*xx2 + rnorm(nobs))>=0
  ests  = lcstrap(yy,xx1,xx2,Nreps=Nreps,m=m)
  beta1true = (beta1 %between% quantile(ests[,1],c(alpha/2,1-alpha/2)))
  beta2true = (beta2 %between% quantile(ests[,2],c(alpha/2,1-alpha/2)))
  c(beta1true,beta2true)
}

ress = replicate(100,testUgly(alpha=0.3,m = 15))
rowMeans(ress)