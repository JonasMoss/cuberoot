source("histogram(k=2)Simulations.R")

kolmogorov = function(dist1,dist2){
  tt = seq(-20,20,by=0.01)
  max(abs(dist1(tt)-dist2(tt)))
}

kolDistance = function(nobs, shape1, shape2, Nreps = 1000000,values=NULL) {
  if (is.null(values)) values = getValues(shape1,shape2)
  scaling = values$scaling
  a = values$a
  xs = replicate(Nreps,histogram(rbeta(nobs,shape1,shape2),k=2,method="greedy")$splits)
  ys = nobs^(1/3)*(xs - a)
  dist1 = ecdf(ys)
  dist2 = function(x) pchernoff(scaling*x)
  kolmogorov(dist1,dist2)  
}



lnormMeans = function(nobs,N=100000) sqrt(nobs)*replicate(N,mean(rlnorm(nobs))-exp(1/2))
expMeans   = function(nobs,N=100000) sqrt(nobs)*replicate(N,mean(rexp(nobs))-1)

sapply(ns,function(nobs) kolmogorov(ecdf(lnormMeans(nobs)),function(x) pnorm(x,0,sqrt((exp(1)-1)*exp(1)))))

lnormDist  = kolmogorov(ecdf(lnormMeans(nobs)),function(x) pnorm(x,0,sqrt((exp(1)-1)*exp(1))))
expDist    = kolmogorov(ecdf(expMeans(nobs)),function(x) pnorm(x,0,1))
lnormDist
expDist



ns = c(50, 100, 500, 1000, 10000, 100000)
shapes = matrix(c(1,1/2,2,7,1,3,8/10,9/10),4,2,byrow=TRUE)
kdists = matrix(NA,ncol=length(ns),nrow=length(shapes)/2)
colnames(kdists) = ns

for (i in 1:(length(shapes)/2)) {
  value = getValues(shapes[i,1],shapes[i,2])
  kdists[i,] = sapply(ns,function(x) kolDistance(x,shapes[i,1],shapes[i,2],values=value))
}
