####################################################
## In this file we simulate histograms with k = 2 ##
####################################################
library("locfit")
library("microbenchmark")
library("RColorBrewer")
source("histogram_base.R")
source("chernoffsDistribution.R")

getValues = function(shape1,shape2,Nreps = 50000){
  P = function(x) pbeta(x,shape1,shape2)
  a = histogram(rbeta(Nreps,shape1,shape2),k=2,method="greedy")$splits
  Fa = P(a)
  f = dbeta(a,shape1,shape2)
  df = grad(dbeta,a,shape1=shape1,shape2=shape2)
  V = df*(-log(a)+log(1-a))-2*f*(1/(1-a)+1/a)+(1-Fa)/(1-a)^2+Fa/a^2
  mult = sqrt(f)*abs(-log(a)+log(1-a))
  scaling = (abs(2*mult/V))^(-2/3)  
  return(list(scaling = scaling, a = a, mult = mult, V = V))
}

