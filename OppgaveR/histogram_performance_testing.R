source("histogram_base.R")
library(microbenchmark)

# Smoothed test ---------------------------------------------------------------

ts = seq(0,1,by=0.001)
data = rbeta(100,2,2)

plot(histogram(data,10,method="smoothed",type="KL",weights="KL"),ylim=c(0,3.5))
lines(ts,dbeta(ts,2,2))
rug(data)

plot(histogram(data,10,method="subbag",bag_factor=0.25,type="KL",weights="KL"),ylim=c(0,3.5))
lines(ts,dbeta(ts,2,2))
rug(data)

plot(histogram(data,10,method="greedy",bag_factor=0.25,type="KL",weights="KL"),ylim=c(0,3.5))
lines(ts,dbeta(ts,2,2))
rug(data)


# Bold attempt ----------------------------------------------------------------

histy = function(data,k){
  n = length(data)
  results = sapply(1:1000,
            function(x) greedy_histogram(sort(sample(data,2*k)),k,
                                         type="KL",weights="KL"))
  weights = rowMeans(sapply(1:1000,function(i) results[ ,i]$weights))
  splits = rowMeans(sapply(1:1000,function(i) results[ ,i]$splits))
  results = results[,1]
  results$method = "subbag"
  results$weights = weights
  results$splits = splits
  class(results) = c("hist")
  return(results)
}

plot(histy(data,k))
lines(ts,dbeta(ts,2,2))
rug(data)

# Histograms ------------------------------------------------------------------

data = rbeta(1000,5,7)
k = 10
par(mfrow=c(2,2))
plot(histogram(data,k,method = "exact",type = "KL",weights = "equal"))
plot(histogram(data,k,method = "exact",type = "L2",weights = "equal"))
plot(histogram(data,k,method = "exact",type = "KL",weights = "KL"))
plot(histogram(data,k,method = "exact",type = "L2",weights = "L2"))

plot()

# The basic function ----------------------------------------------------------
library("microbenchmark")

histogram_bm = function(n,a,b,k=10){
    exact=median(microbenchmark(histogram(rbeta(n,a,b),k=k))$time)
    greedy=median(microbenchmark(histogram(rbeta(n,a,b),k=k,method="greedy"))$time)
    c("exact"=median(exact),"greedy"=median(greedy),"rel"=exact/greedy)
}

greedy_bm = function(n,a,b,k=10,times=100){
  greedy=median(microbenchmark(histogram(rbeta(n,a,b),k=k,method="greedy"),times=times)$time)
  c("greedy"=median(greedy))
}

# The basic set ups -----------------------------------------------------------

as = c(2,4,3)
bs = c(7,5,3)
ks = c(rep(3,times=3),rep(10,times=3),rep(30,times=3))

bm_result_100 = mapply(histogram_bm,n=100,a=as,b=bs,k=ks)
bm_result_1000 = mapply(histogram_bm,n=1000,a=as,b=bs,k=ks)

# A single distribution -------------------------------------------------------

a = 2
b = 7
ks = c(3,4,5,7,1:6*10)

bm_single_result = sapply(ks,function(x) histogram_bm(n=1000,a=a,b=b,x))

# Only greedy -----------------------------------------------------------------
ks = 3:60
bm_greedy_result = sapply(ks,function(x) greedy_bm(n=1000,a=a,b=b,x,times=1000))