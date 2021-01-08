library("ks")
library("locfit")

bootmean = function(data,n,N){
  boots = replicate(N,mean(sample(data[1:n],n,replace=TRUE)))
  quantile(boots,0.90)
}

bootmax = function(data,maxi,n,N){
  boots = replicate(N,n*(maxi-max(sample(data[1:n],n,replace=TRUE)))/maxi)
  ecd = ecdf(boots)
  ecd(2)
}


data = runif(30000)
maxi=max(data)
results = sapply((1:30)*1000,function(i) bootmax(data,maxi,i,60000))
plot(results,col="red",type="l",ylim=c(-0.1,1.1))

data = runif(30000)
maxi=max(data)
results = sapply((1:30)*1000,function(i) bootmax(data,maxi,i,60000))
lines(results,col="blue")

data = runif(30000)
maxi=max(data)
results = sapply((1:20)*1000,function(i) bootmax(data,maxi,i,60000))
lines(results,col="green")


data = runif(5000)
maxi=max(data)
results = sapply((1:50)*100,function(i) bootmax(data,maxi,i,1000))
lines(results,col="purple")

n = 100

testy = function(N,n) {
  replicate(N,{data = runif(n)
               bootmax(data,max(data),n,1000)})
}

res = testy(10000,1000)
kern = kde(res,positive=TRUE)
hist(res)
plot(kern)

N = 
  
  rcool = function(n,m) {
    rmultinom(n, m, exp(-(1:m)+1)-exp(-(1:m)))
    
  }

spacings = function(n,i) {
  data = sort(runif(n))
  n*(data[n]-data[n-i])/data[n]
}

ss = seq(0,5,by=0.01)
kern = kde(replicate(1000,spacings(50000,1)),positive=TRUE)
plot(kern)
lines(ss,dexp(ss))

# Piene -----------------------------------------------------------------------

conv = function(n){
  is   = 0:n
  Pis  = function(i) exp(-i)-exp(-i-1)
  use  = rmultinom(1,1,Pis(is))
  exps = c(0,rexp(n))
  us   = cumsum(exps)
  sum(use*us)
}

# Unconditional convergence ---------------------------------------------------

bootmax = function(data,maxi,n,N){
  boots = replicate(N,n*(maxi-max(sample(data[1:n],n,replace=TRUE)))/maxi)
  
}

checkconv = function(n,N,boots=10*n) {
  replicate(N,{data = runif(n)
               maxi = max(data)
               n*(maxi-max(sample(data,n,replace=TRUE)))/maxi})
}

hist(checkconv(100,1000),breaks=100)

# Smoothed bootstrap ----------------------------------------------------------


