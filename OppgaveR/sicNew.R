data = rbeta(100,200,200)
qhat = histogram(data, k = 5, method="greedy")
plot(qhat)
lines(kk,dbeta(kk,10,10),type="l")
details(qhat$splits,data)
Pnm(qhat$splits,data)

n = 100
ks  =  2:80
shape1 = 9
shape2 = 9
biasesEx = sapply(ks,function(x) biasCalc(n,x,shape1,shape2,Nreps=1000))
biasesGr = sapply(ks,function(x) biasCalc(n,x,shape1,shape2,Nreps=1000,method="greedy"))
plot((biasesEx-biasesGr)[20:80])

data = rbeta(n,shape1,shape2)
estEx = lapply(ks,function(x) histogram(data,k=x,method="exact")$splits)
trueEx = sapply(ks,function(x) Pm(estEx[[x-1]],function(z) pbeta(z,shape1,shape2))-log(x))
valsEx = sapply(ks,function(x) Pnm(estEx[[x-1]],data)-log(x))
#valsGreed = sapply(ks,function(x) Pnm(histogram(data,k=x,method="greedy")$splits,data)-log(x))
#valsRand = sapply(ks,function(x) Pnm(randhist(data,k=x,tries=10)$splits,data)-log(x))

#mod = lm(vals~I(log(ks)))
plot(ks,valsEx-biases,ylim=c(min(valsEx-biasesEx,trueEx),max(valsEx-biases,trueEx)),col="blue",
     xlab="k",ylab="Discrepancy")
points(ks,trueEx,col="red")
points(which.max(trueEx)+1,max(trueEx),pch=19,col="red")
points(which.max(valsEx-biasesEx)+1,max(valsEx-biases),pch=19,col="blue")
#points(ks,valsGreed,col="red")
#plot(ks,valsRand,col="blue")

randhist = function(data,k,tries = 10) {
  samples = replicate(tries,sort(sample(data,k-1)))
  res = which.max(apply(samples,2,function(x) Pnm(x,data)))
  ret_object = list()
  ret_object$specification = c(type="KL",weights="equal")
  ret_object$k = k
  ret_object$splits = samples[,res]
  ret_object$weights = rep(1/k,k)
  ret_object$method = "random"
  class(ret_object) = c("hist")
  ret_object    
}

qhat = histogram(data,k=5,method="greedy")$splits
plot(histogram(data,k=5,method="greedy"))
lines(randhist(data,k=5,tries=10),col="red")
# Serves as the true objective function of the data
Pm = function(obj,P){
  w     = obj$weights
  k     = obj$k
  q     = obj$splits
  Pn    = P(c(0,q,1))
  qAug  = c(0,q,1)
  dis   = sapply(1:k,function(i) log(w[i]) - log(qAug[i+1]-qAug[i]))
  probs = sapply(1:k,function(i) Pn[i+1]-Pn[i])
  sum(probs*dis)
}

biasTrue = function(n,k,shape1,shape2, method="exact",lim=lim, type, weights){
  P     = function(x) pbeta(x, shape1, shape2)
  data  = rbeta(n,shape1,shape2)
  obj   = histogram(data, k=k, method=method,lim=lim, type=type, weights=weights)
  Pnm(obj,data) - Pm(obj,P)
}

biasCalc = function(n, k, shape1, shape2, Nreps = 10000, method="exact",lim=0.01, type, weights){
  biases = replicate(Nreps,biasTrue(n, k, shape1, shape2, method=method,lim=lim, type, weights))
  mean(biases)
}


biasSubs <- function(data, b=0.5, k, K=1000,lim){
  b = ceiling(length(data)*b)
  samples = replicate(K,{
    dataSample = sample(data,b)
    obj = histogram(dataSample,k=k,method="exact",lim=lim)
    Pnm(obj,dataSample) - Pnm(obj,data)
  })
  mean(samples)
}
 
biasExp = function(ks, Nreps = 1000, nobs, shape1, shape2,b = 0.5, K = 1000,lim) {
  replicate(Nreps,{
    data = rbeta(nobs,2,7)
    sapply((1:10)*2,function(x) biasSubs(data,k = x,K=K,lim=lim))
  })
}



ks = (1:20)*2
shape1 = 2
shape2 = 7
trues1.05.100  = sapply(ks, function(x) 
  biasCalc(n = 100, k = x, shape1 = shape1, shape2 = shape2,Nreps = 100,lim = 0.0001, type="KL", weights ="KL"))
#subs1.05.100  = biasExp(ks = ks, Nreps = 50, nobs = 100, shape1 = 1, shape2 = 0.5, lim = 0.0001, K = 20) 

plot(ks,trues1.05.100)
#points(rowMeans(subs1.05.100),col="red")

signif(rowMeans(subs1.05.100),4)
signif(trues1.05.100,4)

trues1.05.500  = sapply((1:10)*2, function(x) biasCalc(500 , x, 1, 0.5))
 subs1.05.500  = biasExp(100, 500, 1, 0.5) 
signif(rowMeans(subs2.7.1000),4)
signif(trues2.7.1000,4)

trues2.7.100  = sapply((1:10)*2, function(x) biasCalc(100 , x, 2, 7))
 subs2.7.100  = biasExp(1000, 100, 2, 7) 

trues2.7.500  = sapply((1:10)*2, function(x) biasCalc(500 , x, 2, 7))
 subs2.7.500  = biasExp(1000, 500, 2, 7) 

trues2.7.1000 = sapply((1:10)*2, function(x) biasCalc(1000, x, 2, 7))
 subs2.7.1000 = biasExp(1000, 1000, 2, 7) 
