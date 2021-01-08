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
  if (obj$specification[1] == "KL") {
    dis   = sapply(1:k,function(i) log(w[i]) - log(qAug[i+1]-qAug[i]))
    probs = sapply(1:k,function(i) Pn[i+1]-Pn[i])
    return(sum(probs*dis))
  } else {
    dis   = sapply(1:k,function(i) 1/(qAug[i+1]-qAug[i]))
    probs = sapply(1:k,function(i) Pn[i+1]-Pn[i])
    return(sum(w*(2*probs-w)*dis))
  }
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


biasSubs <- function(data, b=0.5, k, K=1000,lim,type=type,weights=weights){
  b = ceiling(length(data)*b)
  samples = replicate(K,{
    dataSample = sample(data,b)
    obj = histogram(dataSample,k=k,method="exact",lim=lim,type=type,weights=weights)
    Pnm(obj,dataSample) - Pnm(obj,data)
  })
  mean(samples)
}
 
biasExp = function(ks, Nreps = 1000, nobs, shape1, shape2,b = 0.5, K = 1000,lim,type,weights) {
  replicate(Nreps,{
    data = rbeta(nobs,shape1,shape2)
    sapply(ks,function(x) biasSubs(data,b=0.5,k = x,K=K,lim=lim,type,weights))
  })
}



ks = (1:10)*2
nobs = 100
shape1 = 1
shape2 = 9
trues1.05.100  = sapply(ks, function(x) 
  biasCalc(n = nobs, k = x, shape1 = shape1, shape2 = shape2,Nreps = 100,lim = 0.0001, type="L2", weights ="L2"))
subs1.05.100  = biasExp(ks = ks, Nreps = 50, nobs = nobs, shape1 = shape1, shape2 = shape2, lim = 0.0001, K = 20, type="L2", weights ="L2") 

plot(ks,trues1.05.100,ylim=c(min(trues1.05.100,rowMeans(subs1.05.100)),max(trues1.05.100,rowMeans(subs1.05.100))))
points(ks,rowMeans(subs1.05.100),col="red")
plot(ks,trues1.05.100-rowMeans(subs1.05.100))





truesKLKL= sapply(ks, function(x) 
  biasCalc(n = 100, k = x, shape1 = shape1, shape2 = shape2,Nreps = 100,lim = 0.01, type="KL", weights ="KL"))

truesKLeq  = sapply(ks, function(x) 
  biasCalc(n = 100, k = x, shape1 = shape1, shape2 = shape2,Nreps = 100,lim = 0.01, type="KL", weights ="eq"))


lim = 0.0001
truesL2L2  = sapply(ks, function(x)
  biasCalc(n = 100, k = x, shape1 = shape1, shape2 = shape2,Nreps = 100,lim = lim, type="L2", weights ="L2"))
subsL2L2  = biasExp(ks = ks, Nreps = 50, nobs = nobs, shape1 = shape1, shape2 = shape2, lim = lim, K = 20, type="L2", weights ="L2") 

truesL2eq  = sapply(ks, function(x) 
  biasCalc(n = 100, k = x, shape1 = shape1, shape2 = shape2,Nreps = 100,lim = lim, type="L2", weights ="eq"))
subsL2eq  = biasExp(ks = ks, Nreps = 50, nobs = nobs, shape1 = shape1, shape2 = shape2, lim = lim, K = 20, type="L2", weights ="eq") 

plot(ks,truesL2L2,col="blue",ylim=c(min(truesL2L2,rowMeans(subsL2L2)),max(truesL2L2,rowMeans(subsL2L2))),
     xlab="k",ylab="Bias",bty="l",sub=paste0("lim = ", lim))
grid()
points(ks,truesL2L2,col="blue")
points(ks,truesL2eq,col="green")
points(ks,rowMeans(subsL2L2),col="red")
points(ks,rowMeans(subsL2eq),col="purple")



ks = (1:10)*2
nobs = 1000
lim = 0.0001

trues27  = sapply(ks, function(x) 
  biasCalc(n = nobs, k = x, shape1 = 2, shape2 = 7  ,Nreps = 100,lim = lim, type="L2", weights ="L2"))
trues13  = sapply(ks, function(x) 
  biasCalc(n = nobs, k = x, shape1 = 1, shape2 = 3  ,Nreps = 100,lim = lim, type="L2", weights ="L2"))
trues910  = sapply(ks, function(x) 
  biasCalc(n = nobs, k = x, shape1 = 9, shape2 = 10 ,Nreps = 100,lim = lim, type="L2", weights ="L2"))
trues105  = sapply(ks, function(x) 
  biasCalc(n = nobs, k = x, shape1 = 1, shape2 = 0.5,Nreps = 100,lim = lim, type="L2", weights ="L2"))

plot(ks,trues27,col=adjustcolor(cols[1],alpha.f=0.6),xlab="k",ylab="Bias",bty="l",
     sub=paste0("lim = ", lim, "; n = ", nobs),
     ylim = c(min(trues27,trues13,trues910,trues105),max(trues27,trues13,trues910,trues105)),pch=19,
     main =  expression(paste("Simulated biases for ",L[2],"-histograms.")))
grid()
points(ks,trues27 ,col=adjustcolor(cols[1],alpha.f=0.6),pch=19)
points(ks,trues13 ,col=adjustcolor(cols[2],alpha.f=0.6),pch=19)
points(ks,trues910,col=adjustcolor(cols[3],alpha.f=0.6),pch=19)
points(ks,trues105,col=adjustcolor(cols[4],alpha.f=0.6),pch=19)

legend("topleft",legend=c(" 2 7"," 1 3"," 9 10","10 5"),col = cols,bty="n",pch=c(19,19,19,19))


ns = (1:10)*100
lim = 0.0001
mlues = sapply(ns, function(x) 
  biasCalc(n = x, k = 7, shape1 = 2, shape2 = 7  ,Nreps = 200,lim = lim, type="KL", weights ="eq"))
plot(ns,ns^(2/3)*mlues)

library("RColorBrewer")
cols = brewer.pal(4,"Set1")

ns = (1:120)*60
plot(ns,ns^(2/3)*klues,xlab="n",ylab="Rescaled bias",bty="l",col=cols[1])
grid()
points(ns,ns^(2/3)*klues,xlab="n",ylab="Rescaled bias",bty="l",col=cols[1])
plot(ns,ns^(2/3)*slues,xlab="n",ylab="Rescaled bias",bty="l",col=cols[2])
grid()
points(ns,ns^(2/3)*slues,xlab="n",ylab="Rescaled bias",bty="l",col=cols[2])
lm()

plot(ks,truesKLKL)
points(ks,truesKLeq,col="red")
plot(ks,truesL2L2,col="blue")
points(ks,truesL2eq,col="green")

signif(rowMeans(subs1.05.100),4)
signif(trues1.05.100,4)



lim = 0.0001
ks = (1:20)*2

nobs = 100
trues100  = sapply(ks, function(x)
  biasCalc(n = nobs, k = x, shape1 = shape1, shape2 = shape2,Nreps = 100,lim = lim, type="L2", weights ="L2"))
subs100  = biasExp(ks = ks, Nreps = 50, nobs = nobs, shape1 = shape1, shape2 = shape2, lim = lim, K = 60, type="L2", weights ="L2") 

nobs = 500
trues500  = sapply(ks, function(x)
  biasCalc(n = nobs, k = x, shape1 = shape1, shape2 = shape2,Nreps = 400,lim = lim, type="L2", weights ="L2"))
subs500  = biasExp(ks = ks, Nreps = 50, nobs = nobs, shape1 = shape1, shape2 = shape2, lim = lim, K = 60, type="L2", weights ="L2") 

nobs = 1000
trues1000  = sapply(ks, function(x)
  biasCalc(n = nobs, k = x, shape1 = shape1, shape2 = shape2,Nreps = 400,lim = lim, type="L2", weights ="L2"))
subs1000  = biasExp(ks = ks, Nreps = 50, nobs = nobs, shape1 = shape1, shape2 = shape2, lim = lim, K = 60, type="L2", weights ="L2") 

library("RColorBrewer")
cols = brewer.pal(6,"Set2")

plot(ks,100^(2/3)*trues100,col=cols[1],bty="l",xlab="k",ylab="Bias",sub="n = 100")
points(ks,100^(2/3)*rowMeans(subs100),col=cols[2])
abline(v=ceiling(2*(100)^(1/3)),lty=2,col="gray")

plot(ks,500^(2/3)*trues500,col=cols[1],bty="l",xlab="k",ylab="Bias",sub="n = 500")
points(ks,500^(2/3)*rowMeans(subs500),col=cols[2])
abline(v=ceiling(2*(500)^(1/3)),lty=2,col="gray")

plot(ks,1000^(2/3)*trues1000,col=cols[1],bty="l",xlab="k",ylab="Bias",sub="n = 1000")
points(ks,1000^(2/3)*rowMeans(subs1000),col=cols[2])
abline(v=ceiling(2*(1000)^(1/3)),lty=2,col="gray")

lim = 0.01
nobs = 100
ks = (2:floor(sqrt(nobs)))
trues100s  = sapply(ks, function(x)
  biasCalc(n = nobs, k = x, shape1 = shape1, shape2 = shape2,Nreps = 400,lim = lim, type="L2", weights ="L2"))
subs100s  = biasExp(ks = ks, Nreps = 50, nobs = nobs, shape1 = shape1, shape2 = shape2, lim = lim, K = 100, type="L2", weights ="L2") 
plot(ks,trues100s)
points(ks,rowMeans(subs100s),col="red")


shape1 = 1
shape2 = 3/2
lim = 0.0001
ks = (1:20)*2

nobs = 100
l12trues100  = sapply(ks, function(x)
  biasCalc(n = nobs, k = x, shape1 = shape1, shape2 = shape2,Nreps = 100,lim = lim, type="L2", weights ="L2"))
l12subs100  = biasExp(ks = ks, Nreps = 50, nobs = nobs, shape1 = shape1, shape2 = shape2, lim = lim, K = 60, type="L2", weights ="L2") 

nobs = 500
l12trues500  = sapply(ks, function(x)
  biasCalc(n = nobs, k = x, shape1 = shape1, shape2 = shape2,Nreps = 100,lim = lim, type="L2", weights ="L2"))
l12subs500  = biasExp(ks = ks, Nreps = 50, nobs = nobs, shape1 = shape1, shape2 = shape2, lim = lim, K = 60, type="L2", weights ="L2") 

nobs = 1000
l12trues1000  = sapply(ks, function(x)
  biasCalc(n = nobs, k = x, shape1 = shape1, shape2 = shape2,Nreps = 100,lim = lim, type="L2", weights ="L2"))
l12subs1000  = biasExp(ks = ks, Nreps = 50, nobs = nobs, shape1 = shape1, shape2 = shape2, lim = lim, K = 60, type="L2", weights ="L2") 


plot(ks,100^(2/3)*l12trues100,col=cols[3],bty="l",xlab="k",ylab="Bias",sub="n = 100")
points(ks,100^(2/3)*rowMeans(l12subs100),col=cols[4])
abline(v=ceiling(2*(100)^(1/3)),lty=2,col="gray")

plot(ks,500^(2/3)*l12trues500,col=cols[3],bty="l",xlab="k",ylab="Bias",sub="n = 500")
points(ks,500^(2/3)*rowMeans(l12subs500),col=cols[4])
abline(v=ceiling(2*(500)^(1/3)),lty=2,col="gray")

plot(ks,1000^(2/3)*l12trues1000,col=cols[3],bty="l",xlab="k",ylab="Bias",sub="n = 1000")
points(ks,1000^(2/3)*rowMeans(l12subs1000),col=cols[4])
abline(v=ceiling(2*(1000)^(1/3)),lty=2,col="gray")

nobs = 200
ks = seq(3,ceiling(2*nobs^(1/3)))
trues  = sapply(ks, function(x)
  biasCalc(n = nobs, k = x, shape1 = 2, shape2 = 7,Nreps = 1000,lim = lim, type="KL", weights ="KL"))
