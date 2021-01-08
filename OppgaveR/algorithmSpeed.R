library("microbenchmark")
library("locfit")
source("histogram_base.R")

Pnm = function(obj){
  q     = obj$splits
  w     = obj$weights
  k     = obj$k
  n     = length(data)
  Pn    = c(0,sapply(q,function(j) sum(data<=j)/n),1)
  qAug  = c(0,q,1)
  dis   = sapply(1:k,function(i) log(qAug[i+1]-qAug[i]) - log(w[i]))
  probs = sapply(1:k,function(i) Pn[i+1]-Pn[i])
  -sum(probs*dis)
}

nobs = 200
ks = 10:100
greeds200 = sapply(ks,function(x) mean(microbenchmark(histogram(rbeta(nobs,2,7),k=x,method = "greedy"))$time))
exacts200 = sapply(ks,function(x) mean(microbenchmark(histogram(rbeta(nobs,2,7),k=x,method = "exact"))$time))
plot(locfit(I(log(exacts200))~ks),ylim=c(10,18),col="red",bty="l",ylab="log mean time",xlab="k",type="l")
grid()
lines(locfit(I(log(greeds200))~ks),col="blue")

k = 5
ns = (1:20)*25
greeds5 = sapply(ns,function(x) mean(microbenchmark(histogram(rbeta(x,2,7),k=k,method = "greedy"))$time))
exacts5 = sapply(ns,function(x) mean(microbenchmark(histogram(rbeta(x,2,7),k=k,method = "exact"))$time))
plot(locfit(I(log(exacts5))~ns),ylim=c(10,18),col="red",bty="l",ylab="log mean time",xlab="n",type="l")
grid()
lines(locfit(I(log(greeds5))~ns),col="blue")


nobs = 300
data = rbeta(nobs,1,9)
tdat = rbeta(600000,1,9)
ks = 10:100
greeds300 = sapply(ks,function(x) Pnm(histogram(data,k=x,method = "greedy")))
exacts300 = sapply(ks,function(x) Pnm(histogram(data,k=x,method = "exact")))
trues     = sapply(ks,function(x) Pnm(histogram(tdat,k=x,method = "greedy")))
plot(ks,exacts300,col="red",bty="l",ylab="Objective",xlab="k",
     ylim=c(min(greeds300,exacts300),max(greeds300,exacts300)))
points(ks,greeds300,col="blue")
points(ks,trues,col="green")
grid()


true     = histogram(rbeta(1000000,3,1),k=k,method = "greedy")$splits[j]

library("RColorBrewer")
cols = brewer.pal(4,"Set2")
ns = (1:20)*50
k = 11
j = 5

greeds  = rep(NA,length(ns))
exacts  = rep(NA,length(ns))
smooths = rep(NA,length(ns))
for (i in 1:length(ns)){
  greeds[i]  = mean((replicate(100,histogram(rbeta(ns[i],3,1),k=k,method = "greedy")$splits[j])-true)^2)
  exacts[i]  = mean((replicate(100,histogram(rbeta(ns[i],3,1),k=k,method = "exact")$splits[j])-true)^2)
  smooths[i] = mean((replicate(100,histogram(rbeta(ns[i],3,1),k=k,method = "smoothed")$splits[j])-true)^2)
  print(i)
}

plot(ns,ns^(2/3)*exacts,bty="l",ylab="Objective",xlab="n",
     ylim=c(min(ns^(2/3)*greeds,ns^(2/3)*exacts)-0.01,max(ns^(2/3)*greeds,ns^(2/3)*exacts)),
     main="Mean squared error",col=cols[1],pch=15)
points(ns,ns^(2/3)*greeds,col=cols[2],pch=16)
points(ns,ns^(2/3)*smooths,col=cols[3],pch=17)
grid()
legend("topright",c("Exact","Coordinate search","Smoothed"),
       col=cols[1:3],pch=c(15,16,17),bty="n")



data = rbeta(100,2,7)
k = 6
plot(histogram(data,method="exact",weights="KL",k=k))
rug(data)




