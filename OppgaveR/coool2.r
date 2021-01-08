library(microbenchmark)
library(Rcpp)
sourceCpp("cool.cpp")
sourceCpp("greedy.cpp")


plotter = function(ranks,N,lines=FALSE,...){
  qs = c(0,data[ranks],1)
  ranks = c(0,ranks,N+1)
  vals = c(0,sapply(1:k,function(i) ranks[i+1]-ranks[i])/N)
  ds = c(1,sapply(1:k,function(i) qs[i+1]-qs[i]))
  if (lines) lines (qs,vals/ds,type="h",...)
  else plot(qs,vals/ds,type="h",...)
  lines(qs,vals/ds,type="S",...)
}

k = 7
N = 30000
data = sort(rbeta(N,2,7))

a = alg(T,F,c(0,data,1),N,k)
b = alg(T,T,c(0,data,1),N,k)
c = alg(F,T,c(0,data,1),N,k)
d = alg(F,F,c(0,data,1),N,k)


ranks = a[k-1,]
ranks = ranks[ranks!=0]
plotter(ranks,N,col="red",ylim=c(0,3.2),lty=3,xlab="x",ylab="Density")
aa = data[ranks]

ranks = b[k-1,]
ranks = ranks[ranks!=0]
plotter(ranks,N,lines=TRUE,col="blue",lty=2)
bb = data[ranks]
lines(ts,dbeta(ts,2,7))

legend("topright",c("KL","L2"), 
       lty=c(3,2), lwd=c(1,1),
       col=c("red","blue"))

qs = data[c[k-1,]]
sde_plotter(qs,lty=3,col="blue",ylim=c(0,3.2),xlab="x",ylab="Density")
qs = data[d[k-1,]]
sde_plotter(qs,lines=TRUE,lty=2,col="red")
lines(ts,dbeta(ts,2,7))
legend("topright",c("KL","L2","Beta(2,7)"), 
       lty=c(3,2,1), lwd=c(1,1,1),
       col=c("red","blue","black"))