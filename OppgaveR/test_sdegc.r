k = 3

q_true = sde_true(function(x) pbeta(x,2,7),function(x) qbeta(x,2,7),k=k)

ns = 20:1000
results = sapply(ns,function(n){
  data = rbeta(n,2,7)
  cbind(real=sde(data,k)-q_true,smooth=sdegc(data,k)-q_true)[1,]
})


plot(ns,results[1,],xlab="n",ylab="Estimate")
points(ns,results[2,],col="blue")
lines(ns,0.07*ns^(-1/3)-0.0018)
lines(ns,-0.07*ns^(-1/3)-0.0018)