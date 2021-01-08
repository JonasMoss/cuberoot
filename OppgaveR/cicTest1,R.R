sicBest = function(data, shape1, shape2, ks = c(2,4,6)){
  max = -Inf
  index = 0
  n = length(data)
  cics = c()
  for (k in ks){
    q_hat = histogram(data,k=k,method="greedy")$splits
    bias = mean(replicate(100,biasTrue(n,k,shape1,shape2)))
    cics = c(cics,-n*log(k) + n*(Pnm(q_hat,data)-bias))
  }
  cics
  
}

kk = seq(0,1,by=0.001)
par(mfrow=c(2,4))
data = rbeta(100,2,7)
res = sicBest(data,2,7, ks = 2:75)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),
     main=NA,sub=paste("Chosen k: ",(which.max(res)+1)))
lines(kk,dbeta(kk,2,7),lty=2)

data = rbeta(100,1,1/2)
res = sicBest(data,1,1/2, ks = 2:75)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),
              main=NA,sub=paste("Chosen k: ",(which.max(res)+1)),ylim=c(0,6))
lines(kk,dbeta(kk,1,1/2),lty=2)

data = rbeta(100,3,1)
res = sicBest(data,3,1, ks = 2:75)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),
     main=NA,sub=paste("Chosen k: ",(which.max(res)+1)),ylim=c(0,6))
lines(kk,dbeta(kk,3,1),lty=2)

data = rbeta(100,8/10,9/10)
res = sicBest(data,8/10,9/10, ks = 2:75)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),main=NA,sub=paste("Chosen k: ",(which.max(res)+1)))
lines(kk,dbeta(kk,8/9,9/10),lty=2)



kk = seq(0,1,by=0.001)
par(mfrow=c(2,4))
data = rbeta(1000,2,7)
res = sicBest(data,2,7, ks = 2:150)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),main=NA,sub=paste("Chosen k: ",(which.max(res)+1)))
lines(kk,dbeta(kk,2,7),lty=2)

data = rbeta(1000,1,1/2)
res = sicBest(data,1,1/2, ks = 2:150)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),
     main=NA,sub=paste("Chosen k: ",(which.max(res)+1)),ylim=c(0,6))
lines(kk,dbeta(kk,1,1/2),lty=2)

data = rbeta(1000,3,1)
res = sicBest(data,3,1, ks = 2:150)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),
     main=NA,sub=paste("Chosen k: ",(which.max(res)+1)),ylim=c(0,6))
lines(kk,dbeta(kk,3,1),lty=2)

data = rbeta(1000,8/10,9/10)
res = sicBest(data,8/10,9/10, ks = 2:150)
plot(res)
plot(histogram(data,k=(which.max(res)+1),method="greedy"),main=NA,sub=paste("Chosen k: ",(which.max(res)+1)))
lines(kk,dbeta(kk,8/9,9/10),lty=2)



kk = seq(0,1,by=0.001)
par(mfrow=c(2,4))
data = rbeta(50,2,7)
res = sicBest(data,2,7, ks = 2:40)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),
     main=NA,sub=paste("Chosen k: ",(which.max(res)+1)),ylim=c(0,6))
lines(kk,dbeta(kk,2,7),lty=2)

data = rbeta(50,1,1/2)
res = sicBest(data,1,1/2, ks = 2:40)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),
     main=NA,sub=paste("Chosen k: ",(which.max(res)+1)),ylim=c(0,6))
lines(kk,dbeta(kk,1,1/2),lty=2)

data = rbeta(50,3,1)
res = sicBest(data,3,1, ks = 2:40)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),
     main=NA,sub=paste("Chosen k: ",(which.max(res)+1)),ylim=c(0,6))
lines(kk,dbeta(kk,3,1),lty=2)

data = rbeta(50,8/10,9/10)
res = sicBest(data,8/10,9/10, ks = 2:40)
plot(res,xlab="k",ylab="CIC")
plot(histogram(data,k=(which.max(res)+1),method="greedy"),main=NA,sub=paste("Chosen k: ",(which.max(res)+1)))
lines(kk,dbeta(kk,8/9,9/10),lty=2)

