# Examples of histograms

xss = rbeta(1000000,9,5)

par(mfrow=c(2,2))

tt = seq(0,1,by=0.001)
plot(histogram(xss,k=10,weights="KL",type="KL",method="greedy"),
     ylim=c(0,3.5),xlim=c(0,1),bty="l",sub=NA,main=NA)
lines(tt,dbeta(tt,9,5),type="l",lty=2)
plot(histogram(xss,k=10,method="greedy",type="L2"),bty="l",
     ylim=c(0,3.5),xlim=c(0,1),sub=NA,main=NA)
lines(tt,dbeta(tt,9,5),type="l",lty=2)

breaks = (1:11)/11
hist(xss,breaks=breaks,freq=FALSE,xlab="x",main=NA,ylim=c(0,3.5),xlim=c(0,1))
lines(sort(xss),dbeta(sort(xss),9,5),type="l",lty=2)

k = 10
qs = quantile(xss,(1:(k-1))/k)
splits = c(0,qs,1)
ys = c(0,1/k*sapply(1:k,function(i) 1/(splits[i+1]-splits[i])))
plot(splits,ys,type="S",bty="l",xlab="x",ylab="Density",ylim=c(0,3.5))
lines(splits,ys,type="h")
lines(sort(xss),dbeta(sort(xss),9,5),type="l",lty=2)


plot(histogram(xss,k=10,method="greedy"),bty="l",sub=NA,main=NA,ylim=c(0,3.5))
lines(sort(xss),dbeta(sort(xss),9,5),type="l",lty=2)
