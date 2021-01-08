source("histogram(k=2)Simulations.R")
plotterSim = function(nobs = 100, shape1 = 2, shape2 = 7,Nreps = 10000){
  values = getValues(shape1,shape2)
  scaling = values$scaling
  a = values$a
  V = values$V
  mult = values$mult  
  xs = replicate(Nreps,histogram(rbeta(nobs,shape1,shape2),k=2,method="greedy")$splits)
  ys = nobs^(1/3)*(xs - a)
  
  cols = brewer.pal(4,"Dark2")
  hist(ys,freq=FALSE,,xlab="q",ylim=c(0,scaling*dchernoff(0)),breaks=50,
       main=substitute(beta(i,j),list(i=shape1,j=shape2)),sub=paste0("n = ",nobs),
       border = cols[1],bty="l",xlim=c(-max(abs(ys)),max((abs(ys)))),las=1)
  lines(locfit(~ys),col = cols[2])
  tt = seq(-0.5,0.5,by=0.001)
  grid()
  #lines(tt,scaling*dchernoff(tt*scaling),col = cols[3]) 
  lines(locfit(~I(1/scaling*csimsText)),col=cols[3])
}

shapes = matrix(c(1,1/2,2,7,1,3,8/10,9/10),4,2,byrow=TRUE)

par(mfrow=c(2,3))

for (nobs in c(100,1000,10000)){
  plotterSim(nobs,1,1/2)
}

for (nobs in c(100,1000,10000)){
  plotterSim(nobs,8/10,9/10)
}



hh = seq(-20,20,by=0.001)
nobs = 5

lnormMeans = function(nobs,N=10000) sqrt(nobs)*replicate(N,mean(rlnorm(nobs))-exp(1/2))
plot(locfit(~lnormMeans(nobs)),col=cols[1])
lines(hh,dnorm(hh,0,sqrt((exp(1)-1)*exp(1))),col=cols[2])

expMeans = function(nobs,N=10000) sqrt(nobs)*replicate(N,mean(rexp(nobs))-1)
plot(locfit(~expMeans(nobs)),col=cols[1])
lines(hh,dnorm(hh,0,1),col=cols[2])

quantile(ys50,c(0.1,0.5,0.9))
quantile(ys100,c(0.1,0.5,0.9))
quantile(ys1000,c(0.1,0.5,0.9))
quantile(ys10000,c(0.1,0.5,0.9))
quantile(ys100000,c(0.1,0.5,0.9))

cols = brewer.pal(5,"Dark2")
plot(locfit(~ys50),type="l",col=cols[1],ylim=c(0,15),bty="l",
     ylab = "Density", xlab="x",main=expression(beta(3,3)),
     xlim = c(-0.42,0.42))
lines(locfit(~ys100),type="l",col=cols[2])
lines(locfit(~ys1000),type="l",col=cols[3])
lines(locfit(~ys10000),type="l",col=cols[4])
lines(locfit(~ys100000),type="l",col=cols[5])
grid()
legend("topleft",legend=c(50,100,1000,10000,"100000"),col=cols,lty=1,bty="n")

shape1=1
shape2=1
nobs = 100
xs100 = replicate(Nreps,histogram(rbeta(nobs,shape1,shape2),k=2,method="greedy")$splits)
yst100 = nobs^(1/2)*(xs100 - 0.5)
sd(yst100)

nobs = 1000
xs1000 = replicate(Nreps,histogram(rbeta(nobs,shape1,shape2),k=2,method="greedy")$splits)
yst1000 = nobs^(1/2)*(xs1000 - 0.5)
sd(yst1000)

nobs = 10000
xs10000 = replicate(Nreps,histogram(rbeta(nobs,shape1,shape2),k=2,method="greedy")$splits)
yst10000 = nobs^(1/2)*(xs10000 - 0.5)
sd(yst10000)

nobs = 50000
xs50000 = replicate(Nreps,histogram(rbeta(nobs,shape1,shape2),k=2,method="greedy")$splits)
yst50000 = nobs^(1/2)*(xs50000 - 0.5)
sd(yst50000)

V = getValues(shape1,shape2)$V
sqrt(4/V^2)

cols = brewer.pal(5,"Dark2")
plot(locfit(~yst100),type="l",col=cols[1],ylim=c(0,2.5),bty="l",
     ylab = "Density", xlab="x",main=expression(beta(3,3)))
lines(locfit(~yst1000),type="l",col=cols[2])

grid()
legend("topleft",legend=c("Histogram (100)","Histogram (1000)",
                          "Mean (1000)","Median (1000)"),col=cols,lty=1,bty="n")

lines(locfit(~replicate(Nreps,sqrt(nobs)*(mean(rbeta(nobs,shape1,shape2))-0.5))),col=cols[3])
lines(locfit(~replicate(Nreps,sqrt(nobs)*(median(rbeta(nobs,shape1,shape2))-0.5))),col=cols[4])

nobs
xs = replicate(Nreps,histogram(rbeta(nobs,shape1,shape2),k=2,method="greedy")$splits)
ys = nobs^(1/2)*(xs - 0.5)


