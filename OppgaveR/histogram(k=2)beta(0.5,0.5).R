par(mfrow=c(1,2))
cols = brewer.pal(4,"Dark2")
gg = seq(0,1,by=0.001)
plot(gg,dbeta(gg,0.1,0.1),typ="l",ylim=c(0,25),bty="l",xlab="x",ylab="Density",
     main=expression(beta(frac(1,10),frac(1,10))),col=cols[1])
grid()
lines(histogram(rbeta(10000,0.1,0.1),k=2,method="greedy"),col=cols[2])

a = histogram(rbeta(1000000,0.1,0.1),k=2,method="greedy")$splits

nobs = 100
beta010010resulst = replicate(10000,histogram(rbeta(nobs,0.1,0.1),k=2,method="greedy")$splits)
beta010010vals    = abs(beta010010resulst-0.999) >= abs(beta010010resulst-0.001)
beta010010vals    = beta010010vals*0.999 + (1-beta010010vals)*0.001
beta010010asym    = (nobs)^(1/1000)*(beta010010resulst-beta010010vals)
quantile(beta010010asym,c(0.1,0.5,0.9))

hist(beta010010resulst,freq=FALSE,breaks=100,xlab="Split point",
     main=paste0("Distribution, n = ",nobs),bty="l",las=1,border=cols[2])
