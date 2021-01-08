plot(Ms,lower,ylim=c(min(lower)-1,max(upper)+1),
     xlab="m",ylab="Confidence interval",bty="l",
     sub = paste0("Subsampling when N = ",nobs),
     main = substitute(beta[i],list(i=index)))
points(Ms,upper)
grid()
abline(h=quantile(trues[1,],c(alpha/2,1-alpha/2)),lty=2)  
abline(h=origin[1],lty=3,col="red")

quants1 = c()
quants2 = c()

for (m in c(5,10,15,20)) {
  quants1 = rbind(quants1,quantile(mboot(yy,xx1,xx2,Nreps=10000,m=m)[,1],c(alpha/2,1-alpha/2)))
  quants2 = rbind(quants2,quantile(mboot(yy,xx1,xx2,Nreps=10000,m=m)[,2],c(alpha/2,1-alpha/2)))
}

quants1
quantile(trues[1,],c(alpha/2,1-alpha/2))
quants2
quantile(trues[2,],c(alpha/2,1-alpha/2))

ests = mboot(yy,xx1,xx2,Nreps=10000,m=5)
quants = matrix(c(quantile(ests$betas[,1],c(alpha/2,1-alpha/2)),
                  quantile(trues[1,],c(alpha/2,1-alpha/2)),               
                  quantile(ests$betas[,2],c(alpha/2,1-alpha/2)),
                  quantile(trues[2,],c(alpha/2,1-alpha/2))),4,2,byrow=TRUE)
quants

plot(ests$betas[,1])
plot(ests$betas[,2],ylim=c(-10,10))

#par(mfrow=c(2,2))
#plot(rollapply(ests[,1], width = 1000, FUN = sd),ylim=c(0,10))
#plot(rollapply(ests[,2], width = 1000, FUN = sd),ylim=c(0,10))
#plot(rollapply(ests[,1], width = 1000, FUN = mean),ylim=c(-50,50))
#plot(rollapply(ests[,2], width = 1000, FUN = mean),ylim=c(-50,50))


`%between%`<- function(x, vec) x>=head(vec,1) & x <= tail(vec,1)

testUgly = function(alpha=0.1,m=10,Nreps=1000) {
  nobs  = 300
  beta1 = 4
  beta2 = -1/2
  xx1   = rnorm(nobs)
  xx2   = rnorm(nobs)
  yy    = (1+beta1*xx1+beta2*xx2 + rnorm(nobs))>=0
  ests = mboot(yy,xx1,xx2,Nreps=Nreps,m=m)
  beta1true = (beta1 %between% quantile(ests$betas[,1],c(alpha/2,1-alpha/2)))
  beta2true = (beta2 %between% quantile(ests$betas[,2],c(alpha/2,1-alpha/2)))
  c(beta1true,beta2true)
}

ress = replicate(100,testUgly(alpha=0.3,m = 5))
rowMeans(ress)