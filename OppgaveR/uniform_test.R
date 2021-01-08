nobs = 1000
res = replicate(10000,histogram(runif(nobs),k=6,method="greedy")$splits)
org = (1:5)/6
tes = sqrt(nobs)*(res-org)
keep61000 = cov(t(tes))

nobs = 10000
k = 4
res = replicate(10000,histogram(runif(nobs),k=k,method="greedy")$splits)
org = (1:(k-1))/k
tes = sqrt(nobs)*(res-org)
keep4 = cov(t(tes))


testy = function(nobs,shape=2) {
  res  = replicate(10000,histogram(rbeta(nobs,shape,shape),k=k,method="greedy")$splits)
  k    = 2
  org  = (1:(k-1))/k
  tes  = sqrt(nobs)*(res-org)  
  keep = var(tes)
  keep/(4/getValues(shape,shape)$V^2)
}


kressy = sapply(50*(1:20),function(x) var(replicate(10000,sqrt(x)*(mean(rexp(x,1)-1)))))
plot(kressy)

ressy = sapply(100*(1:50),testy)
ressy2 = sapply(100*(50:100,test))
plot(100*(1:50),ressy,bty="l", xlab = "n", ylab="Empirical variance / Limit variance",
     main=substitute(paste(nn, beta(2,2)), list(nn="Variance ratios of ")))
grid()

mod = lm(ressy~I(kk^(-1/3)))
mod
lines(kk,coef(mod)[1]+coef(mod)[2]*kk^(-1/3),lty=2,col="blue")


nobs = 1000
k = 2
shape = 3
res2 = replicate(10000,histogram(rbeta(nobs,shape,shape),k=k,method="greedy")$splits)
org2 = (1:(k-1))/k
tes2 = sqrt(nobs)*(res2-org2)
keep2 = var(tes2)
keep2/(4/getValues(shape,shape)$V^2)

hh = seq(-10,10,by=0.001)
plot(locfit(~tes2))
lines(hh,dnorm(hh,0,sqrt(keep2)),col="red")
lines(hh,dnorm(hh,0,sqrt(4/getValues(shape,shape)$V^2)),col="blue")