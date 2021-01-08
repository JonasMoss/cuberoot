library(locfit)

x1 = runif(100,0,1)
x2 = runif(100,0,1)
y = x1^2 + rnorm(100)
mod=locfit(y~lp(x1))
plot(mod)
plot(x1,y)

par(mfrow=c(2,2))
plot(locfit(NOx~lp(E,nn=0.2),data=ethanol),get.data=TRUE)
plot(locfit(NOx~lp(E,nn=0.4),data=ethanol),get.data=TRUE)
plot(locfit(NOx~lp(E,nn=0.6),data=ethanol),get.data=TRUE)
plot(locfit(NOx~lp(E,nn=0.8),data=ethanol),get.data=TRUE)


data = rbeta(100,1,1)
trans = qnorm(data)
fit = locfit.raw(lp(trans,nn=.5,h=0.1))
plot(fit)

kern = kde(trans)
plot(kern)
news = rkde(1000,kern)
news = pnorm(news)
par(mfrow=c(2,2))
hist(news)
hist(data)
plot(kde(news))
plot(kde(data))



