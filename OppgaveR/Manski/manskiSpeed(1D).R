library("microbenchmark")
library("locfit")
library("RColorBrewer")

nobs = 10000
xx = rnorm(nobs)
yy = (1 + beta*xx + rnorm(nobs))>=0
values = -1/xx
isRed = c((yy & xx >=0 ) | (!yy & xx < 0))
ww = rep(1,nobs)

mansky = sapply(1:10000, function(x) {
  mean(microbenchmark(redBlue(values[1:x], ww[1:x], isRed[1:x], isSorted = FALSE))$time)
  })

sorty = sapply(1:10000, function(x) mean(microbenchmark(sort(xx[1:x]))$time))
meany = sapply(1:10000, function(x) mean(microbenchmark(mean(xx[1:x]))$time))

plot(1:10000,mansky)
points(1:10000,sorty,col="red")

zz = 1:10000
manskys = mansky/10^6
sortys = sorty/10^6
meanys = meany/10^6
cols = brewer.pal(8, "Set2")
plot(locfit(manskys~zz),ylab="Time in microsec",bty="l",xlab="n",col=cols[1])
lines(locfit(sortys~zz),col=cols[2])
lines(locfit(meanys~zz),col=cols[3])
legend("topleft",legend=c("Manski","Sort","Mean"),col=cols,lty=c(1,1,1),bty="n")