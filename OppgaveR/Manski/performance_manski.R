nobs  = 800
xx   = rnorm(nobs)
#temp  = rbinom(nobs,1,0.5)
#xx    = temp*rnorm(nobs,-3) + (1-temp)*rnorm(nobs,3)
beta1 = 6
yy    = (1 + beta1*xx + rnorm(nobs)) >= 0

redBlues_full = manski1d(yy,xx)
redBlues = redBlues_full[[1]]
maxWeight = max(redBlues[3,])

lim = 10
xs = seq(-lim,lim,by=0.01)
# Plotting of both negative and positive.
plot(redBlues[1,],redBlues[3,]/nobs,xlim=c(-lim,lim),ylim=c(-0.1,1.1), col = -2*redBlues[2,] + 4,pch=2)
lines(xs,sapply(xs,function(beta) manskiObjective(beta,yy,xx)/nobs), 
      type="s", col = "purple", lty = 3)

points(-redBlues[1,],redBlues[4,]/nobs, col = -2*redBlues[2,] + 4, pch = 4)
lines(xs,sapply(xs,function(beta) manskiObjective(beta,yy,xx,,positive=FALSE)/nobs), 
      type="s", col = "gray", lty = 4)

abline(v=beta1,lty=2,col="green")


lim = 10
xs = seq(-lim,lim,by=0.01)
# Plotting of both negative and positive.
plot(xs,sapply(xs,function(beta) manskiObjective(beta,yy,xx)/nobs), 
      type="s", col = "purple", lty = 3)

for (x in redBlues_full[[2]][,1]) {
  points(x,maxWeight/nobs, col = "red", pch = 2)
}

for (x in redBlues_full[[2]][,2]) {
  points(x,maxWeight/nobs, col = "blue", pch = 4)
}


xs = seq(3,10,by=0.01)
# Plotting of both negative and positive.
plot(xs,sapply(xs,function(beta) manskiObjective(beta,yy,xx)/nobs), 
     type="s", col = "purple", lty = 3)

for (x in redBlues_full[[2]][,1]) {
  points(x,maxWeight/nobs, col = "red", pch = 2)
}

for (x in redBlues_full[[2]][,2]) {
  points(x,maxWeight/nobs, col = "blue", pch = 4)
}


