library("numDeriv")

theta = 1
integrate(function(x) abs(x-theta)*dnorm(x),-Inf,Inf)

expectLow = integrate(function(x) x*dnorm(x),-Inf,theta)$value
expectUpper = integrate(function(x) x*dnorm(x),theta,Inf)$value

theta*(2*pnorm(theta)-1) + (expectUpper - expectLow)

fun = function(theta,origin=0) {
  upper = integrate(function(x) abs(x-theta)*dnorm(x,origin),-Inf,Inf)$value
  lower = integrate(function(x) abs(x-origin)*dnorm(x,origin),-Inf,Inf)$value
  upper - lower
}

origin = 3
df = grad(dnorm,origin,mean=origin)
thetas = seq(-0.1+origin,0.1+origin,by=0.001)
plot(thetas,sapply(thetas,fun,origin=origin),type="l",bty="l")
lines(thetas,(thetas-origin)^2*dnorm(0),col="blue")



fun = function(theta,shape1=2,shape2=7,origin=3) {
  upper = integrate(function(x) abs(x-theta)*dbeta(x,shape1,shape2),-Inf,Inf)$value
  lower = integrate(function(x) abs(x-origin)*dbeta(x,shape1,shape2),-Inf,Inf)$value
  upper - lower
}

shape1 = 1
shape2 = 5
origin = nlm(function(x) (pbeta(x,shape1,shape2)-0.5)^2,0.3)$estimate
df = grad(dbeta,origin,shape1=shape1,shape2=shape2)
thetas = seq(-0.1+origin,0.1+origin,by=0.001)
plot(thetas,sapply(thetas,fun,origin=origin,shape1=shape1,shape2=shape2),type="l",bty="l")
lines(thetas,(thetas-origin)^2*(dbeta(origin,shape1,shape2)+df*origin),col="blue")
lines(thetas,(thetas-origin)^2*(dbeta(origin,shape1,shape2)),col="red")


# Median experiment -----------------------------------------------------------

par(mfrow=c(2,2))

set.seed(1812)
nobs = 100
xx = rnorm(nobs)
kun = function(theta)  mean(abs(xx-theta)-abs(xx))
thetas = seq(-0.1,0.1,by=0.001)
plot(thetas,sapply(thetas,kun),type="l",bty="l",ylab="Process",xlab=expression(theta),
     sub = "n = 100")
Z = (2*sum(xx <= 0) - nobs)/nobs
lines(thetas,Z*(thetas)+dnorm(0)*thetas^2,col="red")
points(median(xx), mean(abs(xx-median(xx))-abs(xx)),lty=2)

set.seed(1812)
nobs = 500
xx = rnorm(nobs)
kun = function(theta)  mean(abs(xx-theta)-abs(xx))
thetas = seq(-0.1,0.1,by=0.001)
plot(thetas,sapply(thetas,kun),type="l",bty="l",ylab="Process",xlab=expression(theta),
     sub = "n = 500")
Z = (2*sum(xx <= 0) - nobs)/nobs
lines(thetas,Z*(thetas)+dnorm(0)*thetas^2,col="red")
points(median(xx), mean(abs(xx-median(xx))-abs(xx)),lty=2)


set.seed(1810)
nobs = 1000
xx = rnorm(nobs)
kun = function(theta)  mean(abs(xx-theta)-abs(xx))
thetas = seq(-0.1,0.1,by=0.001)
plot(thetas,sapply(thetas,kun),type="l",bty="l",ylab="Process",xlab=expression(theta),
     sub = "n = 1000")
Z = (2*sum(xx <= 0) - nobs)/nobs
lines(thetas,Z*(thetas)+dnorm(0)*thetas^2,col="red")
points(median(xx), mean(abs(xx-median(xx))-abs(xx)),lty=2)


set.seed(1810)
nobs = 10000
xx = rnorm(nobs)
kun = function(theta)  mean(abs(xx-theta)-abs(xx))
thetas = seq(-0.1,0.1,by=0.001)
plot(thetas,sapply(thetas,kun),type="l",bty="l",ylab="Process",xlab=expression(theta),
     sub = "n = 10000")
Z = (2*sum(xx <= 0) - nobs)/nobs
lines(thetas,Z*(thetas)+dnorm(0)*thetas^2,col="red")
legend("top",legend=c("True","Approximation"),col=c("black","red"),bty="n",lty=c(1,1))
points(median(xx), mean(abs(xx-median(xx))-abs(xx)),lty=2)

nobs = 50
meds = replicate(80000,median(rnorm(nobs)))
hist(sqrt(nobs)*meds,freq=FALSE,breaks=100)
tt = seq(-10,10,by=0.01)
lines(tt,dnorm(tt,0,1/(2*dnorm(0))))
