a = 0.3
xs1 <- seq(0,a,by=0.001)
xs0 <- seq(a,1,by=0.001)
plot(c(xs1,xs0),c(xs1,xs0),type="n",xlab="x",ylab="y")
lines(xs1,xs1/xs1)
lines(xs0,xs0*0)
lines(xs1,1/10*(xs1)*sin(50*xs1)+0.8)
lines(xs0,1/10*(1-xs0)*sin(50*xs0)+0.2)

rademacher <- function(N) sample(c(-1,1),N,replace=TRUE)

n <- 1000
xs <- seq(-0.5,0.5,by=0.001)
plot(xs,-xs^2,type="l",ylim=c(-0.5,0.25))
lines(xs,rademacher(length(xs))/sqrt(n)*(sqrt(abs(xs)))-xs^2)

funit <- function(N,n,beta=1/2){
  xs <- seq(-0.5,0.5,length.out=N)
  ret <- -xs^2 + runif(N,-1,1)/sqrt(n)*abs(xs)^((4*beta-1)/(2*beta))
  n^beta*xs[which.max(ret)]
}

result <- replicate(1000,funit(10000,10000,1/3))
hist(result,freq=FALSE,breaks=100)


beta <- 2
N <- 1000
n <- 10
xs <- seq(-0.5,0.5,length.out=N)
ret <- -xs^2 + runif(N)/sqrt(n)*abs(xs)^((4*beta-1)/(2*beta))
plot(xs,ret,type="l")