
qs <- seq(0,1,by=0.001)
n <- 100000
N <- 10000
maxi <- function(n,a,b){
  xs <- rbeta(n,a,b)
  Pn <- ecdf(xs)
  vals <- sapply(qs,function(q) test2(Pn,q))
  qs[which.max(vals)]
}

res4 <- replicate(N,maxi(n,2,7))

hist(n^(1/2)*(res-mean(res)),freq=FALSE)

hist(n^(1/2)*(res2-mean(res2)),freq=FALSE)

hist(n^(1/2)*(res3-mean(res3)),freq=FALSE)

hist(n^(1/2)*(res4-mean(res4)),freq=FALSE)


test1 <- function(q,a,b){
  pbeta(q,a,b)*(log(1-q)-log(q))-log(1-q)
}

test2 <- function(Pn,q){
  Pn(q)*(log(1-q)-log(q))-log(1-q)
}

maxi2 <- function(n,a,b){
  xs <- rbeta(n,a,b)
  Pn <- ecdf(xs)
  vals <- sapply(qs,function(q) test2(Pn,q))
  plot(qs,test2(Pn,qs),type="l")
  qs[which.max(vals)]
}


for (m in 1:100) plot(qs,test1(qs,m/100,m/100),type="l",main=m/100)

###########################
# How the functions look. #
###########################

q <- 0.1
q0 <- 0.5
xs <- seq(0,1,by=0.001)
plot(qs,-(qs<=q)*log(q)-(qs>q)*log(1-q),xlim=c(0,1),ylim=c(0,5),type="s")
for (q in (4:10)/10){
  lines(xs,-(xs<=q)*log(q)-(xs>q)*log(1-q)+(-(xs<=q0)*log(q0)-(xs>q0)*log(1-q0)),col=q*10)
}

q <- 0.3
q0 <- 0.3
xs <- seq(q0-1/10,q0+1/10,by=0.00001)
plot(xs,-(xs<=q)*log(q)-(xs>q)*log(1-q)+(xs<=q0)*log(q0)+(xs>q0)*log(1-q0),type="s")
for (i in -5:5){
  q <- i/100+q0
  lines(xs,-(xs<=q)*log(q)-(xs>q)*log(1-q)-(-(xs<=q0)*log(q0)-(xs>q0)*log(1-q0)),col=i+10,type="s")
}

#############
# Example ###
#############

cdf <- function(x,theta) x^theta
pdf <- function(x,theta) theta*x^(theta-1)
xs <- seq(0,1,by=0.001)
plot(xs,pdf(xs,6),type="l")


#########################
# Different assumpitons #
#########################

cdf <- function(x,a) 1/a*x + 1/(1-a)*x*(x>a)
pdf <- function(x,a) 1/a*(x<=a) + 1/(1-a)*(x>a)
ran <- function(n,a) {
  ts <- rbinom(n,1,.5)
  ts*(runif(n,0,a))+(1-ts)*runif(n,a,1)
}
xs <- seq(0,0.1,by=0.00001)
plot(xs,pdf(xs,0.3),type="l")
hist(ran(100000,a))

maxer <- function(q,a){
  cdf(q,a)*(log(1-q)-log(q))-log(1-q)
}

plot(xs,maxer(xs,0.3),type="l")

optimize(maxer,interval=c(0,1),a=0.3,maximum=TRUE)


qs <- seq(0,1,by=0.001)
n <- 1000
N <- 10000
maxi <- function(n,a,b){
  xs <- ran(n,a)
  Pn <- ecdf(xs)
  vals <- sapply(qs,function(q) test2(Pn,q))
  qs[which.max(vals)]
}

res5 <- replicate(N,maxi(n,0.3))
hist(n*(res5-0.3),freq=FALSE,breaks=100)
hist(n^(1/2)*(res5-0.3),freq=FALSE,breaks=100)
hist(n^(1/3)*(res5-0.3),freq=FALSE,breaks=100)

res6 <- sapply(1:10000,maxi,a=0.3)

ks <- 1:500
res7 <- sapply(ks,maxi,a=0.3)
plot(res7-0.3,ylim=c(-.2,.2))
lines(1/ks,col="red")
lines(-1/ks,col="red")
lines(1/ks^(1/2),col="blue")
lines(-1/ks^(1/2),col="blue")
lines(1/ks^(1/3),col="green")
lines(-1/ks^(1/3),col="green")


ks <- 1:250
res8 <- rnorm(250,0,1/sqrt(ks))
plot(res8)
lines(1/ks,col="red")
lines(-1/ks,col="red")
lines(1/ks^(1/2),col="blue")
lines(-1/ks^(1/2),col="blue")
lines(1/ks^(1/3),col="green")
lines(-1/ks^(1/3),col="green")

ks <- 1:500
res8 <- rbeta(500,0,1/sqrt(ks))
plot(res8)
lines(1/ks,col="red")
lines(-1/ks,col="red")
lines(1/ks^(1/2),col="blue")
lines(-1/ks^(1/2),col="blue")
lines(1/ks^(1/3),col="green")
lines(-1/ks^(1/3),col="green")



