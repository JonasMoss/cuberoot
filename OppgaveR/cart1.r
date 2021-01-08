 <- 50
xs <- sort(20*rbeta(N,1/2,1/2)-10)
ts <- seq(-10,10,by=0.001)
ys <- exp(xs)/(1+exp(xs))+rnorm(N,0,0.1*abs(sin(xs)))
plot(xs,ys,type="p")
lines(ts,exp(ts)/(1+exp(ts)))

estimator <- function(ys,xs){
  d <- rpart(ys~xs)$splits[1,4]
  l <- mean(ys[xs<=d])
  u <- mean(ys[xs>d])
  c(l=l,u=u,d=d)
}

testfun <- function(n){
  xs <- runif(n,-10,10)
  ys <- exp(xs)/(1+exp(xs))+rnorm(n)
  estimator(ys,xs)
}

plfun <- function(t,l,u,d) (t<=d)*l + (t>d)*u

par <- estimator(ys,xs)

attach(as.list(par))
lines(ts,plfun(ts,l,u,d),type="s")
detach(as.list(par))

n <- 1000
N <- 1000000
result <- replicate(N,testfun(n))
hist(n^(1/3)*(result[1,]-0.06931018),freq=FALSE,breaks=50)
hist(n^(1/3)*(result[2,]-0.9306898),freq=FALSE,breaks=50)
hist(n^(1/3)*result[3,],freq=FALSE,breaks=50)
rowMeans(result)
