library(numDeriv)
library(ks)

Pnm2 <- Vectorize(function(q,data){
  k = 2
  n = length(data)
  Pq = sum(data<=q)/n
  dis <- c(log(1-Pq)-log(1-q), log(Pq) - log(q))
  probs <- c(1-Pq,Pq)
  sum(probs*dis)
},vectorize.args="q")

Pnm3 <- Vectorize(function(i){
  q = data[i]
  Pq = i/n
  dis <- c(log(1-Pq)-log(1-q), log(Pq) - log(q))
  probs <- c(1-Pq,Pq)
  sum(probs*dis)
},vectorize.args=c("i"))

Pm2 <- function(q,P){
  k <- length(q)+1
  Pn = P(c(0,q,1))
  q <- c(0,q,1)
  dis <- sapply(1:k,function(i) log(q[i+1]-q[i])-log(Pn[i+1]-Pn[i]))
  probs <- sapply(1:k,function(i) Pn[i+1]-Pn[i])
  -sum(probs*dis)
}


data = rbeta(10000,2,7)
ts = seq(0,1,by=0.0001)
plot(ts,sapply(ts,Pnm2,data=data),type="l")
for (i in data) abline(v=i,lty=2,col="blue")

histt = function(data,q=2){
  
  data = sort(data)
  n = length(data)
  
  Pnm <- Vectorize(function(i){
    q = data[i]
    Pq = i/n
    dis <- c(log(1-Pq)-log(1-q), log(Pq) - log(q))
    probs <- c(1-Pq,Pq)
    sum(probs*dis)
  },vectorize.args=c("i"))
  
  i = which.max(Pnm(1:n))
  val = i/n
  res = data[i]
  c(res,val)
}



plot(ts,dbeta(ts,2,7),xlim=c(0,1),type="l",col="gray")
lines(histogram(data, k=2, method="greedy"),lty=2,col="blue")
lines(histogram(data, k=2, method = "exact"),lty=3,col="black")

ns = (1:1000)*30
true = histt(rbeta(100000,2,7))
res = sapply(ns,function(i) histt(rbeta(i,2,7))-true)
plot(ns,res)

res4 = replicate(10000,histt(rbeta(50000,2,7)))
res2 = replicate(10000,histt(rbeta(1000,2,7)))

#############################
# Simulate brownian motions #
#############################
### put X ~ beta(2,7) and k = 2

#### True values
shape1 = 2
shape2 = 7
P = function(x) pbeta(x,shape1,shape2)
a = optimize(function(x) -Pm2(x,P),interval=c(0,1))$minimum
Fa = P(a)
f = dbeta(a,shape1,shape2)
df = grad(dbeta,a,shape1=shape1,shape2=shape2)

## Calculating the V matrix.
c = df*(log(Fa)-log(1-Fa)-log(a)+log(1-a))-2*f*(1/(1-a)+1/a)+(1-Fa)/(1-a)^2+Fa/a^2
V = matrix(c(c,f/Fa+f/(1-Fa),f/Fa+f/(1-Fa),-1/Fa-1/(1-Fa)),2,2)

# The Brownian motion multiplier
mult = sqrt(f)*abs(log(Fa)-log(1-Fa)-log(a)+log(1-a))

maxxer = function(){
  delta = 0.01
  ts = seq(-1,1,by=delta)
  n = length(ts)
  motion = cumsum(rnorm(n,0,sqrt(delta)))
  grid = expand.grid(ts,ts)
  maxers = apply(grid,1,function(x) 1/2*t(x)%*%V%*%x)+mult*motion
  maxers[is.na(maxers)] = NaN
  maxer = which.max(maxers)
  c(as.matrix(grid[maxer,]))
}

limit_values = replicate(10000,maxxer())

############################################################
# Find the empirical distribution for n = 50,100,1000,5000 #
############################################################


res50 = replicate(60000,histt(rbeta(50,2,7)))
res100 = replicate(60000,histt(rbeta(100,2,7)))
res1000 = replicate(60000,histt(rbeta(1000,2,7)))
res5000 = replicate(60000,histt(rbeta(5000,2,7)))
res10000 = replicate(60000,histt(rbeta(10000,2,7)))
res20000 = replicate(10000,histt(rbeta(20000,2,7)))
res50000 = replicate(10000,histt(rbeta(50000,2,7)))
res100000 = replicate(10000,histt(rbeta(100000,2,7)))

#################
# Plots come!!! #
#################

# First coord.
par(mfrow=c(2,3))
plot(density(limit_values[1,]),col="blue")
lines(density((50)^(1/3)*(res50[1,]-a)),lty=2)

plot(density(limit_values[1,]),col="blue")
lines(density((100)^(1/3)*(res100[1,]-a)),lty=2)

plot(density(limit_values[1,]),col="blue")
lines(density((1000)^(1/3)*(res1000[1,]-a)),lty=2)

plot(density(limit_values[1,]),col="blue")
lines(density((5000)^(1/3)*(res5000[1,]-a)),lty=2)

plot(density(limit_values[1,],adjust=2),col="blue")
lines(density((10000)^(1/3)*(res10000[1,]-a)),lty=2)

# Second coord.
plot(density(limit_values[2,],adjust=2),col="blue")
lines(density((50)^(1/3)*(res50[2,]-Fa),adjust=3),lty=2)

plot(density(limit_values[2,],adjust=2),col="blue")
lines(density((100)^(1/3)*(res100[2,]-Fa),adjust=3),lty=2)

plot(density(limit_values[2,],adjust=2),col="blue")
lines(density((1000)^(1/3)*(res1000[2,]-Fa)),lty=2)

plot(density(limit_values[2,],adjust=2),col="blue")
lines(density((5000)^(1/3)*(res5000[2,]-Fa)),lty=2)

plot(density(limit_values[2,]),col="blue")
lines(density((50000)^(1/3)*(res50000[2,]-Fa)),lty=2)
