library(numDeriv)

#######################################
# Functions for simple ML-estimation. #
#######################################

ml_true <- function(q,a,b){
  pbeta(q,a,b)*(log(1-q)-log(q))-log(1-q)
}

ml_estimate <- function(q,Pn){
  Pn(q)*(log(1-q)-log(q))-log(1-q)
}
qs <- seq(0+0.0001,1-0.0001,by=0.000001)
maxi_ml <- function(n,a,b){
  xs <- rbeta(n,a,b)
  Pn <- ecdf(xs)
  estim <- function(q){
    Pn(q)*(log(1-q)-log(q))-log(1-q)
  }
  ys <- estim(qs)
  qs[which.max(ys)]
}
####################
# Brownian motions #
####################

a <- 2
b <- 7

# Find the true values.
true_ml <- optimize(ml_true,interval=c(0,1),a=a,b=b,maximum=TRUE)$maximum
sd_ml <- abs(log(1-true_ml)-log(true_ml))*sqrt(dbeta(true_ml,a,b))
V_ml <- hessian(ml_true,true_ml,a=a,b=b)

brown_path <- function(ts){
  # Make a brownian path from ts. We'll assume that it's ordered.
  ts <- sort(ts)
  n <- length(ts)
  diffs <- sapply((1:(n-1)),function(i) ts[i+1]-ts[i])
  vals <- rnorm(n,0,sqrt(diffs))
  cumsum(vals)
}


N <- 1000000
n <- 10000
estimates_ml <- replicate(N,maxi_ml(n,a,b))
vals_ml <- n^(1/3)*(estimates_ml-true_ml)

maxer <- function(xs){
  ys <- sd_ml*brown_path(xs)+0.5*V_ml*xs^2
  xs[which.max(ys)]
}

xs <- seq(-1,1,by=0.001)
N <- 10000
maxes_ml <- replicate(N,maxer(xs))

hist(vals_ml,freq=FALSE,breaks=100)
hist(maxes_ml,freq=FALSE,breaks=100)

plot(density(vals_ml,adjust=2),type="l",col="blue")
lines(density(maxes_ml,adjust=2))
