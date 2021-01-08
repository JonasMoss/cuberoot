library(numDeriv)

#######################################
# Functions for simple LS-estimation. #
#######################################

ml_true <- function(q,a,b){
  pbeta(q,a,b)*(log(1-q)-log(q))-log(1-q)
}

ml_estimate <- function(q,Pn){
  Pn(q)*(log(1-q)-log(q))-log(1-q)
}

ls_true <- function(q,a,b){
  2*pbeta(q,a,b)*1/(2*q)+2*pbeta(q,a,b,lower.tail=FALSE)*1/(2*(1-q))-1/4*(1/q+1/(1-q))
}

ls_estimate <- function(q,Pn){
  2*Pn(q)*1/(2*q)+2*(1-Pn(q))*1/(2*(1-q))-1/4*(1/q+1/(1-q))
}

a <- 2
b <- 7
maxi_ls <- function(n,a,b){
  xs <- rbeta(n,a,b)
  Pn <- ecdf(xs)
  optimize(ls_estimate,interval=c(0,1),Pn=Pn,maximum=TRUE)$maximum
}

N <- 100000
n <- 100
estimates_ls <- replicate(N,maxi_ls(n,a,b))
vals_ls <- n^(1/3)*(estimates_ls-true_ls)

####################
# Brownian motions #
####################

# Find the true values.
true_ls <- optimize(ls_true,interval=c(0,1),a=a,b=b,maximum=TRUE)$maximum
sd_ls <- abs((1/(1-true_ls)-1/true_ls))*sqrt(dbeta(true_ls,a,b))
V_ls <- hessian(ls_true,true_ls,a=a,b=b)

brown_path <- function(ts){
  # Make a brownian path from ts. We'll assume that it's ordered.
  ts <- sort(ts)
  n <- length(ts)
  diffs <- sapply((1:(n-1)),function(i) ts[i+1]-ts[i])
  vals <- rnorm(n,0,sqrt(diffs))
  cumsum(vals)
}

maxer_ls <- function(xs){
  ys <- sd*brown_path(xs)+0.5*V*xs^2
  xs[which.max(ys)]
}

xs <- seq(-1,1,by=0.001)
N <- 100000
maxes_ls <- replicate(N,maxer_ls(xs))

hist(vals_ls,freq=FALSE,breaks=100)
hist(maxes_ls,freq=FALSE,breaks=100)

plot(density(vals_ls,adjust=2),type="l",col="blue")
lines(density(maxes_ls,adjust=2))

