###########################
# Understanding the mode. #
###########################

##############################
# Path of a brownian motion. #
##############################

brown_path <- function(ts){
  # Make a brownian path from ts. We'll assume that it's ordered.
  ts <- sort(ts)
  n <- length(ts)
  diffs <- sapply((1:(n-1)),function(i) ts[i+1]-ts[i])
  vals <- rnorm(n,0,sqrt(diffs))
  cumsum(vals)
}

xs <- seq(-2,2,by=0.01)
plot(xs,brown_path(xs)-xs^2,type="l")


Xs <- rnorm(10000,0,1)

Pn <- function(n,x,a){
  fun <- ecdf(Xs[1:n])
  fun(x+a)-fun(x-a)
}

cool <- function(n,x,a) Pn(n,x,a) - Pn(n,0,a)

plot(xs,cool(10000,xs,2),type="l",col="blue")
lines(xs,cool(50,xs,2),col="red")
lines(xs,cool(100,xs,2),col="green")
lines(xs,cool(1000,xs,2),col="purple")