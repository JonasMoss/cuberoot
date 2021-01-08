n <- 1000
ts <- seq(0,2,by=0.001)
plot(ts,1-ts^(1/4),type="l")
xs <- sort(runif(n,0,20))
ys <- rnorm(n,1-xs^(1/4),0.1)
plot(xs,ys)