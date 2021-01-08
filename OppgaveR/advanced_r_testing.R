xx = rbeta(1000000,2,7)
qtrue = histogram(xx,k=2,method="greedy")$splits

bootit = function(xx) {
  n = length(xx)
  #weights = rmultinom(1, n, (1:n)/n)
  weights = rexp(n,1)
  histogram(weights*xx,k=2,method="greedy")$splits
}

smoothit = function(xx) {
  n = length(xx)
  xx_new = rgcde(n,xx)
  histogram(xx_new,k=2,method="greedy")$splits
}

xx = rbeta(5000,2,7)
samples = replicate(1000,smoothit(xx))
hist(samples)

xx = rbeta(5000,2,7)
quantiles1 = apply(sapply((1:50)*100,
                          function(i) replicate(10000,bootit(xx[1:i]))),
                   2,quantile,probs=c(0.90))

xx = rbeta(1000,2,7)
quantiles2 = apply(sapply((1:50)*100,
                          function(i) replicate(10000,bootit(xx[1:i]))),
                   2,quantile,probs=c(0.90))
xx = rbeta(1000,2,7)
quantiles3 = apply(sapply((1:50)*100,
                          function(i) replicate(10000,bootit(xx[1:i]))),
                   2,quantile,probs=c(0.90))
xx = rbeta(1000,2,7)
quantiles4 = apply(sapply((1:50)*100,
                          function(i) replicate(10000,bootit(xx[1:i]))),
                   2,quantile,probs=c(0.90))

library("RColorBrewer")
cols = brewer.pal(4, "Set2")
plot(quantiles1,col=cols[1],pch=1)
points(quantiles2,col=cols[2],pch=2)
points(quantiles3,col=cols[3],pch=3)
points(quantiles4,col=cols[4],pch=4)

bootit = function(xx) {
  n = length(xx)
  #weights = rmultinom(1, n, (1:n)/n)
  weights = rexp(n,1)
  sqrt(n)*(mean(weights*xx)-mean(xx))
}

xx = rnorm(1000,0,1)
samples = replicate(10000,bootit(xx))
plot(density(samples))