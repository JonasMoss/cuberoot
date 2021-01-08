k = 5
nobs = 1000
seeUs = replicate(1000,quantile(runif(nobs),(1:(k-1))/k))
seeUs2 = replicate(1000,histogram(runif(nobs),k=5,method="greedy")$splits)

cov(t(sqrt(nobs)*(seeUs-(1:(k-1))/k)))
cov(t(sqrt(nobs)*(seeUs2-(1:(k-1))/k)))

k = 2
nobs = 1000
shape1 = 4
shape2 = 4
seeUs = replicate(2000,quantile(rbeta(nobs,shape1,shape2),c(0.5)))
seeUs2 = replicate(2000,histogram(rbeta(nobs,shape1,shape2),k=2,method="greedy")$splits)
var(sqrt(nobs)*(seeUs-0.5))
var(sqrt(nobs)*(seeUs2-0.5))
