library(reshape)
xs <- rexp(1000,2)
b <- 100
K <- 100000
mu_stars <- 1/colMeans(matrix(replicate(K,sample(xs,b)),b,K))
mu_hat <- 1/mean(xs)
subs <- sqrt(b)*(mu_stars-mu_hat)
hist(subs,breaks=1000)


X <- read.table(url("http://www.stat.umn.edu/geyer/5601/mydata/big-unif.txt"),header=TRUE)
x <- X$x

x <- rnorm(10000,0,1)

N <- 10000
bs <- (1:20)*6

subsamples <- data.frame(matrix(sapply(bs, function(b) apply(matrix(replicate(N,sample(x,b)),N,b),1,mean)),N,length(bs)))
names(subsamples) <- bs
subsamples <- melt(subsamples)
boxplot((value-mean(x))~variable,data=subsamples)
lines(ks,1/sqrt(ks),col="blue")
lines(ks,-1/sqrt(ks),col="red")