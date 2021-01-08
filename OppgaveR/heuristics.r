N <- 250
xs <- rnorm(sum(1:N))
mat <- matrix(NA,N,N)
mat[row(mat)+col(mat) >=(N+1)] <- xs
mus <- rowMeans(mat,na.rm=TRUE)

ts <- seq(0,N,by=0.01)
plot(1:N,mus)
lines(ts,1/sqrt(ts))
lines(ts,-1/sqrt(ts))

N <- 100
xs <- runif(sum(1:N))
mat <- matrix(NA,N,N)
mat[row(mat)+col(mat) >=(N+1)] <- xs
mus <- apply(mat,1,max,na.rm=TRUE)

ts <- seq(0,N,by=0.01)
plot(1:N,mus)
lines(ts,ts*0+1)
lines(ts,1-1/ts)

ts <- seq(0,3,by=0.01)
trend <- function(t,lambda=1) -(t-lambda)^2/lambda^3
rand <- Vectorize(function(n,t,lambda) rnorm(1,0,1/n*abs(t-lambda)^2),vectorize.args="t")
plot(ts,trend(ts,1)+rand(5,ts,1),type="l")
