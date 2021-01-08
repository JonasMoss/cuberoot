sde_objective <- function(q,Pn,k=2,...){
  q <- c(0,q,1)
  dis <- sapply(1:k,function(i) log(q[i+1]-q[i]))
  probs <- sapply(1:k,function(i) Pn(q[i+1],...)-Pn(q[i],...))
  -sum(probs*dis)
}


ts <- seq(0,1,by=0.001)
k <- 2
n <- 10
a <- 2
b <- 7
data <- sort(rbeta(n,a,b))
lcd <- lecdf(data)
ecd <- ecdf(data)
q <- quantile(data,c(1:(k-1))/k)

plot(ts,sapply(ts,sde_objective,Pn=lcd),type="l")
lines(ts,ecd(ts),col="blue")
plot(ts,log(1-ts)-log(ts),col="red",type="l")
lines(ts,-log(1-ts),col="blue",type="l")

approx <- approxfun(data,sapply(data,sde_objective,Pn=ecd))

plot(ts,sapply(ts,sde_objective,Pn=ecd),col="red",type="l")
lines(ts,approx(ts),col="green")
points(data,sapply(data,sde_objective,Pn=ecd))
lines(ts,sapply(ts,sde_objective,Pn=lcd),col="purple")