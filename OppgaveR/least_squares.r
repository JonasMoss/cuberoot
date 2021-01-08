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


qs <- seq(0,1,by=0.001)
N <- 1000
n <- 50
a = 7
b = 5
xs <- rbeta(n,a,b)
Pn <- ecdf(xs)


q_ml <- optimize(ml_true,interval=c(0,1),a=a,b=b,maximum=TRUE)$maximum
q_me <- optimize(ml_estimate,interval=c(0,1),Pn=Pn,maximum=TRUE)$maximum

q_lt <- optimize(ls_true,interval=c(0,1),a=a,b=b,maximum=TRUE)$maximum
q_le <- optimize(ls_estimate,interval=c(0,1),Pn=Pn,maximum=TRUE)$maximum



plot(qs,dbeta(qs,a,b),type="l")
lines(qs,1/(2*(q_ml))*(qs<q_ml)+1/(2*(1-q_ml))*(qs>=q_ml),type="s",col="green")
lines(qs,1/(2*(q_lt))*(qs<q_lt)+1/(2*(1-q_lt))*(qs>=q_lt),type="s",col="blue")

plot(qs,dbeta(qs,a,b),type="l")
lines(qs,1/(2*(q_ml))*(qs<q_ml)+1/(2*(1-q_ml))*(qs>=q_ml),type="s")
lines(qs,1/(2*(q_me))*(qs<q_me)+1/(2*(1-q_me))*(qs>q_me),type="s")

plot(qs,dbeta(qs,a,b),type="l")
lines(qs,1/(2*(q_lt))*(qs<q_lt)+1/(2*(1-q_lt))*(qs>=q_lt),type="s")
lines(qs,1/(2*(q_le))*(qs<q_le)+1/(2*(1-q_le))*(qs>q_le),type="s")

maxi_ml <- function(n,a,b){
  xs <- rbeta(n,a,b)
  Pn <- ecdf(xs)
  vals <- sapply(qs,function(q) ml_estimate(q,Pn))
  qs[which.max(vals)]
}

maxi_ls <- function(n,a,b){
  xs <- rbeta(n,a,b)
  Pn <- ecdf(xs)
  vals <- sapply(qs,function(q) ls_estimate(q,Pn))
  qs[which.max(vals)]
}

res_ml <- replicate(N,maxi_ml(n,a,b))
res_ls <- replicate(N,maxi_ls(n,a,b))
plot(density(res_ml))
plot(density(res_ls))