library(ks)
library(alabama)
library(numDeriv)

#We will calculate the necessary stuff involved in the Nils distribution.

#################
# The V matrix. #
#################

#  q,f'(q), F_n(q),f(q) for all the qs.

Vij <- function(i,q,fq,Fq){
  (fq[i+1]+fq[i])/(q[i+1]-q[i])-(Fq[i+1]-Fq[i])/(q[i+1]-q[i])^2
}

Vii <- function(i,q,dfq,fq,Fq){
  dfq[i]*(log(q[i+1]-q[i])-log(q[i]-q[i-1]))-2*fq[i]*(1/(q[i+1]-q[i])+1/(q[i]-q[i-1]))+(Fq[i+1]-Fq[i])/(q[i+1]-q[i])^2+(Fq[i]-Fq[i-1])/(q[i]-q[i-1])^2
}

V <- function(q,dfq,fq,Fq){
  k <- length(q)
  q <- c(0,q,1) 
  fq <- c(0,fq,1)
  dfq <- c(0,dfq,1)
  Fq <- c(0,Fq,1)
  
  if (k==1) {
    Vii(2,q,dfq,fq,Fq)
  }
  
  else {
    dia <- diag(sapply(1:k,function(i) Vii(i+1,q,dfq,fq,Fq)))
    odia <- diag(sapply(1:(k-1),function(i) Vij(i+1,q,fq,Fq)),nrow=(k-1),ncol=(k-1))
    upper.dia <- rbind(cbind(rep.int(0,k-1),odia),rep.int(0,k))
    lower.dia <- t(upper.dia)
    dia + upper.dia + lower.dia
  }
}

V_wrapper <- function(data,q){
  n <- length(data)
  Fq = sapply(q,function(j) sum(data<=j)/n)
  dfq <- kdde(data,deriv.order=1,eval.points=q)$estimate
  fq <- kde(data,eval.points=q)$estimate
  V(q,dfq,fq,Fq)
}



##########################
# Perfomance comparison. #
##########################

# Looks pretty good to me!
data <- rbeta(100,2,7)
k <- 2
q <- sde_true(pbeta,qbeta,k,shape1=2,shape2=7)$par
q_hat <- sde_iter(data,k=k)
Fq <- Pn(q)
fq <- dbeta(q,2,7)
dfq <- grad(dbeta,q,shape1=2,shape2=7)
Pn <- function(x) pbeta(x,2,7)
V(q,dfq,fq,Fq)
V_est = V_wrapper(data,q)
V_wrapper(data,q_hat)


###########################
# Calculate G(s) ##########
###########################

fun <- function(W,q,fq){
  k <- length(q)+1
  q <- c(0,q,1)
  logs <- abs(sapply(2:k,function(i) log(q[i+1]-q[i])-log(q[i]-q[i-1])))
  sum(logs*sqrt(fq)*W)
}

## Simulate Brownpaths! These should be saved and loaded.

brown_path <- function(ts,sigma=1){
  # Make a brownian path from ts. We'll assume that it's ordered.
  ts <- sort(ts)
  n <- length(ts)
  diffs <- sapply((1:(n-1)),function(i) ts[i+1]-ts[i])
  vals <- rnorm(n,0,sqrt(diffs))
  cumsum(vals)
}


# Looks pretty good to me!
data <- rbeta(100,6,7)
k <- 2
q_hat <- sde_iter(data,k=k)
Fq <- Pn(q)
fq <- dbeta(q,2,7)
dfq <- grad(dbeta,q,shape1=2,shape2=7)
Pn <- function(x) pbeta(x,2,7)
V_est = V_wrapper(data,q_hat)

N <- 50000
ts <- seq(-2,2,by=0.01)

find_max <- function(path,V_est,q,fq){
  zs <- sapply(path,fun,q=q,fq=fq) + 0.5*ts^2*V_est
  ts[which.max(zs)]
}

res <- replicate(N,find_max(brown_path(ts),V_est,q_hat,fq))
res2 <- replicate(N,ts[which.max(brown_path(ts)-ts^2)])

gamma <- (-2/V_est*fq^(1/2)*abs(log(1-q_hat)-log(q_hat)))^(-2/3)

plot(density(res,adjust=2))
lines(density(1/gamma*res2,adjust=2),col="blue")

