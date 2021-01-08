#############################################################################
# Data -> qs                                                                #
#############################################################################


sde <- function(data,k=2,q0=NULL,...){
  q <- c(1:(k-1))/k
  q <- quantile(data,q)
  Pn <- ecdf(data)
  ret <- optim(q,sde_objective,Pn=Pn,k=k)
  ret$par
}

sde_objective <- function(q,Pn,k=2,...){
  q <- c(0,q,1)
  dis <- sapply(1:k,function(i) log(q[i+1]-q[i]))
  probs <- sapply(1:k,function(i) Pn(q[i+1],...)-Pn(q[i],...))
  sum(probs*dis)
}

genfun <- function(k,n,a=3,b=7){
  data <- rbeta(n,a,b)
  sde(data,k)
}

subbag <- function(data,k,m,size=100){
  samples <- replicate(size,sample(data,m))
  matrix(apply(samples,2,sde,k=k),(k-1),size)
}

stepfun <- function(q){
  k <- length(q)+1
  q <- c(0,q,1)
  dis <- sapply(1:k,function(i) 1/(q[i+1]-q[i]))
  inds <- function(y,q){
    as.integer(sapply(1:k,function(i) y<q[i+1]&y>=q[i]))
  }  
  function(y){
    sum(inds(y,q)*dis)
  }
  } 

dest <- function(bagged){
  function(y){
   sum(sapply(as.list(apply(test,2,stepfun)),function(f) f(y)))/length(test)
  }
}

a <- 5
b <- 6
data <- rbeta(100,a,b)
qs <- seq(0,1,0.01)
par(mfrow=c(2,2))

for (i in 2:5){
  test <- subbag(data,14,5,10000)
  shoo <- dest(test)
  lines(qs,sapply(qs,shoo),type="l")
}
plot(qs,dbeta(qs,5,6),type="l",ylim=c(0,4))

