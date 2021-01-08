library(alabama)


#############################################################################
# Data -> qs                                                                #
#############################################################################

# This function (sde) takes data and a k, and returns the q-estimates together with 
# kernel density estimates of f(q_i) for each of the i's. Currently
# restricted to [0,1]-support. 

sde <- function(data,k=2,N=10000){
  tests <- replicate(N,sort(sample(data,k)))
  Pn <- ecdf(data)
  ys <- sapply((1:N),function(i) sde_objective(tests[,i],Pn))
  ret <- tests[,which.min(ys)]
  ret
}

sde_objective2 <- function(q,Pn,k=2,...){
  q <- c(0,q,1)
  dis <- sapply(1:k,function(i) log(q[i+1]-q[i]))
  probs <- sapply(1:k,function(i) Pn(q[i+1],...)-Pn(q[i],...))
  sum(probs*dis)
}

sde <- function(data,k=2,q0=NULL,...){
  if (!is.function(data)){
    q <- c(1:(k-1))/k
    q <- quantile(data,q)
    Pn <- ecdf(data)
    ret <- optim(q,sde_objective,Pn=Pn,k=k)
  }
  
  else {
   if (is.null(q0)) q <- c(1:(k-1))/k
   else q <- q0
   Pn <- function(x) data(x,...)
   ret <- optim(q,sde_objective,Pn=Pn,k=k)
  }
  
  ret$par
}

sde_plotter <- function(q,lims=c(0,1),lines=FALSE,...){
  k <- length(q)+1
  q <- c(lims[1],q,lims[2])
  ys <- c(0,1/k*sapply(1:k,function(i) 1/(q[i+1]-q[i])))
  if (lines) {
    lines(q,ys,type="S",...)
    lines(q,ys,type="h",...)
  }
  else {
    plot(q,ys,type="S",...)
    lines(q,ys,type="h",...)
  }
}

###################################
# For finding the true parameter. #
###################################

sde_true <- function(P,Q,k,...){
  q0 <- Q(c(1:(k-1))/k,...)
  hin <- function(x,...){
    y <- c(0,x,1)
    h <- sapply(1:k,function(i) y[i+1]-y[i])-0.0001
    h <- c(h,x)
    h
  }

  auglag(q0,sde_objective2,hin=hin,Pn=P,k=k,...)
}

q_true <- sde_true(pbeta,qbeta,10,shape1=2,shape2=7)$par
#############################################################################
# Tests and such                                                            #
#############################################################################
n <- 100
a <- 2
b <- 7
k <- 3
data <- rbeta(n,a,b)
Pn <- ecdf(data)
qs <- seq(0,1,by=0.001)


q_hat <- sde(data,k)
q_true <- sde(pbeta,k,q0=qbeta(c(1:(k-1))/k,a,b),shape1=a,shape2=b)
plot(qs,dbeta(qs,a,b),type="l",xlim=c(0,1),ylim=c(0,5))
sde_plotter(q_hat,lines=TRUE,col="blue")
sde_plotter(q_true,lines=TRUE,col="red")
sde_plotter(qbeta(c(1:(k-1))/k,a,b),lines=TRUE,col="green")

par(mfrow=c(2,1))
plot(ks,sapply(ks,function(q) sde_objective(q,Pn,k=2)),type="l")
abline(v=q_hat)
plot(ks,sapply(ks,function(q)ls_estimate(q,Pn)),type="l")
abline(v=q_le)



