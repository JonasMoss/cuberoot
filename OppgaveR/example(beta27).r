library(alabama)

###############################################
# Objective functions and actual minimizers. ##
###############################################

sde_l2_objective <- function(q,Pn,k=2,...){
  q <- c(0,q,1)
  dis <- sapply(1:k,function(i) 1/(q[i+1]-q[i]))
  probs <- -2/k*sapply(1:k,function(i) Pn(q[i+1],...)-Pn(q[i],...))
  resid <- 1/k^2*sum(dis)
  sum(probs*dis) + resid
}

sde_ml_objective <- function(q,Pn,k=2,...){
  q <- c(0,q,1)
  dis <- sapply(1:k,function(i) log(q[i+1]-q[i]))
  probs <- sapply(1:k,function(i) Pn(q[i+1],...)-Pn(q[i],...))
  sum(probs*dis)
}

sde_l2_true <- function(P,Q,k,...){
  q0 <- Q(c(1:(k-1))/k,...)
  
  hin <- function(x,...){
    y <- c(0,x,1)
    h <- sapply(1:k,function(i) y[i+1]-y[i])
    h <- c(h,x)
    h
  }
  
  auglag(q0,sde_l2_objective,hin=hin,Pn=P,k=k,...)
}

sde_ml_true <- function(P,Q,k,...){
  q0 <- Q(c(1:(k-1))/k,...)
  
  hin <- function(x,...){
    y <- c(0,x,1)
    h <- sapply(1:k,function(i) y[i+1]-y[i])
    h <- c(h,x)
    h
  }
  
  auglag(q0,sde_objective,hin=hin,Pn=P,k=k,...)
}

###################################################
# We wish to study beta for different as and bs.  #
###################################################
sde_plotter <- function(q,lines=FALSE,...){
  k <- length(q)
  q <- c(0,q,1)
  ys <- c(0,1/k*sapply(1:(k+1),function(i) 1/(q[i+1]-q[i])))
  if (lines) lines(q,ys,type="S",...)
  else plot(q,ys,type="S",...)
}

plotem <- function(k,a=2,b=7){
  q_ml_true <- sde_ml_true(pbeta,qbeta,k,shape1=a,shape2=b)$par
  qs <- seq(0,1,by=0.0001)
  plot(qs,dbeta(qs,a,b),type="l",ylim=c(0,4))
  sde_plotter(q_ml_true,lines=TRUE,col="red")
}

#par(mfrow=c(2,3))
#sapply(c(2,3,6,12,20,40),plotem)

plotem(5)