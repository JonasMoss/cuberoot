source("lde.r")
library(ks)
library(reshape2)
library(dplyr)

###########################
# Subsampling procedures. #
###########################


ssci <- function(data,k=2,alpha=0.05,b=50,K=1000){
  n <- length(data)
  est <- sde_iter(data,k=k)
  ests <- replicate(K,sde_iter(sample(data,b),k=k))
  subs <- b^(1/3)*matrix((ests-est),(k-1),n)
  quants <- sapply(1:(k-1),function(k) quantile(subs[k,],c(alpha/2,1-alpha/2)))
  ret <- est + t(quants)/n^(1/3)
  rownames(ret) <- paste("Jump", 1:(k-1))
  ret
}

inside <- function(k,vec=NULL,q_true=NULL){
  q_true[k] <= vec[k,2] & q_true[k] >= vec[k,1] 
}

verify_ss <- function(k=3,n=100,b=50,alpha=0.05,K=1000,q_true=NULL){
  data <- rbeta(n,2,7)
  vec <- ssci(data,k=k,alpha=alpha,b=b,K=K)
  sapply(1:(k-1),inside,vec=vec,q_true=q_true)
}

tryem <- function(b,N=100,k=2,n=100,q_true=0.2783792){
  result <- matrix(replicate(N,verify_ss(k=k,n=n,b=b,q_true=q_true)),(k-1),N)
  rowMeans(result)
  
}

###########################
# Asymptotic CI for k = 2 #
###########################

aci = function(data){
  n = length(data)
  q_hat = sde_iter(data,k=2)
  fq = kde(data,eval.points=q_hat)$estimate
  V_est = V_wrapper(data,q_hat)
  gamma = (-2/V_est*fq^(1/2)*abs(log(1-q_hat)-log(q_hat)))^(-2/3)
  quants = c(-1.01,1.01)*1/gamma
  q_hat + quants/n^(1/3)
}

sci <- function(data,alpha=0.05,b=50,K=1000){
  n <- length(data)
  est <- sde(data)
  ests <- replicate(K,sde(sample(data,b)))
  subs <- b^(1/3)*(ests-est)
  quants <- quantile(subs,c(alpha/2,1-alpha/2))
  ret <- est + t(quants)/n^(1/3)
  ret
}


checkem <- function(data_new,q_hat,b){
  vec=sci(data_new,b=b)
  q_hat <= vec[2] & q_hat >= vec[1] 
}

findit <- function(data,alpha=0.05,K=1000,L=100){
  q_hat = sde(data)
  m = 50
  n = length(data)
  bs = (1:8)*10
  b = 20
  mean(replicate(10,mean(replicate(L,checkem(sample(data,m),q_hat,floor(b*m/n))))))
  
}




tryone = function(q_true=0.2783792,a=2,b=7,n=100){
  vec = aci(rbeta(n,a,b))
  q_true <= vec[2] & q_true >= vec[1] 
}

tryone2 = function(q_true=0.2783792,a=2,b=7,n=100,B=30){
  vec = sci(rbeta(n,a,b),b=B)
  q_true <= vec[2] & q_true >= vec[1] 
}

N <- 1000
mean(replicate(N,tryone()))
mean(replicate(N,tryone2(n=100,B=20,a=6,b=7,q_true=q_true)))
mean(replicate(N,tryone2(n=200,B=28)))
mean(replicate(N,tryone2(n=200,B=35)))

ns <- (1:6)*100
sapply(ns, function(i) mean(replicate(N,tryone2(n=i,B=i^(2/3)))))
sapply(ns, function(i) mean(replicate(N,tryone2(n=i,B=i^(1/2)))))
sapply(ns, function(i) mean(replicate(N,tryone2(n=i,B=2*i^(1/2)))))
sapply(ns, function(i) mean(replicate(N,tryone2(n=i,B=i^(1/3)))))

a = 6
b = 7
N = 1000
q_true <- sde_true(pbeta,qbeta,2,shape1=a,shape2=b)$par
sapply(ns, function(i) mean(replicate(N,tryone2(q_true=q_true,n=i,B=i^(1/3),a=a,b=b))))


######################
# Minimum volatility #
######################

N = 1000
n = 100
a = 6
b = 7
bs <- floor(n^(1/3)):floor(10*n^(1/3))
bs <- 15 + (1:30)*2                      
data = rbeta(n,a,b)
res <- apply(sapply(bs,function(b) sci(data,b=b)),2,function(x) abs(x[2]-x[1]))
plot(bs,res)
mean(replicate(N,tryone2(n=100,B=30,a=a,b=b,q_true=q_true)))

##########################
# Looking at the rate. ###
##########################


set.seed(1848)
x <- rbeta(10000,3,7)
est <- sde(x)
N <- 10000
bs <- (1:20)*6
k <- 100000
ks <- seq(0,100)

subsamples <- data.frame(matrix(sapply(bs, function(b) apply(matrix(replicate(N,sample(x,b)),N,b),1,sde)),N,length(bs)))
names(subsamples) <- bs
subsamples <- melt(subsamples)
boxplot((value-est)~variable,data=subsamples)
lines(ks,0.12*(1/ks)^(1/3),col="blue")
lines(ks,-0.12*(1/ks)^(1/3),col="red")
sqrt(var(filter(subsamples,variable==100)))




f <- function(x) dbeta(x,2,7)
df <- function(x) grad(dbeta,x,shape1=2,shape2=7)
q <- q_true

V <- function(q,fq,dfq,Fq) {
  dfq*(log(1-q)-log(q))-2*fq*(1/(1-q)+1/q)+(1/q^2-1/(1-q)^2)*Fq+1/(1-q)^2
}