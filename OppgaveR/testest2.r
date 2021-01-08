library(rgl)

ts <- seq(0,1,by=0.001)
k <- 2
n <- 100
a <- 2
b <- 7
data <- sort(rbeta(n,a,b))
lcd <- lecdf(data)
ecd <- ecdf(data)


ks <- seq(0,1,by=0.01)
kks <- expand.grid(ks,ks)
z <- apply(kks,1,sde_objective,Pn=ecd,k=3)
z[is.nan(z)] <- NA
z <- matrix(z,length(ks),length(ks))
persp3d(ks,ks,z,col="red",xlab="1",ylab="2")

plot(ts,sapply(ts,function(k) sde_objective(c(0.2551838,k),ecd,k=3)),col="blue",type="l")
plot(ts,sapply(ts,function(k) sde_objective(c(k,0.2551838),ecd,k=3)),type="l")
abline(v=0.2574553)
abline(v=0.1767307)
#####################################
## Attempt at iterative algorithm. ##
#####################################

sde_objective <- function(q,Pn,k=2){
  q <- c(0,q,1)
  dis <- sapply(1:k,function(i) log(q[i+1]-q[i]))
  probs <- sapply(1:k,function(i) Pn(q[i+1])-Pn(q[i]))
  -sum(probs*dis)
}

one_dim_sde <- function(Pn,data,q,index,k){
  vals <- sapply(data,function(h) sde_objective(c(head(q,index-1),h,tail(q,k-1-index)),Pn,k=k))
  q_new <- data[which.max(vals)]
  c(head(q,index-1),q_new,tail(q,k-1-index))
}

sde_iter_2 <- function(data,k=2,m=3,q=seq(0.1,0.99,length.out=k-1)){
  ecd <- ecdf(data)
  M <- (k-1)*m-1
  #q <- quantile(data,c(1:(k-1))/k)
  print(q)
  for (index in ((0:M)%%(k-1)+1)){
    q <- one_dim_sde(ecd,data,q,index,k)
    print(q)
  }
  q
}

########################
## Another maximizer. ##
########################

sde_2 <- function(data,k,m=3){
  ecd <- ecdf(data)
  dd <- expand.grid(rep(list(data),k-1))
  vals <- apply(dd,1,function(q) sde_objective(q,ecd,k=k))
  dd[which.max(vals),]
}

n <- 20
a <- 1
b <- 7
data <- sort(rbeta(n,a,b))
ecd <- ecdf(data)
(alg1 <- sde(data,k=3,m=2))
(alg2 <- sde_2(data,k=3,m=5))