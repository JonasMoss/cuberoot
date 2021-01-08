Pnm <- function(q,data){
  k <- length(q)+1
  n <- length(data)
  Pn = c(0,sapply(q,function(j) sum(data<=j)/n),1)
  q <- c(0,q,1)
  dis <- sapply(1:k,function(i) log(q[i+1]-q[i]))
  probs <- sapply(1:k,function(i) Pn[i+1]-Pn[i])
  -sum(probs*dis)
}

Pm <- function(q,P){
  k <- length(q)+1
  Pn = P(c(0,q,1))
  q <- c(0,q,1)
  dis <- sapply(1:k,function(i) log(q[i+1]-q[i]))
  probs <- sapply(1:k,function(i) Pn[i+1]-Pn[i])
  -sum(probs*dis)
}

bias_true <- function(n,k){
  data <- rbeta(n,2,7)
  q_hat <- sde(data,k=k,method="iter")
  Pnm(q_hat,data) - Pm(q_hat,P)
}

subbias <- function(data,b,k,K=1000){
  n <- length(data)
  est <- sde(data,k=k,method="iter")
  samples <- matrix(replicate(K,sample(data,b)),b,K)
  ests <- apply(samples,2,sde,k=k,method="iter")
  mean(sapply(1:K,function(x) Pnm(ests[,x],samples[,x]))-sapply(1:K,function(x) Pnm(ests[,x],data)))
}

smooth_bias <- function(data,k,K=1000,L=10000){
  h = hgcde(data)
  n <- length(data)
  b = L/2
  newdata = rgcde(L,data,h=h)
  est <- sde_iter(newdata,k=k)
  samples <- matrix(replicate(K,sample(newdata,b)),b,K)
  ests <- apply(samples,2,sde_iter,k=k)
  L^(2/3)*mean(sapply(1:K,function(x) Pnm(ests[,x],samples[,x]))-sapply(1:K,function(x) Pnm(ests[,x],newdata)))
}


P <- function(x) pbeta(x,2,7)
N = 1000
subbias(rbeta(N,2,7),N/2,k,K=100)*N^(2/3)


ns1 <- (1:10)*50
res1 <- sapply(ns1,function(i) c(mean(replicate(N,bias_true(i,k))),subbias(rbeta(i,2,7),i/2,k)))
res1f = res1
res1f[1,] = res1[1,]*((ns1)^(2/3))
res1f[2,] = res1[2,]*((ns1/2)^(2/3))
res1f


ns2 <- (15:20)*500
res2 <- sapply(ns2,function(i) c(mean(replicate(N,bias_true(i,k))),subbias(rbeta(i,2,7),i^(1/3),k)))
res2*(ns2^(2/3))


n = 100
ks1 = 3:20
res <- sapply(ks1,function(i) c(mean(replicate(N,bias_true(n,i))),subbias(rbeta(n,2,7),n/2,i)))
res*(n^(2/3))


n = 1000
ks1 = 3:20
res <- sapply(ks1,function(i) c(mean(replicate(N,bias_true(n,i))),subbias(rbeta(n,2,7),n/2,i)))
res*(n^(2/3))
