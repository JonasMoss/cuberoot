#####################################
# Attempts at parametric bootstrap. #
#####################################

rsde = function(n,q){
  k = length(q)+1
  selector = sample(1:k,n,replace=TRUE)
  q_aug = c(0,q,1)
  qs = (q_aug[selector+1] - q_aug[selector])
  vals = runif(n,0,1)
  vals*qs +q_aug[selector]
}

######################
# Smoothed sampling. #
######################

rsecd = function(m,data,b1=1/2,b2=1/2){
  # Out of production!
  n = length(data)
  data = sort(data)
  aug = c(0,data,1)
  selector = sample(1:n,m,replace=TRUE)
  qs = (aug[selector+1] - aug[selector])
  vals = rbeta(m,b1,b2)
  vals*qs + aug[selector]
}

bootCI = function(data,k,alpha=0.05){
  n = length(data)
  q_hat = sde_iter(data,k=k)
  kernel = kde(data)
  boots = n^(1/3)*(replicate(N,sde_iter(rkde(n,kernel,positive=TRUE),k)-q_hat))
  quants = quantile(boots,c(alpha/2,1-alpha/2))
  q_hat + quants/n^(1/3)
}

bootCI = function(data,k,alpha=0.05){
  n = length(data)
  q_hat = sde_iter(data,k=k)
  vals = replicate(N,sde_iter(rsecd(n,data),k))
  boots = n^(1/3)*(vals-mean(vals))
  quants = quantile(boots,c(alpha/2,1-alpha/2))
  q_hat + quants/n^(1/3)
}


bootCI = function(data,k,alpha=0.05){
  n = length(data)
  q_hat = sde_iter(data,k=k)
  kernel = kde(data)
  vals = replicate(N,sde_iter(rkde(n,kernel,positive=TRUE),k))
  boots = n^(1/3)*(vals-mean(vals))
  quants = quantile(boots,c(alpha/2,1-alpha/2))
  q_hat + quants/n^(1/3)
}

boot_inside = function(vec,q_true){
  vec[1]<=q_true & vec[2]>=q_true
}

boot_check = function(q_true,data,k,alpha){
  vec = bootCI(data,k,alpha)
  c(boot_inside(vec,q_true),vec[2]-vec[1])
}

boot_replicate = function(a=2,b=7,n=100,M=1000,alpha=0.05,k=2){
  q_true <- sde_true(pbeta,qbeta,k,shape1=a,shape2=b)$par
  set.seed(1848)
  result = rowMeans(replicate(M,boot_check(q_true=q_true,data=rbeta(n,a,b),k=k,alpha=alpha)))
  names(result) = c("Coverage","Length")
  result
}





a = 6
b = 7
k = 2
n = 100
alpha = 0.05
q_true <- sde_true(pbeta,qbeta,k,shape1=a,shape2=b)$par

first = mapply(boot_replicate,a=c(2,5,1/3),b=c(7,6,1),k=2,n=100,M=1000)
second = mapply(boot_replicate,a=c(2,5,1/3),b=c(7,6,1),k=2,n=1000,M=1000)
third = mapply(boot_replicate,a=c(2,5,1/3),b=c(7,6,1),k=2,n=10000,M=1000)

boot_replicate(a=2,b=7,k=k,alpha=alpha,M=100)
boot_replicate(a=6,b=7,k=k,alpha=alpha,M=100,n=10000)
boot_replicate(a=1/3,b=1,k=k,alpha=alpha,M=100)