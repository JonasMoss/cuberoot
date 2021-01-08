library(alabama)

sds = function(vec,l=3){
  sapply(l:(length(vec[1,])-l),function(i) sd(vec[1,(i-l):(i+l)])+sd(vec[2,(i-l):(i+l)]))
}

N = 1000
n = 100
a = 2
b = 7
l = 3
q_true <- sde_true(pbeta,qbeta,2,shape1=a,shape2=b)$par
bs <- floor(n^(1/3)):floor(10*n^(1/3))       

reapit = function(n=100,a=2,b=7,l=3,q_true,alpha=alpha){
  data = rbeta(n,a,b)
  bs = floor(n^(1/3)):floor(10*n^(1/3))      
  res = sapply(bs,function(b) sci(data,b=b,alpha))
  use = bs[which.min(sds(res))+l]
  vec = sci(data,b=use,alpha=alpha)
  q_true <= vec[2] & q_true >= vec[1] 
}

res = mean(replicate(1000,reapit(n=n,a=a,b=b,l=l,q_true)))

###################
# k > 2 ###########
###################

sdsk = function(vec,l=3){
  K = dim(vec)[1]
  len = dim(vec)[2]
  ret = sapply(1:K, function(j) {
         sapply(l:(len-l),
                function(i) sd(vec[j,(i-l):(i+l)]))
         })
  rowSums(ret)
}

reapitk = function(n=100,a=2,b=7,k=3,l=3,q_true=NULL,alpha=0.05){
  data = rbeta(n,a,b)
  bs = max(floor(n^(1/3)),k+5):floor(10*n^(1/3))      
  res = sapply(bs,function(b) ssci(data,b=b,k=k,alpha=alpha))
  use = bs[which.min(sdsk(res))+l]
  vec = ssci(data,b=use,k=k,alpha=alpha)
  sapply(1:(k-1),inside,vec=vec,q_true=q_true)
}


###############
# Simulations #

set.seed(1848)

N = 5
n = 100
a = 2
b = 7
l = 3
k = 4
q_true <- sde_true(pbeta,qbeta,k,shape1=a,shape2=b)$par
mean(replicate(N,reapitk(k=k,q_true=q_true)))

wrapper = function(N=2,n=100,k=3,a=2,b=7,alpha=0.05,l=3){
  set.seed(1848)
  q_true = sde_true(pbeta,qbeta,k,shape1=a,shape2=b)$par
  if (k==2) mean(replicate(N,reapit(q_true=q_true,alpha=alpha,l=l)))
  else mean(replicate(N,reapitk(k=k,q_true=q_true,alpha=alpha,l=l)))
}

first = mapply(wrapper,a=c(2,5,1/3),b=c(7,6,1),k=2,n=rep(c(100,1000),each=3),N=2)


