# We make functions that calculate the Hellinger and IMSE distance betweeen a
# a histogram (q,w9 and 


data = rbeta(100,2,7)
qe = histogram(data,k = 7,method="exact")$splits
qg = histogram(data,k = 7,method="greedy")$splits
Pnm(qe,data)
Pnm(qg,data)


n = 200
shape1 = 1
shape2 = 8
dist = function(x) dbeta(x,shape1,shape2)
k = 10


hellL2L2 = replicate(1000,hellinger(histogram(rbeta(n,shape1,shape2),method="greedy",k=k,type="L2",weights="L2"),dist))
hellL2eq = replicate(1000,hellinger(histogram(rbeta(n,shape1,shape2),method="greedy",k=k,type="L2",weights="equal"),dist))
hellKLKL = replicate(1000,hellinger(histogram(rbeta(n,shape1,shape2),method="greedy",k=k,type="KL",weights="KL"),dist))
hellKLeq = replicate(1000,hellinger(histogram(rbeta(n,shape1,shape2),method="greedy",k=k,type="KL",weights="equal"),dist))
imseL2L2 = replicate(1000,imse(histogram(rbeta(n,shape1,shape2),method="greedy",k=k,type="L2",weights="L2"),dist))
imseL2eq = replicate(1000,imse(histogram(rbeta(n,shape1,shape2),method="greedy",k=k,type="L2",weights="equal"),dist))
imseKLKL = replicate(1000,imse(histogram(rbeta(n,shape1,shape2),method="greedy",k=k,type="KL",weights="KL"),dist))
imseKLeq = replicate(1000,imse(histogram(rbeta(n,shape1,shape2),method="greedy",k=k,type="KL",weights="equal"),dist))

mean(hellL2L2)
mean(hellL2eq)
mean(hellKLKL)
mean(hellKLeq)

mean(imseL2L2)
mean(imseL2eq)
mean(imseKLKL)
mean(imseKLeq)

plot(histogram(rbeta(n,shape1,shape2),method="greedy",k=k,type="KL",weights="equal"))



Nreps = 100
n = 200
shape1 = 2
shape2 = 7
dist = function(x) dbeta(x,shape1,shape2)
k = 50

hellKLKLg = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                           method="greedy"  ,k=k,type="KL",weights="KL"),dist))
hellKLKLe = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                           method="exact"   ,k=k,type="KL",weights="KL"),dist))
hellKLKLs = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                           method="smoothed",k=k,type="KL",weights="KL"),dist))


imseKLKLg = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                           method="greedy"  ,k=k,type="KL",weights="KL"),dist))
imseKLKLe = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                           method="exact"   ,k=k,type="KL",weights="KL"),dist))
imseKLKLs = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                           method="smoothed",k=k,type="KL",weights="KL"),dist))


hellL2L2g = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                                               method="greedy"  ,k=k,type="L2",weights="L2"),dist))
hellL2L2e = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                                               method="exact"   ,k=k,type="L2",weights="L2"),dist))
hellL2L2s = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                                               method="smoothed",k=k,type="L2",weights="L2"),dist))

imseL2L2g = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                                          method="greedy"  ,k=k,type="L2",weights="L2"),dist))
imseL2L2e = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                                          method="exact"   ,k=k,type="L2",weights="L2"),dist))
imseL2L2s = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                                          method="smoothed",k=k,type="L2",weights="L2"),dist))


hellVec = c(mean(hellKLKLg),
  mean(hellKLKLe),
  mean(hellKLKLs),
  mean(hellL2L2g),
  mean(hellL2L2e),
  mean(hellL2L2s))

imsVec = c(mean(imseKLKLg),
  mean(imseKLKLe),
  mean(imseKLKLs),
  mean(imseL2L2g),
  mean(imseL2L2e),
  mean(imseL2L2s))

hellims = cbind(hellVec,imsVec)
colnames(hellims) = c(paste0("hellVec",n),paste0("imsVec",n))
rownames(hellims) = c("KLg","KLe","KLs","L2g","L2e","L2s")

plot(histogram(rbeta(200,shape1,shape2),weights="KL",k=6,method="smoothed"))
lines(hh,dbeta(hh,2,7))

simulator = function(n,k,weights="KL",Nreps=100){
  shape1 = 2
  shape2 = 7
  dist = function(x) dbeta(x,shape1,shape2)
  
  hellKLKLg = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                                                  method="greedy"  ,k=k,type="KL",weights=weights),dist))
  hellKLKLe = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                                                  method="exact"   ,k=k,type="KL",weights=weights),dist))
  hellKLKLs = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                                                  method="smoothed",k=k,type="KL",weights=weights),dist))
  
  
  imseKLKLg = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                                             method="greedy"  ,k=k,type="KL",weights=weights),dist))
  imseKLKLe = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                                             method="exact"   ,k=k,type="KL",weights=weights),dist))
  imseKLKLs = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                                             method="smoothed",k=k,type="KL",weights=weights),dist))
  
  
  hellL2L2g = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                                                  method="greedy"  ,k=k,type="L2",weights=weights),dist))
  hellL2L2e = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                                                  method="exact"   ,k=k,type="L2",weights=weights),dist))
  hellL2L2s = replicate(Nreps,hellinger(histogram(rbeta(n,shape1,shape2),
                                                  method="smoothed",k=k,type="L2",weights=weights),dist))
  
  imseL2L2g = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                                             method="greedy"  ,k=k,type="L2",weights=weights),dist))
  imseL2L2e = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                                             method="exact"   ,k=k,type="L2",weights=weights),dist))
  imseL2L2s = replicate(Nreps,imse(histogram(rbeta(n,shape1,shape2),
                                             method="smoothed",k=k,type="L2",weights=weights),dist))
  
  
  hellVec = c(mean(hellKLKLg),
              mean(hellKLKLe),
              mean(hellKLKLs),
              mean(hellL2L2g),
              mean(hellL2L2e),
              mean(hellL2L2s))
  
  imsVec = c(mean(imseKLKLg),
             mean(imseKLKLe),
             mean(imseKLKLs),
             mean(imseL2L2g),
             mean(imseL2L2e),
             mean(imseL2L2s))
  
  hellims = cbind(hellVec,imsVec)
  colnames(hellims) = c(paste0("hellVec",n),paste0("imsVec",n))
  rownames(hellims) = c("KLg","KLe","KLs","L2g","L2e","L2s")
  hellims
}

matrix = cbind(c(100,100,100,500,500),c(2,8,50,10,60))
varWeights = apply(matrix,1,function(x) simulator(x[1],k=x[2],Nreps=100,weights="L2"))
eqWeights  = apply(matrix,1,function(x) simulator(x[1],k=x[2],Nreps=100,weights="equal"))

i = 1
use = eqWeights[,i]
dim(use) = c(6,2)
round(use[,1],3)

round(t(eqWeights[7:12,]),3)

round(t(varWeights[7:12,]),3)

