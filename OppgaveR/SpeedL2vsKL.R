microbenchmark(histogram(rbeta(100,2,7),k=2,method="exact",type="KL",weights="KL",lim=0.001),
               histogram(rbeta(100,2,7),k=2,method="exact",type="KL",weights="eq",lim=0.001),
               histogram(rbeta(100,2,7),k=2,method="exact",type="L2",weights="L2",lim=0.001),
               histogram(rbeta(100,2,7),k=2,method="exact",type="L2",weights="eq",lim=0.001)
               )

L2 = sapply(ks <- 2:70,function(k) 
  mean(microbenchmark(histogram(rbeta(1000,2,7),k=k,method="exact",type="L2",weights="L2"),times=10)$time))

TE = sapply(ks <- 2:70,function(k) 
  mean(microbenchmark(histogram(rbeta(1000,2,7),k=k,method="sexact",type="KL",weights="KL"),times=10)$time))

KL = sapply(ks <- 2:70,function(k) 
  mean(microbenchmark(histogram(rbeta(1000,2,7),k=k,method="exact",type="KL",weights="KL"),times=10)$time))

plot((KL-TE)/(L2-TE))
plot(L2)
plot(ks,0.5*ks*(n-ks)^2+2*k*(n-k))


sourceCpp("exactTest.cpp")

sexact_histogram = function(data,k,lim=0.0001){
  ret_object = list()
  ret_object$specification = c(type=type,weights=weights)
  ret_object$k = k
  n = length(data)
  type = FALSE
  weights = TRUE
  vals = scpp_exact_alg(weights,type,c(0,data,1),n,k,lim=lim)
  vals = 1:k
  as = data[vals]
  ret_object$splits = as
  if (!weights) {
    ret_object$weights = rep(1/k,k)
  } else {
    ret_object$weights = c(sapply(1:k,
                                  function(i) c(0,vals[-k]/n,1)[i+1]-c(0,vals[-k]/n,1)[i]))
    ret_object$weights[1] = ret_object$weights[1] - 1/n
    ret_object$weights[k] = ret_object$weights[k] + 1/n
  }
  ret_object$method = "exact"
  ret_object$lim = lim
  class(ret_object) = c("hist")
  ret_object  
}