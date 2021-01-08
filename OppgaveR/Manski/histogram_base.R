library("Rcpp")
library("alabama")
source("copula_base.R")
sourceCpp("greedy.cpp")
sourceCpp("exact.cpp")

# Support functions and variables ---------------------------------------------

`%||%` <- function(a, b) if (! is.null(a)) a else b

# Algorithms ------------------------------------------------------------------
greedy_histogram = function(data,k,type="KL",weights="equal",lim=NULL,
                            modulator=10,init=NULL){
  ret_object = list()
  ret_object$specification = c(type=type,weights=weights)
  ret_object$k = k
  n = length(data)
  type = (type == "L2")
  weights = (weights != "equal")
  init = (init %||% floor(seq(1,n,length.out=k-1)))
  lim = (lim %||% 1/sqrt(n))
  vals = cpp_greedy_alg(weights,type,c(0,data,1),n,k,
           modulator=modulator,init=init,lim=lim)
  as = data[vals[-k]]
  ret_object$splits = as
  if (!weights) ret_object$weights = rep(1/k,k)
  else ret_object$weights = c(sapply(1:k,function(i) c(0,vals[-k]/n,1)[i+1]-c(0,vals[-k]/n,1)[i]))
  ret_object$iterations = vals[k]
  ret_object$method = "greedy"
  class(ret_object) = c("hist")
  ret_object  
}

exact_histogram = function(data,k,type="KL",weights="equal",lim=NULL){
  ret_object = list()
  ret_object$specification = c(type=type,weights=weights)
  ret_object$k = k
  n = length(data)
  type = (type=="L2")
  weights = (weights!="equal")
  lim = (lim %||% 1/sqrt(n))
  vals = cpp_exact_alg(weights,type,c(0,data,1),n,k,lim=lim)
  vals = vals[k-1,]
  as = data[vals]
  ret_object$splits = as
  if (!weights) ret_object$weights = rep(1/k,k)
  else {
    ret_object$weights = c(sapply(1:k,
            function(i) c(0,vals[-k]/n,1)[i+1]-c(0,vals[-k]/n,1)[i]))
  }
  ret_object$method = "exact"
  class(ret_object) = c("hist")
  ret_object  
}

# Histogram class constructor -------------------------------------------------

histogram = function(data,k,method="exact",seed=1882,bag_factor=1/2,...){
  data = sort(data)
  
  if (method == "exact") {
    return(exact_histogram(data,k,...))
  }
  
  if (method == "smoothed") {
    n = length(data)
    new_data = sort(rgcde(50*k*n,data))
    result = greedy_histogram(new_data,k,...)
    result$method = "smoothed"
    return(result)
  }
  
  if (method == "subbag") {
    n = length(data)
    results = sapply(1:1000,function(x) 
      greedy_histogram(sort(sample(data,n*bag_factor)),init=sort(sample(1:(n*bag_factor),k-1)),k,...))
    weights = rowMeans(sapply(1:1000,function(i) results[ ,i]$weights))
    splits = rowMeans(sapply(1:1000,function(i) results[ ,i]$splits))
    results = results[,1]
    results$method = "subbag"
    results$weights = weights
    results$splits = splits
    class(results) = c("hist")
    return(results)
  }
  
  else {
    return(greedy_histogram(data,k,...))
  }
  
}

# Generics --------------------------------------------------------------------

plot.hist = function(hist_obj,main=NULL,sub=NULL,...){
  splits <- c(0,hist_obj$splits,1)
  ys <- c(0,hist_obj$weights*sapply(1:hist_obj$k,function(i) 1/(splits[i+1]-splits[i])))
  if (is.null(main)) main = "Histogram"
  if (is.null(sub)) sub = paste0("Irregular ",(hist_obj$specification)[1],"-histogram with ",(hist_obj$specification)[2]," weights. Method: ",hist_obj$method,"; k = ",hist_obj$k)
  plot(splits,ys,type="S",xlab="x",ylab="Density",main=main,sub=sub,...)
  lines(splits,ys,type="h",...)
}

lines.hist = function(hist_obj,...){
  splits <- c(0,hist_obj$splits,1)
  ys <- c(0,hist_obj$weights*sapply(1:hist_obj$k,function(i) 1/(splits[i+1]-splits[i])))
  lines(splits,ys,type="S",...)
  lines(splits,ys,type="h",...)
}

print.hist = function(hist_obj){
  cat("***-----------------------------*** \n")
  cat("*** Irregular histogram object. *** \n")
  cat("***-----------------------------***")
  cat("\n Type:",(hist_obj$specification)[1])
  cat("\n Weights:",(hist_obj$specification)[2])
  cat("\n Method:",hist_obj$method)
  cat("\n Splits:",hist_obj$splits)
  cat("\n Weights:",hist_obj$weights)
}
# Error measurements ----------------------------------------------------------

compare_mse = function(n, k, rand, method1 = "greedy", 
                                   method2 = "exact", N = 1000,...){
  
  compare_mse_helper = function(data){
    ests1 = histogram(data, k, method = method1, ...)
    ests2 = histogram(data, k, method = method2, ...)
    
    c(splits=sum((ests1$splits - ests2$splits)^2),
      weights=sum((ests1$weights - ests2$weights)^2))
  }
  
  replicate(N,compare_mse_helper(rand(n)))
}

estimate_mse = function(n, k, rand, method = "greedy", N = 1000,...){
  # First of, we will have to decide on a true value. Greedy works well. 
  true_ests = histogram(rand(100*N*k),k,method="greedy")
  
  compare_mse_helper = function(data){
    ests = histogram(data, k, method = method, ...)
    
    c(splits=sum((ests$splits - true_ests$splits)^2),
      weights=sum((ests$weights - true_ests$weights)^2))
  }
  
  replicate(N,compare_mse_helper(rand(n)))
}
# Average subbagged histogram -------------------------------------------------

subhist = function(data,k,N=100){
  
  n = length(data)
  
  quotients = function(splits){
    splits = c(0,splits)
    c(1/k*sapply(1:k,function(i) 1/(splits[i+1]-splits[i])))
  }
  
  find_index = function(splits,x){
    temp = FALSE
    i = 0
    while (!temp) {
      temp = (x < splits[i+1])
      i = i + 1
    }
    i
  }
  
  matr = unlist(replicate(N,histogram(sort(sample(data,n*0.5)),k,method="greedy",
                        init=sort(sample(1:(n*0.5),k-1)))$splits))
  dim(matr) = c(k-1,N)
  matr = rbind(matr,rep(1,times=N))
  
  quot_matr = apply(matr,2,quotients)
  
  retfun = function(x){
    indices = apply(matr,2,find_index,x=x)
    quots = sapply(1:N,function(i) quot_matr[indices[i],i])
    mean(quots)
  }
  
}

# Testing ---------------------------------------------------------------------
# res1 = compare_mse(200,5,rand)
# res2 = estimate_mse(200,7,rand,method="subbag",bag_factor=0.5)
# res3 = estimate_mse(200,7,rand,method="greedy")
# 
# mean(res2[1,])
# mean(res3[1,])
# 
# var(res2[1,])
# var(res3[1,])
# 
# k = 6
# n = 10000
# data = rbeta(n,2,7)
# init = sort(sample(1:n,k-1))
# plot(histogram(data,k=k,method="subbag",weights="KL"))
# lines(histogram(data,k=k,method="smoothed",weights="KL"),lty=2,col="blue")
# lines(histogram(rbeta(10000,2,7),k=k,method="greedy",weights="KL"),lty=2,col="red")
# rug(data)
# 
# 
# n = 100
# k = 5
# data = rbeta(n,2,7)
# kern = subhist(rbeta(100,2,7),k=k,N=5000)
# 
# plot(xx,sapply(xx,kern),type="h")
