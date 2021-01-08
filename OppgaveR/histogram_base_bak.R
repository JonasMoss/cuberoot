library("Rcpp")
source("copula_base.R")
sourceCpp("greedy.cpp")
sourceCpp("exact.cpp")

# Support functions and variables ---------------------------------------------

`%||%` <- function(a, b) if (! is.null(a)) a else b

# Algorithms ------------------------------------------------------------------
coordHistogram = function(data,k,type="KL",weights="equal",lim=0.0001,
                            modulator=10,init=NULL){
  ret_object = list()
  ret_object$specification = c(type=type,weights=weights)
  ret_object$k = k
  n = length(data)
  type = (type == "L2")
  weights = (weights!="equal")
  init = (init %||% quantile(1:n,(1:(k-1))/k))
  vals = cpp_greedy_alg(weights,type,c(0,data,1),n,k,
           modulator=modulator,init=init,lim=lim)
  as = data[vals[-k]]
  ret_object$splits = as
  if (!weights) {
    ret_object$weights = rep(1/k,k)
  } else {
    ret_object$weights = c(sapply(1:k,
                                  function(i) c(0,vals[-k]/n,1)[i+1]-c(0,vals[-k]/n,1)[i]))
    ret_object$weights[1] = ret_object$weights[1] - 1/n
    ret_object$weights[k] = ret_object$weights[k] + 1/n
  }
  ret_object$iterations = vals[k]
  ret_object$method = "greedy"
  ret_object$lim = lim
  class(ret_object) = c("hist")
  ret_object  
}

dpHistogram = function(data,k,type="KL",weights="equal",lim=0.0001){
  ret_object = list()
  ret_object$specification = c(type=type,weights=weights)
  ret_object$k = k
  n = length(data)
  type = (type=="L2")
  weights = (weights!="equal")
  vals = cpp_exact_alg(weights,type,c(0,data,1),n,k,lim=lim)
  vals = vals[k-1,]
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

# "loglikelihood"-function ----------------------------------------------------

Pnm = function(obj,useData=data){
  q     = obj$splits
  w     = obj$weights
  k     = obj$k
  n     = length(useData)
  Pn    = c(0,sapply(q,function(j) sum(useData<=j)/n),1)
  qAug  = c(0,q,1)
  if (obj$specification[1] == "KL") {
    dis   = sapply(1:k,function(i) - log(qAug[i+1]-qAug[i]) + log(w[i]))
    probs = sapply(1:k,function(i) Pn[i+1]-Pn[i])
    probs[1] = probs[1] - 1/n
    probs[k] = probs[k] + 1/n
    return(sum(probs*dis))
  } else {
    dis   = sapply(1:k,function(i) 1/(qAug[i+1]-qAug[i]))
    probs = sapply(1:k,function(i) Pn[i+1]-Pn[i])
    probs[1] = probs[1] - 1/n
    probs[k] = probs[k] + 1/n
    return(sum(w*(2*probs-w)*dis))
  }
}

# Histogram class constructor -------------------------------------------------

histogram = function(data,k,method="exact",seed=1882,lim=0.0001, ...){
  data = sort(data)
  
  if (method == "exact") {
    obj        = dpHistogram(data,k,lim=lim,...)
    obj$loglik = Pnm(obj,data)
  } else if (method == "smoothed") {
    n = length(data)
    new_data = sort(rgcde(50*k*n,data))
    obj = coordHistogram(new_data,k,lim=lim,...)
    obj$method = "smoothed"
    obj$loglik = Pnm(obj,new_data)
    return(result)
  } else {
    obj = coordHistogram(data,k,lim=lim,...)
    obj$method = "coordinateSearch"
    obj$loglik = Pnm(obj,data)
  }

  return(obj)
  
}

# Shortcuts -------------------------------------------------------------------

L2histogram = function(data,k,lim=0.0001) {
  histogram(data,k,method="exact",lim=lim,type="L2",weights="L2")
}

KLhistogram = function(data,k,lim=0.0001) {
  histogram(data,k,method="exact",lim=lim,type="KL",weights="KL")
}

# Bin selection procedures ----------------------------------------------------

CIC = function(data,k,b=.5,K=50,type="L2",weights="L2",lim=0.001){
  n = length(data)
  obj = histogram(data,k=k,type=type,weights=type,lim=lim,method="exact")
  disc = obj$loglik
  bias = biasSubs(data, b=b, k=k, K=K, type=type, weights=type, lim=lim)
  n*disc - n*bias
}

CICselect = function(data,b=.5,K=50,type="L2",weights="L2",lim=0.0001,ks = seq(3,2*ceiling(nobs^(1/3)))) {
  nobs = length(data)
  CICs = sapply(ks,function(k) CIC(data,k,b,K,type,weights,lim=lim))
  which.max(CICs) + 2
}

penB = function(data,k){
  n = length(data)
  obj = histogram(data,k=k,type=type,weights=type,lim=lim,method="exact")
  lik = n*obj$loglik
  pen = log(choose(n-1,k-1)) + (k-1) + (log(k))^(2.5)
  lik - pen
}

penBselect = function(data,ks = seq(3,2*ceiling(nobs^(1/3)))){
  nobs = length(data)
  penBs = sapply(ks,function(k) penB(data,k))
  which.max(penBs) + 2
}

penR = function(data,k,lim=.0001){
  n = length(data)
  obj = histogram(data,k=k,type=type,weights=type,lim=lim,method="exact")
  lik = n*obj$loglik
  pen = log(choose(n-1,k-1)) + 0.5*sum(obj$weights/diff(c(0,obj$splits,1))) + (log(k))^(2.5)
  lik - pen
}

penRselect = function(data,ks = seq(3,2*ceiling(nobs^(1/3)))){
  nobs = length(data)
  penRs = sapply(ks,function(k) penR(data,k))
  which.max(penRs) + 2
}

AICselect = function(data,ks = seq(3,2*ceiling(nobs^(1/3)))){
  nobs = length(data)
  AICs = sapply(ks,function(k) -AIC(histogram(data,k=k,type=type,weights=type,lim=lim,method="exact")))
  which.max(AICs) + 2  
}

# Generics --------------------------------------------------------------------

plot.hist = function(hist_obj,main=NULL,sub=NULL,xlab=NULL,ylab=NULL,rescale = 1,grid=TRUE,...){
  splits <- c(0,hist_obj$splits,1)
  xlab = ifelse(is.null(xlab),"x",xlab)
  ylab = ifelse(is.null(ylab),"Density",ylab)
  ys <- c(0,hist_obj$weights*sapply(1:hist_obj$k,function(i) 1/(splits[i+1]-splits[i])))
  if (is.null(main)) main = "Histogram"
  if (is.null(sub)) sub = paste0("Irregular ",(hist_obj$specification)[1],"-histogram with ",(hist_obj$specification)[2]," weights. Method: ",hist_obj$method,"; k = ",hist_obj$k)
  plot(rescale*splits,ys/rescale,type="S",bty="l",
       xlab=xlab,ylab=ylab,main=main,sub=sub,...)
  if(grid) {
    grid()
    lines(rescale*splits,ys/rescale,type="S",bty="l",
         xlab=xlab,ylab=ylab,main=main,sub=sub,...)
  }
  lines(rescale*splits,ys/rescale,type="h",...)
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

logLik.hist = function(hist_obj) obj$loglik

AIC.hist = function(hist_obj) {
  -2*n*hist_obj$loglik + 2*(hist_obj$k-1)
}

# Error measurements ----------------------------------------------------------

imse = function(obj,dist){
  k = obj$k
  q = obj$splits
  w = obj$weights
  qAug = c(0,q,1)
  error = 0
  for (i in 2:(k+1)){
    error = error + integrate(function(x) (dist(x) - w[i-1]/(qAug[i]-qAug[i-1]))^2,
                              lower = qAug[i-1],upper=qAug[i])$value
  }
  
  error
}

hellinger = function(obj,dist){
  k = obj$k
  q = obj$splits
  w = obj$weights
  qAug = c(0,q,1)
  error = 0
  for (i in 2:(k+1)){
    error = error + integrate(function(x) (sqrt(dist(x)) - sqrt(w[i-1]/(qAug[i]-qAug[i-1])))^2,
                              lower = qAug[i-1],upper=qAug[i])$value
  }
  
  sqrt(0.5*error)
}
