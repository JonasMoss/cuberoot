library(microbenchmark)
library(Rcpp)
sourceCpp("greedy.cpp")
sourceCpp("exact.cpp")

greedy = function(data,k,type="KL",weights="equal",modulator=10,lim=NULL,init=NULL){
  ret_object = list()
  ret_object$specification = c(type=type,weights=weights)
  ret_object$k = k
  data = sort(data)
  n = length(data)
  type = (type=="L2")
  weights = (weights!="equal")
  if (is.null(init)) init=floor(seq(1,N,length.out=k-1))
  if (is.null(lim)) lim = 1/sqrt(n)
  vals = cpp_greedy_alg(weights,type,c(0,data,1),N,k,
           modulator=modulator,init=init,lim=lim)
  as = data[vals[-k]]
  ret_object$splits = as
  if (!weights) ret_object$weights = rep(1/k,k)
  else ret_object$weights = c(sapply(1:k,function(i) c(0,vals[-k],n+1)[i+1]-c(0,vals[-k],n+1)[i])/N)
  ret_object$iterations = vals[k]
  ret_object$method = "greedy"
  class(ret_object) = c("histogram")
  ret_object  
}

exact = function(data,k,type="KL",weights="equal",lim=NULL){
  ret_object = list()
  ret_ = c(type=type,weights=weights)
  ret_object$k = k
  n = length(data)
  type = (type=="L2")
  weights = (weights!="equal")
  if (is.null(lim)) lim = 1/sqrt(n)
  vals = cpp_exact_alg(weights,type,c(0,data,1),N,k,lim=lim)
  vals = vals[k-1,]
  as = data[vals]
  ret_object$splits = as
  if (!weights) ret_object$weights = rep(1/k,k)
  else ret_object$weights = c(sapply(1:k,function(i) c(0,vals[-k],n+1)[i+1]-c(0,vals[-k],n+1)[i])/N)
  ret_object$method = "exact"
  class(ret_object) = c("histogram")
  ret_object  
}

histogram = function(data,k,method="exact",...){
  if (method == "exact") exact(data,k,...)
  else greedy(data,k,...)
}

plot.histogram = function(hist_obj,main=NULL,sub=NULL,...){
  splits <- c(0,hist_obj$splits,1)
  ys <- c(0,hist_obj$weights*sapply(1:hist_obj$k,function(i) 1/(splits[i+1]-splits[i])))
  if (is.null(main)) main = "Histogram"
  if (is.null(sub)) sub = paste0("Irregular ",(hist_obj$specification)[1],"-histogram with ",(hist_obj$specification)[2]," weights. Method: ",hist_obj$method,"; k = ",hist_obj$k)
  plot(splits,ys,type="S",xlab="x",ylab="Density",main=main,sub=sub,...)
  lines(splits,ys,type="h",...)
}

lines.histogram = function(hist_obj,...){
  splits <- c(0,hist_obj$splits,1)
  ys <- c(0,hist_obj$weights*sapply(1:hist_obj$k,function(i) 1/(splits[i+1]-splits[i])))
  lines(splits,ys,type="S",...)
  lines(splits,ys,type="h",...)
}


microbenchmark(exact(data,k),sdeiter(data,k))

par(mfrow=c(2,2))
hist_obj = exact(data,k,weights="L2",type="L2")
plot(hist_obj)
rug(data)
hist_obj = exact(data,k,weights="equal",type="L2")
plot(hist_obj)
rug(data)
hist_obj = greedy(data,k,weights="KL",type="KL")
plot(hist_obj)
rug(data)
hist_obj = exact(data,k,weights="equal",type="KL")
plot(hist_obj)
rug(data)


N = 10000
k = 10
data = sort(rbeta(N,2,7))

a = algo(F,T,c(0,data,1),N,k,modulator=10,init=floor(seq(1,N,length.out=k-1)),lim=1/sqrt(N))
b = alg(F,T,c(0,data,1),N,k,lim=1/sqrt(N))
q_a = data[a[-k]]
q_b = data[b[k-1,]]

sde_plotter(q_a,ylim=c(0,4))
sde_plotter(q_b,lines=TRUE,lty=2,col="blue")
rug(data)
a

microbenchmark(algo(F,T,c(0,data,1),N,k,modulator=10,init=floor(seq(1,N,length.out=k-1)),lim=1/N^(1/2)))
