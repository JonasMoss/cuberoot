library(boot)

generate = function(n,a,b){
  data = rgamma(n,a,b)
  exp_hat = 1/mean(data)
  gamma_hat = optim(c(1,1),function(par) -mean(dgamma(data,par[1],par[2],log=TRUE)))$par
  c(exp=exp_hat,gamma=gamma_hat[2])
}

deltas = 1:10
ns = 1:15*10

res_fun = function(n,delta,N=5000){
  a = 1 + delta/sqrt(n)
  b = 2
  values = replicate(N,generate(n,a,b))
  bias = rowMeans(values)-b
  variance = apply(values,1,var)
  mse = variance + bias^2
  c(bias=bias,var=variance,mse=mse)
}

res = lapply(deltas,function(delta) {
  vec = sapply(ns,res_fun,delta=delta)
  colnames(vec)=paste0("n",ns)
  vec })
names(res) = paste0("delta",deltas)

#################
### Bootstrap ###
#################

gamma_opt = function(data){
  optim(c(1,1),function(par) -mean(dgamma(data,par[1],par[2],log=TRUE)))$par
}


calc_gamma <- function(data, indices) {
  d <- data[indices] 
  gamma_hat = optim(c(1,1),function(par) -mean(dgamma(d,par[1],par[2],log=TRUE)))$par
  gamma_hat[2]
}

calc_exp <- function(data, indices) {
  d <- data[indices] 
  1/mean(d)
}
delta = 1
n = 100
data = rgamma(n,1+delta/sqrt(n),2)
get_gamma = boot(data=data, statistic=calc_gamma,R=1000)
get_exp = boot(data=data, statistic=calc_exp,R=1000)

biabia = function(n,a,b){
  data = rgamma(n,a,b)
  get_gamma = boot(data=data, statistic=calc_gamma,R=1000)
  (mean(get_gamma$t) - get_gamma$t0)
}

biabia2 = function(n,a,b){
  data = rgamma(n,a,b)
  theta =  optim(c(1,1),function(par) -mean(dgamma(data,par[1],par[2],log=TRUE)))$par
  get_gamma = boot(data=data, statistic=calc_exp,R=1000)
  c(bias=mean(get_gamma$t) - theta[2],
    var=var(get_gamma$t),
    mse=(mean(get_gamma$t) - theta[2])^2)
}

biabia3 = function(n,a,b){
  data = rgamma(n,a,b)
  theta =  optim(c(1,1),function(par) -mean(dgamma(data,par[1],par[2],log=TRUE)))$par
  get_gamma = replicate(1000,1/mean(rgamma(n,theta[1],theta[2])))
  c(bias=mean(get_gamma) - theta[2],
    var=var(get_gamma),
    mse=(mean(get_gamma) - theta[2])^2)
}


subbia = function(n,a,b,B=sqrt(n),K=1000){
  data = rgamma(n,a,b)
  ests = replicate(K,1/mean(sample(data,B)))
  theta =  optim(c(1,1),function(par) -mean(dgamma(data,par[1],par[2],log=TRUE)))$par
  c(bias=mean(ests) - theta[2],
    var=var(ests),
    mse=(mean(ests) - theta[2])^2)
}

checkit = subbia(n,1+delta/sqrt(n),b)
checkit2 = biabia2(n,1+delta/sqrt(n),b)

n = 1000
ref = res_fun(n,delta)
#bia_bia = replicate(100,biabia(n,1+delta/sqrt(n),2))
bia_bia2 = replicate(1000,biabia2(n,1+delta/sqrt(n),2))
bia_bia3 = replicate(1000,biabia3(n,1+delta/sqrt(n),2))
#bia_bias = replicate(1000,subbia(n,1+delta/sqrt(n),2))
rowMeans(bia_bia2)
rowMeans(bia_bia3)

#rowMeans(bia_bias)

### Find asympt bias
data = rgamma(10000,1,2)
hess = hessian(function(par) -mean(dgamma(data,par[1],par[2],log=TRUE)),c(1,2))
(1/hess[1,1]*hess[1,2]*delta)^2+1/hess[1,1]
