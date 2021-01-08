N = 1000
delta = 1
ns = (1:10)*100

points = sapply(ns,function(n) mean(replicate(N,1/mean(rgamma(n,1+delta/sqrt(n),2)))))
pointsv = sapply(ns,function(n) var(replicate(N,1/mean(rgamma(n,1+delta/sqrt(n),2)))))
(points-2)*sqrt(ns)
pointsv*ns


data = rgamma(10000,1,2)
hess = (hessian(function(par) -mean(dgamma(data,par[2],par[1],log=TRUE)),c(2,1)))
bias = 1/hess[1,1]*hess[1,2]*delta
var = 1/hess[1,1]
mse = bias^2 + var

subbias = function(data){
  n = length(data)
  theta = (optim(c(1,1),function(par) -mean(dgamma(data,par[1],par[2],log=TRUE)))$par)[2]
  mean(replicate(N,1/mean(sample(data,n/2))-theta))
}

bootbias = function(data){
  n = length(data)
  theta = (optim(c(1,1),function(par) -mean(dgamma(data,par[1],par[2],log=TRUE)))$par)[2]
  mean(replicate(N,1/mean(sample(data,n,replace=TRUE))-theta))
}

parabias = function(data){
  n = length(data)
  theta = (optim(c(1,1),function(par) -mean(dgamma(data,par[1],par[2],log=TRUE)))$par)
  mean(replicate(N,1/mean(rgamma(n,theta[1],theta[2]))-theta[2]))
}


pointssub = sapply(ns,function(n) mean(replicate(N,subbias(rgamma(n,1+delta/sqrt(n),2)))))
pointsboot = sapply(ns,function(n) mean(replicate(N,bootbias(rgamma(n,1+delta/sqrt(n),2)))))

n = 400
mean(replicate(N,subbias(rgamma(n,1+delta/sqrt(n),2))))
mean(replicate(N,bootbias(rgamma(n,1+delta/sqrt(n),2))))
mean(replicate(N,parabias(rgamma(n,1+delta/sqrt(n),2))))