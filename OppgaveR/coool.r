library(microbenchmark)
library(Rcpp)
sourceCpp("cool.cpp")

data = c(0, 0.01493237, 0.08022630, 0.08027604, 0.08949319, 
         0.13476742, 0.14132722, 0.16919392, 0.20063495,
         0.25673700, 0.27708105, 0.34524149, 0.36377234,
         0.39178968, 0.42651664, 0.45333694, 0.46897896, 1)

k = 4
N = 10000
data = sort(rbeta(N,2,7))
microbenchmark(alg(T,F,c(0,data,1),N,k))


#a = alg(F,F,c(0,data,1),N,k)
b = alg(T,F,c(0,data,1),N,k)
c = alg(T,T,c(0,data,1),N,k)

ranks = b[k-1,]
ranks = ranks[ranks!=0]
plotter = function(ranks,N){
  qs = c(0,data[ranks],1)
  ranks = c(0,ranks,N+1)
  k = length(ranks)-1
  vals = c(0,sapply(1:k,function(i) ranks[i+1]-ranks[i])/N)
  plot(qs,vals*k,type="h")
  lines(qs,vals*k,type="S")
  lines(ts,dbeta(ts,2,7))
}

plotter(ranks,N)

q_kl = data[a[k-1,]]
q_l2 = data[c[k-1,]]
sde_plotter(q_l2,ylim=c(0,3.5))
sde_plotter(q_kl,lines=TRUE,col="blue",lty=2)
lines(ts,dbeta(ts,2,7))