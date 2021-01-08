library(locfit)

lkde = function(data){
  trans = qnorm(data)
  mod = lp(trans,deg=2)
  mod = locfit(~mod)
  function(ts){
   transts = qnorm(ts)
   transts = transts[transts!=Inf & transts!=-Inf]
   vals = predict(mod,newdata=transts)
   cbind(pnorm(transts),vals/dnorm(transts))
  }
}

data = rcool(100,2,1)

h = hgcde(data)
kern_lk = lkde(data)
kern_gc = gcde(data,h=1.5*h)
#kern_mgc = mgcde(data)
#kern_dsgc = dsgcde(data)
plot(ts,dcool(ts,2,1),lwd=2,type="l",ylim=c(0,3.5))
lines(kern_lk(ts),type="l")
lines(ts,sapply(ts,kern_gc),col="blue")
#lines(ts,sapply(ts,kern_mgc),col="green")
#lines(ts,sapply(ts,kern_dsgc),col="purple")
rug(data)





data = rcool(100,5,1)
k = 10
P = function(x) pcool(x,5,1)
Q = function(x) qcool(x,5,1)
q = sde_true(P,Q,k=k)$par

q_hat = sde_iter(data,k=k)
q_hat_smooth = sde_iter(rgcde(10000,data),k=k)
q_hat_sub = sde_sub(data,k=k)
kernel = gcde(data)

par(mfrow=c(2,2))

plot(ts,dcool(ts,5,1),type="l")
sde_plotter(q,lines=TRUE)
sde_plotter(q_hat,col="red")
sde_plotter(q_hat_sub,col="purple")
rug(data)
plot(ts,sapply(ts,kernel),lty=2,type="l")
sde_plotter(q_hat_smooth,col="blue",lines=TRUE)
rug(data)

sum((q-q_hat)^2)
sum((q-q_hat2)^2)
























data = rbeta(100,2,7)
k = 10
P = function(x) pbeta(x,2,7)
Q = function(x) qbeta(x,2,7)
q = sde_true(P,Q,k=k)$par

q_hat = sde_iter(data,k=k)
q_hat2 = sde_iter(rgcde(50000,data),k=k)
kernel = gcde(data)


par(mfrow=c(2,2))

plot(ts,dbeta(ts,2,7),type="l")
sde_plotter(q,col="purple")
sde_plotter(q_hat,col="red")
rug(data)
plot(ts,sapply(ts,kernel),lty=2,type="l")
sde_plotter(q_hat2,col="blue",lines=TRUE)
rug(data)

sum((q-q_hat)^2)
sum((q-q_hat2)^2)