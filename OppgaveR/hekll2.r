data = rbeta(100000,2,7)
k = 14
q = sde(data,k,method="iter")

plot(ts,dbeta(ts,2,7),type="l",lty=2)
lines(q)

kl = function(t){
  q_new = c(0,q,1)
  ind = max((1:length(q_new))*(q_new <= t))
  dbeta(t,2,7)*log(dbeta(t,2,7)/(k*(q_new[ind+1]-q_new[ind])))
}

l2 = function(t){
  q_new = c(0,q,1)
  ind = max((1:length(q_new))*(q_new <= t))
  ((dbeta(t,2,7)-1/(k*(q_new[ind+1]-q_new[ind])))^2)
}

he = function(t){
  q_new = c(0,q,1)
  ind = max((1:length(q_new))*(q_new <= t))
  ((dbeta(t,2,7)^(1/2)-(1/(k*(q_new[ind+1]-q_new[ind])))^(1/2))^2)
}


plot(ts,dbeta(ts,2,7),type="l",lty=2,ylim=c(0,7))
lines(q)
lines(ts,sapply(ts,kl),type="l")
lines(ts,3*sapply(ts,l2),type="l",col="blue",lty=2)
lines(ts,3*sapply(ts,he),type="l",col="red")