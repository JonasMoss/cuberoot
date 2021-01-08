p = function(x) pbeta(x,2,7)

plotti = function(xs,a=.3) {
  -log(a)*(xs>=0 & xs < a) - log(1-a)*(xs>=a & xs <= 1)
}

plow = function(xs,a=.3,eps=0.001) {
  -log(a+eps)*(xs>=0 & xs < (a-eps)) - log(1-a+eps)*(xs>=(a-eps) & xs <= 1)
}

pupper = function(xs,a=.3,eps=0.001) {
  -log(a-eps)*(xs>=0 & xs < (a+eps)) - log(1-a-eps)*(xs>=(a+eps) & xs <= 1)
}


plow = function(xs,a=.3,eps=0.001) {
  -log(a+eps)*(xs>=0 & xs < (a+eps)) - log(1-a+eps)*(xs>=(a+eps) & xs <= 1)
}

pupper = function(xs,a=.3,eps=0.001) {
  -log(a-eps)*(xs>=0 & xs < (a-eps)) - log(1-a-eps)*(xs>=(a-eps) & xs <= 1)
}

xs = seq(0.001,0.999,by=0.001)

a = 0.5
plot(xs,plotti(xs,a=a),type="s",ylim=c(0,2))
lines(xs,plotti(xs,a=a+0.01),type="s",col="green")
lines(xs,plow(xs,a=a,eps=0.05),type="s",col="red")
lines(xs,pupper(xs,a=a,eps=0.05),type="s",col="blue")

p = function(x) pbeta(x,2,7)

Pnm = function(x,q){
  k     = length(q)+1
  qAug  = c(0,q,1)
  dis   = sapply(1:k,function(i) log(qAug[i+1]-qAug[i])-log(k))
  probs = sapply(1:k,function(i) x<= qAug[i+1] & x > qAug[i])
  -sum(probs*dis)
}

plow = function(x,q,eps=0.05) {
  k     = length(q)+1
  qAug  = c(0,q,1)
  dis   = sapply(1:k,function(i) log(qAug[i+1]-qAug[i]+eps)-log(k))
  probs = sapply(1:k,function(i) x <= (qAug[i+1]-eps) & x > (qAug[i] - eps))
  -sum(probs*dis)
}



xs = seq(0.001,0.999,by=0.001)
eps = 0.01
q = sort(sample(1:99,6)/100)

plot(xs,sapply(xs,function(x) Pnm(x,q=q)),type="s")
lines(xs,sapply(xs,function(x) Pnm(x,q=c(0.2-eps,0.7-eps))),type="s",col="red")
lines(xs,sapply(xs,function(x) Pnm(x,q=c(0.2+eps,0.7+eps))),type="s",col="blue")
lines(xs,sapply(xs,function(x) Pnm(x,q=c(0.2+eps,0.7-eps))),type="s",col="green")
lines(xs,sapply(xs,function(x) Pnm(x,q=c(0.2-eps,0.7+eps))),type="s",col="purple")