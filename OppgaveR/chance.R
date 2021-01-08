eps = 0.01
a = 0.3
b = 0.4
c = 0.7
N = 10000

rows = rmultinom(N,1,rep(1,4)/4)
vals = (runif(N,0,a-eps))
rowMeans(rmultinom(10000,1,rep(1,4)/4))
hist(rbeta(10000,1,2))

maxer = function(x,q) {
  val = 1/k*1/q*(x>=0 & x < q) + 1/k*1/(1-q)*(x>=q & x < 1) 
  val
}

N = 5000
data = sort(rbeta(N,2,7))
summy = function(x,q){
  if (x < q) {
    return(1/k*1/q*x)
  } else {
    return(1/k + 1/k*1/(1-q)*(x-q))
  }
}


q = quantile(data,0.5)
qs = seq(0.01,0.99,by=0.01)
ecd  = ecdf(data)
res = sapply(qs,function(q) max(abs(ecd(data)-sapply(data,summy,q=q))))

plot(ecdf(data))
lines(data,sapply(data,summy,q=q),col="red")

plot(ecdf(data))
lines(data,sapply(data,summy,q=qs[which.min(res)]),col="red")
lines(data,sapply(data,summy,q=q),col="blue")

plot(qs,res)
abline(v=q,lty=2)

q = qs[which.min(res)]
plot(c(0,q,1),c(0,0.5/q,0.5/(1-q)),type="S",bty="l",lty=2)
lines(c(0,q,1),c(0,0.5/q,0.5/(1-q)),type="h",lty=2)
lines(histogram(data,k=2))
lines(ss,dbeta(ss,2,7))

N = 100
data = sort(rbeta(N,2,7))
ecd = ecdf(data)
summy = function(x,q,w){
  if (x < q) {
    return(w*1/q*x)
  } else {
    return(w + (1-w)*1/(1-q)*(x-q))
  }
}


aa = c()
bb = seq(0.38,0.42,by=0.001)
ww = c()

for (b in bb) {
  q = b
  ws = seq(0.8,0.9,by=0.001)
  ecd  = function(x) pbeta(x,2,7)
  res = sapply(ws,function(w) max(abs(ecd(ss)-sapply(ss,summy,q=q,w=w))))
  ww = c(ww,ws[which.min(res)])
  aa = c(aa,min(res))
}

plot(bb,aa)
abline(v=bb[which.min(aa)])

plot(ss,pbeta(ss,2,7),type="l")
lines(data,sapply(data,summy,q=q,ws[which.min(res)]),col="red")

plot(bb,ww,ylim=c(0,1),xlim=c(0.35,.45))
lines(bb,ecd(bb),col="blue")
points(bb,aa,col="red")
abline(v=bb[which.min(aa)],lty=2)
abline(h=0)

plot(bb,ww-ecd(bb))
abline(v=bb[which.min(aa)],lty=2)
abline(h=0)

n = 8
2^n*factorial(n)*factorial(n)/factorial(2*n)
2^(n-1)*factorial(n)*factorial(n)/factorial(2*n)*choose(n,2)
(factorial(n))^3/factorial(2*n)/factorial(n-2)*2^(n-2)

chance = function(k,n){
  (factorial(n))^3/factorial(2*n)/factorial(n-k)*2^(n-k)
}

chance2 = function(k,n){
  1/choose(2*n,n)*2^(n-k)*factorial(k)*choose(n,k)
}

chance3 = function(k,n){
  1/choose(2*n,n)*2^(n-k)*choose(n,k/2)*choose(n-k/2,k/2)
}

chance4 = function(k,n){
  1/choose(2*n,n)*2^(n-k)*choose(n,k)*choose(k,k/2)
}


chance4 = function(k,n){
  2^(-n)*sqrt(2)*sqrt(n/k)*choose(n,k)
}

chance3(0,8)
chance3(2,8)
chance3(4,8)
chance3(6,8)
chance3(8,8)

ns = (0:4)*2
plot(ns,chance4(ns,8))

n = 100
ns = (0:(n/2))*2
eps = 0.02
vals = chance3(ns,n)
plot(ns,vals,bty="n",xlab="Fine kamper",ylab="Sannsynlighet",
     type="h",lty=1,ylim=c(0,max(vals)+eps),col="blue")
text(ns,vals+eps,labels=round(vals,3))
max(chance3(ns,n))

plot(ns,choose(ns,ns/2)*2^(n-ns))
lines(ns,2^n*sqrt(2)/sqrt(pi*ns))



