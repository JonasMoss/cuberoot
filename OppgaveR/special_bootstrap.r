library(ks)

dcool = function(x,a,b,log=FALSE){
  if (log) a*log(x)+log(a+1)-(a+1)*log(b)
  else x^a*(a+1)/(b^(a+1))
}

pcool = function(x,a,b){
  (x/b)^(a+1)
}

qcool = function(x,a,b){
  b*x^(1/(a+1))
}

rcool = function(n,a,b){
  unifs = runif(n)
  qcool(unifs,a,b)
}


a = 5
b = 6
data = rcool(1000,a,b)
b_hat = max(data)
lims=c(0,b_hat)
h = hgcde(data,lims=lims)
nkern = gcde(data,h=h,lims=lims)
kkern = ckde(data/b_hat)
ks = seq(0,b_hat,by=0.01)
#plot(ks,sapply(ks,nkern),type="l")
hist(data,freq=FALSE,ylim=c(0,1.3),xlim=c(0,6))
#plot(kde(data),col="blue",ylim=c(0,1.3))
lines(ks,sapply(ks,nkern),type="l",col="blue")
lines(density(data),col="red")
lines(ks,sapply(ks/b_hat,kkern)/b_hat,col="green")
lines(ks,dcool(ks,a,b))


a = 5
b = 6
n = 1000
xs = rcool(n,a,b)
b_hat = max(xs)
lims=c(0,b_hat+0.0001)
h = hgcde(xs,lims=lims)
#ys = sort(xs)[-1000]
ys = xs

boots_non = replicate(N,max(sample(xs,n,replace=TRUE)))
boots_para = replicate(N,max(rcool(n,a,b_hat)))
boots_smooth = replicate(N,max(rgcde(n,ys,h,lims=lims)))

quantile(-n*(boots_smooth - b_hat),c(0.025,0.975))
quantile(-n*(boots_para - b_hat),c(0.025,0.975))
quantile(-n*(boots_non - b_hat),c(0.025,0.975))
qexp(c(0.025,0.975))

var(boots_non)
var(boots_para)
var(boots_smooth)
mean(boots_non-theta_hat)
mean(boots_para-theta_hat)
mean(boots_smooth-theta_hat)

plot(density(n*(b_hat+0.0001-boots_smooth)))
plot(density(n*(b_hat-boots_para)))
plot(density(n*(b_hat-boots_non)))

nkern = gcde(ys,h=h,lims=lims)
plot(ks,sapply(ks,nkern),type="l",col="blue")
lines(ks,dcool(ks,a,b))


