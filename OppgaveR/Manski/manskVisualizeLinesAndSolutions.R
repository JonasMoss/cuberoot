
seed = 20
set.seed(seed)
nobs = 15
beta1 = 1
beta2 = 1
xx1  = rnorm(nobs)
xx2  = rnorm(nobs)
yy   = (1+beta1*xx1+beta2*xx2 + rnorm(nobs))>=0
obj  = manski2d(yy,xx1,xx2)
#plot(obj,full=TRUE)
#linePlotter(yy,xx1,xx2)

configurationPlotter(yy,xx1,xx2,lim=10)
#linePlotter(yy,xx1,xx2)
# 
# for ( i in 1:nobs) {
#   for (j in (1:nobs)[-i]){
#     yya = yy
#     yya[i] = !yya[i]
#     yya[j] = !yya[j]
#     configurationPlotter(yya,xx1,xx2)
#   }
# }

a = -5
b = 5
xs = seq(a,b,by=0.01)
ys = xs
manskfun = function(beta){
  beta1 = beta[1]
  beta2 = beta[2]
  sum(yy*(1+beta1*xx1+xx2*beta2>=0)) + sum((1-yy)*(1+beta1*xx1+xx2*beta2<0))
}
zs = apply(expand.grid(xs,xs),1,manskfun)

library(rgl)
persp3d(xs, ys, zs, col="skyblue")