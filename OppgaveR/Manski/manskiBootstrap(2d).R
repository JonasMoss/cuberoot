library("zoo")

# Ordinary bootstrap ----------------------------------------------------------

# There are some possible ways to proceed here. Since there are many different
# points to chose from, we must have a rule to follow. A natural rulse is to
# alway chose the points with the smallest norm.


seed = 891
set.seed(seed)
nobs = 300
beta1 = 2
beta2 = 5
xx1  = rnorm(nobs)
xx2  = rnorm(nobs)
yy   = (1+beta1*xx1+beta2*xx2 + rnorm(nobs))>=0
obj = manski2d(yy,xx1,xx2)

M = 10
bootN = 1000
points = manski2d(yy,xx1,xx2)$points
index = which.min(apply(points,1,function(z) (z[1]^2+z[2]^2)))
origin = c(manski2d(yy,xx1,xx2)$points[index,1],manski2d(yy,xx1,xx2)$points[index,2])

bootstrapper = function(sample){
  xx1Boot = xx1[sample]
  xx2Boot = xx2[sample]
  yyBoot  = yy[sample]
  boot    = manski2d(yyBoot,xx1Boot,xx2Boot)
  boot    = boot$points
  index   = which.min(apply(boot,1,function(z) (z[1]^2+z[2]^2)))
  
  return(c(boot[index,1],boot[index,2]))
}

# Subsampling -----------------------------------------------------------------

for (i in c(5,10,15,20,25)) {}
M = 15
boots = replicate(1000,bootstrapper(sample(1:nobs,M)))
boots = M^(1/3)*(boots - origin)
plot(locfit(~sort(boots[1,])[1:900]),ylim=c(0,0.6),xlim=c(-5,10))
lines(locfit(~sort(trues[1,])),lty=2,col="red")
#plot(locfit(~sort(boots[2,])[100:900]),ylim=c(0,0.6),xlim=c(-5,10))
#lines(locfit(~sort(trues[2,])),lty=2,col="red")
quantile(boots[1,],c(0.05,0.95))
quantile(trues[1,],c(0.05,0.95))

# Plot some shit --------------------------------------------------------------

shitplot = function(alpha=0.1,index=1,Ms=c((1:150)*2),Nreps=1000){
  alpha = 0.1
  lower = c()
  upper = c()
  for (M in Ms) {
   boots = replicate(Nreps,bootstrapper(sample(1:nobs,M)))
   boots = M^(1/3)*(boots - origin)
   quant = quantile(boots[index,],c(alpha/2,1-alpha/2))
   lower = c(lower,quant[1])
   upper = c(upper,quant[2])  
  }

  plot(Ms,lower,ylim=c(min(lower)-1,max(upper)+1),
       xlab="m",ylab="Confidence interval",bty="l",
       sub = paste0("Subsampling when N = ",nobs),
       main = substitute(beta[i],list(i=index)))
  points(Ms,upper)
  grid()
  abline(h=quantile(trues[index,],c(alpha/2,1-alpha/2)),lty=2)  
  abline(h=origin[index],lty=3,col="red")
}

shitplot(index=1)
shitplot(index=2)
# Minimum volatility ----------------------------------------------------------

minvol = function(nobs,lower,upper,Nreps=100,alpha=0.05){
  limits = matrix(rep(NA,2*(upper-lower)),upper-lower,2)
  for ( i in 1:(upper-lower) ){
    boots = replicate(Nreps,bootstrapper(sample(1:nobs,i+lower-1)))
    boots = (i+lower-1)^(1/3)*(boots - origin)
    limits[i,] = quantile(boots[1,],c(alpha/2,1-alpha/2))
    limits[i,] = 1/nobs^(1/3)*limits[i,] + origin
  }
  return(limits)
}

res = minvol(842,10,150)
plot(res[,2])

getVar = function(vec, step = 5){
  rollapply(res[,1],step,var) + rollapply(res[,2],step,var)
}