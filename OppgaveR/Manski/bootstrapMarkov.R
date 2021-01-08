library("microbenchmark")
library("arm")
sourceCpp("bootstrapMarkov.cpp")

microbenchmark(bootIndependent(zz,100),
               bootMarkov(zz,100,sumTimes=1))

zz = runif(1000,0,1)
maxi = max(zz)
lower = 1
upper = 1000
subs = bootUniform(zz,30000,lower=lower,upper=upper)
cools = list()
for (i in lower:upper){
  cools[[i]] = i*(maxi - subs[[i]])/maxi
}

hist(cools[[800]],freq=FALSE,breaks=40)
xs = seq(0,20,by=0.01)
lines(xs,dexp(xs))

zz = rnorm(1000)
boos = bootSubs(zz,10000,lower=1,upper=1000)
length(boos)

hist(sqrt(nobs)*(bootIndependent(zz/nobs,2000)-mean(zz)))
hist(sqrt(nobs)*(bootMarkov(zz/nobs,80000,sumTimes=1)-mean(zz)))
result = bootMarkov(zz/nobs,599,sumTimes=1)
plot(result)

circles = (table(cools[[800]])/30000)
xx      = as.numeric(names(circles))
yy      = xx
symbols(xx,yy,squares=sqrt(circles),inches=FALSE)

plotzi = function(nn=800,j=1000){
  heights = sapply(0:999,function(i) (1-i/n)^nn-(1-(i+1)/n)^nn)[1:j]
  heights = heights/heights[1]
  xx = sort(zz,decreasing=TRUE)[1:j]
  xx = nn*(maxi-xx)/maxi
  new_xx = c(0,sapply(1:(j-1),function(i) xx[i] + (xx[i+1]-xx[i])/2))
  plot(xs,dexp(xs),xlim=c(0,4),type="l",lty=2)
  lines(new_xx,heights,type="s",xlab="x",ylab="Probability")
  #lines(xs,dexp(xs))
  #lines(new_xx,heights,type="h")
  #discrete.histogram(xx,heights)
  #hist(xx,breaks=j)
}

for (i in 1:200) plotzi(202-i,1000)

jpeg("foo%02d.jpg")
for (i in 2:1000) plotzi(i,1000)
dev.off()














nobs = 100
N = 5000
zz = rnorm(100)

hist(sqrt(nobs)*(bootIndependent(zz/nobs,N)-mean(zz)))
hist(sqrt(nobs)*(bootMarkov(zz/nobs,40*N,sumTimes=1)-mean(zz)))


result = bootMarkov(zz/nobs,599,sumTimes=1)
plot(result)


strapper = function(j=1){
  addIndex = sample(1:nobs,j)
  removeIndex = sample(1:nobs,j)
  current_mean <<- current_mean - sum(meaned[indices[removeIndex]]) + sum(meaned[addIndex])
  indices <<- c(indices[-removeIndex],addIndex)
}

current_mean = mean(zz)
indices = 1:nobs
boots_fake = rep(NA,N)


for (i in 1:N){
  strapper()
  boots_fake[i] = current_mean
}

plot(1:N,boots_fake)
plot(1:1000,boots_real)

boots_real = replicate(1000,mean(sample(zz,nobs,replace=TRUE)))




hist(sqrt(nobs)*(boots_real - mean(zz)),freq=FALSE)
lines(as,dnorm(as,0,1))
hist(sqrt(nobs)*(boots_fake - mean(zz)),freq=FALSE)
lines(as,dnorm(as,0,1))

