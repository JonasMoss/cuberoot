library(RColorBrewer)


cols = brewer.pal(12,"Set1")
nobs = 50
xx = sort(rexp(nobs))

yy = 2 + atan(sqrt(xx)) + rnorm(nobs,0,0.01)

mod = lm(yy ~ xx)
plot(xx,yy,bg=cols[2],pch=23,col=NULL,xlab=NA,ylab=NA,ylim=c(2.1,3.2),axes=NULL)
lines(xx,xx*coef(mod)[2] + coef(mod)[1],col=cols[1])



# Wuaaa!!

wom = matrix(c(15.86,3.05,
               16.88,3.12,
               17.50,3.17,
               18.62,3.25,
               19.97,3.36,
               21.06,3.46,
               22.11,3.55),ncol=2,nrow=7,byrow=TRUE)

man = matrix(c(15.86,2.92,
               16.88,2.98,
               17.50,3.03,
               18.62,3.11,
               19.97,3.22,
               21.06,3.31,
               22.11,3.41),ncol=2,nrow=7,byrow=TRUE)

speed = c(wom[,1],man[,1])
stridt = c(wom[,2],man[,2])

cols = brewer.pal(12,"Set1")
plot(wom[,2],wom[,1], bg = cols[2],col = NULL,pch=21,xlim=c(2.9,3.6),ylim=c(15,23),
     xlab = NA, ylab = NA,axes=0)
points(man[,2],man[,1], bg = cols[3], col = NULL, pch=22)
mod = lm(speed ~ stridt)
xs = seq(-10,10,by=1)
lines(xs,xs*coef(mod)[2] + coef(mod)[1],col=cols[1],lwd=2)
drmod = dlm(speed,stridt)
for (i in 1:(length(drmod)/2)){
  lines(xs,drmod[i,1]*xs+drmod[i,2],col=cols[i+3],lty=2)
}


# New one.
nobs = 20
xx1 = runif(nobs,0,20)
xx2 = runif(nobs,20,40)
yy1 = 0 + xx1 + rnorm(nobs,0,0.1)
yy2 = 1 + xx2 + rnorm(nobs,0,0.1)
xx = c(xx1,xx2)
yy = c(yy1,yy2)

plot(xx,yy)
drmod = dlm(yy,xx)
xs = seq(-50,50,by=50)
lines(xs,drmod[1,1]*xs+drmod[1,2],col=cols[3],lty=2)
lines(xs,0.5 +xs)


for (i in 1:(length(drmod)/2)){
  lines(xs,drmod[i,1]*xs+drmod[i,2],col=cols[i+3],lty=2)
}
drmod
