seed = 2000
set.seed(seed)
nobs = 200
xx   = runif(nobs,-1,1)
yy   = (1 + 2*xx + rnorm(nobs)) >= 0

xs   = seq(-5.5,5.5,by=0.001)
plot(xs,xs,col="white",ylim=c(-6,6),xlim=c(-6,6),
     xlab=expression(beta[0]),
     ylab=expression(beta[1]))

colit = function(bool,x){
  if ((bool & x > 0) | (!bool & x < 0)) return("red")
  else return("blue")
}

for (i in 1:nobs){
  lines(xs,-1/xx[i]*xs,col=colit(yy[i],xx[i]))
}

abline(v=1,lty=2,col="grey")
abline(v=-1,lty=2,col="grey")
lines(cos(xs),sin(xs),col="purple",lty=3)

solutions_pos = manski(yy,xx)$positive$solutions[[1]]
solutions_neg = manski(yy,xx)$negative$solutions[[1]]
points(c(1,1),solutions_pos,pch=2)
points(-c(1,1),solutions_neg,pch=2)
