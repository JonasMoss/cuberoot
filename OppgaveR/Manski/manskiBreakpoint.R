library("RColorBrewer")

nobs = 15
xx = rnorm(nobs)
beta1 = 0.5
yy = ( 1 + beta1*xx + rnorm(nobs)) >= 0
isReds = c((yy & xx >=0 ) | (!yy & xx < 0))
colindex = ifelse(isReds,1,2)
mansk = manski1d(yy,xx,type="pos")
plot(-1/xx,rep(0,nobs),bg=cols[colindex],pch=(isReds+23),col=NA,
     xlab=paste0("  Reds: ",sum(isReds),"; Blues: ", sum(1-isReds), 
                 "; Maximum: ",mansk$maxWeight/mansk$total, 
                 "; Breakdown: ", breakdown(mansk)/nobs),
     ylim=c(-0.2,0.2),ylab=NA,yaxt="n",bty="l",main="Binary choice model, d = 1.")
grid()
positives = mansk$positives

for (i in 1:(length(positives)/2)){
  lines(c(mansk$positives[i,1],mansk$positives[i,2]),y=c(0,0),col="purple",lty=1)
  abline(v=mansk$positives[i,1],col="purple",lty=2)
  abline(v=mansk$positives[i,2],col="purple",lty=2)
}


breaks = function(beta1,codens=dnorm,eps=pnorm,lower=-Inf,upper=Inf){
  if(beta1<0) beta1 = -beta1
  # redleft - blueright
#   a = integrate(function(x) eps(1+x*beta1,lower.tail = FALSE)*codens(x), lower = lower      , upper = min(-1/beta1,upper))$value
#   b = integrate(function(x) eps(1+x*beta1)*codens(x),                    lower = 0          , upper = upper    )$value
# 
#   c = integrate(function(x) eps(1+x*beta1)*codens(x),                    lower = lower      , upper = min(-1/beta1,upper))$value
#   d = integrate(function(x) eps(1+x*beta1,lower.tail = FALSE)*codens(x), lower = 0          , upper = upper)$value

  # Calculate blueright-redright
  e = integrate(function(x) eps(-1-x*beta1,lower.tail = FALSE)*codens(x),                    lower = max(-1/beta1,lower), upper = 0)$value

  f = integrate(function(x) eps(-1-x*beta1,lower.tail = TRUE)*codens(x), lower = max(-1/beta1,lower), upper = 0)$value

  #min(a+b-c-d,e-f)
  (e-f)/2
}

maximizer = function(beta1,codens=dnorm,eps=pnorm,lower=-Inf,upper=Inf){

  e = integrate(function(x) eps(-1-x*beta1,lower.tail = FALSE)*codens(x), lower = max(-1/beta1,lower), upper = Inf)$value
  f = integrate(function(x) eps(-1-x*beta1,lower.tail = TRUE)*codens(x),  lower = max(-Inf,lower), upper = -1/beta1)$value

  e+f
}

simPoint = function(beta1,nobs,corand=rnorm,eps=rnorm){
  xx = corand(nobs)
  yy = ( 1 + beta1*xx + eps(nobs)) >= 0
  mansk = manski1d(yy,xx,type="pos")
  breakdown(mansk)/nobs
}


xs = seq(0.001,5,by=0.01)
ys = seq(0.05,5,by=0.05)

# Logistic simulation
plot(xs,sapply(xs,breaks,codens=dnorm,eps=plogis),type="l",ylim=c(-0.01,.13),
     xlab=expression(beta[1]),ylab="Breakdown point",bty="l",col=cols[4])
grid()
points(ys,sapply(ys,function(beta) simPoint(beta, 100,corand=rnorm,eps=rlogis)),
       col=cols[1],pch=1)
points(ys,sapply(ys,function(beta) simPoint(beta, 500,corand=rnorm,eps=rlogis)),
       col=cols[2],pch=2)
points(ys,sapply(ys,function(beta) simPoint(beta,2500,corand=rnorm,eps=rlogis)),
       col=cols[3],pch=5)
abline(h=0,lty=2,col="gray")
legend("topright",c("n = 100","n = 500","n = 2500","Asympotic"),
       lty=c(NA,NA,NA,1),pch=c(1,2,5,NA),col=cols[1:4],bty="n")



# Nice picture
xs = seq(0.001,3,by=0.01)
cols = brewer.pal(9,"Set1")
plot(xs,sapply(xs,breaks,codens=dlogis,eps=plogis),type="l",ylim=c(-0.01,.26),xlab=expression(beta[1]),
     ylab="Breakdown point",bty="l",col=cols[1])
grid()
abline(h=0,lty=2,col="gray")
lines(xs,sapply(xs,breaks,eps=function(x,...) pexp(x+log(2),...),codens=dcauchy),col=cols[2])
lines(xs,sapply(xs,breaks,eps=function(x,...) plnorm(x+1,...)),col=cols[3])
lines(xs,sapply(xs,breaks,eps=pcauchy,codens=dcauchy),col=cols[4])
lines(xs,sapply(xs,breaks),col=cols[5])
legend("topright",c("Logistic","Exponential","Lognormal","Cauchy","Normal"),lty=rep(1,5),col=cols[1:5],bty="n")

# Plot of limit maximizers.
xs = seq(0.1,5,by=0.01)
cols = brewer.pal(9,"Set1")
plot(xs,sapply(xs,maximizer,codens=dlogis,eps=plogis),type="l",ylim=c(0.1,1),xlab=expression(beta[1]),
     ylab="Breakdown point",bty="l",col=cols[1])
grid()
abline(h=0,lty=2,col="gray")
lines(xs,sapply(xs,maximizer,eps=function(x,...) pexp(x+log(2),...),codens=dnorm),col=cols[2])
lines(xs,sapply(xs,maximizer,eps=function(x,...) plnorm(x+1,...)),col=cols[3])
lines(xs,sapply(xs,maximizer,eps=pcauchy),col=cols[4])
lines(xs,sapply(xs,maximizer),col=cols[5])
legend("bottomright",c("Logistic","Exponential","Lognormal","Cauchy","Normal"),lty=rep(1,5),col=cols[1:5],bty="n")

