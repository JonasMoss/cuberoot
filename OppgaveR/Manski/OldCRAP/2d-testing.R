plotzi = function(xs,x1,x2,col="red",plot=FALSE,...){
  if(plot) {
    plot(xs,-1/x1-xs*x2/x1,type="l",col=col,...)
  }
  else {
    lines(xs,-1/x1-xs*x2/x1,type="l",col=col,...)    
  }
}


nobs  = 7
xx1   = rnorm(nobs)
xx2   = rnorm(nobs)
beta0 = 1
beta1 = 2
beta2 = 3
yy    = (beta0 + xx1*beta1 + xx2*beta2 + rnorm(nobs)) >= 0 
slope = -xx2/xx1
isRed = (slope >= 0 & yy) | ( slope < 0 & !yy)

plotzi(xs,xx1[1],xx2[1],plot=TRUE,col=(-2*isReds[1]+4),ylim=c(-5,5))
for (i in 2:nobs) plotzi(xs,xx1[i],xx2[i],col=-2*isReds[i]+4)
