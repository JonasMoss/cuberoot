library("Rcpp")
sourceCpp("manskiAlgorithm[2d].cpp")

# Wrapper for the algorithm. --------------------------------------------------

manski2d = function(yy,xx1,xx2,weights=rep(1,length(yy))) {
  isReds = (xx1 >= 0 & yy) | ( xx1 < 0 & !yy)
  res = redBlue2(xx1,xx2,isReds,weights)
  points = cbind(beta1 = res$yCoord,beta2 = res$xCoord)
  sol2d = list()
  class(sol2d)    = c("manski2d","manski")
  sol2d$maxWeight = res$maxWeight
  sol2d$total     = sum(weights)
  sol2d$points    = points
  sol2d$yy        = yy
  sol2d$xx1       = xx1
  sol2d$xx2       = xx2
  sol2d$weights   = weights
  return(sol2d)
}

# Generics. -------------------------------------------------------------------

plot.manski2d = function(manski_obj,main=NULL,sub=NULL,
                         full=FALSE,lim=NULL,...){
  
  len = length(manski_obj$points[,1])/2
  xCoord = matrix(manski_obj$points[,1],len,2,byrow=TRUE)
  yCoord = matrix(manski_obj$points[,2],len,2,byrow=TRUE)
  xx1 = manski_obj$xx1
  xx2 = manski_obj$xx2
  yy  = manski_obj$yy  
  par(pty="m")  
  
  plot(NULL,xlim=c(min(xCoord),max(xCoord)),
       col=adjustcolor("black",alpha.f=0.6),
       ylim=c(min(yCoord),max(yCoord)),type="l",
       xlab=expression(beta[2]),ylab=expression(beta[1]),
       bty="l")
  grid()

  if (full){
    nobs = length(xx1)
    xs = seq(-1000,1000,by=2)
    isReds = (xx1 >= 0 & yy) | ( xx1 < 0 & !yy)
    colIndex = ifelse(isReds,2,4)
    for (i in 1:nobs){
      lines(xs,-1/xx2[i]-xs/xx2[i]*xx1[i],col=adjustcolor(colIndex[i],alpha.f=0.7),lty=3)
    }
    
  }

  if (is.null(lim)){
  
    for (i in 1:len) {
     lines(xCoord[i,],yCoord[i,],lwd=1,col=adjustcolor("black",alpha.f=0.6))
    }
  } else {
    plot(xCoord[1,],yCoord[1,],xlim=c(min(xCoord),lim),col=adjustcolor("black",alpha.f=0.6),
         ylim=c(min(yCoord),lim),type="l",xlab=expression(beta[2]),ylab=expression(beta[1]))
    
    for (i in 2:len) {
      lines(xCoord[i,],yCoord[i,],lwd=1,col=adjustcolor("black",alpha.f=0.6))
    }    
  }

}

coef.manski2d = function(obj,type="min"){
  # The point is to automatically select coefficients in a smart way.
  # Most importantly, we _don't_ want beta1 or beta2 to be Inf (that
  # is 10^10 in this case.)
  
  if (type == "min") {
    points = obj$points
    index = which.min(apply(points,1,function(z) (z[1]^2+z[2]^2)))
    return(c(points[index,1],points[index,2]))
  } else {
    xs = obj$points[,1]
    ys = obj$points[,2] 
    for (i in 1:length(xs)){
     if (xs[i] <= 10^9 & xs[i] >= -10^9 & ys[i] <= 10^9 & ys[i] >= -10^9){
       return(c(beta1=xs[i],beta2=ys[i]))
      }
    }
  }
}

