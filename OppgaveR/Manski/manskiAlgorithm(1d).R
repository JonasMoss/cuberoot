library("Rcpp")
sourceCpp("manskiAlgorithm[1d].cpp")

# The objective function. -----------------------------------------------------

manskiObjective = function(beta,yy,xx,weights=rep(1,length(yy)),positive = TRUE){
  if (positive) {
    sum(weights*yy*(1+beta*xx>=0)) + sum(weights*(1-yy)*(1+beta*xx<0))
  }
  else {
    sum(weights*yy*(-1+beta*xx>=0)) + sum(weights*(1-yy)*(-1+beta*xx<0))
  }
}

# Wrapper. --------------------------------------------------------------------

manski1d = function(yy, xx, xxConst = 1, weights=rep(1,length(yy)),
                    isSorted = FALSE) {
  
  # xxConst is used to modulate the values of the constant beta. By
  # putting it unequal to 1, one can let y = x1 + bx2, for instance.
  #
  # isSorted = TRUE if the values yy,xx are already sorted.
  
  
  # First we find the colors and transform the input.
  # isRed = c((yy & xx >=0 ) | (!yy & xx < 0))
  
  values = -xxConst/xx
  isRed = c((yy & xx >=0 ) | (!yy & xx < 0))
  
  redBlues = redBlue(values,weights,isRed,isSorted=isSorted)

  maxWeight = max(redBlues[3,])
  points    = matrix(redBlues[1,][redBlues[3,] == maxWeight],
                       ncol=2,byrow=TRUE)
  colnames(points) = c("beta1","beta2")
  sol1d = list()
  class(sol1d)    = c("manski1d","manski")
  sol1d$maxWeight = maxWeight
  sol1d$total     = sum(weights)
  sol1d$points    = points
  sol1d$yy        = yy
  sol1d$xx        = xx
  sol1d$weights   = weights
    
  return(sol1d)
}

# Generics. -------------------------------------------------------------------

plot.manski1d = function(manski_obj,main=NULL,sub=NULL,lim=NULL,...){
  
  redBlues = manski_obj$details
  nobs = length(redBlues[1,]) - 2
  maxWeight = manski_obj$maxWeight
  total = manski_obj$total
  weights = manski_obj$weights
  positives = manski_obj$points
  xx = manski_obj$xx
  yy = manski_obj$yy 

   if(max(positives)==Inf) {
     positives[positives==Inf] = max(positives[positives!=Inf])+4
     upper = max(positives[positives!=Inf])
   } else {
     upper = max(positives,na.rm=TRUE) + abs(min(positives,na.rm=TRUE))
   }
   
   xs = seq(min(positives,na.rm=TRUE)-abs(min(positives,na.rm=TRUE)),
             upper,by=0.001)
    
   plot(xs,sapply(xs,function(beta) manskiObjective(beta,yy,xx,weights)/total),type="s",
        col=adjustcolor("purple",alpha.f=0.6),xlab=expression(beta[1]),ylab="Percent hits",
        main = main, bty="l",
        xlim=c(min(positives)-0.01,max(positives)+0.01))
    for (i in 1:dim(positives)[1]){
      lines(positives[i,],c(maxWeight,maxWeight)/total)
   }
  
}

breakdown = function(obj){
  nobs = length(obj$yy)
  isReds = obj$details[2,][-c(1,nobs+2)]
  vals   = obj$details[1,][-c(1,nobs+2)]
  left   = (vals <= obj$positives[1,1])
  left   = sum(ifelse(isReds,1,-1)*left)
  right  = (vals >= obj$positives[1,2])
  right  = sum(ifelse(isReds,-1,1)*right)  
  ceiling(min(left,right))/2
}

coef.manski1d = function(obj,rand=FALSE){
  xs = obj$positives
  xsFiltered = xs[xs<=10^9 & xs >= -10^9]
  if (rand) {
    return (sample(xsFiltered,1))
  } else {
    xsAbs = abs(xsFiltered)
    index = which.min(xsAbs)
    return(xsFiltered[index])
  }
}
