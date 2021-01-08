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

logistic = function(x,theta = 1) {
  1/(1+exp(-x*theta))
}

smoothObjective = function(beta, yy, xx, positive = TRUE,theta = 1){
  if (positive) {
    sum(yy*logistic(1+beta*xx, theta)) + sum((1-yy)*logistic(1+beta*xx<0, theta))
  }
  else {
    sum(yy*logistic(-1+beta*xx, theta)) + sum((1-yy)*logistic(-1+beta*xx<0, theta))
  }  
}

# Wrapper. --------------------------------------------------------------------

manski1d = function(yy, xx, xxConst = 1, weights=rep(1,length(yy)),
                    isSorted = FALSE, type="both") {
  
  # First we find the colors and transform the input.
  # isRed = c((yy & xx >=0 ) | (!yy & xx < 0))
  
  values = -xxConst/xx
  isRed = c((yy & xx >=0 ) | (!yy & xx < 0))
  
  redBlues = redBlue(values,weights,isRed,isSorted=isSorted)
  
  if (type == "pos") {
    maxWeight = max(redBlues[3,])
    positives = matrix(redBlues[1,][redBlues[3,] == maxWeight],
                       ncol=2,byrow=TRUE)
    sol1d = list()
    class(sol1d) = "manski1d"
    sol1d$xx        = xx
    sol1d$yy        = yy
    sol1d$details   = redBlues
    sol1d$type      = "positive"
    sol1d$maxWeight = maxWeight
    sol1d$positives = positives
    sol1d$weights   = weights
    sol1d$total     = sum(weights)
    
    return(sol1d)
  }
  
  if (type == "neg") {
    redBlues = rbind(redBlues, length(xx) - redBlues[3,] + 1)
    redBlues[4,1] = redBlues[4,1] - 1
    redBlues[4,length(xx)+2] = redBlues[4,length(xx)+2] - 1
    maxWeight = max(redBlues[4,])  
    
    negatives = matrix(rev(-redBlues[1,][redBlues[4,] == maxWeight]),
                       ncol=2,byrow=TRUE)    
    sol1d = list()
    class(sol1d) = "manski1d"
    sol1d$xx        = xx
    sol1d$yy        = yy
    sol1d$details   = redBlues
    sol1d$type      = "negative"
    sol1d$maxWeight = maxWeight
    sol1d$negatives = negatives
    sol1d$weights   = weights
    sol1d$total     = sum(weights)
    return(sol1d)
    
  }
  
  # We add the negative solutions as well.
  redBlues = rbind(redBlues, length(xx) - redBlues[3,] + 1)
  redBlues[4,1] = redBlues[4,1] - 1
  redBlues[4,length(xx)+2] = redBlues[4,length(xx)+2] - 1
  
  maxWeight = max(c(redBlues[3,],redBlues[4,])) 
  
  positives = matrix(redBlues[1,][redBlues[3,] == maxWeight],
                     ncol=2,byrow=TRUE)
  negatives = matrix(rev(-redBlues[1,][redBlues[4,] == maxWeight]),
                     ncol=2,byrow=TRUE)
  if (dim(negatives)[1] == 0) {
    negatives = NULL
  }
  
  if (dim(positives)[1] == 0) {
    positives = NULL
  }  
  
  sol1d = list()
  class(sol1d) = "manski1d"
  sol1d$details   = redBlues
  sol1d$xx        = xx
  sol1d$yy        = yy
  sol1d$type      = "both"
  sol1d$maxWeight = maxWeight
  sol1d$positives = positives
  sol1d$negatives = negatives
  sol1d$weights   = weights
  sol1d$total     = sum(weights)

  
  return(sol1d)
}

# Generics. -------------------------------------------------------------------

plot.manski1d = function(manski_obj,main=NULL,sub=NULL,lim=NULL,...){
  
  redBlues = manski_obj$details
  nobs = length(redBlues[1,]) - 2
  maxWeight = manski_obj$maxWeight
  total = manski_obj$total
  weights = manski_obj$weights
  positives = manski_obj$positives
  negatives = manski_obj$negatives
  xx = manski_obj$xx
  yy = manski_obj$yy 
  
  if (!is.null(positives)){
    
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
        frame.plot=F,main = expression(paste("Manski plot for ",beta[0], " = 1.")))
    for (i in 1:dim(positives)[1]){
      lines(positives[i,],c(maxWeight,maxWeight)/total)
   }
  }
  
  if (!is.null(negatives)){
    
    if(max(negatives)==Inf) {
      negatives[negatives==Inf] = max(negatives[negatives!=Inf])+4
      upper = max(negatives[negatives!=Inf])
    } else {
      upper = max(negatives,na.rm=TRUE) + abs(min(negatives,na.rm=TRUE))
    }    
    
    xs = seq(min(negatives,na.rm=TRUE)-abs(min(negatives,na.rm=TRUE)),
             upper,by=0.001)
    
    plot(xs,sapply(xs,function(beta) manskiObjective(beta,yy,xx,weights,positive=FALSE)/total),type="s",
         col=adjustcolor("purple",alpha.f=0.6),
         xlab=expression(beta[1]),ylab="Percent hits",
         frame.plot=F,main = expression(paste("Manski plot for ",beta[0], " = -1.")))
    for (i in 1:dim(negatives)[1]){
      lines(negatives[i,],c(maxWeight,maxWeight)/total)
    }
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
