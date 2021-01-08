library("Rcpp")
sourceCpp("manskiAlgorithm.cpp")
//sourceCpp("manskiAlgorithm_bak.cpp")

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

manski1d = function(yy,xx,weights=rep(1,length(yy))) {
  # First we find the colors and transform the input.
  isRed = c((yy & xx >=0 ) | (!yy & xx < 0))
  weigths = c(weights)
  values = c(-1/xx)
  redBlues = redBlue(values,weights,isRed,isSorted=FALSE)
  redBlues = rbind(redBlues, length(xx) - redBlues[3,] + 1)
  redBlues[4,1] = redBlues[4,1] - 1
  redBlues[4,length(xx)+2] = redBlues[4,length(xx)+2] - 1
  positives = matrix(redBlues[1,][redBlues[3,]==max(c(redBlues[3,],redBlues[4,]))],
                     ncol=2,byrow=TRUE)
  negatives = matrix(rev(-redBlues[1,][redBlues[4,]==max(c(redBlues[3,],redBlues[4,]))]),
                     ncol=2,byrow=TRUE)
  return(list(redBlues,positives = positives,negatives = negatives))
}

# Testing is here -------------------------------------------------------------
values  = 0:9
weights = c(1,1,1,1,1,1,1,1,1,1)
isReds  = c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE,FALSE,TRUE)

redBlue(values,weights,isReds,FALSE)

values = c(4.17953,0.0555039,-0.523832,-0.0573289,-0.293231,-0.720473)
isReds = c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)
weights = rep(1,6)
redBlue(values,weights,isReds,FALSE)
