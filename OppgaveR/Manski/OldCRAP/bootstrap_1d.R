manski1d_bootstrap = function(yy,xx,m,N=1000,replace=TRUE){
  # We begin by sorting the values and defining key values.
  nobs     = length(yy)
  xx_order = order(-1/xx)
  isRed    = yy[xx_order]
  values   = -1/xx[xx_order]
  
  # Now we begin estimation.
  estimate_m = function(weights) {
    redBlues  = redBlue(values,weights,isRed,isSorted=TRUE)
    positives = matrix(redBlues[1,][redBlues[3,]==max(c(redBlues[3,],redBlues[4,]))],
                     ncol=2,byrow=TRUE)
    negatives = matrix(rev(-redBlues[1,][redBlues[4,]==max(c(redBlues[3,],redBlues[4,]))]),
                     ncol=2,byrow=TRUE)
  }
  
  # Check if it's an m-of-n bootstrap.
  if (replace = TRUE){
    for (i in 1:N){
      weights = rmultinom(1, size = nobs+1, prob = c(rep(1/nobs,nobs)/2,1/2))[-(nobs+1)]
      estimate_m(weights)
    }
  }
  
  # Or if it's a subsampling bootstrap.
  else {
    
  }
  
  
  return(list(redBlues,positives = positives,negatives = negatives))
  
}



nobs = 1000
xx = rnorm(nobs)
yy = (1 + rnorm(nobs)) >= 0