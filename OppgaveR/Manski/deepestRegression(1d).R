library("Rcpp")
sourceCpp("manskiAlgorithm[1d].cpp")

dlm = function(yy,xx){
  # This function works by calling redBlue from within the Manski C++
  # code. We (naïvely) iterate trough all combinations i,j.
  ord = order(xx)
  yy = yy[ord]
  xx = xx[ord]
  nobs = length(yy)
  
  iteratives = combn(1:nobs,2)
  currentMax = -Inf
  maxList = matrix(ncol=2)
  
  # Add everything except the last observation.
  for (k in 1:(length(iteratives)/2)) {
      # i and j are the first and second coordinates we're interested in.
    
      i = iteratives[1,k]
      j = iteratives[2,k]
      
      # Find the slopes and stuff.
      slope      = (yy[i] - yy[j])/(xx[i] - xx[j])  
      intercept  = yy[i] - slope*xx[i] 
      
      # Now we can find the residuals and calculate its deepity.
      resids = (yy[-c(i,j)] - (intercept+slope*xx[-c(i,j)])) >= 0
      vals = redBlue(xx[-c(i,j)],rep(1,nobs-2),resids,isSorted = TRUE)[3,]
      deepity = min(vals,nobs-vals)
      
      # If deepity is bigger than every other deepity, make a new maxList.
      if (deepity > currentMax){
        maxList = matrix(ncol=2)
        maxList[1,] = c(slope,intercept)
        currentMax = deepity
      } else if (deepity == currentMax){
        maxList = rbind(maxList,c(slope,intercept))
      }
  }
  colnames(maxList) = c("slope","intercept")
  rownames(maxList) = rep(currentMax+1,length(maxList)/2)
  return(maxList)

}

nobs = 100
xx = rnorm(nobs)
yy = 5 + xx + rlnorm(nobs)

result = dlm(yy,xx)
plot(xx,yy)
xs = seq(-10,10,by=1)
for (i in 1:(length(result)/2)){
  lines(xs,result[i,1]*xs+result[i,2])
}
result
mod = lm(yy~xx)
lines(xs,xs*coef(mod)[2] + coef(mod)[1],col="blue",lty=2)
