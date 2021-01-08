cdensText = as.matrix(read.table("chernoffDensity.dat",header=TRUE))
cdistText = as.matrix(read.table("chernoffDistribution.dat",header=TRUE))
cquanText = as.matrix(read.table("chernoffQuantiles.dat",header=TRUE))
cquanText = rbind(c(0,-Inf),cquanText,c(1,Inf))
csimsText = as.vector(as.matrix(read.table("chernoff.dat")))
dchernoff = function(x, log = FALSE){
  lowerInd = max(which(cdensText[,1] <= x))
  upperInd = min(which(cdensText[,1] >= x))
  lowerLim = cdensText[lowerInd,2]
  upperLim = cdensText[upperInd,2]
  weights = c(1,1)/2
  a = weights[1]*lowerLim + weights[2]*upperLim
  ifelse(log,return(log(a)),return(a))
}

dchernoff = Vectorize(dchernoff)

pchernoff = function(x){
  cdist = rbind(c(-Inf,0),cdistText,c(Inf,1))
  lowerLim = cdist[max(which(cdist[,1] <= x)),2]
  upperLim = cdist[min(which(cdist[,1] >= x)),2]
  (lowerLim + upperLim)/2  
}

pchernoff = Vectorize(pchernoff)

qchernoff = function(q){
  lowerLim = cquanText[max(which(cquanText[,1] <= q)),2]
  upperLim = cquanText[min(which(cquanText[,1] >= q)),2]
  (lowerLim + upperLim)/2
}

qchernoff = Vectorize(qchernoff)

simulateBrown = function(){
  delta = 0.001
  ts = seq(-3,3,by=delta)
  n = length(ts)
  motionRight = cumsum(rnorm(n/2,0,sqrt(delta)))
  motionLeft  = rev(cumsum(rnorm(n/2,0,sqrt(delta))))
  motion = c(motionLeft,0,motionRight)
  maxers = -ts^2+motion
  maxers[is.na(maxers)] = NaN
  maxer = which.max(maxers)
  ts[maxer]
}

