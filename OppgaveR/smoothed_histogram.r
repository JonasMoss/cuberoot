
### Real histograms.

histogram = function(data,k,method="smooth",...){
  if (is.function(data)) { 
    phisto(data,k,...)
  } else if (method=="smooth") {
    shisto(data,k)
  } else if (method=="L2") {
    lhisto(data,k)
  } else {
    dhisto(data,k)
  }
}

phisto = function(P,k,...){
  qs = c(0,(1:k)/k)
  qs = sapply(1:k,function(i) P(qs[i+1],...)-P(qs[i],...))
  class(qs) = c("histo","numeric")
  qs*k
}


lhisto = function(data,k){
  n = length(data)
  qs = c(0,(1:k)/k)
  qs = sapply(1:k,function(i) sum(data < qs[i+1] & data >= qs[i]))
    
  objective = function(weights){
    sum(-2*qs/n*weights + 1/k*weights^2)
  }
  weights = constrOptim.nl(rep(1,k)/k,objective,hin=function(weights) weights,
              heq=function(weights) sum(weights/k)-1)$par
  class(weights) = c("histo","numeric")
  weights
}

dhisto = function(data,k){
  qs = c(0,(1:k)/k)
  qs = sapply(1:k,function(i) sum(data < qs[i+1] & data >= qs[i]))
  qs = qs/length(data)
  class(qs) = c("histo","numeric")
  qs*k
}

shisto = function(data,k){
  new_data = rgcde(10000,data)
  dhisto(new_data,k)
}

plot.histo = function(x,...){
  k = length(x)
  ks = c(0,(1:k)/k)
  plot(ks,c(0,x),"S")
  lines(ks,c(0,x),"h")
}

lines.histo = function(x,...){
  k = length(x)
  ks = c(0,(1:k)/k)
  lines(ks,c(0,x),"S",...)
  lines(ks,c(0,x),"h",...)
}

k = 10
plot(histogram(data,k))
lines(sde(data,k),lty=2,col="red")
lines(histogram(data,k,method="real"),lty=3,col="blue")

lines(histogram(function(x) pbeta(x,2,7),k),lty=3,col="blue")

plot(shisto(data,k))
lines(histo(function(x) pbeta(x,2,7),k),lty=3,col="blue")