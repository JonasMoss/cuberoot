CIplotter = function(data, k, rescale = 1,upper=0.1,xlim=c(0,1)) {
  n = length(data)
  m = 50
  bwPal = colorRampPalette(c('white','pink'))
  cols = bwPal(m)
  obj = histogram(data,k=k,method="smoothed")
  q = obj$splits
  boots = replicate(100,histogram(rgcde(n,data),k=k,method="smoothed")$splits)
  plot(obj,bty="l",rescale=rescale,xlim=xlim)
  grid()
  rug(data)
  
  m = 80
  qs = (1:m)*4/(m*10)
  
  for (j in 1:(k-1)){
    for (i in 1:m){
      quants = rescale*quantile(n^(1/3)*(boots[j,]-q[j]),c(qs[i],1-qs[i]))
      polygon(c(n^(-1/3)*quants+q[j],rev(n^(-1/3)*quants+q[j])),
              c(upper,upper,0,0),col=cols[i],border=NA)
      
    }
  }

  lines(obj)  
}

CIplotter(rbeta(500,2,7),5)
lines(histogram(rbeta(1000000,2,7),k=5,method="greedy"),lty=3)





