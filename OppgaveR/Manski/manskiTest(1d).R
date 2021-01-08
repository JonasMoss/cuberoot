nobs = 50
xx = rnorm(nobs)
yy = (1 + 5*xx + rnorm(nobs)) >= 0
orderIt = order(-1/xx)
xx = xx[orderIt]
yy = yy[orderIt]
remove(orderIt)
obj = manski1d(yy,xx,isSorted=TRUE,type="pos")
plot(obj)

# Bootstrapper ----------------------------------------------------------------
# This bootstrapper uses the sampling rule of selecting the smallest
# observation in absolute value.


manski1dboot = function(yy, xx, Nreps = 1000, m = sqrt(nobs), 
                        replace = TRUE, type = "pos") {
  orderIt = order(-1/xx)
  xx = xx[orderIt]
  yy = yy[orderIt]
  
  vals = rep(0,Nreps)
  origin = manski1d(yy,xx,,isSorted=TRUE,type = "pos")$positives[1,1]
  
  for (i in 1:Nreps){
    newWeights  = sample(1:nobs,m,replace = TRUE)
    newWeights_ = tabulate(newWeights,nbins=nobs)
    newxx       = xx[newWeights_ > 0 ] 
    newyy       = yy[newWeights_ > 0]
    manski_obj  = manski1d(newyy,newxx,weights=table(newWeights),isSorted=TRUE,type = "pos")  
    vals[i]     = manski_obj$positives[1,1]
  }
  return(m^(1/3)*(vals-origin))
}

lines(density(manski1dboot(yy,xx,m = 30,Nreps = 10000),adjust=2),xlim=c(-20,200),col="red")
lines(density(nobs^(1/3)*(res-5),adjust=2))
plot(kde(fuck),xlim=c(-20,200))

# Kolmogorov-Smirnof
betahat = obj$positives[1]
