# Data generation.

nobs = 10
xx = rnorm(nobs)
yy = (1 + 5*xx + rnorm(nobs)) >= 0

rbs = manski1d(yy,xx)
rbs
sol = rbs$positives[1,1]
plot(rbs)

# Subsampling -----------------------------------------------------------------

boot1d = function(sample){
  xxBoot  = xx[sample]
  yyBoot  = yy[sample]
  boot    = manski1d(yyBoot,xxBoot)
  if (!is.null(boot$positives)) {
    if (boot$positives[1,2]>0) {
      return (boot$positives[1,1])
    } 
    else {
      return (boot$positives[1,2])
    }
  }
  
  else {
    if (boot$negatives[1,2]>0) {
      return (boot$negatives[1,1])
    } 
    else {
      return (boot$negatives[1,2])
    }
  }
  
}

M = 10
res = replicate(50000,boot1d(sample(1:nobs,M,replace=TRUE)))
hist(res,breaks=100,freq=FALSE)
#hist(M^(1/3)*(res-sol),breaks=100,freq=FALSE)
confint = quantile(M^(1/3)*(res-sol),c(0.025,0.975)) + sol
confint

# Comparison with probit. -----------------------------------------------------

probits = function(beta,xx){
  mean(yy*pnorm(xx*beta,log=TRUE) + (1-yy)*pnorm(xx*beta,log=TRUE,lower.tail=FALSE))
}

# Actual distribution

Nreps = 10000
nobs  = 10000
getManski = function(nobs){
  xx = rnorm(nobs)
  yy = (1 + 5*xx + rnorm(nobs)) >= 0
  boot = manski1d(yy,xx,type="pos")
  boot$positives[1,1]
}
res = replicate(Nreps,getManski(nobs))
plot(density(nobs^(1/3)*(res-5),adjust=4),col="red")