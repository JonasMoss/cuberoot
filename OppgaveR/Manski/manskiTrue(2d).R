Nreps = 10000

getManski = function(nobs,beta1,beta2){
  xx1   = rnorm(nobs)
  xx2   = rnorm(nobs)
  yy    = (1 + beta1*xx1 + beta2*xx2 + rnorm(nobs)) >= 0
  boot  = manski2d(yy,xx1,xx2)
  boot  = boot$points
  index = which.min(apply(boot,1,function(z) (z[1]^2+z[2]^2)))
  
  return(c(boot[index,1],boot[index,2]))
}

origin = c(beta1,beta2)
trues = replicate(Nreps,getManski(nobs,beta1,beta2))
trues = nobs^(1/3)*(trues - origin)
plot(density(trues[1,]))
plot(density(trues[2,]))