biasVarianceR = function(beta1, beta2, nobs, error = function(xx1,xx2,nobs) rnorm(nobs),
                         dist1 = rnorm, dist2 = rnorm, Nreps = 1000,
                         epsilon = 0, delta = 0){
  # We calculate the bias and variance for Manski, probit and logit.
  
  estimateThem = function(yy,xx1,xx2){
    
    logitMod  = glm(yy~xx1+xx2,family=binomial(link=logit))   
    logit    = coef(logitMod)[2]/coef(logitMod)[1]
    manskiMod = manski2d(yy,xx1,xx2)
    manski   = coef(manskiMod)[1]
    return(c(logit,manski))
  }
  estimates = c()
  
  for (i in 1:Nreps){
    xx1 = dist1(nobs)
    xx2 = dist2(nobs)
    yy = (1 + beta1*xx1 + beta2*xx2 + error(xx1,xx2,nobs))>=0
    yy[(nobs*(1-epsilon)):nobs] = !yy[(nobs*(1-epsilon)):nobs]
    xx1[(nobs*(1-delta)):nobs] = 10*xx1[(nobs*(1-delta)):nobs]
    xx2[(nobs*(1-delta)):nobs] = 10*xx2[(nobs*(1-delta)):nobs]
    estimates = rbind(estimates,estimateThem(yy,xx1,xx2))
  }
  ests = c(apply(estimates,2,var), 
           apply(estimates,2,function(x) var(x) + (mean(x)-beta1)^2), 
           colMeans(estimates) - beta1)
  names(ests) = c("Var","Var","MSE","MSE","Bias","Bias")
  ests = ests[c(3,1,5,4,2,6)]  
  
  return(ests)
  
}



error = function(xx1,xx2,nobs){
  rnorm(nobs)
}

biasVarianceR(beta1, beta2, nobs, error = error, 
              dist1 = rnorm, dist2 = rnorm, 100, 
              epsilon = epsilon, delta = delta)

nobs = 800
beta1 = 1
beta2 = 1
endIt = c()
iter = expand.grid(delta=c(0,0.1,0.5),epsilon=c(0,0.1,0.2,0.4))
for (i in 1:12) {
  epsilon = iter[i,2]
  delta = iter[i,1]
  endIt = rbind(endIt,biasVarianceR(beta1, beta2, nobs, error = error, 
                                    dist1 = rnorm, dist2 = rnorm, 50000, 
                                    epsilon = epsilon, delta = delta))
}