library("skewt")

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
epsilon = 0
delta = 0
iter = list(function(xx1,xx2,nobs) rcauchy(nobs),
            function(xx1,xx2,nobs) rlnorm(nobs)-1,          
            function(xx1,xx2,nobs) rskt(nobs,df=3,gamma=3) - 1.984529,         
            function(xx1,xx2,nobs) {
              vals = rbinom(1,p=0.5,n=nobs)
              vals*rnorm(nobs,1,1) + (1-vals)*rnorm(nobs,-1,1)
            },
            function(xx1,xx2,nobs) rcauchy(nobs,scale=sqrt(xx1^2+xx2^2)),
            function(xx1,xx2,nobs) rcauchy(nobs,scale=exp(-xx1)),
            function(xx1,xx2,nobs) rlnorm(nobs,0,sqrt(xx1^2+xx2^2))-1,
            function(xx1,xx2,nobs) rlnorm(nobs,0,exp(-xx1))-1,
            function(xx1,xx2,nobs) {
              vals = rbinom(1,p=0.5,n=nobs)
              vals*rnorm(nobs,1,sqrt(xx1^2+xx2^2)) + (1-vals)*rnorm(nobs,-1,sqrt(xx1^2+xx2^2))
            },
            function(xx1,xx2,nobs) {
              vals = rbinom(1,p=0.5,n=nobs)
              vals*rnorm(nobs,1,exp(-xx1)) + (1-vals)*rnorm(nobs,-1,exp(-xx1))
            }
            
            )
for (i in 1:10) {
  error = iter[[i]]
  endIt = rbind(endIt,biasVarianceR(beta1, beta2, nobs, error = error, 
                                    dist1 = rnorm, dist2 = rnorm, 1000, 
                                    epsilon = epsilon, delta = delta))
}

endIt = cbind(endIt,ratio=endIt[,1]/endIt[,4])
round(endIt,5)

error = function(xx1,xx2,nobs) rskt(nobs,df=3,gamma=3) - 1.984529

biasVarianceR(beta1, beta2, 2000, error = error, 
              dist1 = rnorm, dist2 = rnorm, 1000, 
              epsilon = epsilon, delta = delta)

