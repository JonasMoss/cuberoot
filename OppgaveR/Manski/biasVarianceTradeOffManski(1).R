library("reshape2")
library("dplyr")

###
### We consider four weird models and calculate the bias and variance for
### Manski, probit and logit.
###

### Here are three distributions from Delgado.
###
### t-distribution with three degrees of freedom. not interesting.
### ordinary logit.
### 0.25*(1+2w^2+w^4)*v, where w = xx1 + xx2, and v ~ logistic.

biasVariance = function(beta1, beta2, nobs, error = rnorm,
                        dist1 = rnorm, dist2 = rnorm, Nreps = 1000){
  # We calculate the bias and variance for Manski, probit and logit.
  
  estimateThem = function(yy,xx1,xx2){
    
    logitMod  = glm(yy~xx1+xx2,family=binomial(link=logit))   
    probitMod = glm(yy~xx1+xx2,family=binomial(link=probit))
    logit1    = coef(logitMod)[2]/coef(logitMod)[1]
    logit2    = coef(logitMod)[3]/coef(logitMod)[1]
    probit1   = coef(probitMod)[2]/coef(probitMod)[1]
    probit2   = coef(probitMod)[3]/coef(probitMod)[1]
    manskiMod = manski2d(yy,xx1,xx2)
    manski1   = coef(manskiMod)[1]
    manski2   = coef(manskiMod)[2]
    
    betas     = rbind(c(logit1, probit1, manski1),c(logit2, probit2, manski2))
    colnames(betas) = c("Logit","Probit","Manski")
    rownames(betas) = c("beta1","beta2")
    
    return(betas)
  }
  estimates = c()
  
  for (i in 1:Nreps){
    xx1 = dist1(nobs)
    xx2 = dist2(nobs)
    yy = (1 + beta1*xx1 + beta2*xx2 + error(xx1,xx2,nobs))>=0
    estimates = rbind(estimates,estimateThem(yy,xx1,xx2))
  }
  
  estimates = melt(estimates)
  colnames(estimates) = c("beta","model","value")
  
  groupIt = group_by(estimates,model,beta)
  estTable = summarise(groupIt,
            variance = var(value),
            bias = mean(value))
  estTable[c(1,3,5),4] = estTable[c(1,3,5),4] - beta1
  estTable[c(2,4,6),4] = estTable[c(2,4,6),4] - beta2  
  estTable$MSE = estTable$variance + (estTable$bias)^2
  return(estTable)
}

error = function(xx1,xx2,nobs){
  rlnorm(nobs,0,abs(xx1+xx2))-1
}

biasVariance(2,2,800,error,1000)


error = function(xx1,xx2,nobs){
  rcauchy(nobs,0,abs(xx1+xx2))
}

biasVariance(2,2,800,error,1000)


error = function(xx1,xx2,nobs){
  rlnorm(nobs)
}




a200 = biasVariance(2,2,200,error,1000)
a400 = biasVariance(2,2,400,error,1000)
a600 = biasVariance(2,2,600,error,1000)
a800 = biasVariance(2,2,800,error,1000)
a1000 = biasVariance(2,2,1000,error,1000)
a1200 = biasVariance(2,2,1200,error,1000)