xxAdd = rnorm(nobs,1,1)
xx = rnorm(nobs,0,1)
yy = ( xxAdd - xx + rnorm(nobs)) >= 0

(obj = manski1d(yy, xx, xxConst = xxAdd,type = "pos"))


bvHorowitz = function(beta, nobs, error = function(xxAdd,xxCov,nobs) rnorm(nobs),
                      distAdd = function(x) rnorm(x,1,1), 
                      distCov = rnorm, Nreps = 1000){
  # We calculate the bias and variance for Manski, probit and logit.
  
  estimateThem = function(yy,xxAdd,xxCov){
    
    logitMod  = glm(yy~0+xxCov + xxAdd,family=binomial(link=logit))  
    probitMod = glm(yy~0+xxCov + xxAdd,family=binomial(link=probit)) 
    logit1    = coef(logitMod)[1]/coef(logitMod)[2]
    probit1   = coef(probitMod)[1]/coef(probitMod)[2]
    manskiMod = manski1d(yy, xxCov, xxConst = xxAdd, type = "pos")
    manski1   = coef(manskiMod)[1]
    
    betas     = c(logit1, probit1, manski1)
    
    return(betas)
  }
  estimates = c()
  
  for (i in 1:Nreps){
    xxAdd = distAdd(nobs)
    xxCov = distCov(nobs)
    yy    = (xxAdd + beta*xxCov + error(xxAdd,xx,nobs))>=0
    estimates = rbind(estimates,estimateThem(yy,xxAdd,xxCov))
  }
  
  colnames(estimates) = c("Logit","Probit","Manski")
  
  return(estimates)
}


error = function(xxAdd,xxCov,nobs) rnorm(nobs)
distAdd = function(x) rnorm(x,1,1)
distAdd = function(x) rep(1,nobs)

######################
# Simulations for L. #
######################

error = function(xxAdd,xxCov,nobs) rlogis(nobs,0,sqrt(3/pi^2))
distAdd = function(x) rnorm(x,1,1)
distCov = function(x) rnorm(x,0,1)


beta = 1
L250b1  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
L500b1  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
L1000b1 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)

beta = 0.1
L250b01  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
L500b01  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
L1000b01 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)

beta = 5
L250b5  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
L500b5  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
L1000b5 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)

apply(L250b01,2,var)
apply(L250b1,2,var)
apply(L250b5,2,var)


######################
# Simulations for U. #
######################

error = function(xxAdd,xxCov,nobs) runif(nobs,-sqrt(3),sqrt(3))
distAdd = function(x) rnorm(x,1,1)
distCov = function(x) rnorm(x,0,1)


beta = 1
U250b1  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
U500b1  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
U1000b1 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)

beta = 0.1
U250b01  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
U500b01  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
U1000b01 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)

beta = 5
U250b5  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
U500b5  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
U1000b5 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)


#######################
# Simulations for T3. #
#######################


error = function(xxAdd,xxCov,nobs) rt(nobs,3)/sqrt(3)
distAdd = function(x) rnorm(x,1,1)
distCov = function(x) rnorm(x,0,1)


beta = 1
T250b1  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10)
T500b1  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
T1000b1 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)

beta = 0.1
T250b01  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
T500b01  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
T1000b01 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)

beta = 5
T250b5  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
T500b5  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
T1000b5 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)


#######################
# Simulations for H.  #
#######################

error = function(xxAdd,xxCov,nobs) 1/4*(1 + 2*(xxAdd + xxCov)^2 + (xxAdd + xxCov)^4)*rlogis(nobs,0,sqrt(3/pi^2))
distAdd = function(x) rnorm(x,1,1)
distCov = function(x) rnorm(x,0,1)


beta = 1
H250b1  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
H500b1  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
H1000b1 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)

beta = 0.1
H250b01  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
H500b01  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
H1000b01 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)

beta = 5
H250b5  = bvHorowitz(beta, nobs = 250 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
H500b5  = bvHorowitz(beta, nobs = 500 ,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)
H1000b5 = bvHorowitz(beta, nobs = 1000,distAdd = distAdd, error = error, distCov = distCov, Nreps = 10000)


################
# Combine data #
################
biasTable1 = c()
biasTable1 = rbind(biasTable1,apply(L250b1 ,2,bias))
biasTable1 = rbind(biasTable1,apply(L500b1 ,2,bias))
biasTable1 = rbind(biasTable1,apply(L1000b1,2,bias))
biasTable1 = rbind(biasTable1,apply(U250b1 ,2,bias))
biasTable1 = rbind(biasTable1,apply(U500b1 ,2,bias))
biasTable1 = rbind(biasTable1,apply(U1000b1,2,bias))
biasTable1 = rbind(biasTable1,apply(T250b1 ,2,bias))
biasTable1 = rbind(biasTable1,apply(T500b1 ,2,bias))
biasTable1 = rbind(biasTable1,apply(T1000b1,2,bias))
biasTable1 = rbind(biasTable1,apply(H250b1 ,2,bias))
biasTable1 = rbind(biasTable1,apply(H500b1 ,2,bias))
biasTable1 = rbind(biasTable1,apply(H1000b1,2,bias))
round(biasTable1,5)




