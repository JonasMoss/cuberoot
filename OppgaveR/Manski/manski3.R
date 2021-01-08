# Make data. ------------------------------------------------------------------
nobs = 100
xx = runif(nobs,-1,1)
yy = (1 + 2*xx + rnorm(nobs)) >= 0

# Coolfun ---------------------------------------------------------------------

coolfun = function(xx,yy,betamax=10){
  # Dette er maksverdien som får alle ziene til å funke.
  nobs = length(xx)
  M = 60
  f.obj = c(rep(1,nobs),0,0)
  f.con = mat.or.vec(nobs+2,nobs+2)
  
  for (i in 1:nobs) {
    f.con[i,i]      = M
    f.con[i,nobs+1] = xx[i]*(1-2*yy[i]) 
    f.con[i,nobs+2] = -xx[i]*(1-2*yy[i]) 
  }
  
  for( i in 1:2) f.con[nobs+i,nobs+i] = 1
  
  
  f.rhs = c(M-(1-2*yy),rep(betamax,2))
  f.dir = c(rep("<=",nobs+2))
  
  obj = Rglpk_solve_LP(obj = f.obj,mat = f.con,
                       dir = f.dir,rhs = f.rhs,
                       types = c(rep("B",nobs),"C","C"),
                       max=TRUE)
  
  obj$solution[nobs+1]-obj$solution[nobs+2]
}

coolfun(xx,yy)

# Betafun ---------------------------------------------------------------------
betafun = function(xx,yy){
  nobs = length(xx)
  zz = cbind(rep(NA,nobs),rep(NA,nobs))
  for (i in 1:nobs) {
    if (yy[i]) {
      if (xx[i] >= 0) zz[i,] = c(-1/xx[i],Inf)
      else zz[i,] = c(-Inf,-1/xx[i])
    } 
    else {
      if (xx[i] >= 0) zz[i,] = c(-Inf,-1/xx[i])
      else zz[i,] = c(-1/xx[i],Inf)
    }
  }
  zz
}

betafun(xx,yy)

# Objective -------------------------------------------------------------------
objective = function(xx,yy,beta) {
  sum((2*yy-1)*((xx*beta)>=-1))
}


# Smoobjective ----------------------------------------------------------------
smoobjective = function(xx,yy,beta) {
  sum((2*yy-1)*exp(10*(xx*beta+1))/(1+exp(10*(xx*beta+1))))
}

nobs = 30
xx = runif(nobs,-1,1)
yy = (1 + 2*xx + rnorm(nobs)) >= 0
betas = betafun(xx,yy)
betas = c(betas[,1],betas[,2])
betas = betas[betas!=Inf & betas!=-Inf]

plot(betas,sapply(betas,function(beta) objective(xx,yy,beta)))
lines(betas,sapply(betas,function(beta) smoobjective(xx,yy,beta)),type="l")
