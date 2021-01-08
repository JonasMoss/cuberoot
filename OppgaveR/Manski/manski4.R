
# Manski ----------------------------------------------------------------------
rglpktest = function(nobs=100,betamax=10){
  xx = cbind(runif(nobs,-1,1),runif(nobs,-1,1))
  yy = ((1 - 6*xx[,1] + 2*xx[,2]) + rnorm(nobs)) >= 0
  
  # Dette er maksverdien som får alle ziene til å funke.
  
  M = 60
  f.obj = c(rep(1,nobs),0,0,0,0)
  f.con = mat.or.vec(nobs+4,nobs+4)
  
  for (i in 1:nobs) {
    f.con[i,i]      = M
    f.con[i,nobs+1] = xx[i,1]*(1-2*yy[i]) 
    f.con[i,nobs+2] = xx[i,2]*(1-2*yy[i])
    f.con[i,nobs+3] = -xx[i,1]*(1-2*yy[i]) 
    f.con[i,nobs+4] = -xx[i,2]*(1-2*yy[i])
  }
  
  for( i in 1:4) f.con[nobs+i,nobs+i] = 1
  
  
  f.rhs = c(M-(1-2*yy),rep(betamax,4))
  f.dir = c(rep("<=",nobs+4))
  
  obj = Rglpk_solve_LP(obj = f.obj,mat = f.con,
                       dir = f.dir,rhs = f.rhs,
                       types = c(rep("B",nobs),"C","C","C","C"),
                       max=TRUE)
  
  obj2 = Rglpk_solve_LP(obj = f.obj,mat = f.con,
                       dir = f.dir,rhs = f.rhs,
                       types = c(rep("C",nobs),"C","C","C","C"),
                       max=TRUE)
  
  c(beta1 = obj$solution[nobs+1]-obj$solution[nobs+3],
    beta2 = obj$solution[nobs+2]-obj$solution[nobs+4],
    beta1a = obj2$solution[nobs+1]-obj2$solution[nobs+3],
    beta2a = obj2$solution[nobs+2]-obj2$solution[nobs+4]
    )
}

rglpktest(100)