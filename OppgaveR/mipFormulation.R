library("Rglpk")
library("microbenchmark")

estim = function(yy,xx,betamax=1000) {
  M = betamax*2*max(xx)
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
  
  c(beta1 = obj$solution[nobs+1]-obj$solution[nobs+3],
    beta2 = obj$solution[nobs+2]-obj$solution[nobs+4])
}

estimNew = function(yy,xx,betamax=100) {
  f.obj = -c(rep(1,nobs),0,0,0,0)
  f.con = mat.or.vec(nobs+4,nobs+4)
  
  M = betamax*2*max(xx)
  
  
  for (i in 1:nobs) {
    f.con[i,i]      = -M
    f.con[i,nobs+1] = xx[i,1]*(1-2*yy[i]) 
    f.con[i,nobs+2] = xx[i,2]*(1-2*yy[i])
    f.con[i,nobs+3] = -xx[i,1]*(1-2*yy[i]) 
    f.con[i,nobs+4] = -xx[i,2]*(1-2*yy[i])
  }
  
  for( i in 1:4) f.con[nobs+i,nobs+i] = 1
  
  
  f.rhs = c(-(1-2*yy),rep(betamax,4))
  f.dir = c(rep("<=",nobs+4))
  
  obj = Rglpk_solve_LP(obj = f.obj,mat = f.con,
                       dir = f.dir,rhs = f.rhs,
                       types = c(rep("B",nobs),"C","C","C","C"),
                       max=TRUE)
  
  c(beta1 = obj$solution[nobs+1]-obj$solution[nobs+3],
    beta2 = obj$solution[nobs+2]-obj$solution[nobs+4])
}

nobs = 100
xx = cbind(rnorm(nobs,0,1),rnorm(nobs,0,1))
yy = ((1 - 6*xx[,1] + 2*xx[,2]) + rnorm(nobs)) >= 0
po = estim(yy,xx,betamax=10)
ro = estimNew(yy,xx,betamax=10)
po
ro
plot(manski2d(yy,xx[,1],xx[,2]))
points(po[1],po[2])
points(ro[1],ro[2])
microbenchmark(estimNew(yy,xx,betamax=10),estim(yy,xx,betamax=10),manski2d(yy,xx[,1],xx[,2]))

# Simulations -----------------------------------------------------------------


