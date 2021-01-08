library("lpSolve")

# Manski ----------------------------------------------------------------------
lpsolvetest = function(nobs=100){
  xx = cbind(runif(nobs,-1,1),runif(nobs,-1,1))
  yy = ((1 + 2*xx[,1] + 3*xx[,2]) + rnorm(nobs)) >= 0

  # Dette er maksverdien som får alle ziene til å funke.

  M = 20
  f.obj = c(rep(1,nobs),0,0)
  f.con = mat.or.vec(nobs,nobs+2)

  for (i in 1:nobs) {
    f.con[i,i]      = M
    f.con[i,nobs+1] = xx[i,1]*(1-2*yy[i]) 
    f.con[i,nobs+2] = xx[i,2]*(1-2*yy[i])  
  }

  f.rhs = c(M-(1-2*yy))
  f.dir = c(rep("<=",nobs))

  obj = lp(direction = "max",
         f.obj,f.con,f.dir,f.rhs,
         binary.vec=1:nobs)

  c(obj$solution[1:2 + nobs],obj$objval)
}

# Testing ---------------------------------------------------------------------

# Objective: x + y
# -2x + 2y >= 1
# -8x + 10y <= 13
# x,y in Z

f.obj = c(1,1)
f.con = t(matrix(c(-2,2,-8,10),2,2))
f.dir = c(">=","<=")
f.rhs = c(1,13)

obj = lp(direction = "max",
         f.obj,f.con,f.dir,f.rhs,
         int.vec=1:2)

