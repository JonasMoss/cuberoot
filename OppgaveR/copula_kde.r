# Here the smoothing parameter h is equal to 1 - rhp^2.
# Sigma[1,2]*(X2)
# 1 - Sigma[1,2]^2

# Functions for the Gaussian copula -------------------------------------------
rgc = function(n,X,rho){
  X = X - 0.000001
  N = length(X)
  X = qnorm(X)
  xs = crnorm(n,rho*X,rep(sqrt(1-rho^2),times=N),1882)
  pnorm(xs)
}

dgc = Vectorize(function(x,X,rho){
  inside = rho^2*(qnorm(x)^2+qnorm(X)^2)-2*rho*qnorm(x)*qnorm(X)
  1/sqrt(1-rho^2)*exp(-inside/(2*(1-rho^2)))
})

# Gaussian cupula KDE functions -----------------------------------------------
hgcde = function(data,lims=NULL){
  if (!is.null(lims)) data = (data-lims[1])/lims[2]
  trans = qnorm(data)
  trans = trans[trans != Inf & trans != -Inf]
  s = sd(trans)
  m = mean(trans)
  n = length(data)
  min(s*(2*m^2*s^2+3*(1-s^2)^2)^(-1/5)*n^(-1/5),0.5)
}

gcde = function(data,h=NULL,lims=NULL){
  if (is.null(h)) h = hgcde(data)
  if (!is.null(lims)) data = (data-lims[1])/lims[2]
  else lims = c(0,1)
  trans = qnorm(data)
  data = data[trans != Inf & trans != -Inf]
  rho = 1-h^2
  function(x){
    mean(dgc((x-lims[1])/lims[2],data,rho))/lims[2]
  }
}

hgcde = function(data,lims=NULL){
  if (!is.null(lims)) data = (data-lims[1])/lims[2]
  trans = qnorm(data)
  trans = trans[trans != Inf & trans != -Inf]
  s = sd(trans)
  m = mean(trans)
  n = length(data)
  min(s*(2*m^2*s^2+3*(1-s^2)^2)^(-1/5)*n^(-1/5),0.5)
}

rgcde = function(n,data,h=NULL,lims=NULL){
  if (!is.null(lims)) data = (data-lims[1])/lims[2]
  else lims = c(0,1)
  if (is.null(h)) h = hgcde(data)
  samples = sample(data,n,replace=TRUE)
  rho = 1-h^2
  (rgc(n,samples,rho))*lims[2]+lims[1]
}

