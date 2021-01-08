nobs   = 10000
xx    = runif(nobs)
beta0 = .1
beta1 = .6
eps   = rnorm(nobs)
yy    = (beta0 + xx*beta1 + eps) >= 0

glm(yy~xx,family=binomial(link="probit"))