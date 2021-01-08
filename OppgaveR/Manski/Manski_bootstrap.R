manskiboot = function(nobs=100,nboots=1000,seed=100){
  set.seed(seed)
  xx   = runif(nobs,-1,1)
  yy   = (1 + 2*xx + rnorm(nobs)) >= 0
  
  # Sample a maximum.
  solution = manski(yy,xx)$positive$solutions
  index = sample(1:length(solution),1)
  
  if (solution[[index]][2] == Inf) {
    estimate = runif(1,solution[[index]][1],rexp(1)+solution[[index]][1])
  } else {
    estimate = runif(1,solution[[index]][1],solution[[index]][2])
  }
  
  bootManski = function(){
    indices = sample(1:nobs,replace=TRUE)
    xx_new = xx[indices]
    yy_new = yy[indices]
    
    # Sample a maximum.
    solution_new = manski(yy,xx)$positive$solutions
    index_new = sample(1:length(solution),1)
    
    if (solution[[index_new]][2] == Inf) {
      estimate_new = runif(1,solution[[index_new]][1],rexp(1)+solution[[index_new]][1])
    } else {
      estimate_new = runif(1,solution[[index_new]][1],solution[[index_new]][2])
    }
    
    return(estimate_new)
  }
  
  nobs^(1/2)*(replicate(nboots,bootManski())-estimate)
}

manskidist = function(nobs=100,nboots=1000,seed=100){
  set.seed(seed)
  
  replicand = function(){
    xx   = runif(nobs,-1,1)
    yy   = (1 + 2*xx + rnorm(nobs)) >= 0  
    solution = manski(yy,xx)$positive$solutions
    index = sample(1:length(solution),1)
    estimate = solution[[index]][1]
    estimate
  }
  
  replicate(nboots,replicand())
}

manskiboot2 = function(nobs = 100,nboots = 1000,seed = 100,
                       m = nobs,indices = sample(1:nobs,m,replace=TRUE) ){
  set.seed(seed)
  xx   = runif(nobs,-1,1)
  yy   = (1 + 2*xx + rnorm(nobs)) >= 0
  
  # Sample a maximum.
  solution = manski(yy,xx)$positive$solutions
  index = sample(1:length(solution),1)

  estimate = solution[[index]][1]
  
  bootManski = function(){
    xx_new = xx[indices]
    yy_new = yy[indices]
    
    # Sample a maximum.
    solution_new = manski(yy_new,xx_new)$positive$solutions
    index_new = sample(1:length(solution_new),1)
    estimate_new = solution_new[[index_new]][1]
    return(estimate_new)
  }
  
  m^(1/3)*(replicate(nboots,bootManski())-estimate)
}

# Special bootstrap index.

bootstrap_result_bay      = manskiboot2(nobs=50000,m=100,indices=rexp(m,1))
bootstrap_result_bay_star = manskiboot2(nobs=50000,m=100,indices=rexp(m,1))
bootstrap_result_m_of_n   = manskiboot2(nobs=50000,m=100)

hist(bootstrap_result_bay)
hist(bootstrap_result_m_of_n)

plot(density(bootstrap_result,adjust=2))
sort(bootstrap_result)