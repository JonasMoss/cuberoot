
# Comparers -------------------------------------------------------------------
compare_mse = function(n, k, rand, method1 = "greedy", 
                       method2 = "exact", N = 1000,...){
  
  compare_mse_helper = function(data){
    ests1 = histogram(data, k, method = method1, ...)
    ests2 = histogram(data, k, method = method2, ...)
    
    c(splits=sum((ests1$splits - ests2$splits)^2),
      weights=sum((ests1$weights - ests2$weights)^2))
  }
  
  replicate(N,compare_mse_helper(rand(n)))
}

estimate_mse = function(n, k, rand, method = "greedy", N = 1000,...){
  # First of, we will have to decide on a true value. Greedy works well. 
  true_ests = histogram(rand(100*N*k),k,method="greedy")
  
  compare_mse_helper = function(data){
    ests = histogram(data, k, method = method, ...)
    
    c(splits=sum((ests$splits - true_ests$splits)^2),
      weights=sum((ests$weights - true_ests$weights)^2))
  }
  
  replicate(N,compare_mse_helper(rand(n)))
}