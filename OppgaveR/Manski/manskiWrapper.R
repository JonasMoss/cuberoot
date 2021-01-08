sourceCpp("manskiAlgorithm(1d).R")
sourceCpp("manskiAlgorithm(2d).R")

mms = function(formula,weights=NULL) {
  vars = all.vars(formula)
  resp = eval(parse(text=vars[1]))
  
  if ( is.null(weights) ) weights = rep(1,length(resp))
  
  if ( length(vars) == 2 ) {
    xx = eval(parse(text=vars[2]))
    return (manski1d(resp,xx,weights=weights))
  } else {
    xx1 = eval(parse(text=vars[2]))
    xx2 = eval(parse(text=vars[3]))
    return(manski2d(resp,xx1,xx2,weights=weights))
  }
}

print.manski = function(obj) {
  cat("***-----------------------------*** \n")
  cat("*** Manski's maximum score      *** \n")
  cat("***-----------------------------***")
  if("manski1d" %in% class(obj)) {
    cat("\n   Covariates: 1")    
  } else {
    cat("\n   Covariates: 2")      
  }
  cat("\n   Objective:",obj$maxWeight)
  cat("\n   Sum of weights:",obj$total)
  cat("\n   Solution edges: \n")
  print(as.data.frame(obj$points))
}
