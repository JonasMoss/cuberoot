library(pryr)
library(Rcpp)
library(RcppParallel)
library(microbenchmark)

# Oppgave 2 i funksjonskapittelet ---------------------------------------------
objs = mget(ls("package:base"), inherits = TRUE)
funs = Filter(is.function, objs)
formal_list = lapply(funs,formals)

argument_number = sapply(formal_list,length)
most_arguments  = names(sort(argument_number,decreasing=TRUE)[1])
zero_arguments  = sum(argument_number==0)
functions_zero  = funs[argument_number==0]
primitive_zero  = mean(sapply(functions_zero,is.primitive))
primitives      = Filter(is.primitive, objs)

# Oppgave om rekursjon over miljøer -------------------------------------------
where_all = function(name){
  envs = c()
  env = parent.frame()
  while(!identical(env,emptyenv())) {
    if (exists(name, envir = env, inherits = FALSE)){
      envs = c(envs,str(env))
    }
    env = parent.env(env)
  } 
  envs
}

get_new = function(name){
  env = parent.frame()
  while(!identical(env,emptyenv())) {
    if (exists(name, envir = env, inherits = FALSE)) break
    env = parent.env(env)
  } 
  get(name,envir=env)
}

fget = function(name,envir=parent.frame(),inherits=TRUE){
  fun = get(name,envir=envir,inherits=inherits)
  if (!is.null(fun) && is.function(fun)) fun
  else NULL
}

exists_new = function(name,env=parent.frame(),inherits=FALSE){
  if (!inherits) return(name %in% ls(env))
  else {
    while(!identical(env,emptyenv()))
      if (name %in% ls(env)) return(name %in% ls(env))
      else env = parent.env(env)
  }
  FALSE
}

# Funksjonermiljøer -----------------------------------------------------------

n = 1000000
xx = runif(n,0,1)
yy = 2*sample(1:n,n)/(n*(n+1))

microbenchmark("sum" = sum(xx*yy),
               "crossprod" = crossprod(xx,yy),
               "%*%" = t(xx) %*% yy)

# Rcpp testing ----------------------------------------------------------------

# The two main functions are sourceCpp and cppFunction. sourceCpp builds
# external .cpp files, while cppFunction allows inline definitions of C++
# functions. Sweet!

sourceCpp("rcpp_examples.cpp")

fibr1 = function(n){
  if (n==0) return(0)
  if (n==1) return(1)
  fibr1(n-1)+fibr1(n-2)
}

fibr2 = function(n){
  val1 = 0
  val2 = 1
  temp = 0
  for (i in seq_len(n)){
    temp = val2
    val2 = val1 + val2
    val1 = temp
  }
  val1
}

c2 = sapply(0:46,function(x) median(microbenchmark(fibc2(x))$time))
r2 = sapply(0:46,function(x) median(microbenchmark(fibr2(x))$time))
