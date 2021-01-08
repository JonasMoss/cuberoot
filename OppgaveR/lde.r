attach(read.table("brown.dat",header=TRUE))

#######################
# Estimators when k=2 #
#######################

sde <- function(data){
  data <- sort(data)
  n <- length(data)
  vals <- ((1:n)/n)*(log(1-data)-log(data)) - log(1-data)
  data[which.max(vals)]
}

###################
# Brownian paths. #
###################

brown_path <- function(ts){
  # Make a brownian path from ts. We'll assume that it's ordered.
  ts <- sort(ts)
  n <- length(ts)
  diffs <- sapply((1:(n-1)),function(i) ts[i+1]-ts[i])
  vals <- rnorm(n,0,sqrt(diffs))
  cumsum(vals)
}

#N <- 100000
#ts <- seq(-3,3,by=0.01)
#paths <- matrix(replicate(N,brown_path(ts)-ts^2),length(ts),N)
#arg_maxes <- apply(paths,2,function(x) ts[which.max(x)])
#maxes <- apply(paths,2,max)
#remove(paths)
#frm <- data.frame(argmaxes=arg_maxes,maxes=maxes)
#write.table(frm,"brown.dat")

######################
# Density estimation #
######################

