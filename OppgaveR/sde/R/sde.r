# This library is used for constrained optimization, specifically in the context of
# finding the true parameter given a probability distribution. 

library(alabama)

###############################################################
# Real maximizer, k=2,3,4,5. These get very slow very quickly #
###############################################################

sde1 <- function(data){
  data <- sort(data)
  n <- length(data)
  vals <- ((1:n)/n)*(log(1-data)-log(data)) - log(1-data)
  data[which.max(vals)]
}

sde2 <- function(data){
  data <- sort(data)
  n <- length(data)
  combos <- combn(data,2)
  ranks <- combn(1:n,2)
  vals <- ranks[1,]/n*log(combos[1,]) + 
          ((ranks[2,])-(ranks[1,]))/n*log(combos[2,]-combos[1,]) + 
          (1-(ranks[2,])/n)*log(1-combos[2,])
  combos[,which.min(vals)]
}

sde3 <- function(data){
  data <- sort(data)
  n <- length(data)
  combos <- combn(data,3)
  ranks <- combn(1:n,3)
  vals <- ranks[1,]/n*log(combos[1,]) + 
    ((ranks[2,])-(ranks[1,]))/n*log(combos[2,]-combos[1,]) + 
    ((ranks[3,])-(ranks[2,]))/n*log(combos[3,]-combos[2,]) +
    (1-(ranks[3,])/n)*log(1-combos[3,])
  combos[,which.min(vals)]
}

sde4 <- function(data){
  data <- sort(data)
  n <- length(data)
  combos <- combn(data,4)
  ranks <- combn(1:n,4)
  vals <- ranks[1,]/n*log(combos[1,]) + 
    ((ranks[2,])-(ranks[1,]))/n*log(combos[2,]-combos[1,]) + 
    ((ranks[3,])-(ranks[2,]))/n*log(combos[3,]-combos[2,]) +
    ((ranks[4,])-(ranks[3,]))/n*log(combos[4,]-combos[3,]) +
    (1-(ranks[4,])/n)*log(1-combos[4,])
  combos[,which.min(vals)]
}

##################################################
## The main iterative maximizer for the moment. ##
##################################################

one_dim <- function(aug,ranks){
  indices <- (ranks[1]+1):(ranks[2]-1)
  if (ranks[2]==length(aug)-1) end <- ranks[2]-1
  else end <- ranks[2]
  vals <- (indices - ranks[1])/n*log(aug[indices+1]-aug[ranks[1]+1]) +
          (end - indices)/n*log(aug[ranks[2]+1]-aug[indices+1])
  which.min(vals)+ranks[1]
}

sde <- function(data,k=2,m=50){
  n = length(data)
  data = sort(data)
  ranks = c(0,seq(1,n,len=k-1),n+1)
  rank_test = 0*ranks
  counter = 0
  aug = c(0,data,1)
  while (!all(rank_test==ranks) & (counter<=m)){
    #print(data[ranks[-c(1,k+1)]])
    rank_test = ranks
    for (index in (1:(k-1)+1)){
      ranks[index] = one_dim(aug,c(ranks[index-1],ranks[index+1]))
    }
    counter = counter + 1
  }
  data[ranks[-c(1,k+1)]]
}

######################################
## Subsampling smoothing. ############
######################################

sdesub = function(data,k=10,alpha=0.5){
  n = length(data)
  rowMeans(replicate(1000,sde(sample(data,alpha*n),k)))
}

########################################
## Copula kernel smoothing. ############
########################################

sdegc = function(data,k,K=length(data)*1000){
  sde(rgcde(K,data),k)
}

######################################
## SDE plotters.          ############
######################################
sde_plotter <- function(q,lims=c(0,1),lines=FALSE,...){
  k <- length(q)+1
  q <- c(lims[1],q,lims[2])
  ys <- c(0,1/k*sapply(1:k,function(i) 1/(q[i+1]-q[i])))
  if (lines) {
    lines(q,ys,type="S",...)
    lines(q,ys,type="h",...)
  }
  else {
    plot(q,ys,type="S",...)
    lines(q,ys,type="h",...)
  }
}
sde_lines = function(q,lims=c(0,1),...) sde_plotter(q,lims,lines=TRUE,...)

######################################
## Find true values. #################
######################################

sde_objective = function(q,Pn,k=2,...){
  q_aug <- c(0,q,1)
  dis <- sapply(1:k,function(i) log(q_aug[i+1]-q_aug[i]))
  probs <- sapply(1:k,function(i) Pn(q_aug[i+1],...)-Pn(q_aug[i],...))
  sum(probs*dis)
}

sde_true <- function(P,Q=NULL,k,...){
  if (is.null(Q)) q0 = ((seq(0,1,length=k+1))[-1])[-k]
  q0 = Q(c(1:(k-1))/k,...)
  hin = function(x,...){
    y = c(0,x,1)
    h = sapply(1:k,function(i) y[i+1]-y[i])
    h = c(h,x)
    h
  }
  
  constrOptim.nl(q0,sde_objective,hin=hin,Pn=P,k=k,control.outer=list(trace=FALSE),...)$par
}
q_true <- sde_true(pbeta,qbeta,10,shape1=2,shape2=7)

######################################
### Testing: Performance vs q_true ###
######################################

sde_iter_test = function(){
  N <- 10000

  q_true_3 <- sde_true(pbeta,qbeta,3,shape1=2,shape2=7)$par
  q_true_4 <- sde_true(pbeta,qbeta,3,shape1=2,shape2=7)$par

  repeatit = function(n=100){
    data = rbeta(n,2,7)
    est1 = sde2(data)
    est2 = sde_iter(data,k=3)
    sqrt(c(sum((est1-q_true)^2),sum((est2-q_true)^2)))
  }

  results <- data.frame(t(replicate(N,repeatit())))
  names(results)<-c("True","Algorithm")
  results <- melt(results,variable.name="estimator",value.name="value")
  boxplot(value~estimator,results)
  data = rbeta(100,2,7)
  microbenchmark(sde_iter(data,k=3),sde2(data))
  microbenchmark(sde_iter(data,k=4),sde3(data))

  ts <- seq(0.01,data[9],len=1000)
  plot(ts,sapply(ts,function(k) sde_objective(c(k,data[9]),ecd,k=3)),type="l")
  
  N = 100
  ns <- (10:30)

  repeatit = function(n=100){
    data = rbeta(n,2,7)
    est1 = sde2(data)
    est2 = sde_iter(data,k=3)
    sum((est1-est2)^2)
  }

 
  diffss <- sapply(ns,function(n) mean(replicate(N,repeatit(n))))
  names(diffss) <- ns

  means <- colMeans(diffs)
  quants <- apply(diffs,2,quantile, probs=c(0.25,0.75))
  names(means) <- ns

  data <- sort(rbeta(100,2,7))
  sde(data)
  data[one_dim(c(0,data,1),c(0,101))]
}