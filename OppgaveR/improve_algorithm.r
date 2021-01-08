

rbeta(40,2,7)

new3 = function(data){
  # Inital quantiles.
  data = sort(data)
  n = length(data)
  q = c(0,quantile(data,(1:2)/3),1)
  
  # Loop trough possible values of q1 and maximize
  # the resulting expression wrt q2.
  
  for (q1 in data[-n]){}
}


one_dim <- function(aug,ranks){
  indices <- (ranks[1]+1):(ranks[2]-1)
  if (ranks[2]==length(aug)-1) end <- ranks[2]-1
  else end <- ranks[2]
  vals <- (indices - ranks[1])/n*log(aug[indices+1]-aug[ranks[1]+1]) +
    (end - indices)/n*log(aug[ranks[2]+1]-aug[indices+1])
  which.min(vals)+ranks[1]
}

sdeiter <- function(data,k=10,m=50,ranks=NULL){
  n = length(data)
  data = sort(data)
  if (is.null(ranks)) ranks = c(0,(n-k+2):n,n+1)
  rank_test = 0*ranks
  counter = 0
  aug = c(0,data,1)
  print(data[ranks[-c(1,k+1)]])
  while (!all(rank_test==ranks) & (counter<=m)){
    #print(data[ranks[-c(1,k+1)]])
    rank_test = ranks
    for (index in (1:(k-1)+1)){
      ranks[index] = one_dim(aug,c(ranks[index-1],ranks[index+1]))
      print(data[ranks[-c(1,k+1)]])
    }
    counter = counter + 1
  }
  q = data[ranks[-c(1,k+1)]]
  class(q) = c("sde","numeric")
  q
}


testy = function(){
  data=rbeta(40,7,2)
  q1=sde2(data)
  q2=sde(data,method="iter",k=3)
  r1=rank(data)[data==q1]
  r2=rank(data)[data==q2]
  cbind(real=sde2(data),
  sde=sde(data,method="iter",k=3),
  low=sdeiter(data,k=3,ranks=c(0,1,2,41)),
  high=sdeiter(data,k=3,ranks=c(0,39,40,41)))
}

data=rbeta(40,2,7)
plot(sdeiter(data,k=3),ylim=c(0,6))
sde_plotter(sde2(data),lines=TRUE,lty=2,col="blue")
rug(data)

cool_fun = function(q,data){
  c(a=sum(data<q[1])*log(q[1]),
    b=sum(q[1]<=data &data<q[2])*log(q[2]-q[1]),
    c=sum(q[2]<=data)*log(1-q[2]),
    sum(data<q[1])*log(q[1])+sum(q[1]<=data &data<q[2])*log(q[2]-q[1]),
    sum(q[1]<=data &data<q[2])*log(q[2]-q[1])+sum(q[2]<=data)*log(1-q[2]))
}

plot(data,sapply(data,function(q) sum(data<q)*log(q)))
