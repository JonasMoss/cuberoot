"""
sde2p <- function(data,q,n){
  data <- sort(data)
  m = length(data)
  combos <- combn(data,2)
  ranks <- combn(1:m,2)
  vals <- ranks[1,]/n*log(combos[1,]) + 
    ((ranks[2,])-(ranks[1,]))/n*log(combos[2,]-combos[1,]) + 
    (1-(ranks[2,])/n)*log(q-combos[2,])
  combos[,which.min(vals)]
}

plot(ts,ts,col='blue')

"""