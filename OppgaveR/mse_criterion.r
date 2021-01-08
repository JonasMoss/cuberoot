f_hat <- Vectorize(function(x,q){
  k = length(q)+1
  q_aug = c(0,q,1)
  q_i = tail(q_aug[q_aug <=x],1)
  q_j = head(q_aug[q_aug >x],1)
  1/((q_j-q_i)*k)
},vectorize.args=c("x"))

mse_crit = function(data,range=3:10,grid=seq(0,1,length=1000)){
  kernel = gcde(data)
  q = sde(data,k=k)
  m = length(grid)
  
  ### Estimation of the bias. This probab
  mean(sapply(grid[-c(1,m)],function(x) (kernel(x)-f_hat(x,q))^2))
  
  ### Estimation of the variance.
  
}

####################
## Example fun. ####
####################

P = Vectorize(function(x) {
  if (x<=0.5) x+1/(4*pi)*(1-cos(4*pi*x))
  else x                
})

P = Vectorize(function(x) {
  x+1/(4*pi)*(1-cos(4*pi*x))       
})

f = function(x){
  1 + sin(4*pi*x)
}

Q
