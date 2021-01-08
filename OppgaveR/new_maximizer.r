
augdata = c(0,data,1)


tsde = function(data,k){
  data = sort(data)
  augdata = c(0,data,1)
  p1 = Vectorize(function(i,j){
    -(j-i)*log(augdata[j+1]-augdata[i+1])
  })
  
  p = outer(0:(n+1),0:(n+1),p1)
  colnames(p) = 0:(n+1)
  rownames(p) = 0:(n+1)
  
  mapply(p1,combn(n+2,2)[1,]-1,combn(n+2,2)[2,]-1)
  
    
}

fine = function(i,j) {
  sapply((i+1):(j-1),function(k) p1(i,k) + p1(k,j))
}

check = function(i,j) which.max(fine(i,j))+i

p = outer(0:(n+1),0:(n+1),p1)
colnames(p) = 0:(n+1)
rownames(p) = 0:(n+1)

Dmax = 10

### When D = 2.
### Find the maximum of p1(0,j) + p1(j,1)

data[which.max(sapply(1:n,function(j) p[0+1,j+1] + p[j+1,n+1+1]))]
sde1(data)

### D = 3.

best2 = function(i){
  ## The ranks are in dataland.
  vals = sapply(1:(i-1),function(j) p[1,j+1] + p[j+1,i+1]) #ps are in augland.
  list(which.max(vals),max(vals,na.rm=TRUE))
}

nsde2 = function(data){
  vals = sapply(2:n,function(i) {
         best = best2(i)
         cbind(best[[1]],p[i+1,n+1+1] + best[[2]])})
  
  max_index = 1+which.max(vals[2,])
  c(data[vals[1,max_index-1]],data[max_index])
}
nsde2(data)
sde2(data)