rho = function(x,b){
  2*b^2 + 2.5 - sqrt(4*b^4 + 6*b^2+2.25-x^2-x/b)
}


ckde = function(data,b=NULL){
  if (is.null(b)) b = kde(data)$h
  data = sort(data)
  middle = data[2*b<=data & data <=(1-2*b)]
  left = data[0<=data & data <2*b]  
  right = data[(1-2*b)< data & data<=1]
  Vectorize(function(x){
  mean(c(dbeta(x,rho(left,b),(1-left)/b),
    dbeta(x,middle/b,(1-middle)/b),
    dbeta(x,right/b,rho(1-right,b))))
  })
}

pckde = function(data,b=NULL){
  if (is.null(b)) b = kde(data)$h
  data = sort(data)
  middle = data[2*b<=data & data <=(1-2*b)]
  left = data[0<=data & data <2*b]  
  right = data[(1-2*b)< data & data<=1]
  list(left=left,lleft=length(left),
       middle=middle,lmiddle=length(middle),
       right=right,lright=length(right),
       n=length(data))
}

rckde = function(n,pckd,b=NULL){
  if (is.null(b)) b = kde(data)$h
  select = rmultinom(1,1000,c(0.1,0.8,0.1))
  xs = sample(data,n,replace=TRUE)
  middle = pckd$middle
  left = pckd$left
  right = pckd$right
  c(rbeta(length(left),rho(left,b),(1-left)/b),
         rbeta(length(middle),middle/b,(1-middle)/b),
         rbeta(length(right),right/b,rho(1-right,b)))
}


data = rbeta(1000,1,1)
ckernel = ckde(data,b=0.1)
kernel = kde(new)
plot(qs,sapply(qs,ckernel),type="l")
lines(qs,dbeta(qs,1,1),col="blue")
lines(qs,dkde(qs,kernel),col="red")