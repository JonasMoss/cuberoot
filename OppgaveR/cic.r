
cic = function(data,tries=3:(length(data)/10)){
  max = -Inf
  index = 0
  n = length(data)
  for (i in tries){
    q_hat = sde_iter(data,i)
    sic = -n*log(i) + n*(Pnm(q_hat,data)-subbias(data,b=n/2,k=i))
    print(sic)
    if (max < sic) {
      max = sic
      index = i
    }
  }
  index
}

biasfun = function(data,tries=3:(length(data)/10)){
  n = length(data)
  sapply(tries,function(i) subbias(data,b=n/2,k=i))
}

ys = mapply(function(x,y) biasfun(rbeta(1000,x,y),tries=3:200),c(1,2,3,4,5,6),c(1/3,7,6,1/2,5,10))
ys2 = n^(2/3)*as.vector(ys)
xs = rep(3:200,6)
gs = seq(3,50,by=0.01)
lm(ys2~I(log(xs)))
lm(ys2~xs+I(xs^2))
lm(ys2~xs+I(log(xs)))
plot(xs,ys2)
lines(gs,-3.834+3.589*log(gs))
lines(gs,-2.50+0.04*gs+2.778*log(gs))
lines(gs,0.44+0.4*gs-0.004*gs^2,col="blue")
