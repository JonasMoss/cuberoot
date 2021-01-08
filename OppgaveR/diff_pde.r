diffkde = function(x,data,h=0.01){
  sum(sapply(-10:10,function(k) dnorm(x,2*k+data,h)+dnorm(x,2*k-data,h)))/length(data)
}

prokde = function(data){
  trans=qnorm(data)
  kern = kde(trans)
  list(pnorm(kern$eval.points),kern$estimate/dnorm(kern$eval.points))
}



a = 1
b = 1
data = rbeta(1000,a,b)
xs = seq(0,1,by=0.01)

plot(prokde(data)[[1]],prokde(data)[[2]],col="blue",type="l")
lines(xs,dbeta(xs,a,b),type="l")



ys = sapply(xs,diffkde,data=data,h=h)
plot(xs,dbeta(xs,2,7),type="l")
lines(xs,ys,col="blue")