data = rbeta(1000,2,7)

xx = seq(0,1,by=0.005)
h = hgcde(data)*1000^(1/5-1/7)

aa = gcded(data,h=h)

fun = function(x) {
  aa(x)
  grad(dbeta,x,shape1=2,shape2=7)
}

bb = gcde(data)

fakes = sapply(xx,aa)
trues = sapply(xx,function(x) grad(dbeta,x,shape1=2,shape2=7))
plot(xx,trues,type="l",lty=2)
lines(xx,fakes)
#plot(xx,trues/fakes)

shakes = sapply(xx,bb)
plot(xx,dbeta(xx,2,7),type="l",lty=2)
lines(xx,shakes)

h = 0.2
getBias = function(x) {
  data = rbeta(100,2,7)
  kde = gcded(data,h=h)
  val = kde(x)
  val
}


expe = median(replicate(1000,getBias(.6))) - grad(dbeta,.6,shape1=2,shape2=7)

x = 0.6
trueBias = h^2*qnorm(x)*(grad(dbeta,.6,shape1=2,shape2=7)*qnorm(x) +
           0.5*dnorm(qnorm(x))*hessian(dbeta,.6,shape1=2,shape2=7))
expe

tb = function(x) {
  h^2*qnorm(x)*(grad(dbeta,x,shape1=2,shape2=7)*qnorm(x) +
                  0.5*dnorm(qnorm(x))*hessian(dbeta,x,shape1=2,shape2=7))
}

h = 0.001
getBias2 = function(x) {
  data = rbeta(50,2,7)
  kde = gcde(data,h=h)
  val = kde(x)
  val
}

expy = replicate(1000,getBias2(.6))
mean(expy,trim=0.6) - grad(dbeta,.6,shape1=2,shape2=7)
trueY = 2*h^2*dnorm(qnorm(x))*(qnorm(x)*grad(dbeta,.6,shape1=2,shape2=7) + 
                                0.5*dnorm(qnorm(x))*hessian(dbeta,.6,shape1=2,shape2=7))

tb2 = function(x) {
 2*h^2*dnorm(qnorm(x))*(qnorm(x)*grad(dbeta,x,shape1=2,shape2=7) + 
                                   0.5*dnorm(qnorm(x))*hessian(dbeta,x,shape1=2,shape2=7))
}

plot(xx,sapply(xx,tb2),type="l")

plot(xx,sapply(xx,tb),type="l")