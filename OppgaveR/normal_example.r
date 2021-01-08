xs5 <- rnorm(5,0,1)
xs10 <- rnorm(10,0,1)
xs100 <- rnorm(100,0,1)
ts <- seq(-10,10,by=0.01)
M <- function(t) -log(2*pi)-1/2*(1+t^2)
Mn <- Vectorize(function(t,xs) -log(2*pi)-1/2*mean((xs-t)^2),vectorize.args="t")

plot(ts,M(ts),type="l",main="Likelihood process")
lines(ts,Mn(ts,xs5),col="blue")
lines(ts,Mn(ts,xs10),col="purple")
lines(ts,Mn(ts,xs100),col="green")

legend("topright", 
       c("Exact","n=5","n=10","n=100"),
      lty=rep(1,4),
      lwd=rep(1,4),
      col=c("black","blue","purple","green"),
      bty="n")