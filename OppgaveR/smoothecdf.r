##########################
# Piecewise linear ecdf. #
##########################

lecdf <- function (data) {
  data <- sort(data)
  n <- length(data)
  augdata <- c(0,data)
  bs <- c(sapply(1:n,function(i) 1/(augdata[i+1]-augdata[i])),0)
  retfun <- function(x) {
    a = sum(as.integer(data<=x))/n
    index <- max(which(augdata<=x))
    a + pbeta((x-augdata[index])*bs[index],3/2,3/2)/n
  }
  Vectorize(retfun)
}

lcd <- lecdf(data)
plot(qs,lcd(qs),type="l")
#lines(qs,ecd(qs),type="l",col="blue")

sde_objective <- function(q,Pn){
  Pn(q)*(log(1-q)-log(q))-log(1-q)
}

ts <- seq(0,1,by=0.001)
k <- 2
n <- 20
a <- 2
b <- 7

data <- sort(rbeta(n,a,b))
lcd <- lecdf(data)
ecd <- ecdf(data)
q <- quantile(data,c(1:(k-1))/k)

lcd_hat <- optim(q,sde_objective,Pn=lcd)$par
ecd_hat <- optim(q,sde_objective,Pn=ecd)$par

plot(ts,sde_objective(ts,lcd),type="l")
lines(ts,sde_objective(ts,ecd),col="blue")
(lcd_hat <- optim(q,sde_objective,Pn=lcd)$par)
(ecd_hat <- optim(q,sde_objective,Pn=ecd)$par)
abline(v=lcd_hat,col="green")
abline(v=ecd_hat,col="red")
