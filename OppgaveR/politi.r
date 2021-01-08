dat <- read.csv2("politi.js",dec=".")
attach(dat)
qs <- seq(0,1,by=0.001)
plot(white,police_white)
hist(white,freq=FALSE)
lines(density(white))
hist(police_white,freq=FALSE)
lines(density(police_white))

police_white_prime <- police_white[police_white<100&police_white>0]
white_dist <- (white + runif(755,0,0.0001))/100
police_white_prime_dist <- (police_white_prime + runif(length(police_white_prime),0,0.0001))/100

max_white <- function(p) -mean(dbeta(white/100,p[1],p[2],log=TRUE))
max_police_white <- function(p) -mean(dbeta(police_white_prime/100,p[1],p[2],log=TRUE))
est1 <- nlm(max_white,c(2,5))$estimate
est2 <- nlm(max_police_white,c(2,5))$estimate

#hist(white/100,freq=FALSE,breaks=20)
plot(qs,dbeta(qs,est1[1],est1[2]),type="l",ylim=c(0,2))
sde_plotter(sde_iter(white_dist,k=9),lines=TRUE,col="blue")


sest = round(est2,3)
#hist(police_white_prime/100,freq=FALSE,breaks=20,ylim=c(0,3))
plot(qs,dbeta(qs,est2[1],est2[2]),type="l",ylim=c(0,3),lty=2,xlab="Percentage of white officers",
     ylab="Density",sub=paste0("Beta estimates: ", sest[1], ", ", sest[2]),
     bty="l")
grid()
lines(histogram(police_white_prime_dist,k = 6, weights="KL",method="smooth"))
lines(histogram(police_white_prime_dist,k = 6, weights="KL",method="exact"),lty=3)
rug(police_white_prime_dist)
