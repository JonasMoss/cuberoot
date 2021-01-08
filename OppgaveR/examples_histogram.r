saves = read.csv("saves.csv",sep=",",header=TRUE)
goals = read.csv("goalssa.csv",sep=",",header=TRUE)
f_saves = saves[saves$GP>10,]
percent = f_saves$SV/f_saves$SOT

k = 14
percent = goals$G/goals$SOT
kernel = gcde(percent)
pars = nlm(function(par) -mean(dbeta(percent,par[1],par[2],log=TRUE)),c(1,1))$estimate
plot(ts,dbeta(ts,pars[1],pars[2]),type="l",col="blue",lty=2)
lines(histogram(percent,30),lty=3,col="red")
lines(histogram(percent,30,method="real"),col="blue")
plot(sde(percent,k))
lines(ts,sapply(ts,kernel))
lines(ts,dnorm(ts,mean(percent),sd(percent)))
rug(percent)

summary(lm(A~MP*G,data=goals))

par(mfrow=c(1,2))
plot(histogram(percent,30),col="red",xlab="Goals / shots on target",
     ylab="Density")
lines(ts,sapply(ts,kernel),lty=2)
rug(percent)
plot(histogram(percent,30,method="real"),col="blue",xlab=NA,ylab=NA)
rug(percent)


#######
# New #
#######

goalspl = read.csv("goalspl.csv",sep=",",header=TRUE)
goalsll = read.csv("goalsll.csv",sep=",",header=TRUE)
goalssa = read.csv("goalssa.csv",sep=",",header=TRUE)
goalsbl = read.csv("goalsbl.csv",sep=",",header=TRUE)
goals = c(goalspl$G,goalsll$G,goalssa$G,goalsbl$G)
sot = c(goalspl$SOT,goalsll$SOT,goalssa$SOT,goalsbl$SOT)
percent = goals/sot
kernel = gcde(percent)
pars = nlm(function(par) -mean(dgamma(percent,par[1],par[2],log=TRUE)),c(1,1))$estimate


par(mfrow=c(1,2))
plot(histogram(percent,7,method="smooth",type="L2",weights="KL"),
     col="red",xlab="Goals / shots on target", ylab="Density")
lines(ts,sapply(ts,kernel),lty=2)
rug(percent)
plot(histogram(percent,7,,method="smooth",type="KL",weights="KL"),
     col="blue",xlab=NA,ylab=NA)
lines(ts,dgamma(ts,pars[1],pars[2]),type="l",col="blue",lty=2)
rug(percent)

smerte = read.csv("smerte.csv",sep=",")
smert = (smerte$smerte_2)
smert2 = runif(266,smert,smert+1)
smert2 = (smert2)/(max(smert2)+.0001)
smert3 = (smert+0.00001)/100.0001
kernel = gcde(smert)
kernel2 = gcde(smert2)
kernel3 = gcde(smert3)
lines(sde(smert3,k=10))
plot(ts,sapply(ts,kernel2),type="l")
lines(ts,1/3*1/ts^(2/3),lty=2,col="red")

library("locfit")
ts = seq(0,1,by=0.001)
kirke = read.table("kirke2014.dat",header=TRUE)
kdat = kirke[,3]/1000
kdat = kdat[!is.na(kdat)]
plot(histogram(kdat,9,method="smoothed",type="L2",weights="L2"),
     xlab="Services / 1000 residents", ylab="Density", 
     rescale = 1000,xlim=c(0,86),bty="l",ylim=c(0,0.05))
grid()
rug(kdat*1000)
kernel = gcde(kdat)
lines(ts*1000,1/1000*sapply(ts,kernel),lty=2)
kkdat = (kdat*1000)
lines(locfit(~kkdat),lty=3)


