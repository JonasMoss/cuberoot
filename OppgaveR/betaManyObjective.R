

for (i in 1:10){
shape1 = 1/i
shape2 = 1/i

shitfun1 = function(a){
  log(1/(2*a))*pbeta(a,shape1,shape2)+log(1/(2*(1-a)))*(1-pbeta(a,shape1,shape2))
}

data = rbeta(100,shape1,shape2)
Pn = ecdf(data)

shitfun2 = function(a){
  +log(1/(2*a))*Pn(a)+log(1/(2*(1-a)))*(1-Pn(a))
}

emp = sapply(hh,shitfun2)
emp[is.nan(emp)] = NA
plot(hh,sapply(hh,shitfun1),typ="l",ylim=c(min(emp,na.rm=TRUE),max(emp,na.rm=TRUE)))

lines(hh,emp,type="s")
}

shape1 = 1/10
shape2 = 1/10

shitfun1 = function(a){
  log(1/(2*a))*pbeta(a,shape1,shape2)+log(1/(2*(1-a)))*(1-pbeta(a,shape1,shape2))
}

data = rbeta(1000,shape1,shape2)
Pn = ecdf(data)

shitfun2 = function(a){
  +log(1/(2*a))*Pn(a)+log(1/(2*(1-a)))*(1-Pn(a))
}

library("RColorBrewer")
cols = brewer.pal(4,"Set2")
hh = seq(0,by=0.00001)
shape1 = 1/10
shape2 = 1/10
plot(hh,sapply(hh,shitfun1),type="l",xlab="x",ylab="Density",bty="l",col=cols[1],
     main=expression(beta(alpha,alpha)),xlim=c(-0.1,1.1),ylim=c(-0.5,0.5))
shape1 = 1/4
shape2 = 1/4
lines(hh,sapply(hh,shitfun1),type="l",xlab="x",ylab="Density",bty="l",col=cols[2],
     main=expression(beta(alpha,alpha),xlim=c(-0.1,1.1),ylim=c(-0.1,1.4)))
shape1 = 1/2
shape2 = 1/2
lines(hh,sapply(hh,shitfun1),type="l",xlab="x",ylab="Density",bty="l",col=cols[3],
      main=expression(beta(alpha,alpha),xlim=c(-0.1,1.1),ylim=c(-0.1,1.4)))
shape1 = 1/3
shape2 = 1/3
lines(hh,sapply(hh,shitfun1),type="l",xlab="x",ylab="Density",bty="l",col=cols[4],
      main=expression(beta(alpha,alpha),xlim=c(-0.1,1.1),ylim=c(-0.1,1.4)))
legend("top",legend=c("1/10","1/4","1/2","1/3"),lty=c(1,1,1,1),col=cols,bty="n")

shape1 = 1/10
shape2 = 1/10
plot(hh,dbeta(hh,shape1,shape2),type="l",xlab="x",ylab="Density",bty="l",col=cols[1],
     main=expression(beta(alpha,alpha)),xlim=c(-0.001,1.001),ylim=c(0,2))
shape1 = 1/4
shape2 = 1/4
lines(hh,dbeta(hh,shape1,shape2),type="l",xlab="x",ylab="Density",bty="l",col=cols[2],
      main=expression(beta(alpha,alpha),xlim=c(-0.1,1.1),ylim=c(-0.1,1.4)))
shape1 = 1/2
shape2 = 1/2
lines(hh,dbeta(hh,shape1,shape2),type="l",xlab="x",ylab="Density",bty="l",col=cols[3],
      main=expression(beta(alpha,alpha),xlim=c(-0.1,1.1),ylim=c(-0.1,1.4)))
shape1 = 1/3
shape2 = 1/3
lines(hh,dbeta(hh,shape1,shape2),type="l",xlab="x",ylab="Density",bty="l",col=cols[4],
      main=expression(beta(alpha,alpha),xlim=c(-0.1,1.1),ylim=c(-0.1,1.4)))
legend("top",legend=c("1/10","1/4","1/2","1/3"),lty=c(1,1,1,1),col=cols,bty="n")


#lines(hh,emp,type="s",col="red",lty=2)