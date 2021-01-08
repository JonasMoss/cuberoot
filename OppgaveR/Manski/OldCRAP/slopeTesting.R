xs = seq(-10,10,by=0.1)


par(mfrow=c(2,1))


# Here is a slope of opposite sign and blue.
a1 = 1/2
b1 = 3
a2 = -2
b2 = 5
sol = (b2-b1)/(a1-a2)
plot(xs,a1*xs+b1,type="l",col="black",xlab=NA,ylab=NA,
     xlim=c(-10,10),ylim=c(-10,10),axes=0)
lines(xs,a2*xs+b2,col="blue")
polygon(c(xs,rev(xs)),c(a2*xs+b2,a2*rev(xs)+b2-100),
        col=adjustcolor("skyblue",alpha.f=0.3),border = NA)
points(sol,a1*sol+b1,col="darkblue",pch=21,bg="darkblue")

# Here is a slope of opposite sign and blue - red!
a1 = 1
b1 = 3
a2 = 3
b2 = 1
sol = (b2-b1)/(a1-a2)
plot(xs,a1*xs+b1,type="l",col="black",xlab=NA,ylab=NA,
     xlim=c(-10,10),ylim=c(-10,10),axes=0)
lines(xs,a2*xs+b2,col="blue")
polygon(c(xs,rev(xs)),c(a2*xs+b2,a2*rev(xs)+b2-100),
        col=adjustcolor("skyblue",alpha.f=0.3),border = NA)
points(sol,a1*sol+b1,col="darkred",pch=21,bg="darkred")

# Here is a slope of opposite sign and blue.
a1 = -1.5
b1 = 3
a2 = -2
b2 = 5
sol = (b2-b1)/(a1-a2)
plot(xs,a1*xs+b1,type="l",col="black",xlab=NA,ylab=NA,
     xlim=c(-10,10),ylim=c(-10,10),axes=0)
lines(xs,a2*xs+b2,col="blue")
polygon(c(xs,rev(xs)),c(a2*xs+b2,a2*rev(xs)+b2-100),
        col=adjustcolor("skyblue",alpha.f=0.3),border = NA)
points(sol,a1*sol+b1,col="darkblue",pch=21,bg="darkblue")



# Here is a slope of opposite sign and red.
a1 = 1/3
b1 = 3
a2 = -1/2
b2 = 5
sol = (b2-b1)/(a1-a2)
plot(xs,a1*xs+b1,type="l",col="black",xlab=NA,ylab=NA,
     xlim=c(-10,10),ylim=c(-10,10),axes=0)
lines(xs,a2*xs+b2,col="red")
polygon(c(xs,rev(xs)),c(a2*xs+b2,a2*rev(xs)+b2+100),
        col=adjustcolor("pink",alpha.f=0.3),border = NA)
points(sol,a1*sol+b1,col="darkred",pch=21,bg="darkred")


# Here is a slope of opposite sign and red.
a1 = -1
b1 = 3
a2 = -1/2
b2 = 5
sol = (b2-b1)/(a1-a2)
plot(xs,a1*xs+b1,type="l",col="black",xlab=NA,ylab=NA,
     xlim=c(-10,10),ylim=c(-10,10),axes=0)
lines(xs,a2*xs+b2,col="red")
polygon(c(xs,rev(xs)),c(a2*xs+b2,a2*rev(xs)+b2+100),
        col=adjustcolor("pink",alpha.f=0.3),border = NA)
points(sol,a1*sol+b1,col="darkblue",pch=19,bg="darkblue")



# Here is a slope of opposite sign and red.
a1 = 4
b1 = 3
a2 = 1
b2 = 5
sol = (b2-b1)/(a1-a2)
plot(xs,a1*xs+b1,type="l",col="black",xlab=NA,ylab=NA,
     xlim=c(-10,10),ylim=c(-10,10),axes=0)
lines(xs,a2*xs+b2,col="red")
polygon(c(xs,rev(xs)),c(a2*xs+b2,a2*rev(xs)+b2+100),
        col=adjustcolor("pink",alpha.f=0.3),border = NA)
points(sol,a1*sol+b1,col="darkred",pch=19,bg="darkred")



