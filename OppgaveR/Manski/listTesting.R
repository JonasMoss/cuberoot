library("Rcpp")
sourceCpp("listTesting.cpp")


xs     = seq(-15,15,by=0.01)
nobs   = 200
xx1    = rnorm(nobs)
xx2    = rnorm(nobs)
beta0  = 1
beta1  = 6
beta2  = 7
yy     = (beta0 + xx1*beta1 + xx2*beta2 + rnorm(nobs)) >= 0 
slope  = -xx2/xx1

# The "b" is xx2. A line is red if y = 1 and b > 0 or if 
isReds = (xx1 >= 0 & yy) | ( xx1 < 0 & !yy)


res = redBlue2(xx1,xx2,isReds,rep(1,nobs))
res
xCoord = matrix(res$xCoord,length(res$xCoord)/2,2,byrow=TRUE)
yCoord = matrix(res$yCoord,length(res$yCoord)/2,2,byrow=TRUE)
xCoord
yCoord


par(pty="m")
lim = 40
xs = seq(-lim-10,lim+10,by=0.11)
plotzi(xs,xx1[1],xx2[1],plot=TRUE,col=adjustcolor(-2*isReds[i]+4,alpha.f=0.3),frame.plot=F,asp = 1,
       xlim=c(min(xCoord),max(xCoord)),ylim=c(min(yCoord),max(yCoord)),xlab=NA,ylab=NA)
for (i in 2:nobs) plotzi(xs,xx1[i],xx2[i],col=adjustcolor(-2*isReds[i]+4,alpha.f=0.3))
#points(res$xs[c(2:7)],res$ys[c(2:7)],col=-2*res$colors[2:7]+4)

lines(xCoord[1,],yCoord[1,],xlim=c(min(xCoord),max(xCoord)),col=adjustcolor("black",alpha.f=0.6),
     ylim=c(min(yCoord),max(yCoord)),type="l",xlab=expression(beta[2]),ylab=expression(beta[1]))
for (i in 2:(length(res$xCoord)/2)) lines(xCoord[i,],yCoord[i,],lwd=1,col=adjustcolor("black",alpha.f=0.6))


# Function for find calculating polygons. -------------------------------------
intercepts = -1/xx1
slopes = -xx2/xx1
xCoord
