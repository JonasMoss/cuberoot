seed = 188
set.seed(seed)

nobs = 10
xs = -100:100
xx2 = rnorm(nobs,0,1)
xx1 = rnorm(nobs,0,1)
beta1 = 1/2
beta2 = 1/2
yy = (1 + beta1*xx1 + beta2*xx2 + rnorm(nobs))>=0

linePlotter = function(yy,xx1,xx2) {
  colindex = ifelse(yy,1,2)
  cols = brewer.pal(9,"Set1")
  plot(xx1,xx2,pch=(yy+21),col=NA,bg=cols[colindex],
       xlab=NA,ylab=NA)
  #plot(xs,1+2*xs,col=cols[3],type="l")
  grid()
  obj = manski2d(yy,xx1,xx2)
  obj
  
  for (i in 1:(length(obj$points)/2)){
    lines(xs,-1/obj$points[i,2] - obj$points[i,1]/obj$points[i,2]*xs,col=cols[4])
  }
  
}


configurationPlotter = function(yy,xx1,xx2,lim=8) {
  plot(NULL,xlim=c(-lim,lim),ylim=c(-lim,lim),xlab=expression(beta[2]),
       ylab=expression(beta[1]),bty="l",axes=0)
  obj  = manski2d(yy,xx1,xx2)
  
  xpoints = obj$points[,1]
  xs = seq(-lim,lim,by=1)
  isReds = (xx1 >= 0 & yy) | ( xx1 < 0 & !yy)
  colIndex = ifelse(isReds,2,4)
  
  for (i in 1:nobs){
    lines(xs,-1/xx2[i]-xs/xx2[i]*xx1[i],col=adjustcolor(colIndex[i],alpha.f=0.9),lty=3)
  }
  
  len = length(obj$points[,1])/2
  xCoord = matrix(obj$points[,1],len,2,byrow=TRUE)
  yCoord = matrix(obj$points[,2],len,2,byrow=TRUE)
  
  for (i in 1:len) {
    lines(xCoord[i,],yCoord[i,],lwd=1,col="black")
  }  
}

colindex = ifelse(yy,1,2)
cols = brewer.pal(9,"Set1")
plot(xx1,xx2,pch=(yy+21),col=NA,bg=cols[colindex],
     xlab=NA,ylab=NA)
lines(xs,-1/beta2-beta1/beta2*xs,col=cols[3],type="l")
#plot(xs,1+2*xs,col=cols[3],type="l")
grid()

prob = glm(yy~xx1+xx2,family=binomial(link=probit))
logi = glm(yy~xx1+xx2,family=binomial(link=logit))

logi1 = coef(logi)[2]/coef(logi)[1]
logi2 = coef(logi)[3]/coef(logi)[1]
lines(xs,-1/logi2-logi1/logi2*xs,col=cols[5])

prob1 = coef(prob)[2]/coef(prob)[1]
prob2 = coef(prob)[3]/coef(prob)[1]
lines(xs,-1/prob2-prob1/prob2*xs,col=cols[6])


obj = manski2d(yy,xx1,xx2)
obj


for (i in 1:(length(obj$points)/2)){
  lines(xs,-1/obj$points[i,1] - obj$points[i,2]/obj$points[i,1]*xs,col=cols[4])
}
# ML for heteroskedastic models.