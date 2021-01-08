seed = 2051
set.seed(seed)

nobs = 200
xs = -100:100
xx1 = rnorm(nobs)
xx2 = rnorm(nobs)
beta1 = -1
beta2 = -1/2
yy = (1 + beta1*xx1 + beta2*xx2 + rnorm(nobs))>=0
colindex = ifelse(yy,2,1)
plot(manski2d(yy,xx1,xx2))



cols = brewer.pal(9,"Set1")
plot(xx1,xx2,pch=(yy+21),col=NA,bg=cols[colindex],axes=0,
      xlab=NA,ylab=NA,ylim=c(-10,10))
lines(xs,-1/beta2-beta1/beta2*xs,col=cols[3],type="l")
#plot(xs,1+2*xs,col=cols[3],type="l")

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



i = 1
lines(xs,-1/obj$points[i,1] - obj$points[i,2]/obj$points[i,1]*xs,col=cols[4])
i = 2
lines(xs,-1/obj$points[i,1] - obj$points[i,2]/obj$points[i,1]*xs,col=cols[4])
i = 3
lines(xs,-1/obj$points[i,1] - obj$points[i,2]/obj$points[i,1]*xs,col=cols[8])
i = 6
lines(xs,-1/obj$points[i,1] - obj$points[i,2]/obj$points[i,1]*xs,col=cols[8])


for (i in 1:(length(obj$points)/2)){
  lines(xs,-1/obj$points[i,1] - obj$points[i,2]/obj$points[i,1]*xs,col=cols[4])
}
# ML for heteroskedastic models.