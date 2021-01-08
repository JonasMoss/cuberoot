library(RColorBrewer)
cols   = brewer.pal(5,"Set1")
pastel = brewer.pal(5,"Pastel1")
  
# seed = 90, nobs = 12 er bra!
seed   = 4
set.seed(seed)
nobs   = 10
xx     = runif(nobs,-1,1)
yy     = (1 + 2*xx + rnorm(nobs)) >= 0

xs   = seq(-5.5,5.5,by=0.001)
# plot(xs,xs,col="white",ylim=c(-3,3),xlim=c(-3,3),
#      xlab=expression(beta[0]),
#      ylab=expression(beta[1]),axes=0)
plot(xs,xs,col="white",ylim=c(-3,3),xlim=c(-3,3),xlab=NA,ylab=NA,axes=0)
solutions_pos = manski(yy,xx)$positive$solutions[[1]]
solutions_neg = manski(yy,xx)$negative$solutions[[1]]
polygon(c(0,10,10),c(0,solutions_pos[1]*10,solutions_pos[2]*10),
        border=NA,col=pastel[1])


colit = function(bool,x){
  if ((bool & x > 0) | (!bool & x < 0)) return(cols[1])
  else return(cols[2])
}

for (i in 1:nobs){
  lines(xs,-1/xx[i]*xs,col=colit(yy[i],xx[i]))
}

abline(v=1,lty=2,col="grey")
abline(v=-1,lty=2,col="grey")
lines(cos(xs),sin(xs),col=cols[3],lty=3)
points(c(1,1),solutions_pos,pch=4)
points(-c(1,1),solutions_neg,pch=4)

reds = -1/xx[((yy & xx > 0) | (!yy & xx < 0))]
blues = -1/xx[!((yy & xx > 0) | (!yy & xx < 0))]
plot(reds,rep(0,length(reds)),xlim=c(min(-1/xx),max(-1/xx)),
     col=cols[1],pch=16,ylim=c(-0.2,0.2),xlab=NA,ylab=NA,yaxt='n',bty="n",axes=0)
points(blues,rep(0,length(blues)),col=cols[2],bg=cols[2],pch=16)
lines(solutions_pos,c(0,0),col=pastel[1])