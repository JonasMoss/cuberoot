
seed = 90
set.seed(seed)
nobs = 12
xx   = runif(nobs,-1,1)
yy   = (xx + rnorm(nobs)) >= 0

solutions_pos = manski_new(yy,xx)
reds = -1/xx[((yy & xx > 0) | (!yy & xx < 0))]
blues = -1/xx[!((yy & xx > 0) | (!yy & xx < 0))]
plot(reds,rep(0,length(reds)),xlim=c(min(-1/xx),max(-1/xx)),
     col=cols[1],pch=16,ylim=c(-0.2,0.2),xlab=NA,ylab=NA,yaxt='n',bty="n",axes=0)
points(blues,rep(0,length(blues)),col=cols[2],bg=cols[2],pch=16)
lines(solutions_pos,c(0,0),col=pastel[1])

reds = 1/xx[((yy & xx > 0) | (!yy & xx < 0))]
blues = 1/xx[!((yy & xx > 0) | (!yy & xx < 0))]
plot(reds,rep(0,length(reds)),xlim=c(min(1/xx),max(1/xx)),
     col=cols[1],pch=16,ylim=c(-0.2,0.2),xlab=NA,ylab=NA,yaxt='n',bty="n",axes=0)
points(blues,rep(0,length(blues)),col=cols[2],bg=cols[2],pch=16)
lines(solutions_pos,c(0,0),col=pastel[1])

manski_new(yy,xx)
