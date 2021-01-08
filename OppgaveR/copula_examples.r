library(rgl)

dgc = Vectorize(function(x,X,rho){
  inside = rho^2*(qnorm(x)^2+qnorm(X)^2)-2*rho*qnorm(x)*qnorm(X)
  1/sqrt(1-rho^2)*exp(-inside/(2*(1-rho^2)))
})

rho = 0.9

ts = seq(0+0.1,1-0.1,by=0.01)
expand = expand.grid(ts,ts)
zs = matrix(apply(expand,1,function(y) dgc(y[1],y[2],rho)),length(ts),length(ts))
zs[is.nan(zs)] <- NA
persp3d(ts,ts,zs,col="red")

nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(zs, nbcol)
persp3d(ts, ts, zs, col=color[zcol],border=NULL,
        ticktype="detailed", xlab="X", ylab="Y", zlab="Z",axes=TRUE)
