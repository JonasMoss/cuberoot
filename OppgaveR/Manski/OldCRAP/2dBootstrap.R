# Ordinary bootstrap ----------------------------------------------------------

# There are some possible ways to proceed here. Since there are many different
# points to chose from, we must have a rule to follow. A natural rulse is to
# alway chose the points with the smallest norm.

M = nobs
bootN = 1000
points = manski2d(yy,xx1,xx2)$points
index = which.min(apply(points,1,function(z) (z[1]^2+z[2]^2)))
origin = c(manski2d(yy,xx1,xx2)$points[index,1],manski2d(yy,xx1,xx2)$points[index,2])

bootstrapper = function(sample){
  xx1Boot = xx1[sample]
  xx2Boot = xx2[sample]
  yyBoot  = yy[sample]
  boot    = manski2d(yyBoot,xx1Boot,xx2Boot)
  plot(boot,lim=50)
  points(origin[1],origin[2])
  boot    = boot$points
  index   = which.min(apply(boot,1,function(z) (z[1]^2+z[2]^2)))
  
  return(c(boot[index,1],boot[index,2]))
}

boots = replicate(bootN,bootstrapper(sample(1:nobs,M,replace=TRUE)))
boots = M^(1/3)*(boots - origin)
hist(boots)

# Subsampling -----------------------------------------------------------------

M = 10
boots = replicate(20,bootstrapper(sample(1:nobs,M)))
boots = boots - origin
hist(boots[1,],breaks=100)
hist(boots[2,])
plot(density(boots[1,]),xlim=c(-20,20))