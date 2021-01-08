# Simulation function.

simIt = function(Nreps,nobs,pDist,rDist,lim=0.01){
  
  results = matrix(ncol=Nreps,nrow=3*4)
  rownames(results) = c("cics","penBs","penRs", "aics","hellcics","hellpenBs","hellpenRs",
                        "hellaics","imsecics","imsepenBs","imsepenRs","imseaics")
  
  for (i in 1:Nreps) {
    data  = rDist(nobs)
    results[ 1,i] = CICselect(data)
    results[ 2,i] = penBselect(data)
    results[ 3,i] = penRselect(data)
    results[ 4,i] = AICselect(data)
    results[ 5,i] = hellinger(histogram(data,results[1,i], type="L2",weights="L2",method="exact",lim=lim),pDist)
    results[ 6,i] = hellinger(histogram(data,results[2,i], type="KL",weights="KL",method="exact",lim=lim),pDist)
    results[ 7,i] = hellinger(histogram(data,results[3,i], type="KL",weights="KL",method="exact",lim=lim),pDist)
    results[ 8,i] = hellinger(histogram(data,results[4,i], type="KL",weights="KL",method="exact",lim=lim),pDist)
    results[ 9,i] = imse(histogram(data,results[1,i], type="L2",weights="L2",method="exact",lim=lim),pDist)
    results[10,i] = imse(histogram(data,results[2,i], type="KL",weights="KL",method="exact",lim=lim),pDist)
    results[11,i] = imse(histogram(data,results[3,i], type="KL",weights="KL",method="exact",lim=lim),pDist)
    results[12,i] = imse(histogram(data,results[4,i], type="KL",weights="KL",method="exact",lim=lim),pDist)
  }
  results
}

# Bias checks -----------------------------------------------------------------

CICbias = function(data,k,b=.5,K=50,type="L2",weights="L2",lim=0.001){
  n = length(data)
  bias = biasSubs(data,b=b,k=k,K=K,type=type,weights=type,lim=lim)
  n*bias
}

penBbias = function(data,k,lim=.0001){
  log(choose(n-1,k-1)) + 0.5*(k-1) + (log(k))^(2.5)
}

penRbias = function(data,k,lim=.0001){
  n = length(data)
  obj = histogram(data,k=k,type="KL",weights="KL",lim=lim,method="exact")
  log(choose(n-1,k-1)) + 0.5*sum(obj$weights/diff(c(0,obj$splits,1))) + (log(k))^(2.5)
}

truebias = function(nobs,shape1,shape2,k,lim=.0001) {
  biasCalc(n = nobs, k = k, shape1 = shape1, shape2 = shape2,Nreps = 400,lim = lim, type="KL", weights ="KL")
}

nobs = 200
ks = seq(3,ceiling(2*nobs^(1/3)))
shape1 = 2
shape2 = 7
data = rbeta(nobs,shape1,shape2)

sCICbias  = sapply(ks,function(k) CICbias(data,k,type="KL",weights="KL"))
struebias = sapply(ks,function(k) truebias(200,shape1,shape2,k))
spenBbias = sapply(ks,function(k) penBbias(data,k))
spenRbias = sapply(ks,function(k) penRbias(data,k))
sAICbias  = ks - 1

library("RColorBrewer")
cols = brewer.pal(5,"Set1")
plot(ks,sCICbias,ylim=c(0,100),col=NA,bty="l",xlab="Penalty",ylab="k",sub="n = 200",
     main=expression(paste("Penalties ",beta,"(2,7)-histograms.")))
grid()
legend("topleft",c("True bias","Subsampled bias","Penalty B","Penalty R","AIC: k-1"),bty="n",
       pch = rep(1,5),col=cols)
points(ks,sCICbias,col=cols[1])
points(ks,nobs*struebias,col=cols[2])
points(ks,spenBbias,col=cols[3])
points(ks,spenRbias,col=cols[4])
points(ks,sAICbias,col=cols[5])



# Simulations for beta(5/6,7/8) -----------------------------------------------

shape1 = 5/6
shape2 = 7/8
pDist = function(x) pbeta(x,shape1,shape2)
rDist = function(n) rbeta(n,shape1,shape2)

simBeta56x78x50   = simIt(100,50,pDist,rDist)
simBeta56x78x100  = simIt(100,100,pDist,rDist)
simBeta56x78x500  = simIt(100,500,pDist,rDist)
simBeta56x78x1000 = simIt(100,1000,pDist,rDist)
rowMeans(simBeta56x78x50)
rowMeans(simBeta56x78x100)
rowMeans(simBeta56x78x200)
rowMeans(simBeta56x78x1000)

# Simulations for beta(3,3) ---------------------------------------------------

shape1 = 3
shape2 = 3
pDist = function(x) pbeta(x,shape1,shape2)
rDist = function(n) rbeta(n,shape1,shape2)

simBeta3x3x50   = simIt(100,50  ,pDist,rDist)
simBeta3x3x100  = simIt(100,100 ,pDist,rDist)
simBeta3x3x500  = simIt(100,500 ,pDist,rDist)
simBeta3x3x1000 = simIt(100,1000,pDist,rDist)

rowMeans(simBeta3x3x50)
rowMeans(simBeta3x3x100)
rowMeans(simBeta3x3x500)
rowMeans(simBeta3x3x1000)

# Simulations for bimodal. ----------------------------------------------------

pDist = function(x) 0.5*pbeta(x,10,40) + 0.5*pbeta(x,40,10)
rDist = function(n) {
  bs = rbinom(n,1,.5)
  rbeta(n,10,40)*bs + rbeta(n,40,10)*(1-bs)
}

simBetaBimx50   = simIt(100,nobs=50,  pDist,rDist)
simBetaBimx100  = simIt(100,nobs=100, pDist,rDist)
simBetaBimx500  = simIt(100,nobs=500, pDist,rDist)
simBetaBimx1000 = simIt(100,nobs=1000,pDist,rDist)

rowMeans(simBetaBimx50)
rowMeans(simBetaBimx100)
rowMeans(simBetaBimx500)
rowMeans(simBetaBimx1000)


# Simulations for beta(2,7) -------------------------------------------------

Nreps = 100
shape1 = 2
shape2 = 7
pDist = function(x) pbeta(x,shape1,shape2)
rDist = function(n) rbeta(n,shape1,shape2)
lim = 0.01

simBeta2x7x50   = simIt(Nreps,50,pDist,rDist)
simBeta2x7x100  = simIt(Nreps,100,pDist,rDist)
simBeta2x7x500  = simIt(Nreps,500,pDist,rDist)
simBeta2x7x1000 = simIt(Nreps,1000,pDist,rDist)
rowMeans(simBeta2x7x50)
rowMeans(simBeta2x7x100)
rowMeans(simBeta2x7x500)
rowMeans(simBeta2x7x1000)

# Putting it together ---------------------------------------------------------

m27 = rbind(rowMeans(simBeta2x7x50),
            rowMeans(simBeta2x7x100),
            rowMeans(simBeta2x7x500),
            rowMeans(simBeta2x7x1000))

mBim = rbind(rowMeans(simBetaBimx50),
             rowMeans(simBetaBimx100),
             rowMeans(simBetaBimx500),
             rowMeans(simBetaBimx1000))

m33 = rbind(rowMeans(simBeta3x3x50),
            rowMeans(simBeta3x3x100),
            rowMeans(simBeta3x3x500),
            rowMeans(simBeta3x3x1000))

m5678 = rbind(rowMeans(simBeta56x78x50),
              rowMeans(simBeta56x78x100),
              rowMeans(simBeta56x78x500),
              rowMeans(simBeta56x78x1000))

total1 = round(rbind(m27,m5678),4)
total2 = round(rbind(m33,mBim),4)
write.table(total1,"tot1.txt")
write.table(total2,"tot2.txt")

data = rDist(1000)
CICselect(data)
penBselect(data)
plot(L2histogram(data,k=11,lim=0.01))

simIt(3,1000,pDist,rDist)

