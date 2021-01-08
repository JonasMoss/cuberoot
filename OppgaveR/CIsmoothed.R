###########################################################################
# In this file we investige the smoothed bootstrap as a way to estimate   #
# confidence intervals.                                                   #
###########################################################################

true = histogram(rbeta(1000000,2,7),method="greedy",k=k,type="L2",weights="equal")$splits

histogramSmoothed = function(data,alpha,h,Nreps=100){
  n = length(data)
  obj = histogram(data,k=k,method="exact",type="L2",weights="equal")
  q = obj$splits
  boots = replicate(Nreps,histogram(rgcde(n,data,h=h),k=2,method="greedy",type="L2",weights="equal")$splits)
  quants = quantile((boots - q),c(alpha,1-alpha)) + q
  quants
}

smoothedMC = function(n,true,N=1000,reps=100,alpha=0.025) {
  data = rbeta(n,2,7)
  h = hgcde(data)*n^(1/5)*n^(-1/2)
  subs = replicate(reps,histogramSmoothed(data, alpha = alpha, h = h, N = N))
  percents = apply(subs,2,function(x) x[1] < true & true < x[2])
  lengths  = apply(subs,2,function(x) x[2]-x[1])
  c(mean(percents),mean(lengths))
}

sm50a   = smoothedMC(50  ,true,1000,100,alpha=0.05)
sm100a  = smoothedMC(100 ,true,100,100,alpha=0.05)
sm500a  = smoothedMC(500 ,true,100,100,alpha=0.05)
sm1000a = smoothedMC(1000,true,100,100,alpha=0.05)
sma     = c(sm50a,sm100a,sm500a,sm1000a)

sm50b   = smoothedMC(50  ,true,100,100,alpha=0.025)
sm100b  = smoothedMC(100 ,true,100,100,alpha=0.025)
sm500b  = smoothedMC(500 ,true,100,100,alpha=0.025)
sm1000b = smoothedMC(1000,true,100,100,alpha=0.025)
smb     = c(sm50b,sm100b,sm500b,sm1000b)
round(rbind(sma,smb),4)



