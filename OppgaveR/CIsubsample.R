library("locfit")

true = histogram(rbeta(1000000,2,7),k=2,method="coord",type="L2",weights="equal")$splits

histogramSubsample = function(data,k,m,N=1000,lim=0.01) {
  n = length(data)
  est = histogram(data,k=k,lim=lim,method="coord",type="L2",weights="equal")$split
  boots = replicate(N,histogram(data[sample(n,m)],k=k,lim=lim,
                      method="coord",type="L2",weights="equal")$splits)
  dist = m^(1/3)*(boots - est)
  if (k > 2) {
    return(est + n^(-1/3)*apply(dist,1,function(x) quantile(x,c(0.025,0.975))))
  } else {
    return(est + n^(-1/3)* quantile(dist,c(0.025,0.975)))
  }
}

subsampleMC = function(n,m,true,N=1000,reps=1000) {
  subs = replicate(reps,histogramSubsample(rbeta(n,2,7), k = 2, m = m, N = N))
  percents = apply(subs,2,function(x) x[1] < true & true < x[2])
  lengths  = apply(subs,2,function(x) x[2]-x[1])
  c(mean(percents),mean(lengths))
}

n50m5    = subsampleMC(50  , 5,true=true)
n50m7    = subsampleMC(50  , 7,true=true)
n50m10   = subsampleMC(50  ,10,true=true)
n50m20   = subsampleMC(50  ,20,true=true)
n50 = rbind(n50m5, n50m7, n50m10, n50m20)

n100m5   = subsampleMC(100 , 5,true=true)
n100m7   = subsampleMC(100 , 7,true=true)
n100m10  = subsampleMC(100 ,10,true=true)
n100m20  = subsampleMC(100 ,20,true=true)
n100 = rbind(n100m5, n100m7, n100m10, n100m20)

n500m5   = subsampleMC(500 , 5,true=true)
n500m7   = subsampleMC(500 , 7,true=true)
n500m10  = subsampleMC(500 ,10,true=true)
n500m20  = subsampleMC(500 ,20,true=true)
n500 = rbind(n500m5, n500m7, n500m10, n500m20)

n1000m5  = subsampleMC(1000, 5,true=true)
n1000m7  = subsampleMC(1000, 7,true=true)
n1000m10 = subsampleMC(1000,10,true=true)
n1000m20 = subsampleMC(1000,20,true=true)
n1000 = rbind(n1000m5, n1000m7, n1000m10, n1000m20)

subs27 = rbind(n50,n100,n500,n1000)



