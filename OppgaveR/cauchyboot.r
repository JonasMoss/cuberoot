cauchyboot = function(yy) {
  n = length(yy)
  weights = rmultinom(1, n, (1:n)/n)
  #weights = rexp(n,1)
  sqrt(n)*(mean(weights*yy)-mean(yy))
}

yy = rcauchy(5000)
cquant1 = apply(sapply((1:50)*100,
                        function(i) replicate(10000,cauchyboot(yy[1:i]))),
                        2,quantile,probs=c(0.90))


yy = rcauchy(5000)
cquant2 = apply(sapply((1:50)*100,
                       function(i) replicate(10000,cauchyboot(yy[1:i]))),
                2,quantile,probs=c(0.90))