library("ggplot2")
library("RColorBrewer")
library("microbenchmark")

# Time complexity -------------------------------------------------------------

g1 = sapply(2:14,function(x) median(microbenchmark(histogram(rbeta(200,2,7),method="exact",k=x))$time))
g2 = sapply(5:29,function(x) median(microbenchmark(histogram(rbeta(1000,2,7),method="exact",k=x))$time))
g3 = sapply(10:90,function(x) median(microbenchmark(histogram(rbeta(10000,2,7),method="exact",k=x))$time))

h1 = sapply(3:100,function(x) median(microbenchmark(histogram(rbeta(10000,2,7),method="greedy",k=x,lim=0))$time))

h2 = sapply(3:100,function(x) median(microbenchmark(histogram(rbeta(10000,2,7),method="greedy",weights="KL",k=x,lim=0))$time))

h3 = sapply((1:50)*100,function(x) median(microbenchmark(histogram(rbeta(x,2,7),method="greedy",weights="KL",k=5,lim=0))$time))


#
# MSE for beta(2,7)
#

qtrue = histogram(rbeta(10000000,2,7),k=5,method="greedy")$splits

greedy_mses = sapply((1:10)*100,function(x) mean(replicate(1000,
                    sum((histogram(rbeta(x,2,7),k=5,method="greedy")$splits-qtrue)^2))))

exact_mses  = sapply((1:10)*100,function(x) mean(replicate(1000,
                    sum((histogram(rbeta(x,2,7),k=5,method="exact")$splits-qtrue)^2))))

smooth_mses = sapply((1:10)*100,function(x) mean(replicate(1000,
                    sum((histogram(rbeta(x,2,7),k=5,method="smoothed")$splits-qtrue)^2))))

cols = brewer.pal(3, "Set2")
plot((1:10)*100,smooth_mses,col=cols[1],pch=1,xlab="n",ylab="MSE")
points((1:10)*100,exact_mses,col=cols[2],pch=2)
points((1:10)*100,greedy_mses,col=cols[3],pch=3)
legend("top",c("Smoothed","Exact","Greedy"),pch=1:3, col=cols,bty="n") 
plot((1:10)*100,smooth_mses/exact_mses,col=cols[1],pch=17,xlab="n",ylab="MSE ratio")