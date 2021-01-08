library("microbenchmark")
library("locfit")
source("histogram_base.R")

ks = 2:20
data = rbeta(1000,2,7)
Pnm(histogram(data,k=20,type="L2",weights="L2",method="exact"))
Pnm(histogram(rbeta(1000,2,7),k=20,type="L2",weights="L2",method="exact"))

#eq = sapply(ks,function(x) Pnm(histogram(data,k=x,type="L2",method="exact")))
var = sapply(ks,function(x) Pnm(histogram(data,k=x,type="L2",weights="L2",method="exact")))
#mean(sort(eq)==eq)
#mean(sort(var)==var)
#plot(ks,eq)
plot(ks,var)

datL2 = replicate(100,Pnm(histogram(rbeta(1000,2,7),k=20,type="L2",weights="L2",method="exact")))
datKL = replicate(100,Pnm(histogram(rbeta(1000,2,7),k=20,type="KL",weights="KL",method="exact")))

data = sort(rbeta(1000,2,7))
objL2 = histogram(data,k=7,type="L2",weights="L2",method="exact")
objKL = histogram(data,k=7,type="KL",weights="KL",method="exact")
Pnm(objL2)
plot(histogram(data,k=7,type="KL",weights="KL",method="exact",lim=0.001))
plot(histogram(data,k=7,type="L2",weights="L2",method="exact",lim=0.001))
Pnm(objKL)

plot(sapply((1:99)/1000,function(x) 
  Pnm(exact_histogram(data,k = 34,type="KL",weights="KL",lim=x))))






nobs = 300
data = sort(rbeta(nobs,1,9))
tdat = rbeta(60000,1,9)
ks = 10:50
exacts300a = sapply(ks,function(x) 
  Pnm(histogram(data,k=x,type="L2",weights="L2",method = "exact", lim=0.002)))
greeds300a = sapply(ks,function(x) 
  Pnm(histogram(data,k=x,type="L2",weights="L2",method = "greedy",lim=0.002)))


plot(histogram(data,k=10,type="L2",weights="L2",method = "exact", lim=0.002))
plot(histogram(data,k=10,type="L2",weights="L2",method = "exact", lim=0.001))

plot(ks,exacts300a,col="red",bty="l",ylab="Objective",xlab="k",
     ylim=c(min(greeds300a,exacts300a),max(greeds300a,exacts300a)))
points(ks,greeds300a,col="blue")

grid()



nobs = 300
data = sort(rbeta(nobs,1,9))
tdat = rbeta(60000,1,9)
ks = 10:50
greeds300 = sapply(ks,function(x) Pnm(histogram(data,k=x,type="KL",weights="equal",method = "greedy",lim=0.1)))
exacts300 = sapply(ks,function(x) Pnm(histogram(data,k=x,type="KL",weights="equal",method = "exact",lim=0.1)))
trues     = sapply(ks,function(x) Pnm(histogram(tdat,k=x,type="KL",weights="equal",method = "greedy",lim=0.1)))
plot(ks,exacts300,col="red",bty="l",ylab="Objective",xlab="k",
     ylim=c(min(greeds300,exacts300),max(greeds300,exacts300)))
points(ks,greeds300,col="blue")
points(ks,trues,col="green")
grid()

Pnm(exact_histogram(data,k = 34,type="KL",weights="equal",lim=0))
Pnm(greedy_histogram(data,k = 34,type="KL",weights="equal",lim=0))


true     = histogram(rbeta(1000000,3,1),k=k,method = "greedy")$splits[j]

library("RColorBrewer")
cols = brewer.pal(4,"Set2")
ns = (1:20)*50
k = 11
j = 5

greeds  = rep(NA,length(ns))
exacts  = rep(NA,length(ns))
smooths = rep(NA,length(ns))
for (i in 1:length(ns)){
  greeds[i]  = mean((replicate(100,histogram(rbeta(ns[i],3,1),k=k,method = "greedy")$splits[j])-true)^2)
  exacts[i]  = mean((replicate(100,histogram(rbeta(ns[i],3,1),k=k,method = "exact")$splits[j])-true)^2)
  smooths[i] = mean((replicate(100,histogram(rbeta(ns[i],3,1),k=k,method = "smoothed")$splits[j])-true)^2)
  print(i)
}

plot(ns,ns^(2/3)*exacts,bty="l",ylab="Objective",xlab="n",
     ylim=c(min(ns^(2/3)*greeds,ns^(2/3)*exacts)-0.01,max(ns^(2/3)*greeds,ns^(2/3)*exacts)),
     main="Mean squared error",col=cols[1],pch=15)
points(ns,ns^(2/3)*greeds,col=cols[2],pch=16)
points(ns,ns^(2/3)*smooths,col=cols[3],pch=17)
grid()
legend("topright",c("Exact","Coordinate search","Smoothed"),
       col=cols[1:3],pch=c(15,16,17),bty="n")



data = rbeta(100,2,7)
k = 6
plot(histogram(data,method="exact",weights="KL",k=k))
rug(data)




