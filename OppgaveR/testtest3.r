sde_sub = function(data,alpha=0.5,k=10){
  n = length(data)
  rowMeans(replicate(1000,sde_iter(sample(data,alpha*n),k)))
}

sde_plotter(sde_iter(data,k=24))
sde_plotter(sde_sub(data,k=8,alpha=0.5),col="blue")
sde_plotter(sde_iter(rgcde(10000,data),k=10),lines=TRUE,col="red")

data = rbeta(100,2,7)

testit = function(alpha=0.5){
  data = rbeta(100,2,7)
  c(sum((sde_iter(data,k=10)-q)^2),
    sum((sde_sub(data,k=10,alpha=0.3)-q)^2),
    sum((sde_sub(data,k=10,alpha=0.5)-q)^2),
    sum((sde_sub(data,k=10,alpha=0.7)-q)^2))
}

a03 = replicate(100,testit(alpha=0.3))
a05 = replicate(100,testit(alpha=0.5))
a07 = replicate(100,testit(alpha=0.7))

aa = replicate(1000,testit())
