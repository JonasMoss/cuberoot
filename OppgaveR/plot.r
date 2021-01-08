n = 100
data = rbeta(n,3,7)

m = 50

q = sde(data,k=3,seed=1000)
bwPal <- colorRampPalette(c('white','pink'))
cols = bwPal(m)

# Get bootstraps.

boots = replicate(25000,sde(rgcde(n,data),k=3,method="iter"))
lower = quantile(boots[1,],c(0.05,0.95))
upper = quantile(boots[2,],c(0.05,0.95))


plot(sde(data,k=3,seed=1000))
rug(data)

qs = (1:m)*4/(m*10)

for (i in 1:m){
  lower = quantile(n^(1/3)*(boots[1,]-q[1]),c(qs[i],1-qs[i]))
  upper = quantile(n^(1/3)*(boots[2,]-q[2]),c(qs[i],1-qs[i]))
  polygon(c(n^(-1/3)*lower+q[1],rev(n^(-1/3)*lower+q[1])),c(1/3*1/(q[1]),1/3*1/(q[1]),0,0),col=cols[i],border=NA)
  polygon(c(n^(-1/3)*upper+q[2],rev(n^(-1/3)*upper+q[2])),c(1/3*1/(q[2]-q[1]),1/3*1/(q[2]-q[1]),0,0),col=cols[i],border=NA)
}

lines(sde(data,k=3,seed=1000))

### Real histograms.

histo = function(P,k,...){
  qs = c(0,(1:k)/k)
  qs = sapply(1:k,function(i) P(qs[i+1],...)-P(qs[i],...))
  class(qs) = c("histo","numeric")
  qs
}

dhisto = function(data,k){
  qs = c(0,(1:k)/k)
  qs = sapply(1:k,function(i) sum(data < qs[i+1] & data >= qs[i]))
  qs = qs/length(data)
  class(qs) = c("histo","numeric")
  qs
}

plot.histo = function(x,...){
  k = length(x)
  ks = c(0,(1:k)/k)
  plot(ks,c(0,x),"S",...)
  lines(ks,c(0,x),"h",...)
}

lines.histo = function(x,...){
  k = length(x)
  ks = c(0,(1:k)/k)
  lines(ks,c(0,x),"S",...)
  lines(ks,c(0,x),"h",...)
}

true = histo(P,k=5)

n = 1000
ressy = sqrt(n)*t(replicate(100000,dhisto(rbeta(n,2,7),k=5)-true))
plot(density(sqrt(10000)*replicate(10000,dhisto(rbeta(10000,2,7),k=2)[2]-true[2])))
plot(100:10000,sqrt(100:10000)*ressy)

gauss = function(){
  grid = seq(-1,1,by=0.01)
  indices = 1:length(grid)
  sapply(indices,)
}





