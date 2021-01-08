data = rbeta(50,2,7)
Pn = ecdf(data)
a = histogram(data,k=7,method="exact")$splits
details(a,data)
plot(histogram(data,k=7,method="exact"))

fnot = function(x,a1s=a1,a2s=a2){
 -log(x-a1s)*(Pn(x)-Pn(a1s-0.000001)) - log(a2s-x)*(Pn(a2s)-Pn(x-0.000001)) 
}

a = histogram(data,k=7,method="exact")$splits
a1 = a[1]
a2 = a[3]

details(a,data)

xs =seq(a1+0.0001,a2-0.0001,by=0.001) 
max(fnot(xs),na.rm=TRUE)
fnot(a[2])
plot(xs,fnot(xs))
data = sort(data)
pointy = data[data < a[3] & data > a[1]]
points(pointy,fnot(pointy),col="red")
points(a[2],fnot(a[2]),pch=19,col="green")
abline(v=a[2])

mid = ((Pn(a[2])-Pn(a1))*a2 + a1*(Pn(a2)-Pn(a[2])))/(Pn(a2)-Pn(a1))

c = 0.2
b = 0.4
plot(xs,-c*log(xs-a1)-b*log(a2-xs),type="l")


details2 = function(q,data){
  k     = length(q)+1
  n     = length(data)
  Pn    = c(0,sapply(q,function(j) sum(data<=j)/n),1)
  qAug  = c(0,q,1)
  dis   = sapply(1:k,function(i) (log(qAug[i+1]-qAug[i]) - log(Pn[i+1]-Pn[i]+1/n)))
  probs = sapply(1:k,function(i) Pn[i+1]-Pn[i]+1/n)
  rbind(dis=-dis,probs=probs,summands=probs*dis)
}

n = 3
fest = replicate(10000,min(diff(sort(rbeta(n,5,6)))))
hist(log(1/(n^2*fest)),breaks=100,freq=FALSE)
lines(locfit(~log(1/(n^2*fest))))

n = 1000
fest = replicate(10000,min(diff(sort(rbeta(n,5,6)))))
lines(locfit(~log(1/(n^2*fest))),col="blue")

n = 50
rest = replicate(5000,histogram(rbeta(n,2,7),k=6)$splits)
fest = replicate(5000,min(diff(sort(rbeta(n,2,7)))))
diffs = apply(rest,2,function(x) min(diff(x)))
plot(locfit(~log(4/(n^2*diffs))),ylab="Density",bty="l",xlab="Log minimal difference",
     sub=paste0("n = ",n,"; max diff: ", round(max((4/(n^2*diffs)),3))))
grid()