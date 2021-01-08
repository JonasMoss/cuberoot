library(ggplot2)

names(kirke) = c("kommune","kgprosent","tjenester")

tptusen = kirke$kgprosent
hist(tptusen)
maxer = max(tptusen,na.rm=TRUE)
data = tptusen[!is.na(tptusen)]/(maxer+0.001)
q_hat_smooth = sde_iter(rgcde(10000,data),k=k)*maxer
q_hat = sde_iter(data+runif(length(data),0,0.0001),k=k)*maxer
tjeneste_kernel = gcde(data)
hist(tptusen,freq=FALSE,breaks=50)
plot(ts*maxer,sapply(ts,tjeneste_kernel)/maxer,type="l",xlim=c(0,maxer))
sde_plotter(q_hat_smooth,col="blue",lims=c(0,maxer),lines=FALSE)

ests = nlm(function(par) -mean(dbeta(data,par[1],par[2],log=TRUE)),c(1,1),hessian=TRUE)
Var = solve(ests$hessian)

kirke=arrange(kirke,tjenester)
mod1 = lm(kgprosent~tjenester,data=kirke)
mod2 = lm(kgprosent~tjenester+I(tjenester^2),data=kirke)
mod3 = lm(kgprosent~tjenester+I(tjenester^2)+I(log(tjenester)),data=kirke)
mod4 = lm(kgprosent~tjenester+I(log(tjenester)),data=kirke)
mod5 = lm(kgprosent~I(log(tjenester)),data=kirke)
mod6 = lm(kgprosent~I(log(tjenester))+I(tjenester^2),data=kirke)


ggplot(kirke, aes(tjenester, kgprosent,color=kgprosent))+geom_point()+
  scale_color_gradient(low="turquoise", high="darkblue",guide=FALSE)
     
     qplot(tjenester,kgprosent,
                 xlab="Antall gudstjenester per 1000 innbyggere",
                 ylab="Prosent kirkegjengere",
                 main="Kirkelig atferd i norske kommuner, 2014",
                 color="blue"))
