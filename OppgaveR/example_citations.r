fresh = c(1.402,  1.583,  1.728,  2.043,  2.424,  2.740,  2.811,  2.826,
          2.953,  2.961,  2.965,  3.258,  3.481,  3.481,  3.481,  3.520,
          3.632,  3.643,  3.718,  3.755,  3.878,  3.879,  4.121,  4.146,
          4.148,  4.268,  4.269,  4.270,  4.301,  4.439)

fresh_new = fresh + runif(length(fresh),0,0.0001)

data = fresh_new/(max(fresh_new)+0.0001)
q_hat = sde_iter(data,k=k)*max(fresh)
q_hat_smooth = sde_iter(rgcde(10000,data),k=k)*max(fresh)
q_hat_sub = sde_sub(data,k=k)*max(fresh)
kernel = gcde(data)
chen_kernel = ckde(data)
par(mfrow=c(2,2))

sde_plotter(q_hat,col="red",lims=c(0,max(fresh)))
rug(fresh)
sde_plotter(q_hat_sub,col="purple",lims=c(0,max(fresh)))
rug(fresh)
plot(ts*max(fresh),sapply(ts,kernel)/max(fresh),lty=2,type="l",ylim=c(0,1))
sde_plotter(q_hat_smooth,col="blue",lims=c(0,max(fresh)),lines=TRUE)
lines(ts*max(fresh),sapply(ts,chen_kernel)/max(fresh))
rug(fresh)


sde_plotter(sde_iter(data,k=k),col="red")