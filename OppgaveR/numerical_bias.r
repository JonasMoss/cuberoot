n <- 100

get_bias <- function(data,k,step=5){
  n <- length(data)
  ns <- seq(floor(n/2),n,by=step)
  data.frame(cbind(ns,bias=sapply(ns,function(i) subbias(sample(data,i),i/2,k))))
}

bias_plot <- function(data,k,step=5){
  res <- get_bias(data,k,step=step)
  fit = lm(bias~0+I(ns^(-2/3)),data=res)
  plot(res$ns,res$bias,ylab="Estimated bias",xlab="n")
  lines(res$ns,(res$ns)^(-2/3)*fit$coef,col="blue")
  list(fit$coef,res)
}

bias_plot(rbeta(1000,2,7),k,step=10)