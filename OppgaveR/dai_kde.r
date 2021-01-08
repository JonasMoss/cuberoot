library(ks)

dskern = function(xs){
  ret = xs*(abs(xs)<=1)
  3/4*(1-ret^2)*(abs(xs)<=1)
}

grh = function(x,h){
  if (x<=h) {
    2*h - x
  } else if (x>(1-h) & x<=1)  {
    2*h - (1-x)
  } else
    h  
}

dsh = function(x,h,epsilon){
  if (x<=h) {
    max(x,epsilon)
  } else if (x>(1-h) & x<=1)  {
    max(1-x,epsilon)
  } else
    h
}

mgcde = function(data,h=NULL,lims=NULL){
  if (is.null(h)) h = hgcde(data)
  if (!is.null(lims)) data = (data-lims[1])/lims[2]
  else lims = c(0,1)
  trans = qnorm(data)
  data = data[trans != Inf & trans != -Inf]
  function(x){
    hx = grh(x,h)
    rho = 1-hx^2
    mean(dgc((x-lims[1])/lims[2],data,rho))/lims[2]
  }
}

dsgcde = function(data,h=NULL,lims=NULL,epsilon=0.01){
  if (is.null(h)) h = hgcde(data)
  if (!is.null(lims)) data = (data-lims[1])/lims[2]
  else lims = c(0,1)
  trans = qnorm(data)
  data = data[trans != Inf & trans != -Inf]
  function(x){
    hx = dsh(x,h,epsilon)
    rho = 1-hx^2
    mean(dgc((x-lims[1])/lims[2],data,rho))/lims[2]
  }
}

dskde = function(data,h=2*kde(data)$h,epsilon=0.01){
  function(x){
    hx = 1/dsh(x,h,epsilon)
    hx*mean(dskern((data-x)*hx))
  }
}

grkde = function(data,h=2*kde(data)$h){
  function(x){
    hx = 1/grh(x,h)
    hx*mean(dskern((data-x)*hx))
  }
}

data = rbeta(100,1,1)
kern = dskde(data,h=0.3)
kern2 = gcde(data)
kern3 = kde(data)
kern4 = grkde(data,h=0.3)
#plot(kern3,col="purple")
plot(ts,dbeta(ts,1,1),type="l",lwd=2)
lines(ts,sapply(ts,kern),col="red")
lines(ts,sapply(ts,kern4),col="green")
lines(ts,sapply(ts,kern2),col="blue")