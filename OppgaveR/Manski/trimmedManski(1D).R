# In this file we develop an L-statistical variant of Manski's estimator 
# and check it's properties.
#
# The point is: The weight assigned to a subset by Manski's estimator can
# be seen as the depth of that line, and Manski's estimator is a 
# binary response-ish generalization of the median.
# 
# From this we can "generalize" L-statistics by considering linear combinations
# of the ranked lines. 
#
# This is strictly speaking not what we done, as we only use the "vertices"
# or bounding lines of every solution set.

nobs = 1000
beta = .5
xx = rnorm(nobs)
yy = (1 + beta*xx + rlnorm(nobs)-1) >= 0
obj = manski1d(yy,xx)

trimmed = function(obj,alpha=0.10) {
  # k designates how many steps to left and right should be taken
  # from the max.
  maxWeight = obj$maxWeight
  nobs = obj$total
  lim = floor((maxWeight*(1-alpha)))
  median((obj$details[1,-c(1,obj$total+2)])[obj$details[3,-c(1,obj$total+2)]>=lim])
}


trimmedm = function(obj,alpha=0.1) {
  # k designates how many steps to left and right should be taken
  # from the max.
  maxWeight = obj$maxWeight
  nobs = obj$total
  lim = floor((maxWeight*(1-alpha)))
  isWithUs = obj$details[3,-c(1,obj$total+2)]>=lim
  mean((obj$details[1,-c(1,obj$total+2)]*(obj$details[3,-c(1,obj$total+2)]-lim+1))[isWithUs])/(maxWeight-lim+1)
}

trues = c()
fakes = c()
for (i in (1:1000)*100){
  nobs = i
  beta = 5
  xx = rnorm(nobs)
  yy = (1 + beta*xx + rlnorm(nobs)-1) >= 0
  obj = manski1d(yy,xx, type="pos")
  trues = c(trues,coef(obj))
  fakes = c(fakes,trimmed(obj,alpha=.01))
}

plot(trues)
points(fakes,col="red")

getTF = function(nobs) {
  trues = c()
  fakes = c()
  for (i in 1:100){
    beta = 0.1
    xx = rnorm(nobs)
    yy = (1 + beta*xx + rlnorm(nobs)-1) >= 0
    obj = manski1d(yy,xx, type="pos")
    trues = c(trues,coef(obj))
    fakes = c(fakes,trimmed(obj,alpha=.1))
  }
  c(trues=nobs^(2/3)*var(trues),fakes=nobs*var(fakes))
}

coolz = sapply((1:20)*1000,getTF)
