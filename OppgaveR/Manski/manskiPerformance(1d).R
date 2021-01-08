library("microbenchmark")
library("locfit")
library("MASS")

# Calculation of runtimes. ----------------------------------------------------
upper = 500
tries = (1:upper)*10
times = 500

testManski = function(nobs) {
  xx = runif(nobs,-1,1)
  isRed = rbinom(nobs,1,runif(1))
  redBlue(xx,rep(1,nobs),isRed,isSorted=FALSE)
}

testManskiSorted = function(nobs) {
  xx = runif(nobs,-1,1)
  isRed = rbinom(nobs,1,runif(1))
  redBlue(xx,rep(1,nobs),isRed,isSorted=TRUE)
}

results1 = sapply(tries, function(nobs) mean(microbenchmark(testManski(nobs)      , times = times)$time))
results2 = sapply(tries, function(nobs) mean(microbenchmark(mean(rnorm(nobs))     , times = times)$time))
results3 = sapply(tries, function(nobs) mean(microbenchmark(sort(rnorm(nobs))     , times = times)$time))
results4 = sapply(tries, function(nobs) mean(microbenchmark(testManskiSorted(nobs), times = times)$time))

# Plotting of runtimes --------------------------------------------------------
plot(  tries, results1, col="blue")
points(tries, results2, col="red")
points(tries, results3, col="green")
points(tries, results4, col="purple")

mod1 = rlm(results1 ~ 0 + tries + I(log(tries)*tries))
mod2 = rlm(results2 ~ 0 + tries)
mod3 = rlm(results3 ~ 0 + tries)
mod4 = rlm(results4 ~ 0 + tries)

plot( tries,coef(mod1)[1]*tries + coef(mod1)[2]*log(tries)*tries, col = "blue", type="l")
lines(tries,coef(mod2)[1]*tries, col = "red")
lines(tries,coef(mod3)[1]*tries, col = "green")
lines(tries,coef(mod4)[1]*tries, col = "purple" )



