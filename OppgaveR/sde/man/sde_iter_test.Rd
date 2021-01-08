\name{sde_iter_test}
\alias{sde_iter_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sde_iter_test()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    N <- 10000
    q_true_3 <- sde_true(pbeta, qbeta, 3, shape1 = 2, shape2 = 7)$par
    q_true_4 <- sde_true(pbeta, qbeta, 3, shape1 = 2, shape2 = 7)$par
    repeatit = function(n = 100) {
        data = rbeta(n, 2, 7)
        est1 = sde2(data)
        est2 = sde_iter(data, k = 3)
        sqrt(c(sum((est1 - q_true)^2), sum((est2 - q_true)^2)))
    }
    results <- data.frame(t(replicate(N, repeatit())))
    names(results) <- c("True", "Algorithm")
    results <- melt(results, variable.name = "estimator", value.name = "value")
    boxplot(value ~ estimator, results)
    data = rbeta(100, 2, 7)
    microbenchmark(sde_iter(data, k = 3), sde2(data))
    microbenchmark(sde_iter(data, k = 4), sde3(data))
    ts <- seq(0.01, data[9], len = 1000)
    plot(ts, sapply(ts, function(k) sde_objective(c(k, data[9]), 
        ecd, k = 3)), type = "l")
    N = 100
    ns <- (10:30)
    repeatit = function(n = 100) {
        data = rbeta(n, 2, 7)
        est1 = sde2(data)
        est2 = sde_iter(data, k = 3)
        sum((est1 - est2)^2)
    }
    diffss <- sapply(ns, function(n) mean(replicate(N, repeatit(n))))
    names(diffss) <- ns
    means <- colMeans(diffs)
    quants <- apply(diffs, 2, quantile, probs = c(0.25, 0.75))
    names(means) <- ns
    data <- sort(rbeta(100, 2, 7))
    sde(data)
    data[one_dim(c(0, data, 1), c(0, 101))]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
