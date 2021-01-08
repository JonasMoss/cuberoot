\name{sde_true}
\alias{sde_true}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sde_true(P, Q = NULL, k, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{P}{
%%     ~~Describe \code{P} here~~
}
  \item{Q}{
%%     ~~Describe \code{Q} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
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
function (P, Q = NULL, k, ...) 
{
    if (is.null(Q)) 
        q0 = ((seq(0, 1, length = k + 1))[-1])[-k]
    q0 = Q(c(1:(k - 1))/k, ...)
    hin = function(x, ...) {
        y = c(0, x, 1)
        h = sapply(1:k, function(i) y[i + 1] - y[i])
        h = c(h, x)
        h
    }
    constrOptim.nl(q0, sde_objective, hin = hin, Pn = P, k = k, 
        control.outer = list(trace = FALSE), ...)$par
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
