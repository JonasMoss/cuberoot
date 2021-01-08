\name{sde_objective}
\alias{sde_objective}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sde_objective(q, Pn, k = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q}{
%%     ~~Describe \code{q} here~~
}
  \item{Pn}{
%%     ~~Describe \code{Pn} here~~
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
function (q, Pn, k = 2, ...) 
{
    q_aug <- c(0, q, 1)
    dis <- sapply(1:k, function(i) log(q_aug[i + 1] - q_aug[i]))
    probs <- sapply(1:k, function(i) Pn(q_aug[i + 1], ...) - 
        Pn(q_aug[i], ...))
    sum(probs * dis)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
