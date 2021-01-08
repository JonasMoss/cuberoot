\name{one_dim}
\alias{one_dim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
one_dim(aug, ranks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aug}{
%%     ~~Describe \code{aug} here~~
}
  \item{ranks}{
%%     ~~Describe \code{ranks} here~~
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
function (aug, ranks) 
{
    indices <- (ranks[1] + 1):(ranks[2] - 1)
    if (ranks[2] == length(aug) - 1) 
        end <- ranks[2] - 1
    else end <- ranks[2]
    vals <- (indices - ranks[1])/n * log(aug[indices + 1] - aug[ranks[1] + 
        1]) + (end - indices)/n * log(aug[ranks[2] + 1] - aug[indices + 
        1])
    which.min(vals) + ranks[1]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
