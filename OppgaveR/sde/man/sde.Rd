\name{sde}
\alias{sde}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sde(data, k = 2, m = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
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
function (data, k = 2, m = 50) 
{
    n = length(data)
    data = sort(data)
    ranks = c(0, seq(1, n, len = k - 1), n + 1)
    rank_test = 0 * ranks
    counter = 0
    aug = c(0, data, 1)
    while (!all(rank_test == ranks) & (counter <= m)) {
        rank_test = ranks
        for (index in (1:(k - 1) + 1)) {
            ranks[index] = one_dim(aug, c(ranks[index - 1], ranks[index + 
                1]))
        }
        counter = counter + 1
    }
    data[ranks[-c(1, k + 1)]]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
