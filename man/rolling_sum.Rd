\name{rolling_sum}
\alias{rolling_sum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Rolling Sum
  %%  ~~function to do ... ~~
}
\description{
  This function returns partial sums of the input vector.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rolling_sum(temp, window_l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{temp}{a vector that needs to be summed.
    %%     ~~Describe \code{x} here~~
  }
  \item{window_l}{number of consecutive elements in \code{temp}
    that are summed every time.}
}

\value{
  A numeric vector of length \code{length(temp)-window_l+1} is returned.
  \cr
  Function will be stopped when \code{window_l > length(temp)}.
}

\author{Xiaojing Ni}

\note{An NA value in x will be treat as 0 in rolling_sum(x).}

\examples{
rolling_sum(1:10, 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}% use one of  RShowDoc("KEYWORDS")
