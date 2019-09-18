\name{sample_mode}
\alias{sample_mode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sample Mode
%%  ~~function to do ... ~~
}
\description{
Generic function for the arithmetic mode.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sample_mode(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numeric/character vector.}
}

\value{
This function returns the element in \code{x} with the highest frequency.
If there are more than one values with the highest frequency,
it returns the first one (smallest one for numeric vector or
character vector in alphabetical order).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Xiaojing Ni
%%  ~~who you are~~
}

\examples{
sample_mode(c(1,2,2,3,3))
sample_mode(c("a","b","a"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}% use one of  RShowDoc("KEYWORDS")

