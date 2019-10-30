\name{bisect}
\alias{bisect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bisection Method
%%  ~~function to do ... ~~
}
\description{This function returns the results of binary searching on a
monotone increasing function \code{fn}.
}
\usage{
bisect(fn, lower, upper, discrete = TRUE, target, right = TRUE,index = TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{a monotone increasing function with numeric/character/logical
  output. If output is character, the increasing defines as alphabetical order;
  if it is logical, the increasing defines as order from `FALSE` to `TRUE`.}
  \item{lower}{lower bound of the function \code{fn} domain.}
  \item{upper}{upper bound of the function \code{fn} domain.}
  \item{discrete}{logical; if `TRUE`, the domain is discrete; otherwise, the
  domain is continuous}
  \item{target}{the target needs to be searched in the range of \code{fn}.}
  \item{right}{logical; if `TRUE`, search for the smallest value (right limit
  for continous domain) of `x` in the domain of \code{fn} such that
  \deqn{fn(x) >= target;}
  otherwise, search for the largest value (left limit for continous domain)
  of `x` in the domain of \code{fn} such that such that \deqn{fn(x) <= target.}}
  \item{index}{logical; if `TRUE`, returns the input value of the \code{fn}
  satisfied the searching condition; otherwise, returns the output value of
  \code{fn} instead.}
  \item{...}{further arguments, see \code{Details}}
}
\details{Here are more arguments that are provided by this function:
\describe{
    \item{unit}{If the domain is discrete, \code{unit} can be used for specified
    the step length. Default value is `1`.}
    \item{tol}{If the function \code{fn} is continous, \code{tol} is used for
    error tolerance. Default value is '10^(-7)'.}}
  For the case one want to apply binary search on a given vector `arr`, a simple
  way is define:
  \deqn{fn <- function(x) arr[x]}
  and search on discrete domain \eqn{c(1,length(arr))} with \eqn{unit = 1}.
}
\value{
  If \code{index} is `TRUE`, returns a number represents the searching results
  on domain of \code{fn};
  else, returns the searching results on range of \code{fn}.
}
\author{
  Zhicong Zhao
}

\examples{
fn <- function(x) if(x>3 && x<5) 4 else x
bisect(fn,0,10,discrete = F,target = 4,right = T) # returns 3 because right limit of f(3) = 4
bisect(fn,0,10,discrete = F,target = 4,right = F) # returns 5 because left limit of f(5) = 4
bisect(fn,0,10,discrete = T,target = 4,right = T) # returns 4 for discrete domain
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}% use one of  RShowDoc("KEYWORDS")
