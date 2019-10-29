\name{bisect}
\alias{bisect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bisection Method
%%  ~~function to do ... ~~
}
\description{This function returns the results of binary searching on sorted
vector \code{arr}.
}
\usage{
bisect(arr,target,right = TRUE,index = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arr}{a sorted numeric/character/logical vector. If it is numeric, the
  vector needs to be in ascending order; if it is character, the vector needs
  to be in alphabetical order; if it is logical, the vector needs to be in
  order from `FALSE` to `TRUE`.}
  \item{target}{the target needs to be searched in \code{arr}.}
  \item{right}{logical; if `TRUE`, search for the first element in \code{arr}
  such that \deqn{arr[i] >= target;} otherwise, search for the last element
  in \code{arr} such that \deqn{arr[i] <= target.}}
  \item{index}{logical; if `TRUE`, returns the index `i` of the \code{arr}
  otherwise, returns the \code{arr[i]} instead.}
}
\value{
  If \code{index} is `TRUE`, returns a number represents the index in
  \code{arr}; else, returns the one of the element in \code{arr} satisfied
  with the searching conditions.
}
\author{
  Zhicong Zhao
}

\examples{
arr <- c(0,1,1,3,4,4,5,5,7,10)
bisect(arr,4)
bisect(arr,6,right = F)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}% use one of  RShowDoc("KEYWORDS")

