\name{cdf_convert}
\alias{cdf_convert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert Random Variables to Cumulative Distribution Function
%%  ~~function to do ... ~~
}
\description{This function returns the \eqn{F(x)} with given \code{x} and
\code{F}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cdf_convert(x, dist_null, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of data values.}
  \item{dist_null}{a character string giving the underline distribution in
  null hypothesis. Distribution options are shown in details.}
  \item{...}{further arguments for distribution parameters.}
}
\details{
The \code{dist_null} indicates the underlying distribution class. The options
follow the distributions regular abbreviation in R, Like \code{norm} is normal
distribution, \code{unif} is uniform distribution, \code{gpd} is
generalized pareto distribution.
\code{\link[stats]{Distributions}} for more distribution options.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A numeric vector with corresponding CDF value is returned
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Zhicong Zhao
%%  ~~who you are~~
}
\note{To use \code{gpd}, package \pkg{POT}
\url{https://cran.r-project.org/package=POT}
needs to be installed first.
%%  ~~further notes~~
}
\examples{
y <- cdf_convert(sort(rnorm(100)), "norm")
plot(seq(0,1,length.out = length(y)), y,
     xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
     main = "Uniform Q-Q Plot") ## CDF uniform distributed
abline(0,1,col = "red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}% use one of  RShowDoc("KEYWORDS")

