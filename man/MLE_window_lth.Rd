\name{MLE_window_lth}
\alias{MLE_window_lth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scan Statistics Empirical Window Length
%%  ~~function to do ... ~~
}
\description{This funtion returns the optimized scan statistics window length
via the empirical estimation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MLE_window_lth(x,dist_null,..., unit = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of data values.}
  \item{dist_null}{a character string giving the underlying distribution in
  null hypothesis. Distribution options are shown in details.}
  \item{...}{Further arguments for distribution parameters.}
  \item{unit}{A number indicating the bin width for counting excess.}
}
\details{
Before applying scan statistics, the window length need to be setted first and
this is an important factor which determines the hypothesis test performance.
In practice, window length should be close to the cluster size.
Too small window length leads to higher false positive while too large window
length leads to lower test power.

This function is for efficiently select
an appropriate window length. The data is splited by \code{unit} and in each
group the excess is defined as number of observations subtract with expected
observations. Then, the maximum excess among those groups is the returned
value.

The \code{dist_null} indicates the underlying distribution class. The options
follow the distributions regular abbreviation in R, Like \code{norm} is normal
distribution, \code{unif} is uniform distribution, \code{gpd} is
generalized pareto distribution.
\code{\link[stats]{Distributions}} for more distribution options.
}
\value{The empirical scan statistics window length is returned.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Zhicong Zhao
%%  ~~who you are~~
}
\note{To use \code{gpd}, package \code{POT}
\url{https://cran.r-project.org/package=POT}
needs to be installed first.
%%  ~~further notes~~
}

\examples{
x <- c(rnorm(1000),rep(1,20)) ## sampled data
wd_lth <- MLE_window_lth(x,dist_null = "norm",mean = 0,sd = 1,unit = 0.01)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
