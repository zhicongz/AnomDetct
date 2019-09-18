\name{fit_dist}
\alias{fit_dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distribution Selection and Parameters Estimation
%%  ~~function to do ... ~~
}
\description{If the distribution family is given, this function returns the
corresponding parameters estimation. If not, a suggested distribution
is choosen by the largest p-value of Kolmogorov-Smirnov tests.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fit_dist(x, dist_null = NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{dist_null}{a character string specifies the distribution family.
  Default is \code{NA}. This means no specified distribution and the function
  will select a distribution from common continuous distributions.
  Distribution options are shown in details.
%%     ~~Describe \code{dist_null} here~~
}
  \item{\dots}{further arguments for distribution parameters.
%%     ~~Describe \code{\dots} here~~
}
}
\details{This function is built up for selecting underlying distribution in
null hypothesis in this anomaly detection method. \code{dist_null} needs
to be one of the strings in the list below:
\describe{
  \item{\code{"norm"}}{normal distribution}
  \item{\code{"unif"}}{uniform distribution}
  \item{\code{"gamma"}}{gamma distribution}
  \item{\code{"chisq"}}{chi-square distribution}
  \item{\code{"beta"}}{beta distribution}
  \item{\code{"cauchy"}}{cauchy distribution}
  \item{\code{"exp"}}{exponential distribution}
  \item{\code{"f"}}{f distribution}
  \item{\code{"logis"}}{logistic distribution}
  \item{\code{"lnorm"}}{log normal distribution}
  \item{\code{"t"}}{t distribution}
  \item{\code{"weibull"}}{Weibull distribution}
  \item{\code{"gpd"}}{generalized pareto distribution}
}

If \code{dist_null} is missing, this function fits the data with all the
distributions in the list and picks the one with the largest p-value of
\code{\link[stats]{ks.test}}.

With hazard rate ratio adjustment, this test is robusted with distribution
misspecification. As long as the underlying distribution is closed to
distribution of sampled data, the test works well.
%%  ~~ If necessary, more details than the description above ~~
}
\value{This function returns a list with components:
\item{dist}{A string indicates the distribution family. It is same as
\code{dist_null} when \code{dist_null} is given.}
\item{para}{A vector with estimated parameter(s) come with the distribution.}
}

\author{Zhicong Zhao
%%  ~~who you are~~
}
\note{To use \code{gpd}, package \code{POT}
\url{https://cran.r-project.org/web/packages/POT/index.html}
needs to be installed first.
%%  ~~further notes~~
}

\examples{
x <- rnorm(10000)
fit_dist(x)

y <- rgamma(10000,2,3)
fit_dist(y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}% use one of  RShowDoc("KEYWORDS")
