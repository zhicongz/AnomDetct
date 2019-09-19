\name{F_exp}
\alias{F_exp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Expectation on A Given Interval
%%  ~~function to do ... ~~
}
\description{This functions returns an approximated expectation of a
given underlying distribution on a bound interval. Specifically, the function
tackles with distribution misspecification issue by applying \code{fhat}. The
approximation is done by Trapezoid Rule.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
F_exp(a, b, N, fhat = NULL, unit = b-a, dist_null = "norm", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{lower bound of interval
  }
  \item{b}{upper bound of interval
  }
  \item{N}{number of observations
  }
  \item{fhat}{a function adjust the distribution misspecification. The default
  value is \code{NULL}, means no adjustment.
  }
  \item{unit}{width of subintervals. The default unit is \eqn{b-a}, means
  approximation is done in one piece.
  }
  \item{dist_null}{a character string giving the underlying distribution in
  null hypothesis. Distribution options are shown in details.
%%     ~~Describe \code{dist_null} here~~
}
  \item{\dots}{further arguments for distribution parameters.
%%     ~~Describe \code{\dots} here~~
}
}
\details{Trapezoid Rule
\deqn{\int_{a}^{b}xf(x)dx = \sum(a-b)(f(a)+f(b))/2}
For more precisely results, set small value of \code{unit} which means
cut interval(a,b) into small piece.

As for the distribution misspecification,
\deqn{f_x = f_ydy/dx}
where \code{f_y} is underlying distribution density function and \code{dy/dx}
is \code{fhat}. Even though \code{f_x} in unknown, it is estimable by equation
above.

The \code{dist_null} indicates the underlying distribution class. The options
follow the distributions regular abbreviation in R, Like \code{norm} is normal
distribution, \code{unif} is uniform distribution, \code{gpd} is
generalized pareto distribution.
\code{\link[stats]{Distributions}} for more distribution options.
}
\value{This function returns the approximated expectation.
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{HRR_pt_est}} for \code{fhat} estimation.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
F_exp(0,1,1000,mean = 0, sd = 1,unit = 1) ## integral on (0,1) by width 1
F_exp(0,1,1000,mean = 0, sd = 1,unit = 0.1) ## integral on (0,1) by width 0.1
F_exp(0,1,1000,mean = 0, sd = 1,unit = 0.01) ## integral on (0,1) by width 0.01
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}% use one of  RShowDoc("KEYWORDS")

