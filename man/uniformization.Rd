\name{unif}
\alias{unif}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Uniformization
%%  ~~function to do ... ~~
}
\description{
This function converts discrete random variables into continuous
pseudorandom variables or deals with duplicated case in continuous
random variables.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
unif(x, unit = 1, rd = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of data values.
%%     ~~Describe \code{x} here~~
}
  \item{unit}{a number indicating the uniformization bin width.}

  \item{rd}{logcial; if \bold{TRUE}, uniformization is done by generating
  uniform distributed random variable. Otherwise, uniformization is done
  by evenly cut the unit.}
}
\details{
As the sample size getting larger and larger,
it is easier to obtain duplicated continuous random variables in the dataset.
There is a subtle contradiction invoked in this case because for
continuous random variable, \deqn{P[x_1=x_2]=0}
This function adds residuals on original dataset to avoid duplicated cases.
It is an important step for cleaning the data for continuous variables
order statistics.
\cr
As the \code{unit} setted appropriately, the target continuous
density function is approximated by sequence of side by side rectangles.
This is somehow similar as Cauchy integral.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A sorted numeric vector of length \code{length(x)} is returned.
  \cr
  Function will be stopped when \eqn{unit \le 0} .
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Zhicong Zhao
%%  ~~who you are~~
}

\examples{
x <- rep(c(0.5,1,2), each = 2)

## Uniformization with unit 1 ##
y <- unif(x)
plot(y)

## Uniformization with unit 0.5 ##
y <- unif(x, unit = 0.5)
plot(y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}% use one of  RShowDoc("KEYWORDS")

