\name{crit_theta_fun}
\alias{crit_theta_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scan Statistics Threshold
%%  ~~function to do ... ~~
}
\description{This function returns the threshold of scan statistics hypothesis
test where the significance level is no larger than \code{alpha}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
crit_theta_fun(N, window_l, p, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{number of Bernoulli trials.}
  \item{window_l}{scan statistics window length.}
  \item{p}{success probabiliy for each Bernoulli trial under null hypothesis.}
  \item{alpha}{hypothesis test significance level.}
}
\details{This function is using 1-dependent stationary sequences approximation
for searching scan statistics threshold. It returns the threshold such that the
p-value is no larger than \code{alpha}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{This function returns the threshold number.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Haiman,G.(2007), Estimating the distribution of one-dimensional discrete
scan statistics viewed as extremes of 1-dependent stationary sequences.
\emph{Journal of Statistical Planning and Inference}, 137(2007), 821-828.
%% ~put references to the literature/web site here ~
}
\author{Zhicong Zhao
%%  ~~who you are~~
}

\seealso{\code{\link{prob_fun}} for details of computing the q-values.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
crit_theta_fun(1000,10,0.1,0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}% use one of  RShowDoc("KEYWORDS")
