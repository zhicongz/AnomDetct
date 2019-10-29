\name{Haiman_window_lth}
\alias{Haiman_window_lth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Minimum Window Length for Scan Statistics
%%  ~~function to do ... ~~
}
\description{This function returns the minimum window length that could be used
in scan statistics hypothesis test with a given significance level.
\cr
Here the q-value is approximated by 1-dependent stationary sequences. This
minimum window length also consider the restrictions of input parameters.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Haiman_window_lth(N, p, alpha, lower_wl=3, upper_wl=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{number of Bernoulli trials.}
  \item{p}{success probabiliy for each Bernoulli trial under null hypothesis.}
  \item{alpha}{hypothesis test significance level.}
  \item{lower_wl}{lower bound for searching the minimum window length.}
  \item{upper_wl}{upper bound for searching the minimum window length.}
}
\details{
This function is searching for the minimum window length with binary search.
\cr
\cr
Too small window length is invalid because the alpha level
of the hypothesis is hard to achieve. On the other hand, too large window
length is also not working well. The reason is that with too large window
length, the test losts the power. Also, to detect the embedded clusters,
considering the edge effects, large window length is hard to have a good
performance in general. So, this function has upper searching bound such that
\eqn{window_lth < N/4}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{minimum scan statistics window length for using 1-dependent stationary
sequences approximation is returned.
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

\examples{
Haiman_window_lth(1000,0.2,0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}% use one of  RShowDoc("KEYWORDS")

