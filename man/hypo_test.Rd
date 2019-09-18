\name{hypo_test}
\alias{hypo_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scan Statistics Hypothesis Test
%%  ~~function to do ... ~~
}
\description{This function returns the clusters detected by scan statistics. If
no cluster is detected, an empty matrix is returned.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hypo_test(cdf,HRR,theta,critical_cnt,window_l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cdf}{a sorted vector that needs to be tested.}
  \item{HRR}{hazard rate ratio point estimations on \code{cdf}. Default is 1
  which means no adjustment is considered in hypothesis test.}
  \item{theta}{the parameter determining success probabiliy for
  each Bernoulli trial, \eqn{p = 1-exp(-theta)}.}
  \item{critical_cnt}{scan statistics threshold.}
  \item{window_l}{scan statistics window length.}
}
\details{
Under the null hypothesis, \code{cdf} is supposed to be uniform distributed in
order. When this happens, each Bernoulli trial should has a constant success
probability \eqn{p = 1-exp(-theta)}. However, if there is a cluster in
\code{cdf}, the Bernoulli trials at corresponding place will have a larger
success probability. The maximum scan statistics hypothesis test is built up
to detect this cluster.

In practice, with model misspecification, the ground success
probability is not a constant which will lead to unexpected bias in cluster
detection. hazard rate ratio is addressed to tackle this issue. Scan statistics
are really sensetive to the success probability. A tiny differenc in success
probability could lead to huge change in results. Unless the population
distribution is known, we suggest people use hazard rate ratio to enhance the
test performance.


More math works are shown in my dissertation. The paper about this method
will be put in references as long as it is published.
%%  ~~ If necessary, more details than the description above ~~
}
\value{this function returns a matrix where the first column is the
starting index of detected clusters in \code{cdf} and second column is the
ending index of detected clusters.
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
## sampled data
x <- sort(c(1:100,rep(30.5,10))/100)
hist(x,breaks = seq(0,1,0.02))

## detected cluster
y <- hypo_test(x,rep(1,length(x)),1,9,9)
rect(x[y[1]],0,x[y[2]],par("yaxp")[2],col = "red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}% use one of  RShowDoc("KEYWORDS")
