\name{prob_fun_mc}
\alias{prob_fun_mc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Q-values for Scan Statistics by Monte Carlo Simulation
%%  ~~function to do ... ~~
}
\description{This function returns the scan statistics q-value which is
approximated by Monte Carlo simulation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prob_fun_mc(N,k,m,p,mc_rep)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{number of Bernoulli trials.}
  \item{k}{scan statistics quantile.}
  \item{m}{scan statistics window length.}
  \item{p}{success probabiliy for each Bernoulli trial under null hypothesis.}
  \item{mc_rep}{number of replications.}
}
\details{It has been proved that calculate excat scan statistics probability
is a NP hard problem. Thus, a straightway to solve this problem is using Monte
Carlo simulation. As \code{mc_rep} getting larger, the result will be closer to
the true value but a longer time it will take to obtain the result.
This is a tradeoff between accuracy and efficiency.
\cr
\cr
\code{N}, \code{k}, \code{m}, \code{mc_rep} are all integers, where
\eqn{m > 0}, \eqn{0 \le k \le m} and \eqn{N/m > 5}.
\cr
\code{p} is a real number where \eqn{0 < p \le 1}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{This function returns the approximated q-value.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Zhicong Zhao
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{prob_fun}} which approximate q-value by
1-dependent stationary sequences.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## estimate running time ##
system.time({q_value <- prob_fun_mc(1000,5,10,0.1,10000)})
q_value
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}% use one of  RShowDoc("KEYWORDS")

