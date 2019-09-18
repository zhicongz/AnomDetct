\name{q_functions}
\alias{q1_function}
\alias{q2_function}
\alias{qL_function}
\alias{prob_fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Q-values of Scan Statistics Hypothesis Test
%%  ~~function to do ... ~~
}
\description{This function returns the scan statistics q-value
which is approximated by 1-dependent stationary sequences.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
q1_function(k,m,p)
q2_function(k,m,p)
qL_function(L,k,m,p)
prob_fun(N,k,m,p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{scan statistics quantile.}
  \item{m}{scan statistics window length.}
  \item{p}{success probabiliy for each Bernoulli trial under null hypothesis.}
  \item{L}{number of 1-dependent stationary sequences.}
  \item{N}{number of Bernoulli trials.}
}
\details{
It has been proved that calculate excat scan statistics probability is a NP
hard problem. So to apply scan statistics, people either use an approximated
distribution or Monte Carlo simulation. Most of the time, approximated
distribution is more efficient but has restrictions on input parameters.
\code{q1_function}, \code{q2_function} and \code{qL_function} here
are estimated by using 1-dependent stationary
sequences.
\cr
\cr
\code{L} and \code{N} represent two different measurement even though those are
all related to the length of scanned sequence.
\deqn{L = N/m-1}
while \code{N} is the length of sequence.
\cr
\cr
For more details of the math works,
please read Haiman's paper shown in references.
\cr
\cr
\code{k}, \code{m}, \code{L}, \code{N} are all integers, where
\eqn{m > 0}, \eqn{0 \le k \le m}, \eqn{L > 3} and \eqn{N/m > 5}.
\cr
\code{p} is a real number where \eqn{0 < p \le 1}.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{q1_function}, \code{q2_function} and \code{qL_function}
return the approximated q-value.
\cr
\cr
\code{prob_fun} returns the linear interpolation of \code{qL_function} results.


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

\seealso{
\code{\link{prob_fun_mc}} which approximate q-value by Monte Carlo simulations.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
q1_function(5,10,0.2)

## compare Monte Carlo simulations and 1-dependent stationary sequences ##
p_MC <- mapply(prob_fun_mc,c(100,500,1000,5000,10000),
               MoreArgs = list(N = 1000,k = 5,m = 10,p=0.1))
p_Haiman <- prob_fun(1000,5,10,0.1)
plot(c(100,500,1000,5000,10000), p_MC,col = "blue",type = "l",
     xlab = "MC replicate times",ylab = "q_values",xlim = c(0.9,1))
abline(h = p_Haiman, col = "red")
legend("topleft",legend = c("MC","Haiman"),col = c("blue","red"),lty = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}% use one of  RShowDoc("KEYWORDS")
