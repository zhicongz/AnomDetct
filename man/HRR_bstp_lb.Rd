\name{HRR_bstp_lb}
\alias{HRR_bstp_lb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Hazard Rate Ratio Bootstrapping Confidence Interval
%%  ~~function to do ... ~~
}
\description{This function estimates hazard rate ratio bootstrapping
confidence interval of U(0,1) random variables.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HRR_bstp_lb(N, pt_int, kernel = "gaussian", hazard_bandwidth = NULL,
            knn = NULL, mc_rep, c_l, est_fun, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{length of sequence
%%     ~~Describe \code{N} here~~
}
  \item{pt_int}{a vector of estimated points.
%%     ~~Describe \code{pt_int} here~~
}
  \item{kernel}{a character string giving the smoothing kernel to be used.
  This must partially match one of "\code{gaussian}", "\code{rectangular}",
  "\code{triangular}" or "\code{knn}", with default "\code{gaussian}".
%%     ~~Describe \code{kernel} here~~
}
  \item{hazard_bandwidth}{the smoothing bandwidth to be used.
%%     ~~Describe \code{hazard_bandwidth} here~~
}
  \item{knn}{number of neighbor points to be considered in smoothing for the
  "\code{knn}" kernel.
%%     ~~Describe \code{knn} here~~
}
  \item{mc_rep}{number of replicates.
%%     ~~Describe \code{mc_rep} here~~
}
  \item{c_l}{one side significance level.
%%     ~~Describe \code{c_l} here~~
}
  \item{est_fun}{a character string giving the hazard rate ratio
  estimation function. This must match with either "\code{pt}" or
  "\code{sbsp}".
}
  \item{...}{further arguments for "\code{sbsp}".
}
}
\details{In scan statistics hypothesis, null hypothesis has success probability
equals to a given constant \code{p}. However, in this anomaly detection, with
hazard rate ratio adjustment, \code{p} is a function with respect to
hazard rate ratio estimation. To eliminate the false positive clusters
caused by the variance of this estimation, we present to tune down the
\code{p} by the return of this function.
This can guarantee that real success probability is not greater
than \code{p} with probability \code{c_l}.

As the \code{N} large enough, the return is close to 1. Considering the
efficiency of the function, it is not always necessary to apply this
function before using functions this anomaly detection package.
%%  ~~ If necessary, more details than the description above ~~
}
\value{This function returns the
\deqn{1/ max(hazard rate ratio)}
  for significance level equals to \code{c_l}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{Zhicong Zhao
%%  ~~who you are~~
}
\seealso{\code{\link{HRR_pt_est}} and \code{\link{HRR_sbsp_est}} for
details of hazard rate ratio estimation function.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## HRR_sbsp_est is faster than HRR_pt_est
# system.time({HRR_bstp_lb(600, seq(0,1,0.1),
#              hazard_bandwidth = 0.1, mc_rep = 100, c_l = 0.05,
#              est_fun = "pt")}) ## HRR_pt_est

# system.time({HRR_bstp_lb(600, seq(0,1,0.1),
#              hazard_bandwidth = 0.1, mc_rep = 100, c_l = 0.05,
#              est_fun = "sbsp", n_hz_sample = 30,
#              n_hz_size = 50)}) ## HRR_sbsp_est
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
