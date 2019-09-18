\name{HRR_sbsp_est}
\alias{HRR_sbsp_est}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Subsampling Hazard Rate Ratio Point Estimations
%%  ~~function to do ... ~~
}
\description{This function returns point Estimations of probability density
function and hazard rate ratio function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HRR_sbsp_est(pt_int,cdf,kernel = "gaussian",hazard_bandwidth = NA,
             knn = NA, n_hz_sample, n_hz_size = length(cdf))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pt_int}{a vector of estimated points.}
  \item{cdf}{a sorted vector that needs to be estimated.}
  \item{kernel}{a character string giving the smoothing kernel to be used.
  This must partially match one of "\code{gaussian}", "\code{rectangular}",
  "\code{triangular}" or "\code{knn}", with default "\code{gaussian}".}
  \item{hazard_bandwidth}{the smoothing bandwidth to be used.}
  \item{knn}{number of neighbor points to be considered in smoothing for the
  "\code{knn}" kernel.}
  \item{n_hz_sample}{number of replicates.}
  \item{n_hz_size}{resampled size. Default to be same as \code{length(cdf)}.}
}
\details{
A smaller \code{n_hz_size} can significantly cut down the running time while
the accuracy is maintained by large \code{n_hz_sample}. It is similar as the
"bagging" method in machine learning.
%%  ~~ If necessary, more details than the description above ~~
}
\value{This function returns a list with components:
  \item{fhat}{A function performing the linear interpolation of smoothed
  probability density function of given data points.}
  \item{HRR}{A function performing the linear interpolation of smoothed
  hazard rate ratio point estimations.}
}

\author{Zhicong Zhao
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{HRR_pt_est}} for each replicate.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
temp <- HRR_sbsp_est(pt_int = seq(0,1,0.1),
                     cdf = sort(rbeta(10000,2,5)),
                     kernel = "triangular",
                     hazard_bandwidth = 0.1,
                     n_hz_sample = 30)
## plot ##
plot(temp$fhat,col = "blue",xlab = NA,ylab = NA)
points(seq(0,1,0.1),dbeta(seq(0,1,0.1),2,5),type = "l",col = "red")
legend("top",legend = c("estimated density","population density"),
       lty = 1, col = c("blue","red"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth}% use one of  RShowDoc("KEYWORDS")

