\name{HRR_pt_est}
\alias{HRR_pt_est}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Hazard Rate Ratio Point Estimations
%%  ~~function to do ... ~~
}
\description{This function returns point Estimations of probability density
function and hazard rate ratio function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HRR_pt_est(pt_int,cdf_sample,kernel = "gaussian",
           hazard_bandwidth = NULL, knn = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pt_int}{a vector of estimated points.}
  \item{cdf_sample}{a sorted vector that needs to be estimated.}
  \item{kernel}{a character string giving the smoothing kernel to be used.
  This must partially match one of "\code{gaussian}", "\code{rectangular}",
  "\code{triangular}" or "\code{knn}", with default "\code{gaussian}".}
  \item{hazard_bandwidth}{the smoothing bandwidth to be used.}
  \item{knn}{number of neighbor points to be considered in smoothing for the
  "\code{knn}" kernel.}
}
\details{
Hazard rate ratio function is defined as:
\deqn{HRR = HR(unif)/HR(est)}
Here, the \code{HR(unif)} is the hazard rate function of uniform distribution
while the \code{HR(est)} is the hazard rate function of estimated density
function.
\deqn{HR(est) = f(est)/(1-F(est))}
\code{f(est)} is the estimated probability density function and
\code{F(est)} is the estimated cumulative distribution function.

\code{f(est)} and \code{F(est)} comes from the local
quadratic polynomial density estimation of cumulative distribution function.
\code{f(est)} is the coefficient or the linear term while \code{F(est)}
is the constant term.

When kernel is "\code{rectangular}" or "\code{triangular}" and
\code{hazard_bandwidth} is over small, number of observations that are
considered in points estimation may not be enough for solving a quadratic
polynomial equation. In this case, if design matrix rank is 2, the function
fit a linear polynomial equation. If design matrix rank is 1, \code{f(est)}
is the percentage of points occur in corresponding bin and \code{F(est)}
is mean of points in corresponding bin. If design matrix rank is 0,
\eqn{f(est) = 0} and \code{F(est)} is missing.

The domain of \code{cdf_sample} is on (0,1), which is a bounded interval. To
elimiate the bias close to boundary points, reflection is being used here. All
the observations are reflected on points 0 and 1. The local quadratic
polynomial density estimation is done on the extended \code{cdf_sample}.

\code{HRR_pt_est} is done by solving systems of linear equations.
With "\code{gaussian}" kernel, the design matrix always use all the obervations
even though the obervations that are far away from the estimated point and
make negligible contribution. However, the computation for large dimension
linear equations system is complicated. Thus, "\code{gaussian}" is not
recommended from the efficiency perspective when \code{length(cdf_sample)}
is huge. Function \code{\link{HRR_sbsp_est}} is designed to solve such problems.

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
\note{If package \code{Matrix} is installed, function
\code{\link[Matrix]{solve}} is used for solving the linear equations.
If not, function \code{\link[base]{qr.solve}} is applied.
%%  ~~further notes~~
}
\seealso{\code{\link[AnomDetct]{HRR_sbsp_est}} for using this function
via subsampling.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
temp <- HRR_pt_est(pt_int = seq(0,1,0.1),
                   cdf_sample = sort(rbeta(10000,2,5)),
                   kernel = "triangular",
                   hazard_bandwidth = 0.1)

## plot ##
plot(temp$fhat,col = "blue",xlab = NA,ylab = NA)
points(seq(0,1,0.1),dbeta(seq(0,1,0.1),2,5),type = "l",col = "red")
legend("top",legend = c("estimated density","population density"),
       lty = 1, col = c("blue","red"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{nonparametric}% use one of  RShowDoc("KEYWORDS")
