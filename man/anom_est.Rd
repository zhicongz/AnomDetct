\name{anom_est}
\alias{anom_est}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Quantify Anomaly Clusters
%%  ~~function to do ... ~~
}
\description{This function returns the total amount of detected anomaly
clusters under different \code{alpha_lvl}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
anom_est(clst_p_values, alpha_lvl, x, fhat = NULL, dist_null = "norm", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clst_p_values}{a list of matrices. Each matrix has three columns
  where the first column is clusters starting indexes, second column is the
  corresponding clusters ending indexes and the third column is the
  corresponding significant level for clusters.
%%     ~~Describe \code{clst_p_values} here~~
}
  \item{alpha_lvl}{significance level for quantifying clusters.
%%     ~~Describe \code{alpha_lvl} here~~
}
  \item{x}{tested data where the clusters are detected.
%%     ~~Describe \code{x} here~~
}
  \item{fhat}{a function adjust the distribution misspecification. The default
  value is \code{NULL}, means no adjustment.
%%     ~~Describe \code{fhat} here~~
}
  \item{dist_null}{a character string giving the underlying distribution in
  null hypothesis. Distribution options are shown in details.
%%     ~~Describe \code{dist_null} here~~
}
  \item{\dots}{further arguments for distribution parameters.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
The \code{dist_null} indicates the underlying distribution class. The options
follow the distributions regular abbreviation in R, Like \code{norm} is normal
distribution, \code{unif} is uniform distribution, \code{gpd} is
generalized pareto distribution.
\code{\link[stats]{Distributions}} for more distribution options.
%%  ~~ If necessary, more details than the description above ~~
}
\value{This function returns the estimated anomalous amount.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Zhicong Zhao
%%  ~~who you are~~
}
\note{\code{clst_p_values} is not same as the return of \code{\link{hypo_test}}.
Corresponding p-values need to be added on the last column.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{F_exp}} for estimated expections.

\code{\link{HRR_pt_est}} and \code{\link{HRR_sbsp_est}}
for \code{fhat} estimation.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## two clusters with different significant levels
clst_p_values <- list(matrix(c(10,15,0.01,25,30,0.1),nrow = 2,byrow = TRUE))
x <- sort(runif(40))
x <- c(x[1:10],rep(x[10],5),x[11:20],rep(x[20],5),x[21:40])
anom_est(clst_p_values,0.05,x,dist_null = "unif")
anom_est(clst_p_values,0.1,x,dist_null = "unif")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}% use one of  RShowDoc("KEYWORDS")
