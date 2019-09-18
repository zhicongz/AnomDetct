\name{clst_plot}
\alias{clst_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cluster Visualization
%%  ~~function to do ... ~~
}
\description{This function plots a list of returns of \code{\link{hypo_test}}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clst_plot(x, org_x = x, clst, alpha_lvl, unit,
          x_range = c(min(org_x), max(org_x)), plt_mgn = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numeric vector of data values where is hypothesis test is
  applied on.
%%     ~~Describe \code{x} here~~
}
  \item{org_x}{A numeric vector of original data values. Default is \code{x},
  means scan statistics hypothesis test is applied on all the observations.
%%     ~~Describe \code{org_x} here~~
}
  \item{clst}{A list of matrices. Each matrix has three columns
  where the first column is clusters starting indexes, second column is the
  corresponding clusters ending indexes and the third column is the
  corresponding significant level for clusters.
%%     ~~Describe \code{clst} here~~
}
  \item{alpha_lvl}{Maximum alpha level in legend.
%%     ~~Describe \code{alpha_lvl} here~~
}
  \item{unit}{A number indicating bin width for histogram in the plot.
%%     ~~Describe \code{unit} here~~
}
  \item{x_range}{\code{xlim} for plot
%%     ~~Describe \code{x_range} here~~
}
  \item{plt_mgn}{extra margin of clusters shown in plot.
%%     ~~Describe \code{plt_mgn} here~~
}
}
\details{
If \pkg{ggplot2} is installed, the plot is done via \code{ggplot}. If not,
the plot is done via basic R \code{plot}.

Number of layers in the plot is determined by number of elements in \code{clst}.
\code{alpha_lvl} must be equal or larger than the maximum significant level
of clusters in the plot.

\code{x} and \code{org_x} are not necessary to be the same. Within some
specific scenarios where we do not want to detect clusters among
all observations, we can apply \code{\link{hypo_test}} only on
\code{cdf} that are converted by \code{x}.
Please be awared the each component in \code{clst} \bold{must}
come from the detected results of \code{x}.

\code{plt_mgn} is for small clusters that can barely be seen in plot. This
happens when the magnitude of cluster is too small as sample size quit large.
%%  ~~ If necessary, more details than the description above ~~
}
\value{ This function returns a plot. If ggplot installed, it returns an
object as \code{ggplot}. If not, it returns an object as \code{recordedplot}
}

\author{Zhicong Zhao
%%  ~~who you are~~
}


\seealso{\code{\link{hypo_test}} for detected clusters and
\code{\link{prob_fun}} for q-values of corresponding clusters.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## generate data
x <- qnorm(seq(0,1,0.001))
x <- x[-c(1,length(x))]
x <- c(x[1:100],rep(x[100],10),x[101:length(x)])
clst <- lapply(1:10,function(x)matrix(c(100,110,0.01*x),nrow = 1))
clst_plot(x = x, clst = clst, alpha_lvl = 0.1, unit = 0.1, plt_mgn = 0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}% use one of  RShowDoc("KEYWORDS")
