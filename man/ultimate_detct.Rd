\name{ultimate_detct}
\alias{ultimate_detct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Anomaly Detection Via Scan Statistics
%%  ~~function to do ... ~~
}
\description{This function is an assemble function that uses other functions
in this package, an ultimated version of this detection method.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ultimate_detct(x, theta_th=1, theta_0 = theta_th,
               alpha_lvl = 0.05, anom_est_alpha_lvl = 0.05,
               dist_null = NA, ..., HRR_kernel = "triangular",
               hazard_bandwidth = 0.1, knn = NULL, est_fun = "pt",
               n_hz_sample = NULL, n_hz_size = NULL,
               pt_int = seq(0,1,by = 0.05), window_lth = NA,
               seq_theta = seq(0.5, 1, by = 0.05)*theta_0/theta_th,
               x_unit = 0.01, plot_unit = 1, MLE_unit = 1,
               plt_mgn = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numeric vector of data values where is hypothesis test is
  applied on.
}
  \item{theta_th}{Initial theoretical theta value of hypothesis test.
}
  \item{theta_0}{Initial real theta value of hypothesis test. Default value is
  same as \code{theta_th}.
}
  \item{alpha_lvl}{Significant level for the hypothesis test with
  Initial theoretical theta value \code{theta_th}.
}
  \item{anom_est_alpha_lvl}{Significant level for cluster quantity estimation.
}
  \item{dist_null}{A character string giving the underlying distribution in
  null hypothesis. Distribution options are shown in details.
}
  \item{...}{Further arguments for distribution parameters.
}
  \item{HRR_kernel}{A character string giving the smoothing kernel to be used
  in \code{\link{HRR_pt_est}} or \code{\link{HRR_sbsp_est}}.
  This must partially match one of "\code{gaussian}", "\code{rectangular}",
  "\code{triangular}" or "\code{knn}". Default is "\code{triangular}".
}
  \item{hazard_bandwidth}{the smoothing bandwidth to be used.
}
  \item{knn}{number of neighbor points to be considered in smoothing for the
  "\code{knn}" kernel.
}
  \item{est_fun}{A character string giving the hazard rate ratio
  estimation function. This must match with either "\code{pt}" or
  "\code{sbsp}". Default is "\code{pt}".
}
  \item{n_hz_sample}{Number of replicates if \code{est_fun} is "\code{sbsp}".
}
  \item{n_hz_size}{Resampled size if \code{est_fun} is "\code{sbsp}".
}
  \item{pt_int}{A vector of hazard rate ratio estimated points.
}
  \item{window_lth}{Window length for scan statistics hypothesis test.
  If missing, window length is selected by \code{\link{MLE_window_lth}} and
  \code{\link{Haiman_window_lth}}.
}
  \item{seq_theta}{A vector of theta values put in \code{\link{hypo_test}}
  for cluster detection. This sequence of theta needs to be in order.
  Default is \eqn{seq(0.5, 1, by = 0.05)*theta_0/theta_th}
}
  \item{x_unit}{A number indicating the uniformization bin width.
}
  \item{plot_unit}{A number indicating bin width for histogram in the plot.
}
  \item{MLE_unit}{A number indicating the bin width for counting excess.
}
  \item{plt_mgn}{Extra margin of clusters shown in plot.
}
}
\details{
This function is an ultimated version of this detection method. All the
parameters in this function have default values except \code{x}. This means
as long as people give the data to this function, it can automatically
detect the embedded clusters in the data, without specify underlying
distributions, number of clusters, location of clusters or any neccssary
parameters that commonly need to be put in the model.

Instead of setting \code{theta_0} same to be \code{theta_th}, people can also
multiply \code{theta_th} with returns of \code{\link{HRR_bstp_lb}}
to remove the potential false positive clusters come from the
bias of success probability estimation.
%%  ~~ If necessary, more details than the description above ~~
}
\value{This function returns a list with components:
  \item{Total}{Estimated quantity of clusters}
  \item{Cluster}{A matrix where first two columns are boundaries of clusters
  and thire column is the corresponding p-value. Notice that clusters are
  not necessary to be exclusive.}
  \item{plot}{The plot.}
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
set.seed(100);x <- c(rgamma(10000,2,0.05),rnorm(200,50,1)) ## generate data
res <- ultimate_detct(x, HRR_kernel = "gaussian", est_fun = "sbsp",
                      n_hz_sample = 50, n_hz_size = 200, MLE_unit = 5,
                      x_unit = 0.001)

res <- ultimate_detct(x, HRR_kernel = "triangular",
                      hazard_bandwidth = 0.2, MLE_unit = 5, x_unit = 0.001)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}% use one of  RShowDoc("KEYWORDS")
