\name{tran_detct}
\alias{tran_detct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Anomaly Detection Via Multiple Window Length Scan Statistics
%%  ~~function to do ... ~~
}
\description{This function applies scan statistics hypothesis test with
multiple different window length for dealing with multiple clusters with
different magnitude. The cluster significant level is estimated by Bonferroni
method.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tran_detct(x, theta_th=1, theta_0 = theta_th, alpha_lvl=0.05,
           loc, HRR_kernel = "triangular",
           hazard_bandwidth=0.1, knn = NULL, est_fun = "pt",
           n_hz_sample = NULL, n_hz_size = NULL,
           pt_int = seq(0,1,by = 0.05),
           seq_theta = seq(0.5, 1, by = 0.05)*theta_0/theta_th,
           x_unit = 0.01, plot_unit = 1, MLE_unit = 1,
           plt_mgn = 0, max_rec = 3, tail_obs = 50)
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
  \item{loc}{Lower bound for applying scan statistics. It is also the
  \code{threshold} of \code{\link[POT]{fitgpd}}
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
  \item{seq_theta}{A vector of theta values put in \code{\link{hypo_test}}
  for cluster detection. This sequence of theta needs to be in order.
  Default is \code{seq(0.5, 1, by = 0.05)*theta_0/theta_th}
}
  \item{x_unit}{A number indicating the uniformization bin width.
}
  \item{plot_unit}{A number indicating bin width for histogram in the plot.
}
  \item{MLE_unit}{A number indicating the bin width for counting excess.
}
  \item{plt_mgn}{Extra margin of clusters shown in plot.
}
  \item{max_rec}{Maximum recursive number.
}
  \item{tail_obs}{Minimum number of observations on tail to continue the
  recursive.
}
}
\details{This function is the method presented in the paper. It may not be
as general as \code{\link{ultimate_detct}}, but it always has a better
performance in dealing with transaction data anomaly detection.

An unique character for transaction data anomalous cluster is the magnitude
of the cluster changes with respect to the price point, which is saying the
clusters occur at large transaction amount commonly have smaller size
compared with clusters occur at small transaction amount. Also, some of the
clusters at small amount are not necessary to be anomalous clusters.
It may comes from popular stuffs sold at specific price, for example:
$ 0.99 for a bottle of water or a bar of chocolate.

In this case, we present to use smaller window length scan statistics only
scanning on this tail part of the data to aviod obtain too much false positive
clusters at small price point and also catch more true positive on large
price point.

This function scans clusters on \code{x} with a given price point
lower bound \code{loc}. After each scanning, the \code{loc} is updated to be
the largest detected cluster locations. Then, the next scanning is working on
the remaining tails with new \code{loc} and \code{window length}. The function
stops when recursive attaches the \code{max_rec} or number of tail observations
is less than \code{tail_obs} or no new clusters are detected.

\code{max_rec} not need to be too large because with Bonferroni method, the
significance level of clusters goes up fast. Calcuate the true
significance level of multiple window length scanning is a future work topic.
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
\note{Package \code{POT}
\url{https://cran.r-project.org/web/packages/POT/index.html}
needs to be installed first.
%%  ~~further notes~~
}

\examples{
set.seed(100);x <- c(rgamma(10000,2,0.05),
                     runif(200,50,51),
                     runif(40,100,101))## generate data

res_no_rec <- ultimate_detct(x, HRR_kernel = "gaussian", est_fun = "sbsp",
                             n_hz_sample = 50, n_hz_size = 200, MLE_unit = 5,
                             x_unit = 0.001,
                             hazard_bandwidth = 0.2) ## no recursive result

res_rec <- tran_detct(x,loc = 30, HRR_kernel = "gaussian", est_fun = "sbsp",
                      n_hz_sample = 50, n_hz_size = 200, MLE_unit = 5,
                      x_unit = 0.001,
                      hazard_bandwidth = 0.2) ## recursive result
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{cluster}% use one of  RShowDoc("KEYWORDS")

