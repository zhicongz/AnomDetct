\name{sim_df}
\alias{sim_df_N}
\alias{sim_df_D}
\alias{sim_df_G}
\alias{sim_df_Z}
\docType{data}
\title{Simulated Datasets
%%   ~~ data name/kind ... ~~
}
\description{Simulated data with anomaly clusters
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{
sim_df_N
sim_df_D
sim_df_G
sim_df_Z
}
\format{
  Numeric vectors.
}
\details{
Data provided here are simulated transaction data. Each of those has
multiple clusters with different locations and cluster sizes.
Some of the clusters can not be seen in plot, but this scan statistics
hypothesis can still be able to detect those.
%%  ~~ If necessary, more details than the __description__ above ~~
}

\examples{
## applied tran_detct on sampled data
set.seed(100); x <- sample(sim_df_N, 5000)
tran_detct(x,loc = 10,HRR_kernel = "gaussian", est_fun = "sbsp",
           n_hz_sample = 30, n_hz_size = 80, MLE_unit = 0.1,
           x_unit = 0.001, hazard_bandwidth = 0.2, tail_obs = 100)
}
\keyword{datasets}
