HRR_sbsp_est <- function(pt_int,cdf,kernel = "gaussian",hazard_bandwidth = NA,
                         knn = NA, n_hz_sample, n_hz_size = length(cdf)){

  cdf_samples <- replicate(n_hz_sample,
                           sort(sample(cdf,n_hz_size,replace = T)),
                           simplify = FALSE)

  if (requireNamespace("parallel", quietly = TRUE)){
    sample_est <- parallel::mcmapply(AnomDetct::HRR_pt_est, cdf_samples,
                                     MoreArgs = list(pt_int = pt_int,
                                                     hazard_bandwidth =
                                                       hazard_bandwidth,
                                                     kernel = kernel,
                                                     knn = knn),SIMPLIFY = F)
  }else{
    sample_est <-mapply(AnomDetct::HRR_pt_est, cdf_samples,
                        MoreArgs = list(pt_int = pt_int,
                                        hazard_bandwidth = hazard_bandwidth,
                                        kernel = kernel,
                                        knn = knn),SIMPLIFY = F)
  }

  sample_est_fhat <- sapply(sample_est, function(x)x[[1]](pt_int))
  sample_est_HRR  <- sapply(sample_est, function(x)x[[2]](pt_int))

  fhat <- apply(sample_est_fhat,1,mean)

  HRR  <- apply(sample_est_HRR,1,function(x)mean(x[!is.infinite(x)],
                                                 na.rm = T))
  HRR[is.nan(HRR)] <- Inf

  return(list(fhat = stats::approxfun(pt_int,fhat),
              HRR  = stats::approxfun(pt_int,HRR)))
}
