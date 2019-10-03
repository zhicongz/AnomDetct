HRR_bstp_lb <- function(N, pt_int, kernel = "gaussian", hazard_bandwidth = NULL,
                        knn = NULL, mc_rep, c_l, est_fun, ...){
  my_list <- replicate(mc_rep, sort(stats::runif(N)),simplify = F)
  if(est_fun == "pt"){
    if (requireNamespace("parallel", quietly = TRUE)){
      res <- parallel::mcmapply(AnomDetct::HRR_pt_est, my_list,
                                MoreArgs = list(pt_int = pt_int,
                                                kernel = kernel,
                                                hazard_bandwidth =
                                                  hazard_bandwidth,
                                                knn = knn), SIMPLIFY = F)
    }else{
      res <-mapply(AnomDetct::HRR_pt_est, my_list,
                   MoreArgs = list(pt_int = pt_int,
                                   kernel = kernel,
                                   hazard_bandwidth = hazard_bandwidth,
                                   knn = knn), SIMPLIFY = F)
    }
  }else if(est_fun == "sbsp"){
    hz <- list(...)
    n_hz_sample <- hz[[1]]
    n_hz_size <- hz[[2]]

    if(requireNamespace("parallel", quietly = TRUE)){
      res <- parallel::mcmapply(AnomDetct::HRR_sbsp_est, my_list,
                                MoreArgs = list(pt_int = pt_int,
                                                kernel = kernel,
                                                hazard_bandwidth =
                                                  hazard_bandwidth,
                                                knn = knn,
                                                n_hz_sample = n_hz_sample,
                                                n_hz_size   = n_hz_size),
                                SIMPLIFY = F)
    }else{
      res <- mapply(AnomDetct::HRR_sbsp_est, my_list,
                    MoreArgs = list(pt_int = pt_int,
                                    kernel = kernel,
                                    hazard_bandwidth = hazard_bandwidth,
                                    knn = knn,
                                    n_hz_sample = n_hz_sample,
                                    n_hz_size   = n_hz_size),SIMPLIFY = F)
    }
  }else stop("Invalid `est_fun`", call. = F)

  HRR  <- sapply(res, function(x)x[[2]](pt_int))
  max_ratio <- apply(HRR,2,max)

  return(stats::quantile(1/max_ratio,1-c_l))
}
