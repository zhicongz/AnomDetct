ultimate_detct <- function(x, theta_th=1, theta_0 = theta_th,
                           alpha_lvl = 0.05, anom_est_alpha_lvl = 0.05,
                           dist_null = NA, ..., HRR_kernel = "triangular",
                           hazard_bandwidth = 0.1, knn = NULL, est_fun = "pt",
                           n_hz_sample = NULL, n_hz_size = NULL,
                           pt_int = seq(0,1,by = 0.05), window_lth = NA,
                           seq_theta = seq(0.5, 1, by = 0.05)*theta_0,
                           x_unit = 0.01, plot_unit = 1, MLE_unit = 1,
                           plt_mgn = 0){
  if(theta_th<=0) stop("Invalid `theta_th`", call. = F)

  N <- length(x)

  x <- sort(x,decreasing = F)

  x_backup <- x

  x <- unif(x,x_unit)

  if(is.na(dist_null)){
    dist_info <- AnomDetct::fit_dist(x_backup)
  }else{
    dist_info <- list(dist_null,c(...))
  }

  str_convert <- paste("AnomDetct::cdf_convert(x,dist_null = '",
                       dist_info[[1]],"',",
                       paste(dist_info[[2]],collapse = ","),
                       ")",sep = "")
  cdf_x <- eval(parse(text = str_convert))

  window_Haiman <- AnomDetct::Haiman_window_lth(N-1, 1-exp(-theta_th),
                                                alpha_lvl,
                                                lower_wl=2, upper_wl=100)
  if(is.na(window_lth)){
    str_MLE_lth <- paste("AnomDetct::MLE_window_lth(x,dist_null = '",
                         dist_info[[1]], "',",
                         paste(dist_info[[2]],collapse = ","),
                         ",unit = ", MLE_unit, ")",sep = "")

    window_MLE <- eval(parse(text = str_MLE_lth))

    window_lth <- max(window_Haiman,window_MLE)
  }else{
    if(window_lth < window_Haiman){
      warning(paste("Too small `window_lth`, enlarge to", window_Haiman,
                    sep = ""))
      window_lth <- window_Haiman
    }
  }

  min_crit <- AnomDetct::crit_theta_fun(N-1, window_lth,
                                        1-exp(-theta_th), alpha_lvl)
  q_values <- mapply(AnomDetct::prob_fun, 1-exp(-seq_theta/theta_0),
                     N = N-1, k = min_crit,m = window_lth)

  if(est_fun == "pt"){
    density_est <- AnomDetct::HRR_pt_est(pt_int, cdf_x, HRR_kernel,
                                         hazard_bandwidth, knn)
  }else if(est_fun == "sbsp"){
    density_est <- AnomDetct::HRR_sbsp_est(pt_int, cdf_x, HRR_kernel,
                                           hazard_bandwidth, knn,
                                           n_hz_sample, n_hz_size)
  }else stop("Invalid `est_fun`", call. = F)

  fhat_fun <- density_est$fhat
  HRR_fun  <- density_est$HRR

  HRR <- HRR_fun(cdf_x)
  clst <- mapply(AnomDetct::hypo_test, seq_theta,
                 MoreArgs = list(cdf = cdf_x,
                                 HRR = HRR,
                                 critical_cnt = min_crit,
                                 window_l = window_lth),
                 SIMPLIFY = F)

  clst_p_values <- mapply(function(x,y)if(nrow(x)>0){cbind(x,y)}
                          else{cbind(x,numeric(0L))},
                          clst, mapply(max, 1 - q_values, 0),
                          SIMPLIFY = F)

  my_plot <- AnomDetct::clst_plot(x = x, org_x = x_backup,
                                  clst_p_values, alpha_lvl,
                                  unit = plot_unit, plt_mgn = plt_mgn)

  str_total <- paste("AnomDetct::anom_est(clst_p_values, anom_est_alpha_lvl, ",
                     "x_backup, fhat_fun, dist_null = '",dist_info[[1]],"',",
                     paste(dist_info[[2]],collapse = ","),")", sep = "")

  total <- eval(parse(text = str_total))

  clst_out <- do.call(rbind,clst_p_values)
  clst_out[,1:2] <- t(apply(clst_out[,1:2,drop = F],1,function(i)x_backup[i]))
  colnames(clst_out)[3] <- "p-value"
  clst_out <- clst_out[order(clst_out[,3],decreasing = T),]

  return(list(Total = total,
              Cluster = unique(clst_out),
              plot = my_plot))
}
