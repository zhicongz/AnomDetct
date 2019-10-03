tran_detct <- function(x, theta_th=1, theta_0 = theta_th, alpha_lvl=0.05,
                       loc, HRR_kernel = "triangular",
                       hazard_bandwidth=0.1, knn = NULL, est_fun = "pt",
                       n_hz_sample = NULL, n_hz_size = NULL,
                       pt_int = seq(0,1,by = 0.05),
                       seq_theta = seq(0.5, 1, by = 0.05)*theta_0,
                       x_unit = 0.01, plot_unit = 1, MLE_unit = 0.01,
                       plt_mgn = 0, max_rec = 3, tail_obs = 50){
  if(!requireNamespace("POT", quietly = TRUE))
    stop("Need package POT", call. = F)

  cur_rec <- 0
  while (cur_rec < max_rec){
    if (cur_rec == 0){
      x <- sort(x,decreasing = F)

      org_x <- x

      x <- x[x>loc]
      x_backup <- x

      x <- AnomDetct::unif(x,x_unit)

      para_hat <- POT::fitgpd(x_backup,threshold = loc)$param

      cdf_x <- AnomDetct::cdf_convert(x,dist_null = "gpd",loc = loc,
                                      scale = para_hat[1], shape = para_hat[2])

      cdf_x_backup <- cdf_x

      N <- length(x)
    }else{
      top_clst <- clst_p_values[[length(clst_p_values)]]
      bd <- top_clst[nrow(top_clst),2]

      x <- x_backup[(bd+1):length(x_backup)]
      cdf_x <- cdf_x_backup[(bd+1):length(cdf_x_backup)]

      N <- length(x)
    }

    if (N <= tail_obs){
      break
    }
    window_Haiman <- AnomDetct::Haiman_window_lth(N-1, p = 1-exp(-theta_th),
                                                  alpha_lvl,
                                                  lower_wl=2, upper_wl=100)

    window_MLE <- AnomDetct::MLE_window_lth(x,dist_null = "gpd",loc = loc,
                                            scale = para_hat[1],
                                            shape = para_hat[2])

    window_lth <- max(window_Haiman,window_MLE)

    if(cur_rec == 0){
      min_crit <- AnomDetct::crit_theta_fun(N-1, window_lth,
                                            1-exp(-theta_th), alpha_lvl)
      q_values <- mapply(AnomDetct::prob_fun, 1-exp(-seq_theta/theta_0),
                         N = N-1, k = min_crit,m = window_lth)
      p_values <- mapply(max, 1 - q_values, 0)

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

      clst_temp <- mapply(AnomDetct::hypo_test, seq_theta,
                          MoreArgs = list(cdf = cdf_x,
                                          HRR = HRR,
                                          critical_cnt = min_crit,
                                          window_l = window_lth),
                          SIMPLIFY = F)

      clst_p_values <- mapply(function(x,y)if(nrow(x)>0){cbind(x,y)}
                              else{cbind(x,numeric(0L))},
                              clst_temp, mapply(max, 1 - q_values, 0),
                              SIMPLIFY = F)

      if(nrow(clst_p_values[[length(clst_p_values)]])==0){
        break
      }
      cur_rec <- cur_rec + 1
    }else{
      min_crit <- AnomDetct::crit_theta_fun(N-1, window_lth,
                                            1-exp(-theta_th), alpha_lvl)
      q_values_temp <- mapply(AnomDetct::prob_fun, 1-exp(-seq_theta/theta_0),
                              N = N-1, k = min_crit,m = window_lth)

      p_values <- mapply(min,
                         mapply(max, 1 - q_values_temp, 0) + p_values,
                         1)

      clst_temp <- mapply(AnomDetct::hypo_test, seq_theta,
                          MoreArgs = list(cdf = cdf_x,
                                          HRR = HRR[bd:length(HRR)],
                                          critical_cnt = min_crit,
                                          window_l = window_lth),
                          SIMPLIFY = F)

      clst_temp <- mapply(function(x,y)if(nrow(x)>0){cbind(x+bd, y)}
                          else{cbind(x+bd, numeric(0L))},
                          clst_temp, mapply(max, 1 - q_values, 0),
                          SIMPLIFY = F)

      if(nrow(clst_temp[[length(clst_temp)]])==0){
        break
      }else{
        clst_p_values <- mapply(rbind,clst_p_values,clst_temp,
                                SIMPLIFY = F)
        cur_rec <- cur_rec + 1
      }
    }
  }

  my_plot <- AnomDetct::clst_plot(x = x_backup, org_x = org_x, clst_p_values,
                                  max(p_values), unit = plot_unit,
                                  plt_mgn = plt_mgn)

  total <- AnomDetct::anom_est(clst_p_values, max(p_values), x_backup,
                               fhat_fun, "gpd", loc = loc,
                               scale = para_hat[1], shape = para_hat[2])

  clst_out <- do.call(rbind,clst_p_values)
  clst_out[,1:2] <- t(apply(clst_out[,1:2,drop = F],1,function(i)x_backup[i]))
  colnames(clst_out)[3] <- "p-value"
  clst_out <- clst_out[order(clst_out[,3],decreasing = T),]

  return(list(Total = total,
              Cluster = clst_out,
              Plot = my_plot))
}
