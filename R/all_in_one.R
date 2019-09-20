clst_plot <- function(x, org_x = x, clst, alpha_lvl,
                      unit, x_range = c(min(org_x),max(org_x)),
                      plt_mgn = 0){
  clst   <- lapply(clst,function(x)x[x[,3]<=alpha_lvl,,drop = F])
  clst_x <- lapply(clst,function(foo)
    t(apply(foo[,1:2,drop = F],1,function(i)x[i])))

  ## cluster colors ##
  p_values <- unlist(lapply(clst,function(x)x[,3,drop = F]))
  num_col  <- (log10(alpha_lvl+10^-16) - log10(p_values+10^-16))/
    (log10(alpha_lvl+10^-16)-min(log10(10^-16)))

  ## histogram breaks ##
  bds <- ceiling(range(org_x/unit))
  breaks <- seq(bds[1]-1,bds[2])*unit

  if(requireNamespace("ggplot2", quietly = TRUE)){
    df_org_x <- data.frame(x = org_x)
    my_plot <- ggplot2::ggplot(df_org_x, ggplot2::aes(x)) +
      ggplot2::stat_bin(breaks = breaks,
                        closed = "right",
                        fill = "white",col = "black") +
      ggplot2::theme_bw() +
      ggplot2::coord_cartesian(xlim = x_range)

    y_max <- ggplot2::layer_scales(my_plot)$y$range$range[2]

    ## cluster data.frame convert ##
    plot_data <- function(ind){
      x_lim <- clst_x[[ind]]
      y_bot <- (ind-1)*y_max/length(clst_x)
      y_up  <- ind*y_max/length(clst_x)

      res   <- data.frame(x = numeric(0),
                          y = numeric(0),
                          col = numeric(0),
                          group = character(0))

      if(ncol(x_lim)>0){
        for (i in seq.int(nrow(x_lim))){
          temp <- data.frame(x = rep(c(x_lim[i,1] - plt_mgn,
                                       x_lim[i,2] + plt_mgn),each = 2),
                             y = c(y_bot,y_up,y_up,y_bot),
                             group = paste(ind,i,sep = "-"))
          res <- rbind.data.frame(res,temp)
        }
      }
      return(res)
    }

    if (requireNamespace("parallel", quietly = TRUE)){
      df <- parallel::mclapply(seq.int(length(clst_x)),plot_data)
    }else{
      df <- lapply(seq.int(length(clst_x)),plot_data)
    }

    df_clst <- do.call("rbind.data.frame",df)
    df_clst$col <- rep(num_col,each = 4)

    ## cluster plot ##
    p_round <- round(alpha_lvl,3)
    a <- nchar(p_round)-2
    b <- p_round*10^a
    p_labels <- b*10^-(seq(a,16,by = 2))
    p_breaks  <- (log10(alpha_lvl+10^-16) - log10(p_labels+10^-16))/
      (log10(alpha_lvl+10^-16)-min(log10(10^-16)))

    p_labels <- formatC(p_labels,format = "e",digits = 0)

    my_plot <- my_plot +
      ggplot2::geom_polygon(data = df_clst,
                            ggplot2::aes(x = x, y = df_clst$y,
                                         group = df_clst$group, fill = col))+
      ggplot2::scale_fill_gradient(
        low = grDevices::rgb(red = 1,green = 0.7,blue = 0.7,alpha = 0.6),
        high = grDevices::rgb(red = 1,green = 0.1,blue = 0.1,alpha = 0.6),
        breaks = p_breaks, name = expression( ~ alpha ~level),
        labels = p_labels, guide = "colourbar",limits = c(0,1)) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     legend.position = "right",
                     legend.key.height = ggplot2::unit(0.1,"npc"),
                     plot.title = ggplot2::element_text(hjust = 0.5,size = 40),
                     axis.title = ggplot2::element_text(size = 25),
                     axis.text = ggplot2::element_text(size = 20),
                     legend.text = ggplot2::element_text(size = 20),
                     legend.title = ggplot2::element_text(size = 25)) +
      ggplot2::guides(fill = ggplot2::guide_colorbar(reverse=T)) +
      ggplot2::xlab("")
    return(my_plot)
  }else{
    old_par_mar <- graphics::par()$mar
    ## split plot in left and right parts ##
    graphics::layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(9,1))

    graphics::par(mar=c(5, 4, 4, 3) + 0.1)
    graphics::par(yaxs = 'r')

    graphics::hist(org_x,breaks = breaks,xlim = x_range,
                   xlab = NA,ylab = NA, main = NA, cex.axis = 1.5)
    graphics::title(ylab = "count", line = 2.5, cex.lab = 2)

    y_scale <- graphics::par("usr")[4]

    g_b_col <- num_col * 0.6 + 0.1
    hm_color <- grDevices::rgb(red = 1, green = g_b_col, blue = g_b_col,
                               alpha = 0.6, maxColorValue = 1)

    ## colorful bars ##
    ## cluster data.frame convert ##
    plot_data <- function(ind){
      x_lim <- clst_x[[ind]]
      y_bot <- (ind-1)*y_max/length(clst_x)
      y_up  <- ind*y_max/length(clst_x)

      res   <- data.frame(x = numeric(0),
                          y = numeric(0))

      if(ncol(x_lim)>0){
        for (i in seq.int(nrow(x_lim))){
          temp <- data.frame(x = c(rep(c(x_lim[i,1] - plt_mgn,
                                         x_lim[i,2] + plt_mgn),each = 2),NA),
                             y = c(y_bot,y_up,y_up,y_bot,NA))
          res <- rbind.data.frame(res,temp)
        }
      }
      return(res)
    }

    if (requireNamespace("parallel", quietly = TRUE)){
      df <- parallel::mclapply(seq.int(length(clst_x)),plot_data)
    }else{
      df <- lapply(seq.int(length(clst_x)),plot_data)
    }

    df_clst <- do.call("rbind.data.frame",df)

    graphics::polygon(df_clst$x, df_clst$y,
                      col = hm_color, border = NA)
    #------------------------------- plot legend ------------------------------#
    graphics::par(mar=c(2,0,2.5,2.5))
    graphics::par(yaxs = 'i')

    ## legend colors ##
    p_round <- round(alpha_lvl,3)
    a <- nchar(p_round)-2
    b <- p_round*10^a
    p_labels <- b*10^-(seq(a,16,by = 2))
    p_breaks  <- (log10(alpha_lvl+10^-16) - log10(p_labels+10^-16))/
      (log10(alpha_lvl+10^-16)-min(log10(p_labels+10^-16)))

    p_labels <- formatC(p_labels,format = "e",digits = 0)

    label_color <- grDevices::rgb(red = 1,
                                  green = p_breaks*0.6+0.1,
                                  blue = p_breaks*0.6+0.1,
                                  alpha = 0.6, maxColorValue = 1)

    graphics::plot(NA, axes = F,
                   ylim = c(0.5,length(p_labels)+0.5), xlim = c(0,1))
    graphics::rect(xleft = 0.2,
                   ybottom = length(p_labels):1 - 0.5,
                   xright = 0.7,
                   ytop = length(p_labels):1 + 0.5,
                   col = label_color,border = NA)

    graphics::axis(4,at = length(p_labels):1,
                   labels = p_labels, cex.axis = 1.5, tick =F)
    graphics::axis(2,at = length(p_labels)/2,
                   labels = expression(alpha),tick = F,
                   cex.axis = 1.5, las = 1)

    ## reset par() ##
    graphics::par(mfrow=c(1,1))
    graphics::par(mar = old_par_mar)
    graphics::par(yaxs = 'r')
    return(grDevices::recordPlot())
  }
}
tran_detct <- function(x, theta_th=1, theta_0 = theta_th, alpha_lvl=0.05,
                       loc, HRR_kernel = "triangular",
                       hazard_bandwidth=0.1, knn = NULL, est_fun = "pt",
                       n_hz_sample = NULL, n_hz_size = NULL,
                       pt_int = seq(0,1,by = 0.05),
                       seq_theta = seq(0.5, 1, by = 0.05)*theta_0/theta_th,
                       x_unit = 0.01, plot_unit = 1, MLE_unit = 0.01,
                       plt_mgn = 0, max_rec = 3, tail_obs = 50){
  if(!requireNamespace("POT", quietly = TRUE)) stop("Need package POT")

  cur_rec <- 0
  while (cur_rec < max_rec){
    if (cur_rec == 0){
      x <- sort(x,decreasing = F)

      org_x <- x

      x <- x[x>loc]
      x_backup <- x

      x <- unif(x,x_unit)

      para_hat <- POT::fitgpd(x_backup,threshold = loc)$param

      cdf_x <- cdf_convert(x,dist_null = "gpd",loc = loc,
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
    window_Haiman <- Haiman_window_lth(N, p = 1-exp(-theta_th),
                                       alpha_lvl, lower_wl=2, upper_wl=100)

    window_MLE <- MLE_window_lth(x,dist_null = "gpd",loc = loc,
                                 scale = para_hat[1], shape = para_hat[2])

    window_lth <- max(window_Haiman,window_MLE)

    if(cur_rec == 0){
      min_crit <- crit_theta_fun(N,window_lth,1-exp(-theta_th),alpha_lvl)
      q_values <- mapply(prob_fun,1-exp(-seq_theta/theta_0*theta_th),
                         N = N, k = min_crit,m = window_lth)
      p_values <- mapply(max, 1 - q_values, 0)

      if(est_fun == "pt"){
        density_est <- HRR_pt_est(pt_int, cdf_x, HRR_kernel,
                                  hazard_bandwidth, knn)
      }else if(est_fun == "sbsp"){
        density_est <- HRR_sbsp_est(pt_int, cdf_x, HRR_kernel, hazard_bandwidth,
                                    knn, n_hz_sample, n_hz_size)
      }else stop("Invalid est_fun")

      fhat_fun <- density_est$fhat
      HRR_fun  <- density_est$HRR

      HRR <- HRR_fun(cdf_x)

      clst_temp <- mapply(hypo_test, seq_theta,
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
      min_crit <- crit_theta_fun(N,window_lth,1-exp(-theta_th),alpha_lvl)
      q_values_temp <- mapply(prob_fun,1-exp(-seq_theta/theta_0*theta_th),
                              N = N, k = min_crit,m = window_lth)

      p_values <- mapply(min,
                         mapply(max, 1 - q_values_temp, 0) + p_values,
                         1)

      clst_temp <- mapply(hypo_test, seq_theta,
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

  my_plot <- clst_plot(x = x_backup, org_x = org_x, clst_p_values,
                       max(p_values), unit = plot_unit, plt_mgn = plt_mgn)

  total <- anom_est(clst_p_values, max(p_values), x_backup, fhat_fun, "gpd",
                    loc = loc, scale = para_hat[1], shape = para_hat[2])

  clst_out <- do.call(rbind,clst_p_values)
  clst_out[,1:2] <- t(apply(clst_out[,1:2,drop = F],1,function(i)x_backup[i]))
  colnames(clst_out)[3] <- "p-value"
  clst_out <- clst_out[order(clst_out[,3],decreasing = T),]

  return(list(Total = total,
              Cluster = clst_out,
              plot = my_plot))
}
ultimate_detct <- function(x, theta_th=1, theta_0 = theta_th,
                           alpha_lvl = 0.05, anom_est_alpha_lvl = 0.05,
                           dist_null = NA, ..., HRR_kernel = "triangular",
                           hazard_bandwidth = 0.1, knn = NULL, est_fun = "pt",
                           n_hz_sample = NULL, n_hz_size = NULL,
                           pt_int = seq(0,1,by = 0.05), window_lth = NA,
                           seq_theta = seq(0.5, 1, by = 0.05)*theta_0/theta_th,
                           x_unit = 0.01, plot_unit = 1, MLE_unit = 1,
                           plt_mgn = 0){
  N <- length(x)

  x <- sort(x,decreasing = F)

  x_backup <- x

  x <- unif(x,x_unit)

  if(is.na(dist_null)){
    dist_info <- fit_dist(x_backup)
  }else{
    dist_info <- list(dist_null,c(...))
  }

  str_convert <- paste("cdf_convert(x,dist_null = '",dist_info[[1]],"',",
                       paste(dist_info[[2]],collapse = ","),
                       ")",sep = "")
  cdf_x <- eval(parse(text = str_convert))

  if(is.na(window_lth)){
    window_Haiman <- Haiman_window_lth(N, 1-exp(-theta_th), alpha_lvl,
                                       lower_wl=2, upper_wl=100)

    str_MLE_lth <- paste("MLE_window_lth(x,dist_null = '",dist_info[[1]],"',",
                         paste(dist_info[[2]],collapse = ","),
                         ",unit = ", MLE_unit,
                         ")",sep = "")

    window_MLE <- eval(parse(text = str_MLE_lth))

    window_lth <- max(window_Haiman,window_MLE)
  }

  min_crit <- crit_theta_fun(N, window_lth, 1-exp(-theta_th), alpha_lvl)
  q_values <- mapply(prob_fun,1-exp(-seq_theta/theta_0*theta_th),
                     N = N, k = min_crit,m = window_lth)

  if(est_fun == "pt"){
    density_est <- HRR_pt_est(pt_int, cdf_x, HRR_kernel, hazard_bandwidth, knn)
  }else if(est_fun == "sbsp"){
    density_est <- HRR_sbsp_est(pt_int, cdf_x, HRR_kernel, hazard_bandwidth,
                                knn, n_hz_sample, n_hz_size)
  }else stop("Invalid est_fun")

  fhat_fun <- density_est$fhat
  HRR_fun  <- density_est$HRR

  HRR <- HRR_fun(cdf_x)

  clst <- mapply(hypo_test, seq_theta,
                 MoreArgs = list(cdf = cdf_x,
                                 HRR = HRR,
                                 critical_cnt = min_crit,
                                 window_l = window_lth),
                 SIMPLIFY = F)

  clst_p_values <- mapply(function(x,y)if(nrow(x)>0){cbind(x,y)}
                          else{cbind(x,numeric(0L))},
                          clst, mapply(max, 1 - q_values, 0),
                          SIMPLIFY = F)

  my_plot <- clst_plot(x = x, org_x = x_backup, clst_p_values,
                       alpha_lvl, unit = plot_unit, plt_mgn = plt_mgn)

  str_total <- paste("anom_est(clst_p_values, anom_est_alpha_lvl, x_backup, ",
                     "fhat_fun, dist_null = '",dist_info[[1]],"',",
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
fit_dist <- function(x,dist_null = NA,...){
  oldw <- getOption("warn")
  options(warn = -1)

  if(min(x)>0)sum_log_x <- sum(log(x))
  sum_x <- sum(x)
  if(is.na(dist_null)){
    list_normal <- list(stats::ks.test(x,stats::pnorm,
                                       mean(x),stats::sd(x))$p.value,
                        "norm",
                        c(mean(x),stats::sd(x)))

    list_unif <- list(stats::ks.test(x,stats::punif,min(x),max(x))$p.value,
                      "unif",
                      c(min(x),max(x)))

    cauchy_den <- function(para){
      l <- para[1]
      s <- para[2]
      n <- length(x)
      return(n*log(pi*s) + sum(log(1+((x-l)/s)^2)))
    }

    para_hat <- tryCatch(stats::optim(par = c(1,1), fn = cauchy_den,
                                      method = "L-BFGS-B",
                                      lower = c(-Inf,0))$par,
                         error = function(e) NA)

    if(is.na(para_hat[1])){
      list_cauchy <- list(0,"cauchy",NA)
    }else{
      list_cauchy <- list(stats::ks.test(x,stats::pcauchy,
                                         para_hat[1],para_hat[2])$p.value,
                          "cauchy",
                          para_hat)
    }

    logis_den <- function(para){
      l <- para[1]
      s <- para[2]
      n <- length(x)
      return(n*log(s) - sum((x-l)/s) + 2*sum(log(1+exp((x-l)/s))))
    }

    para_hat <- tryCatch(stats::optim(par = c(1,1), fn = logis_den,
                                      method = "L-BFGS-B",lower = c(-Inf,0))$par,
                         error = function(e) NA)

    if(is.na(para_hat[1])){
      list_logis <- list(0,"logis",NA)
    }else{
      list_logis <- list(stats::ks.test(x,stats::plogis,
                                        para_hat[1],para_hat[2])$p.value,
                         "logis",
                         para_hat)
    }

    t_den <- function(v){
      n <- length(x)
      return(-n*log(gamma((v+1)/2)/sqrt(v*pi)/gamma(v/2)) +
               (v+1)/2*sum(log(1+x^2/v)))
    }

    para_hat <- tryCatch(stats::optim(par = c(1,1), fn = t_den,
                                      method = "L-BFGS-B",lower = c(-Inf,0))$par,
                         error = function(e) NA)

    if(is.na(para_hat[1])){
      list_t <- list(0,"t",NA)
    }else{
      list_t <- list(stats::ks.test(x,stats::pt,para_hat)$p.value,
                     "t",
                     para_hat)
    }

    candid <- list(list_normal,list_unif,list_cauchy,list_logis)

    if(min(x)>0){
      gamma_den <- function(para){
        alp <- para[1]
        bet <- para[2]
        n <- length(x)
        return(-(n*alp*log(bet)-n*log(gamma(alp))+(alp-1)*sum_log_x-bet*sum_x))
      }

      para_hat <- tryCatch(stats::optim(par = c(1,1), fn = gamma_den,
                                        method = "L-BFGS-B",lower = 0)$par,
                           error = function(e) NA)

      if(is.na(para_hat[1])){
        list_gamma <- list(0,"gamma",NA)
      }else{
        list_gamma <- list(stats::ks.test(x,stats::pgamma,
                                          para_hat[1],para_hat[2])$p.value,
                           "gamma",
                           para_hat)
      }

      chisq_den <- function(p){
        n <- length(x)
        return(log(gamma(p/2)) + n*p/2*log(2) - (p/2-1)*sum_log_x + sum_x/2)
      }

      para_hat <- tryCatch(stats::optim(par = 1, fn = chisq_den,
                                        method = "L-BFGS-B",lower = 0)$par,
                           error = function(e) NA)

      if(is.na(para_hat[1])){
        list_chisq <- list(0,"chisq",NA)
      }else{
        list_chisq <- list(stats::ks.test(x,stats::pchisq,para_hat)$p.value,
                           "chisq",
                           para_hat)
      }

      exp_den <- function(lambda){
        n <- length(x)
        return(-n*log(lambda) + lambda*sum_x)
      }

      para_hat <- tryCatch(stats::optim(par = 1, fn = exp_den,
                                        method = "L-BFGS-B",lower = 0)$par,
                           error = function(e) NA)

      if(is.na(para_hat[1])){
        list_exp <- list(0,"exp",NA)
      }else{
        list_exp <- list(stats::ks.test(x,stats::pexp,para_hat)$p.value,
                         "exp",
                         para_hat)
      }

      f_den <- function(para){
        n1 <- para[1]
        n2 <- para[2]
        n <- length(x)
        return(-n*log(gamma((n1+n2)/2)/(gamma(n1/2)*gamma(n2/2))) -
                 n*n1/2*log((n1/n2)) - (n1/2-1)*sum_log_x +
                 (n1+n2)/2*sum(log(1+n1/n2*x)))
      }

      para_hat <- tryCatch(stats::optim(par = c(1,1), fn = f_den,
                                        method = "L-BFGS-B",lower = 0)$par,
                           error = function(e) NA)

      if(is.na(para_hat[1])){
        list_f <- list(0,"f",NA)
      }else{
        list_f <- list(stats::ks.test(x,stats::pf,
                                      para_hat[1],para_hat[2])$p.value,
                       "f",
                       para_hat)
      }

      lnorm_den <- function(para){
        l <- para[1]
        s <- para[2]
        n <- length(x)
        return(sum_log_x + n*log(s*sqrt(2*pi)) + sum((log(x)-l)^2/(2*s^2)))
      }

      para_hat <- tryCatch(stats::optim(par = c(1,1), fn = lnorm_den,
                                        method = "L-BFGS-B",lower = 0)$par,
                           error = function(e) NA)

      if(is.na(para_hat[1])){
        list_lnorm <- list(0,"lnorm",NA)
      }else{
        list_lnorm <- list(stats::ks.test(x,stats::plnorm,
                                          para_hat[1],para_hat[2])$p.value,
                           "lnorm",
                           para_hat)
      }

      weibull_den <- function(para){
        a <- para[1]
        b <- para[2]
        n <- length(x)
        return(-n*log(a/b)-(a-1)*sum(log(x/b))+sum((x/b)^a))
      }

      para_hat <- tryCatch(stats::optim(par = c(1,1), fn = weibull_den,
                                        method = "L-BFGS-B",lower = 0)$par,
                           error = function(e) NA)

      if(is.na(para_hat[1])){
        list_weibull <- list(0,"weibull",NA)
      }else{
        list_weibull <- list(stats::ks.test(x,stats::pweibull,
                                            para_hat[1],para_hat[2])$p.value,
                             "weibull",
                             para_hat)
      }

      candid <- c(candid,list(list_gamma,list_chisq,list_exp,
                              list_f,list_lnorm,list_weibull))

      if(max(x)<1){
        beta_den <- function(para){
          alp <- para[1]
          bet <- para[2]
          n <- length(x)
          return(-(n*log(gamma(alp+bet)/(gamma(alp)*gamma(bet))) +
                     (alp-1)*sum_log_x + (bet-1)*sum(log(1-x))))
        }

        para_hat <- tryCatch(stats::optim(par = c(1,1), fn = beta_den,
                                          method = "L-BFGS-B",lower = 0)$par,
                             error = function(e) NA)

        if(is.na(para_hat[1])){
          list_beta <- list(0,"beta",NA)
        }else{
          list_beta <- list(stats::ks.test(x,stats::pbeta,
                                           para_hat[1],para_hat[2])$p.value,
                            "beta",
                            para_hat)
        }
        candid <- c(candid,list(list_beta))
      }
    }
    options(warn = oldw)

    max_ind <- which.max(sapply(candid,function(x)x[[1]]))
    return(list(dist = candid[[max_ind]][[2]],
                para = candid[[max_ind]][[3]]))
  }

  if(dist_null == "gpd"){
    if(requireNamespace("POT", quietly = TRUE)){
      para_hat <- POT::fitgpd(x,...)$param
    }else{
      stop("Need to install POT package")
    }
    return(list(dist = dist_null, para = c(...,para_hat)))
  }else if(dist_null == "norm"){
    return(list(dist = dist_null, para = c(mean(x),stats::sd(x))))
  }else if(dist_null == "unif"){
    return(list(dist = dist_null, para = c(min(x),max(x))))
  }else if(dist_null == "gamma"){
    gamma_den <- function(para){
      alp <- para[1]
      bet <- para[2]
      n <- length(x)
      return(-(n*alp*log(bet)-n*log(gamma(alp))+(alp-1)*sum_log_x-bet*sum_x))
    }

    para_hat <- stats::optim(par = c(1,1), fn = gamma_den,
                             method = "L-BFGS-B",lower = 0)$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "chisq"){
    chisq_den <- function(p){
      n <- length(x)
      return(log(gamma(p/2)) + n*p/2*log(2) - (p/2-1)*sum_log_x + sum_x/2)
    }

    para_hat <- stats::optim(par = 1, fn = chisq_den,
                             method = "L-BFGS-B",lower = 0)$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "beta"){
    beta_den <- function(para){
      alp <- para[1]
      bet <- para[2]
      n <- length(x)
      return(-(n*log(gamma(alp+bet)/(gamma(alp)*gamma(bet))) +
                 (alp-1)*sum_log_x + (bet-1)*sum(log(1-x))))
    }

    para_hat <- stats::optim(par = c(1,1), fn = beta_den,
                             method = "L-BFGS-B",lower = 0)$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "cauchy"){
    cauchy_den <- function(para){
      l <- para[1]
      s <- para[2]
      n <- length(x)
      return(n*log(pi*s) + sum(log(1+((x-l)/s)^2)))
    }

    para_hat <- stats::optim(par = c(1,1), fn = cauchy_den,
                             method = "L-BFGS-B",lower = c(-Inf,0))$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "exp"){
    exp_den <- function(lambda){
      n <- length(x)
      return(-n*log(lambda) + lambda*sum_x)
    }

    para_hat <- stats::optim(par = 1, fn = exp_den,
                             method = "L-BFGS-B",lower = 0)$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "f"){
    f_den <- function(para){
      n1 <- para[1]
      n2 <- para[2]
      n <- length(x)
      return(-n*log(gamma((n1+n2)/2)/(gamma(n1/2)*gamma(n2/2))) -
               n*n1/2*log((n1/n2)) - (n1/2-1)*sum_log_x +
               (n1+n2)/2*sum(log(1+n1/n2*x)))
    }

    para_hat <- stats::optim(par = c(1,1), fn = f_den,
                             method = "L-BFGS-B",lower = 0)$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "logis"){
    logis_den <- function(para){
      l <- para[1]
      s <- para[2]
      n <- length(x)
      return(n*log(s) - sum((x-l)/s) + 2*sum(log(1+exp((x-l)/s))))
    }

    para_hat <- stats::optim(par = c(1,1), fn = logis_den,
                             method = "L-BFGS-B",lower = c(-Inf,0))$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "lnorm"){
    lnorm_den <- function(para){
      l <- para[1]
      s <- para[2]
      n <- length(x)
      return(sum_log_x + n*log(s*sqrt(2*pi)) + sum((log(x)-l)^2/(2*s^2)))
    }

    para_hat <- stats::optim(par = c(1,1), fn = lnorm_den,
                             method = "L-BFGS-B",lower = c(0,0))$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "t"){
    t_den <- function(v){
      n <- length(x)
      return(-n*log(gamma((v+1)/2)/sqrt(v*pi)/gamma(v/2)) +
               (v+1)/2*sum(log(1+x^2/v)))
    }

    para_hat <- stats::optim(par = 1, fn = t_den,
                             method = "L-BFGS-B",lower = 0)$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "weibull"){
    weibull_den <- function(para){
      a <- para[1]
      b <- para[2]
      n <- length(x)
      return(-n*log(a/b)-(a-1)*sum(log(x/b))+sum((x/b)^a))
    }

    para_hat <- stats::optim(par = c(1,1), fn = weibull_den,
                             method = "L-BFGS-B",lower = 0)$par

    return(list(dist = dist_null, para = para_hat))
  }
}
HRR_bstp_lb <- function(N, pt_int, kernel = "gaussian", hazard_bandwidth = NULL,
                        knn = NULL, mc_rep, c_l, est_fun, ...){
  my_list <- replicate(mc_rep, sort(stats::runif(N)),simplify = F)
  if(est_fun == "pt"){
    if (requireNamespace("parallel", quietly = TRUE)){
      res <- parallel::mcmapply(HRR_pt_est, my_list,
                                MoreArgs = list(pt_int = pt_int,
                                                kernel = kernel,
                                                hazard_bandwidth =
                                                  hazard_bandwidth,
                                                knn = knn), SIMPLIFY = F)
    }else{
      res <-mapply(HRR_pt_est, my_list,
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
      res <- parallel::mcmapply(HRR_sbsp_est, my_list,
                                MoreArgs = list(pt_int = pt_int,
                                                kernel = kernel,
                                                hazard_bandwidth =
                                                  hazard_bandwidth,
                                                knn = knn,
                                                n_hz_sample = n_hz_sample,
                                                n_hz_size   = n_hz_size),
                                SIMPLIFY = F)
    }else{
      res <- mapply(HRR_sbsp_est, my_list,
                    MoreArgs = list(pt_int = pt_int,
                                    kernel = kernel,
                                    hazard_bandwidth = hazard_bandwidth,
                                    knn = knn,
                                    n_hz_sample = n_hz_sample,
                                    n_hz_size   = n_hz_size),SIMPLIFY = F)
    }
  }else stop("Invalid est_fun")

  HRR  <- sapply(res, function(x)x[[2]](pt_int))
  max_ratio <- apply(HRR,2,max)

  return(stats::quantile(1/max_ratio,1-c_l))
}
anom_est <- function(clst_p_values, alpha_lvl,
                     x, fhat = NULL, dist_null = "norm", ...){
  clst <- do.call(rbind,clst_p_values)
  clst[,1] <- x[clst[,1]]
  clst[,2] <- x[clst[,2]]

  bds <- clst[clst[,3]<=alpha_lvl,1:2,drop = F]

  n <- nrow(bds)
  if(n==0){
    return(0)
  }else if(n==1){
    res <- bds
  }else{
    bds - bds[order(bds[,1]),,drop = F]

    res <- bds[1,,drop = F]
    for (i in seq.int(2,nrow(bds))){
      if(res[nrow(res),2]<bds[i,1]){
        res <- rbind(res,bds[i,])
      }else{
        res[nrow(res),2] <- max(res[nrow(res),2],bds[i,2])
      }
    }
  }

  amt <- apply(res,1,function(temp){
    a <- temp[1]
    b <- temp[2]
    total_amt <- sum(x[x>=a & x<=b])
    exp_amt   <- F_exp(a,b,length(x),fhat,unit = b-a,
                       dist_null = dist_null, ...)
    return(total_amt - exp_amt)
  })

  return(sum(amt))
}
F_exp <- function(a, b, N, fhat = NULL, unit = b-a,
                  dist_null = "norm", ...){
  f_x <- function(x_pt){
    if(dist_null == "gpd"){
      if(requireNamespace("POT", quietly = TRUE)){
        y <- POT::pgpd(x_pt, ...)
        exp_density <- POT::dgpd(x_pt, ...)
      }else{
        stop("Need package POT")
      }
    }else{
      my_str <- paste("stats::p",dist_null,"(x_pt,...)",sep = "")
      y <- eval(parse(text = my_str))

      my_str <- paste("stats::d",dist_null,"(x_pt,...)",sep = "")
      exp_density  <- eval(parse(text = my_str))
    }

    if(is.null(fhat)){
      return(exp_density)
    }else{
      return(fhat(y)*exp_density)
    }
  }

  if(a==b) return(0)
  if(unit>b-a) stop("need smaller unit")
  pt_int <- seq(a, b, by = unit)
  if((b-a)%%unit!=0) pt_int <- c(pt_int, b)
  return(sum(sapply(seq.int(length(pt_int)-1),
                    function(i)1/4*N*(f_x(pt_int[i])+f_x(pt_int[i+1]))*
                      (pt_int[i+1]-pt_int[i])*(pt_int[i+1]+pt_int[i]))))
}
cdf_convert <- function(x, dist_null, ...){
  if(dist_null == "gpd"){
    if(requireNamespace("POT", quietly = TRUE)){
      cdf_x <- POT::pgpd(x, ...)
    }else{
      stop("Need package POT")
    }
  }else{
    my_str <- paste("p",dist_null,"(x, ...)",sep = "")
    cdf_x  <- eval(parse(text = my_str))
  }
  return(cdf_x)
}
MLE_window_lth <- function(x,dist_null,..., unit = 1){
  if(unit<=0 | is.infinite(unit)){
    stop("Unit needs to be positive finite number")
  }

  y <- x/unit
  bds <- ceiling(range(y))
  breaks <- seq(bds[1]-1,bds[2])*unit
  x_cnt <- table(cut(x,breaks = breaks))

  if(dist_null == "gpd"){
    if(requireNamespace("POT", quietly = TRUE)){
      exp_cnt <- diff(POT::pgpd(breaks, ...))
    }else{
      stop("Need package POT")
    }
  }else{
    my_str <- paste("p",dist_null,"(breaks, ...)",sep = "")
    exp_cnt  <- diff(eval(parse(text = my_str)))*length(x)
  }
  return(round(max(x_cnt - exp_cnt)))
}
hypo_test <- function(cdf, HRR = rep(1,length(cdf)-1),
                      theta, critical_cnt, window_l){
  N <- length(cdf)
  expd_cdf <- theta*HRR[1:(N-1)]*(1 - cdf[1:(N-1)])/(N:2) + cdf[1:(N-1)]
  indicator <- cdf[2:N] <= expd_cdf

  cnt <- rolling_sum(temp = indicator, window_l = window_l)
  clst_loc <- which(cnt >= critical_cnt)

  start_index <- c(clst_loc[1],
                   ifelse(diff(clst_loc) < window_l, NA, clst_loc[-1])) + 1
  end_index <- c(ifelse(diff(clst_loc) < window_l,
                        NA, clst_loc[-length(clst_loc)] + window_l),
                 clst_loc[length(clst_loc)] + window_l)
  start_index <- stats::na.omit(start_index)
  end_index <- stats::na.omit(end_index)

  clst <- matrix(c(start_index, end_index), ncol = 2, byrow = F)
  colnames(clst) <- c("start", "end")

  return(clst)
}
crit_theta_fun <- function(N, window_l, p, alpha){
  lower_crit <- 0
  upper_crit <- window_l

  while(lower_crit < upper_crit){
    critical_cnt <- floor(mean(c(lower_crit,upper_crit)))
    p_sc <- prob_fun(N, critical_cnt, window_l, p)
    if(p_sc < 1-alpha){
      critical_cnt <- critical_cnt + 1
      lower_crit <- critical_cnt
    }else{
      upper_crit <- critical_cnt
    }
  }

  return(critical_cnt)
}
HRR_sbsp_est <- function(pt_int,cdf,kernel = "gaussian",hazard_bandwidth = NA,
                         knn = NA, n_hz_sample, n_hz_size = length(cdf)){

  cdf_samples <- replicate(n_hz_sample,
                           sort(sample(cdf,n_hz_size,replace = T)),
                           simplify = FALSE)

  if (requireNamespace("parallel", quietly = TRUE)){
    sample_est <- parallel::mcmapply(HRR_pt_est, cdf_samples,
                                     MoreArgs = list(pt_int = pt_int,
                                                     hazard_bandwidth =
                                                       hazard_bandwidth,
                                                     kernel = kernel,
                                                     knn = knn),SIMPLIFY = F)
  }else{
    sample_est <-mapply(HRR_pt_est, cdf_samples,
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
HRR_pt_est <- function(pt_int,cdf_sample,kernel = "gaussian",
                       hazard_bandwidth = NULL, knn = NULL){
  if(requireNamespace("Matrix", quietly = TRUE)){
    ls_fit <- function(pt,cdf_sample,kernel,bandwidth,knn){
      cdf_ext <- c(rev(-cdf_sample),cdf_sample,rev(2-cdf_sample))
      Yhat <- (1:length(cdf_ext))/length(cdf_ext)
      if (kernel == "gaussian"){
        if(is.null(hazard_bandwidth))stop("Need hazard_bandwidth")
        Wp       <- diag(sqrt(stats::dnorm((cdf_ext-pt)/bandwidth)))
        dsg_mtx  <- cbind(rep(1,length(cdf_ext)),
                          (cdf_ext-pt),
                          (cdf_ext-pt)^2)

        X_mtx    <- Wp %*% dsg_mtx
        beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx, t(X_mtx)%*%Wp%*%Yhat)
      }else if (kernel == "rectangular"){
        if(is.null(hazard_bandwidth))stop("Need hazard_bandwidth")
        index <- abs(cdf_ext-pt) < bandwidth/2

        cdf_sample  <- cdf_ext[index]
        rnk <- length(unique(cdf_sample))

        if(rnk==0){
          beta_hat <- c(NA,0,0)
        }else if(rnk==1){
          beta_hat <- c(mean(cdf_sample),
                        length(cdf_sample)/length(cdf_ext)/hazard_bandwidth*3,
                        0)
        }else if(rnk==2){
          Yhat_sample <- Yhat[index]

          X_mtx  <- cbind(rep(1,length(cdf_sample)), (cdf_sample-pt))
          beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx,
                                    t(X_mtx)%*%Wp%*%Yhat_sample)
        }else{
          Yhat_sample <- Yhat[index]

          X_mtx  <- cbind(rep(1,length(cdf_sample)),
                          (cdf_sample-pt), (cdf_sample-pt)^2)
          beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx,
                                    t(X_mtx)%*%Yhat_sample)
        }
      }else if (kernel == "triangular"){
        if(is.null(hazard_bandwidth))stop("Need hazard_bandwidth")
        index <- abs(cdf_ext-pt)<bandwidth/2

        cdf_sample  <- cdf_ext[index]
        rnk <- length(unique(cdf_sample))

        if(rnk==0){
          beta_hat <- c(NA,0,0)
        }else if(rnk==1){
          beta_hat <- c(mean(cdf_sample),
                        length(cdf_sample)/length(cdf_ext)/hazard_bandwidth*3,
                        0)
        }else if(rnk==2){
          Yhat_sample <- Yhat[index]

          Wp <- diag(sqrt(abs(cdf_ext-pt)[index]))
          dsg_mtx  <- cbind(rep(1, length(cdf_sample)), (cdf_sample-pt))

          X_mtx    <- Wp %*% dsg_mtx
          beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx,
                                    t(X_mtx)%*%Wp%*%Yhat_sample)
        }else{
          Yhat_sample <-    Yhat[index]

          Wp <- diag(sqrt(abs(cdf_ext-pt)[index]))
          dsg_mtx  <- cbind(rep(1,length(cdf_sample)),
                            (cdf_sample-pt),
                            (cdf_sample-pt)^2)

          X_mtx    <- Wp %*% dsg_mtx
          beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx,
                                    t(X_mtx)%*%Wp%*%Yhat_sample)
        }
      }else if (kernel == "knn"){
        if(is.null(knn))stop("Need knn")
        if(knn<=3)stop("Need larger knn")
        dist <- abs(cdf_ext-pt)
        index <- order(dist)[1:knn]

        cdf_sample  <- sort(cdf_ext[index])
        Yhat_sample <- sort(Yhat[index])

        X_mtx  <- cbind(rep(1,knn), (cdf_sample-pt), (cdf_sample-pt)^2)
        beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx,
                                  t(X_mtx)%*%Yhat_sample)
      }else{
        stop("Invalid kernel")
      }

      H_hat    <- beta_hat[2]/(1 - beta_hat[1])
      H_unif   <- (1/3)/(1-(pt+1)/3)

      theta_i  <- H_unif/H_hat
      return(list(fhat = beta_hat[2]*3,
                  HRR  = theta_i))
    }
  }else{
    ls_fit <- function(pt,cdf_sample,kernel,bandwidth,knn){
      cdf_ext <- c(rev(-cdf_sample),cdf_sample,rev(2-cdf_sample))
      Yhat <- (1:length(cdf_ext))/length(cdf_ext)
      if (kernel == "gaussian"){
        if(is.null(hazard_bandwidth))stop("Need hazard_bandwidth")
        Wp       <- diag(sqrt(stats::dnorm((cdf_ext-pt)/bandwidth)))
        dsg_mtx  <- cbind(rep(1,length(cdf_ext)),
                          (cdf_ext-pt),
                          (cdf_ext-pt)^2)

        X_mtx    <- Wp %*% dsg_mtx
        beta_hat <- qr.solve(X_mtx, Wp%*%Yhat, tol = .Machine$double.eps)
      }else if (kernel == "rectangular"){
        if(is.null(hazard_bandwidth))stop("Need hazard_bandwidth")
        index <- abs(cdf_ext-pt) < bandwidth/2

        cdf_sample  <- cdf_ext[index]
        rnk <- length(unique(cdf_sample))

        if(rnk==0){
          beta_hat <- c(NA,0,0)
        }else if(rnk==1){
          beta_hat <- c(mean(cdf_sample),
                        length(cdf_sample)/length(cdf_ext)/hazard_bandwidth*3,
                        0)
        }else if(rnk==2){
          Yhat_sample <- Yhat[index]

          X_mtx  <- cbind(rep(1,length(cdf_sample)), (cdf_sample-pt))
          beta_hat <- qr.solve(X_mtx, Yhat_sample, tol = .Machine$double.eps)
        }else{
          Yhat_sample <- Yhat[index]

          X_mtx  <- cbind(rep(1,length(cdf_sample)),
                          (cdf_sample-pt), (cdf_sample-pt)^2)
          beta_hat <- qr.solve(X_mtx, Yhat_sample, tol = .Machine$double.eps)
        }
      }else if (kernel == "triangular"){
        if(is.null(hazard_bandwidth))stop("Need hazard_bandwidth")
        index <- abs(cdf_ext-pt)<bandwidth/2

        cdf_sample  <- cdf_ext[index]
        rnk <- length(unique(cdf_sample))

        if(rnk==0){
          beta_hat <- c(NA,0,0)
        }else if(rnk==1){
          beta_hat <- c(mean(cdf_sample),
                        length(cdf_sample)/length(cdf_ext)/hazard_bandwidth*3,
                        0)
        }else if(rnk==2){
          Yhat_sample <- Yhat[index]

          Wp <- diag(sqrt(abs(cdf_ext-pt)[index]))
          dsg_mtx  <- cbind(rep(1, length(cdf_sample)), (cdf_sample-pt))

          X_mtx    <- Wp %*% dsg_mtx
          beta_hat <- qr.solve(X_mtx, Wp%*%Yhat_sample,
                               tol = .Machine$double.eps)
        }else{
          Yhat_sample <- Yhat[index]

          Wp <- diag(sqrt(abs(cdf_ext-pt)[index]))
          dsg_mtx  <- cbind(rep(1,length(cdf_sample)),
                            (cdf_sample-pt),
                            (cdf_sample-pt)^2)

          X_mtx    <- Wp %*% dsg_mtx
          beta_hat <- qr.solve(X_mtx, Wp%*%Yhat_sample,
                               tol = .Machine$double.eps)
        }
      }else if (kernel == "knn"){
        if(is.null(knn))stop("Need knn")
        if(knn<=3)stop("Need larger knn")
        dist <- abs(cdf_ext-pt)
        index <- order(dist)[1:knn]

        cdf_sample  <- sort(cdf_ext[index])
        Yhat_sample <- sort(Yhat[index])

        X_mtx  <- cbind(rep(1,knn), (cdf_sample-pt), (cdf_sample-pt)^2)
        beta_hat <- qr.solve(X_mtx, Yhat_sample, tol = .Machine$double.eps)
      }else{
        stop("Invalid kernel")
      }

      H_hat    <- beta_hat[2]/(1 - beta_hat[1])
      H_unif   <- (1/3)/(1-(pt+1)/3)

      theta_i  <- H_unif/H_hat
      return(list(fhat = beta_hat[2]*3,
                  HRR  = theta_i))
    }
  }
  if (requireNamespace("parallel", quietly = TRUE)){
    res <- parallel::mcmapply(ls_fit, pt_int,
                              MoreArgs = list(cdf_sample = cdf_sample,
                                              kernel = kernel,
                                              bandwidth = hazard_bandwidth,
                                              knn = knn))
  }else{
    res <- mapply(ls_fit, pt_int,
                  MoreArgs = list(cdf_sample = cdf_sample,
                                  kernel = kernel,
                                  bandwidth = hazard_bandwidth,
                                  knn = knn))
  }
  return(list(fhat = stats::approxfun(pt_int,res[1,]),
              HRR  = stats::approxfun(pt_int,res[2,])))
}
Haiman_window_lth <- function(N, p, alpha, lower_wl=2, upper_wl=100){
  check_fun <- function(N, critical_cnt, window_l, p, alpha){
    p1 <- 1-q1_function(critical_cnt, window_l, p)

    n <- floor(N/window_l-1)
    return(p1 <= 0.025 &
             n>3 &
             3.3*n*p1^2 < 1 &
             prob_fun(N,critical_cnt,window_l,p) > (1-alpha)*
             (1 + p1^2*(3.3*n*(1+4.7*n*p1^2)+9+561*p1)))
  }

  upper_wl <- min(ceiling(N/4),upper_wl)

  if(lower_wl>upper_wl) stop("Can't be approximate in given range.")
  if(lower_wl==upper_wl){
    if(check_fun(N = N, critical_cnt = lower_wl-1,
                 window_l = lower_wl, p = p, alpha = alpha)){
      return(lower_wl)
    }else{
      stop("Can't be approximate in given range.")
    }
  }
  while(lower_wl<upper_wl){
    window_l_Haiman <- floor(mean(c(lower_wl,upper_wl)))
    satis <- check_fun(N = N,
                       critical_cnt = window_l_Haiman-1,
                       window_l = window_l_Haiman,
                       p = p,
                       alpha = alpha)
    if(satis){
      upper_wl <- window_l_Haiman
    }else{
      window_l_Haiman <- window_l_Haiman + 1
      lower_wl <- window_l_Haiman
    }
  }

  return(window_l_Haiman)
}
prob_fun_mc <- function(N,k,m,p,mc_rep){
  mc_each <- function(foo){
    temp <- stats::rbinom(N,1,p)
    return(max(rolling_sum(temp,m)))
  }

  if(requireNamespace("parallel", quietly = TRUE)){
    res <- parallel::mcmapply(mc_each,seq.int(mc_rep))
  }else{
    res <- sapply(seq.int(mc_rep),mc_each)
  }

  return(mean(res<=k))
}
q1_function <- function(k,m,p){
  if(k>=0 & m>=k & p>=0 & p<=1){
    return(stats::pbinom(k,m,p)^2 -
             k*stats::dbinom(k+1,m,p)*stats::pbinom(k-1,m,p)+
             m*p*stats::dbinom(k+1,m,p)*stats::pbinom(k-2,m-1,p))
  }else{
    stop("Input values out of range")
  }
}

q2_function <- function(k,m,p){
  A1 <- function(k,m,p){
    2*stats::dbinom(k+1,m,p)*stats::pbinom(k,m,p)*
      (k*stats::pbinom(k-1,m,p) - m*p*stats::pbinom(k-2,m-1,p))
  }

  A2 <- function(k,m,p){
    0.5*stats::dbinom(k+1,m,p)^2*
      (k*(k-1)*stats::pbinom(k-2,m,p) -
         2*(k-1)*m*stats::pbinom(k-3,m-1,p) +
         m*(m-1)*p^2*stats::pbinom(k-4,m-2,p))
  }

  A3 <- function(k,m,p){
    sum(mapply(stats::dbinom, 2*(k+1)-1:k, size = m, prob = p) *
          mapply(stats::pbinom, 0:(k-1), size = m, prob = p)^2)
  }

  A4 <- function(k,m,p){
    sum(mapply(stats::dbinom, 2*(k+1)-2:k, size = m, prob = p) *
          mapply(stats::dbinom, 2:k+1, size = m, prob = p) *
          (2:k * mapply(stats::pbinom, 2:k-1, size = m, prob = p) -
             m*p*mapply(stats::pbinom, 2:k-2, size = m-1, prob = p)))
  }

  stats::pbinom(k,m,p)^3 - A1(k,m,p) + A2(k,m,p) + A3(k,m,p) - A4(k,m,p)
}

qL_function <- function(L, k, m, p){
  if(L<=3) stop("Sequence is not long enough")
  q1 <- q1_function(k,m,p)
  q2 <- q2_function(k,m,p)
  (2*q1 - q2)/(1 + q1 - q2 + 2*(q1-q2)^2)^L
}

prob_fun <- function(N,k,m,p){
  n <- N/m-1
  if (n<4) warning("Sequence is not long enough")
  qL_lower <- qL_function(floor(n), k, m, p)
  qL_upper <- qL_function(ceiling(n), k, m, p)
  return(qL_upper*(n-floor(n)) + qL_lower*(1-(n-floor(n))))
}
sample_mode <- function(x){
  input_type <- typeof(x)
  frequence <- table(x)
  res <- names(frequence)[which.max(frequence)]

  str_res <- paste("as.",input_type,"(res)",sep = "")
  return(eval(parse(text = str_res)))
}
unif <- function(x, unit = 1, rd = T){
  if(unit<=0 | is.infinite(unit)){
    stop("Unit needs to be positive finite number")
  }
  y <- floor(sort(x)/unit)
  bds <- range(y)
  breaks <- seq(bds[1],bds[2]+1)*unit
  x_cnt <- table(cut(x, breaks = breaks, right = F))

  if(rd){
    if(requireNamespace("parallel", quietly = TRUE)){
      dec <- unlist(parallel::mclapply(as.vector(x_cnt),
                                       function(i)stats::runif(i)*unit))
    }else{
      dec <- unlist(lapply(as.vector(x_cnt), function(i)stats::runif(i)*unit))
    }
  }else{
    if(requireNamespace("parallel", quietly = TRUE)){
      dec <- unlist(parallel::mclapply(as.vector(x_cnt),
                                       function(i)if(i>0)(1:i/(i+1))*unit))
    }else{
      dec <- unlist(lapply(as.vector(x_cnt),
                           function(i)if(i>0)(1:i/(i+1))*unit))
    }
  }

  return(y*unit+dec)
}
rolling_sum <- function(temp, window_l){
  if (window_l > length(temp)) stop("Too large window length")

  m <- length(temp)
  cum_sum <- c(0, cumsum(temp))
  count <- diff(cum_sum,lag = window_l)
  return(count)
}
