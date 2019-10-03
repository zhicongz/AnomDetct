fit_dist <- function(x,dist_null = NA,...){
  if(length(x)==0)
    stop("No observations", call. = F)

  if(min(x)>0) sum_log_x <- sum(log(x))
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
      if(s<=0)
        return(NA)
      else
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
      if(s<=0)
        return(NA)
      else
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
      if(v <= 0)
        return(NA)
      else
        return(-n*log(gamma((v+1)/2)/sqrt(v*pi)/gamma(v/2)) +
                 (v+1)/2*sum(log(1+x^2/v)))
    }

    para_hat <- tryCatch(stats::optim(par = 1, fn = t_den,
                                      method = "L-BFGS-B",lower = 0)$par,
                         error = function(e) NA)

    if(is.na(para_hat[1])){
      list_t <- list(0,"t",NA)
    }else{
      list_t <- list(stats::ks.test(x,stats::pt,para_hat)$p.value,
                     "t",
                     para_hat)
    }

    candid <- list(list_normal,list_unif,list_cauchy,list_logis,list_t)

    if(min(x)>0){
      gamma_den <- function(para){
        alp <- para[1]
        bet <- para[2]
        n <- length(x)
        if(alp<=0 || bet <=0)
          return(NA)
        else
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
        if(p<=0)
          return(NA)
        else
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
        if(lambda<=0)
          return(NA)
        else
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
        if(n1<=0 || n2<=0)
          return(NA)
        else
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
        if(l<=0 || s<= 0)
          return(NA)
        else
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
        if(a<=0 || b<=0)
          return(NA)
        else
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
          if(alp<=0 || bet<=0)
            return(NA)
          else
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

    max_ind <- which.max(sapply(candid,function(x)x[[1]]))
    return(list(dist = candid[[max_ind]][[2]],
                para = candid[[max_ind]][[3]]))
  }

  if(dist_null == "gpd"){
    if(requireNamespace("POT", quietly = TRUE)){
      para_hat <- POT::fitgpd(x,...)$param
    }else{
      stop("Need POT package", call. = F)
    }
    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "norm"){
    return(list(dist = dist_null, para = c(mean(x),stats::sd(x))))
  }else if(dist_null == "unif"){
    return(list(dist = dist_null, para = c(min(x),max(x))))
  }else if(dist_null == "gamma"){
    gamma_den <- function(para){
      alp <- para[1]
      bet <- para[2]
      n <- length(x)
      if(alp<=0 || bet <=0)
        return(NA)
      else
        return(-(n*alp*log(bet)-n*log(gamma(alp))+(alp-1)*sum_log_x-bet*sum_x))
    }

    para_hat <- stats::optim(par = c(1,1), fn = gamma_den,
                             method = "L-BFGS-B",lower = 0)$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "chisq"){
    chisq_den <- function(p){
      n <- length(x)
      if(p<=0)
        return(NA)
      else
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
      if(alp<=0 || bet<=0)
        return(NA)
      else
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
      if(s<=0)
        return(NA)
      else
        return(n*log(pi*s) + sum(log(1+((x-l)/s)^2)))
    }

    para_hat <- stats::optim(par = c(1,1), fn = cauchy_den,
                             method = "L-BFGS-B",lower = c(-Inf,0))$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "exp"){
    exp_den <- function(lambda){
      n <- length(x)
      if(lambda<=0)
        return(NA)
      else
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
      if(n1<=0 || n2<=0)
        return(NA)
      else
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
      if(s<=0)
        return(NA)
      else
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
      if(l<=0 || s<= 0)
        return(NA)
      else
        return(sum_log_x + n*log(s*sqrt(2*pi)) + sum((log(x)-l)^2/(2*s^2)))
    }

    para_hat <- stats::optim(par = c(1,1), fn = lnorm_den,
                             method = "L-BFGS-B",lower = c(0,0))$par

    return(list(dist = dist_null, para = para_hat))
  }else if(dist_null == "t"){
    t_den <- function(v){
      n <- length(x)
      if(v <= 0)
        return(NA)
      else
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
      if(a<=0 || b<=0)
        return(NA)
      else
        return(-n*log(a/b)-(a-1)*sum(log(x/b))+sum((x/b)^a))
    }

    para_hat <- stats::optim(par = c(1,1), fn = weibull_den,
                             method = "L-BFGS-B",lower = 0)$par

    return(list(dist = dist_null, para = para_hat))
  }
}
