crit_theta_fun <- function(N, window_l, p, alpha){
  lower_crit <- 0
  upper_crit <- window_l

  while(lower_crit < upper_crit){
    critical_cnt <- floor(mean(c(lower_crit,upper_crit)))
    p_sc <- AnomDetct::prob_fun(N, critical_cnt, window_l, p)
    if(p_sc < 1-alpha){
      critical_cnt <- critical_cnt + 1
      lower_crit <- critical_cnt
    }else{
      upper_crit <- critical_cnt
    }
  }

  return(critical_cnt)
}
