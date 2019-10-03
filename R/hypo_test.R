hypo_test <- function(cdf, HRR = rep(1,length(cdf)-1),
                      theta, critical_cnt, window_l){
  N <- length(cdf)
  expd_cdf <- theta*HRR[1:(N-1)]*(1 - cdf[1:(N-1)])/(N:2) + cdf[1:(N-1)]
  indicator <- cdf[2:N] <= expd_cdf

  cnt <- AnomDetct::rolling_sum(temp = indicator, window_l = window_l)
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
