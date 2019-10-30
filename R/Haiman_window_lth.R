Haiman_window_lth <- function(N, p, alpha, lower_wl=3, upper_wl=100){
  check_fun <- function(N, critical_cnt, window_l, p, alpha){
    p1 <- 1 - AnomDetct::q1_function(critical_cnt, window_l, p)

    n <- floor(N/window_l)
    return(p1 <= 0.025 &
             n>3 &
             3.3*n*p1^2 < 1 &
             AnomDetct::prob_fun(N,critical_cnt,window_l,p) > (1-alpha)*
             (1 + p1^2*(3.3*n*(1+4.7*n*p1^2)+9+561*p1)))
  }

  upper_wl <- min(ceiling(N/4),upper_wl)

  if(lower_wl>upper_wl) stop("Inappropriate `lower_wl`/`upper_wl`", call. = F)
  fn <- function(x) check_fun(N = N, critical_cnt = x-1, window_l = x,
                              p = p, alpha = alpha)
  return(AnomDetct::bisect(fn = fn, lower = lower_wl, upper = upper_wl,
                           target = TRUE))
}
