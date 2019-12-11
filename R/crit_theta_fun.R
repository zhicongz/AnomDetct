crit_theta_fun <- function(N, window_l, p, alpha){
  p_sc <- function(x) AnomDetct::prob_fun(N = N, k = x, m = window_l, p = p)

  return(AnomDetct::bisect(fn = p_sc, lower = 2, upper = window_l, 
                             target = 1-alpha))
}
