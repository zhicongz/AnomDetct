rolling_sum <- function(temp, window_l){
  temp <- suppressWarnings(as.numeric(temp))
  temp[is.na(temp)] <- 0
  if (window_l > length(temp)) stop("Too large `window_l`", call. = F)

  m <- length(temp)
  cum_sum <- c(0, cumsum(temp))
  count <- diff(cum_sum,lag = window_l)
  return(count)
}
