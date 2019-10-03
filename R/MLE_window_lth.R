MLE_window_lth <- function(x,dist_null,..., unit = 1){
  if(unit<=0 | is.infinite(unit)){
    stop("`unit` needs to be positive finite number")
  }

  y <- x/unit
  bds <- ceiling(range(y))
  breaks <- seq(bds[1]-1,bds[2])*unit
  x_cnt <- table(cut(x,breaks = breaks))

  if(dist_null == "gpd"){
    if(requireNamespace("POT", quietly = TRUE)){
      exp_cnt <- diff(POT::pgpd(breaks, ...))*length(x)
    }else{
      stop("Need package POT", call. = F)
    }
  }else{
    my_str <- paste("p",dist_null,"(breaks, ...)",sep = "")
    exp_cnt  <- diff(eval(parse(text = my_str)))*length(x)
  }
  return(round(max(x_cnt - exp_cnt)))
}
