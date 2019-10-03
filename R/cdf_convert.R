cdf_convert <- function(x, dist_null, ...){
  if(dist_null == "gpd"){
    if(requireNamespace("POT", quietly = TRUE)){
      cdf_x <- POT::pgpd(x, ...)
    }else{
      stop("Need package POT", call. = F)
    }
  }else{
    my_str <- paste("stats::p",dist_null,"(x, ...)",sep = "")
    cdf_x  <- eval(parse(text = my_str))
  }
  return(cdf_x)
}
