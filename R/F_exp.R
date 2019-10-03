F_exp <- function(a, b, N, fhat = NULL, unit = b-a,
                  dist_null = "norm", ...){
  f_x <- function(x_pt){
    if(dist_null == "gpd"){
      if(requireNamespace("POT", quietly = TRUE)){
        y <- POT::pgpd(x_pt, ...)
        exp_density <- POT::dgpd(x_pt, ...)
      }else{
        stop("Need package POT", call. = F)
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
  if(unit>b-a) stop("Need smaller `unit`")
  pt_int <- seq(a, b, by = unit)
  if((b-a)%%unit!=0) pt_int <- c(pt_int, b)
  return(sum(sapply(seq.int(length(pt_int)-1),
                    function(i)1/4*N*(f_x(pt_int[i])+f_x(pt_int[i+1]))*
                      (pt_int[i+1]-pt_int[i])*(pt_int[i+1]+pt_int[i]))))
}
