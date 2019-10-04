unif <- function(x, unit = 1, rd = T){
  if(unit<=0 | is.infinite(unit)){
    stop("`unit` needs to be positive finite number")
  }
  y <- floor(sort(stats::na.omit(x)/unit))
  bds <- range(y)
  breaks <- seq(bds[1],bds[2]+1)*unit
  x_cnt <- table(cut(x, breaks = breaks, right = F))

  if(rd){
    if(requireNamespace("parallel", quietly = TRUE)){
      dec <- unlist(parallel::mclapply(as.vector(x_cnt),
                                       function(i)stats::runif(i)*unit))
    }else{
      dec <- unlist(lapply(as.vector(x_cnt),
                           function(i)stats::runif(i)*unit))
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
