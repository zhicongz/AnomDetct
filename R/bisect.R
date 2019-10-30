bisect <- function(fn, lower, upper, discrete = TRUE,
                   target, right = TRUE,index = TRUE,...){
  if(target > fn(upper) || target < fn(lower))
    stop("`target` out of range of `fn`", call. = F)
  if(discrete){
    if(!exists("unit",inherits = F)) unit <- 1
    gn <- function(x) fn(x*unit+lower)
    lb <- 0
    ub <- floor((upper-lower)/unit)
    if(right){
      while(lb<ub){
        temp <- floor((lb+ub)/2)
        if(gn(temp) < target) lb <- temp+1 else ub <- temp
      }
    }else{
      while(lb<ub){
        temp <- ceiling((lb+ub)/2)
        if(gn(temp) <= target) lb <- temp else ub <- temp-1
      }
    }
    if(index) return(lb*unit+lower) else return(gn(lb))
  }else{
    if(!exists("tol",inherits = F)) tol <- 10^(-7)
    if(right){
      while(lower < upper-tol){
        temp <- (lower+upper)/2
        if(fn(temp) < target) lower <- temp else upper <- temp
      }
      if(index) return(upper) else return(fn(upper))
    }else{
      while(lower < upper-tol){
        temp <- (lower+upper)/2
        if(fn(temp) <= target) lower <- temp else upper <- temp
      }
      if(index) return(lower) else return(fn(lower))
    }
  }
}
