bisect <- function(arr,target,right = TRUE,index = TRUE){
  n <- length(arr)
  if(target > arr[n] || target < arr[1])
    stop("`target` out of range of `arr`", call. = F)

  lower <- 1
  upper <- n

  if(right){
    while(lower<upper){
      temp <- floor((lower+upper)/2)
      if(arr[temp] < target) lower <- temp+1 else upper <- temp
    }
  }else{
    while(lower<upper){
      temp <- ceiling((lower+upper)/2)
      if(arr[temp] <= target) lower <- temp else upper <- temp-1
    }
  }

  if(index) return(lower) else return(arr[lower])
}
