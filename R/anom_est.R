anom_est <- function(clst_p_values, alpha_lvl,
                     x, fhat = NULL, dist_null = "norm", ...){
  clst <- do.call(rbind,clst_p_values)
  clst[,1] <- x[clst[,1]]
  clst[,2] <- x[clst[,2]]

  bds <- clst[clst[,3]<=alpha_lvl,1:2,drop = F]

  n <- nrow(bds)
  if(n==0){
    return(0)
  }else if(n==1){
    res <- bds
  }else{
    bds - bds[order(bds[,1]),,drop = F]

    res <- bds[1,,drop = F]
    for (i in seq.int(2,nrow(bds))){
      if(res[nrow(res),2]<bds[i,1]){
        res <- rbind(res,bds[i,])
      }else{
        res[nrow(res),2] <- max(res[nrow(res),2],bds[i,2])
      }
    }
  }

  amt <- apply(res,1,function(temp){
    a <- temp[1]
    b <- temp[2]
    total_amt <- sum(x[x>=a & x<=b])
    exp_amt   <- AnomDetct::F_exp(a,b,length(x),fhat,unit = b-a,
                                  dist_null = dist_null, ...)
    return(total_amt - exp_amt)
  })

  if(any(sign(amt))==-1)
    warning("Detected cluster(s) come from negative domain.")

  return(sum(amt))
}
