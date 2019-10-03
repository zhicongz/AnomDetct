prob_fun_mc <- function(N,k,m,p,mc_rep){
  mc_each <- function(foo){
    temp <- stats::rbinom(N,1,p)
    return(max(AnomDetct::rolling_sum(temp,m)))
  }

  if(requireNamespace("parallel", quietly = TRUE)){
    res <- parallel::mcmapply(mc_each,seq.int(mc_rep))
  }else{
    res <- sapply(seq.int(mc_rep),mc_each)
  }

  return(sapply(k, function(i)mean(res<=i)))
}
