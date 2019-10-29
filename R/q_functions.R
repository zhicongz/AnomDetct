q1_function <- function(k,m,p){
  if(k>=2 & m>=k & p>=0 & p<=1){
    return(stats::pbinom(k,m,p)^2 -
             k*stats::dbinom(k+1,m,p)*stats::pbinom(k-1,m,p)+
             m*p*stats::dbinom(k+1,m,p)*stats::pbinom(k-2,m-1,p))
  }else{
    stop("Invalid inputs", call. = F)
  }
}

q2_function <- function(k,m,p){
  A1 <- function(k,m,p){
    2*stats::dbinom(k+1,m,p)*stats::pbinom(k,m,p)*
      (k*stats::pbinom(k-1,m,p) - m*p*stats::pbinom(k-2,m-1,p))
  }

  A2 <- function(k,m,p){
    0.5*stats::dbinom(k+1,m,p)^2*
      (k*(k-1)*stats::pbinom(k-2,m,p) -
         2*(k-1)*m*stats::pbinom(k-3,m-1,p) +
         m*(m-1)*p^2*stats::pbinom(k-4,m-2,p))
  }

  A3 <- function(k,m,p){
    sum(mapply(stats::dbinom, 2*(k+1)-1:k, size = m, prob = p) *
          mapply(stats::pbinom, 0:(k-1), size = m, prob = p)^2)
  }

  A4 <- function(k,m,p){
    sum(mapply(stats::dbinom, 2*(k+1)-2:k, size = m, prob = p) *
          mapply(stats::dbinom, 2:k+1, size = m, prob = p) *
          (2:k * mapply(stats::pbinom, 2:k-1, size = m, prob = p) -
             m*p*mapply(stats::pbinom, 2:k-2, size = m-1, prob = p)))
  }

  stats::pbinom(k,m,p)^3 - A1(k,m,p) + A2(k,m,p) + A3(k,m,p) - A4(k,m,p)
}

qL_function <- function(L, k, m, p){
  if(L==1)
    AnomDetct::q1_function(k,m,p)
  else if (L==2)
    AnomDetct::q2_function(k,m,p)
  else{
    q1 <- AnomDetct::q1_function(k,m,p)
    q2 <- AnomDetct::q2_function(k,m,p)
    (2*q1 - q2)/(1 + q1 - q2 + 2*(q1-q2)^2)^(L-1)
  }
}

prob_fun <- function(N,k,m,p){
  n <- N/m
  if (n<4)
    stop("1-dependent sequence is not long enough", call. = F)
  qL_lower <- AnomDetct::qL_function(floor(n), k, m, p)
  qL_upper <- AnomDetct::qL_function(ceiling(n), k, m, p)
  return(qL_upper*(n-floor(n)) + qL_lower*(1-(n-floor(n))))
}
