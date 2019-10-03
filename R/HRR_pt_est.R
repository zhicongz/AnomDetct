HRR_pt_est <- function(pt_int,cdf_sample,kernel = "gaussian",
                       hazard_bandwidth = NULL, knn = NULL){
  if(requireNamespace("Matrix", quietly = TRUE)){
    ls_fit <- function(pt,cdf_sample,kernel,bandwidth,knn){
      cdf_ext <- c(rev(-cdf_sample),cdf_sample,rev(2-cdf_sample))
      Yhat <- (1:length(cdf_ext))/length(cdf_ext)
      if (kernel == "gaussian"){
        if(is.null(hazard_bandwidth))stop("Need `hazard_bandwidth`", call. = F)
        Wp       <- diag(sqrt(stats::dnorm((cdf_ext-pt)/bandwidth)))
        dsg_mtx  <- cbind(rep(1,length(cdf_ext)),
                          (cdf_ext-pt),
                          (cdf_ext-pt)^2)

        X_mtx    <- Wp %*% dsg_mtx
        beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx, t(X_mtx)%*%Wp%*%Yhat)
      }else if (kernel == "rectangular"){
        if(is.null(hazard_bandwidth))stop("Need `hazard_bandwidth`", call. = F)
        index <- abs(cdf_ext-pt) < bandwidth/2

        cdf_sample  <- cdf_ext[index]
        rnk <- length(unique(cdf_sample))

        if(rnk==0){
          beta_hat <- c(NA,0,0)
        }else if(rnk==1){
          beta_hat <- c(mean(cdf_sample),
                        length(cdf_sample)/length(cdf_ext)/hazard_bandwidth*3,
                        0)
        }else if(rnk==2){
          Yhat_sample <- Yhat[index]

          X_mtx  <- cbind(rep(1,length(cdf_sample)), (cdf_sample-pt))
          beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx,
                                    t(X_mtx)%*%Wp%*%Yhat_sample)
        }else{
          Yhat_sample <- Yhat[index]

          X_mtx  <- cbind(rep(1,length(cdf_sample)),
                          (cdf_sample-pt), (cdf_sample-pt)^2)
          beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx,
                                    t(X_mtx)%*%Yhat_sample)
        }
      }else if (kernel == "triangular"){
        if(is.null(hazard_bandwidth))stop("Need `hazard_bandwidth`", call. = F)
        index <- abs(cdf_ext-pt)<bandwidth/2

        cdf_sample  <- cdf_ext[index]
        rnk <- length(unique(cdf_sample))

        if(rnk==0){
          beta_hat <- c(NA,0,0)
        }else if(rnk==1){
          beta_hat <- c(mean(cdf_sample),
                        length(cdf_sample)/length(cdf_ext)/hazard_bandwidth*3,
                        0)
        }else if(rnk==2){
          Yhat_sample <- Yhat[index]

          Wp <- diag(sqrt(abs(cdf_ext-pt)[index]))
          dsg_mtx  <- cbind(rep(1, length(cdf_sample)), (cdf_sample-pt))

          X_mtx    <- Wp %*% dsg_mtx
          beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx,
                                    t(X_mtx)%*%Wp%*%Yhat_sample)
        }else{
          Yhat_sample <-    Yhat[index]

          Wp <- diag(sqrt(abs(cdf_ext-pt)[index]))
          dsg_mtx  <- cbind(rep(1,length(cdf_sample)),
                            (cdf_sample-pt),
                            (cdf_sample-pt)^2)

          X_mtx    <- Wp %*% dsg_mtx
          beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx,
                                    t(X_mtx)%*%Wp%*%Yhat_sample)
        }
      }else if (kernel == "knn"){
        if(is.null(knn))stop("Need `knn`", call. = F)
        if(knn<=3)stop("Need larger `knn`", call. = F)
        dist <- abs(cdf_ext-pt)
        index <- order(dist)[1:knn]

        cdf_sample  <- sort(cdf_ext[index])
        Yhat_sample <- sort(Yhat[index])

        X_mtx  <- cbind(rep(1,knn), (cdf_sample-pt), (cdf_sample-pt)^2)
        beta_hat <- Matrix::solve(t(X_mtx)%*%X_mtx,
                                  t(X_mtx)%*%Yhat_sample)
      }else{
        stop("Invalid `kernel`", call. = F)
      }

      H_hat    <- beta_hat[2]/(1 - beta_hat[1])
      H_unif   <- (1/3)/(1-(pt+1)/3)

      theta_i  <- H_unif/H_hat
      return(list(fhat = beta_hat[2]*3,
                  HRR  = theta_i))
    }
  }else{
    ls_fit <- function(pt,cdf_sample,kernel,bandwidth,knn){
      cdf_ext <- c(rev(-cdf_sample),cdf_sample,rev(2-cdf_sample))
      Yhat <- (1:length(cdf_ext))/length(cdf_ext)
      if (kernel == "gaussian"){
        if(is.null(hazard_bandwidth))stop("Need `hazard_bandwidth`", call. = F)
        Wp       <- diag(sqrt(stats::dnorm((cdf_ext-pt)/bandwidth)))
        dsg_mtx  <- cbind(rep(1,length(cdf_ext)),
                          (cdf_ext-pt),
                          (cdf_ext-pt)^2)

        X_mtx    <- Wp %*% dsg_mtx
        beta_hat <- qr.solve(X_mtx, Wp%*%Yhat, tol = .Machine$double.eps)
      }else if (kernel == "rectangular"){
        if(is.null(hazard_bandwidth))stop("Need `hazard_bandwidth`", call. = F)
        index <- abs(cdf_ext-pt) < bandwidth/2

        cdf_sample  <- cdf_ext[index]
        rnk <- length(unique(cdf_sample))

        if(rnk==0){
          beta_hat <- c(NA,0,0)
        }else if(rnk==1){
          beta_hat <- c(mean(cdf_sample),
                        length(cdf_sample)/length(cdf_ext)/hazard_bandwidth*3,
                        0)
        }else if(rnk==2){
          Yhat_sample <- Yhat[index]

          X_mtx  <- cbind(rep(1,length(cdf_sample)), (cdf_sample-pt))
          beta_hat <- qr.solve(X_mtx, Yhat_sample, tol = .Machine$double.eps)
        }else{
          Yhat_sample <- Yhat[index]

          X_mtx  <- cbind(rep(1,length(cdf_sample)),
                          (cdf_sample-pt), (cdf_sample-pt)^2)
          beta_hat <- qr.solve(X_mtx, Yhat_sample, tol = .Machine$double.eps)
        }
      }else if (kernel == "triangular"){
        if(is.null(hazard_bandwidth))stop("Need `hazard_bandwidth`", call. = F)
        index <- abs(cdf_ext-pt)<bandwidth/2

        cdf_sample  <- cdf_ext[index]
        rnk <- length(unique(cdf_sample))

        if(rnk==0){
          beta_hat <- c(NA,0,0)
        }else if(rnk==1){
          beta_hat <- c(mean(cdf_sample),
                        length(cdf_sample)/length(cdf_ext)/hazard_bandwidth*3,
                        0)
        }else if(rnk==2){
          Yhat_sample <- Yhat[index]

          Wp <- diag(sqrt(abs(cdf_ext-pt)[index]))
          dsg_mtx  <- cbind(rep(1, length(cdf_sample)), (cdf_sample-pt))

          X_mtx    <- Wp %*% dsg_mtx
          beta_hat <- qr.solve(X_mtx, Wp%*%Yhat_sample,
                               tol = .Machine$double.eps)
        }else{
          Yhat_sample <- Yhat[index]

          Wp <- diag(sqrt(abs(cdf_ext-pt)[index]))
          dsg_mtx  <- cbind(rep(1,length(cdf_sample)),
                            (cdf_sample-pt),
                            (cdf_sample-pt)^2)

          X_mtx    <- Wp %*% dsg_mtx
          beta_hat <- qr.solve(X_mtx, Wp%*%Yhat_sample,
                               tol = .Machine$double.eps)
        }
      }else if (kernel == "knn"){
        if(is.null(knn))stop("Need `knn`", call. = F)
        if(knn<=3)stop("Need larger `knn`", call. = F)
        dist <- abs(cdf_ext-pt)
        index <- order(dist)[1:knn]

        cdf_sample  <- sort(cdf_ext[index])
        Yhat_sample <- sort(Yhat[index])

        X_mtx  <- cbind(rep(1,knn), (cdf_sample-pt), (cdf_sample-pt)^2)
        beta_hat <- qr.solve(X_mtx, Yhat_sample, tol = .Machine$double.eps)
      }else{
        stop("Invalid `kernel`", call. = F)
      }

      H_hat    <- beta_hat[2]/(1 - beta_hat[1])
      H_unif   <- (1/3)/(1-(pt+1)/3)

      theta_i  <- H_unif/H_hat
      return(list(fhat = beta_hat[2]*3,
                  HRR  = theta_i))
    }
  }
  if (requireNamespace("parallel", quietly = TRUE)){
    res <- parallel::mcmapply(ls_fit, pt_int,
                              MoreArgs = list(cdf_sample = cdf_sample,
                                              kernel = kernel,
                                              bandwidth = hazard_bandwidth,
                                              knn = knn))
  }else{
    res <- mapply(ls_fit, pt_int,
                  MoreArgs = list(cdf_sample = cdf_sample,
                                  kernel = kernel,
                                  bandwidth = hazard_bandwidth,
                                  knn = knn))
  }
  return(list(fhat = stats::approxfun(pt_int,res[1,]),
              HRR  = stats::approxfun(pt_int,res[2,])))
}
