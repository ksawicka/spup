# Stefan van Dam
# 14-01-2016

stratsamp <- function(object, n, p) {

  # Note that here p is a vector of probs
  # n is sample size per stratum
  # mu and sigma are parameters of normal distribution to be sampled
  # function returns matrix with n rows and length(p)-1 columns
  if (object$dist == "norm") {
    lims <- qnorm(p, object$par[1], object$par[2])
    outmat <- matrix(NA, n, length(p)-1)
    counts <- rep(0, length(lims)-1)
    while(any(counts < n)){
      r <- rnorm(1, object$par[1], object$par[2])
      intvl <- findInterval(r,lims)
      if(counts[intvl] < n){
        counts[intvl] <- counts[intvl] + 1
        outmat[counts[intvl], intvl] <- r
      }
    }
  }

  if (object$dist == "log") {
    lims <- qlnorm(p, object$par[1], object$par[2])
    outmat <- matrix(NA, n, length(p)-1)
    counts <- rep(0, length(lims)-1)
    while(any(counts < n)){
      r <- rlnorm(1, object$par[1], object$par[2])
      intvl <- findInterval(r,lims)
      if(counts[intvl] < n){
        counts[intvl] <- counts[intvl] + 1
        outmat[counts[intvl], intvl] <- r
      }
    }
  }
  return(outmat)
}

stratsampSpatial <- function(mu, sigma, n, p) {
  # Note that here p is a vector of probs
  # n is sample size per stratum
  # mu and sigma are parameters of normal distribution to be sampled
  # function returns matrix with n rows and length(p)-1 columns
  if (is.na(mu) || (is.na(sigma))) {
    outmat <- matrix(NA, nrow = n, ncol = length(p)-1)
  } else {
    lims <- qnorm(p, mu, sigma)
    outmat <- matrix(data = NA, nrow = n, ncol = length(p)-1)
    counts <- rep(0, length(lims)-1)
    while (any(counts < n)) {
      r <- rnorm(1, mu, sigma)
      intvl <- findInterval(r,lims)
      if (counts[intvl] < n) {
        counts[intvl] <- counts[intvl] + 1
        outmat[counts[intvl], intvl] <- r
      }
    }
  }
  return(outmat)
}
