#' Squared Exponential Kernel Function
#'
#' @param x Kernel input
#' @param y Kernel input
#' @param sig Hyper parameter
#' @param l Hyper parameter
#'
#' @return
#' @export
#'
#' @examples
squared_exp_kernel <- function(x,y,sig,l){
  return(sig^2 * exp(-1/2/l^2 * (x-y)^2))
}

#' Produce Covariance matrix
#'
#' @param x Kernel input
#' @param y Kernel input
#' @param sig Hyper parameter
#' @param l Hyper parameter
#'
#' @return
#' @export
#'
#' @examples
Kernel <- function(x,y,sig,l){
  outer(x, y, Vectorize(function(x,y)  squared_exp_kernel(x,y,sig,l)))
}


#' Fit Gaussian Process with squared exp kernel
#'
#' @param x Input time indicies
#' @param y Known values for input indices
#' @param sig Kernel hyper parameter
#' @param sigma_n Noise parameter
#' @param l Kernel hyper parameter
#' @param xstar Time indices to predict values for
#'
#' @return
#' @export
#'
#' @examples
Fit_GP <- function(x,y,sig,sigma_n,l,xstar){
  K <- Kernel(x,x, sig,l) # produce kernel matrix
  L <- chol(K + sigma_n^2*diag(length(x)))

  alpha <- chol2inv(L) %*% y

  kstar <- Kernel(x,xstar,sig,l)

  fhatstar <- t(kstar)%*%alpha    # Predictive mean


  v <- solve(L,kstar)
  Varhatstar <- outer(xstar, xstar, Vectorize(function(x,y)  squared_exp_kernel(x,y,sig,l))) - t(v)%*%v
  logmarglikelihood <-  -0.5*t(y)%*%alpha - sum(diag(L)) - (length(x)/2)*log(2*pi)  #log likelihood
  val <- list(mean = fhatstar, variance = Varhatstar, loglikelihood = logmarglikelihood)
  return(val)
}
