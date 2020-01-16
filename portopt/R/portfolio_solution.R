#' Analytical solution to Mean-Variance Portfolio.
#'
#' @param returns A matrix of stock prices
#' @param model an object of S4 class "model".
#' @param covars output from the function `approx.covar()`
#' @param sigma0 upper bound of variance.
#' @export
#'
portfolio.solution <- function(returns,model,covars,sigma0){
  mean <- rowMeans(returns)
  a <- solve(covars$error.covar)
  b <- solve(covars$f.covar)
  p <- nrow(returns)
  loadings <- model@loadings
  covar.inv <- a - a %*% t(loadings) %*% solve(b + loadings %*% a %*% t(loadings),loadings %*% a)

  A <- t(mean) %*% covar.inv %*% mean
  B <- t(rep(1,p)) %*% covar.inv %*% mean
  C <- t(rep(1,p)) %*% covar.inv %*% rep(1,p)
  D <- (C*sigma0^2 - 1)/(A*C - B^2) %>% sqrt()

  if (sigma0 * B <= sqrt(A)){
    portfolio <- as.numeric(sigma0/(sqrt(A))) * (covar.inv %*% mean)
  } else{
    portfolio <- as.numeric(1/C) * covar.inv %*% rep(1,p) +
      as.numeric(D)*(covar.inv%*%mean - as.numeric(B/C)*covar.inv%*%rep(1,p))
  }
  return(portfolio)
}
