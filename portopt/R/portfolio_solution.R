#' Analytical solution to Mean-Variance Portfolio.
#'
#' @param returns A matrix of stock prices
#' @param model an object of S4 class "model".
#' @param covars output from the function `approx.covar()`
#' @param var upper bound of variance.
#' @export
#'
portfolio.solve <- function(returns,model,covars,var){
  mean <- rowMeans(returns)
  a <- solve(covars$error.covar)
  b <- solve(covars$f.covar)
  p <- nrow(returns)
  loadings <- model@loadings
  covar.inv <- a - a %*% t(loadings) %*% solve(b + loadings %*% a %*% t(loadings),loadings %*% a)

  sigma0 <- sqrt(var)
  A <- t(mean)%*%covar.inv%*%mean %>% as.numeric()
  B <- t(rep(1,p))%*%covar.inv%*%mean %>% as.numeric()
  C <- t(rep(1,p))%*%covar.inv%*%rep(1,p) %>% as.numeric()
  D <- (C*var -1)/(A*C - B^2) %>% sqrt
  ones <- rep(1,p)

  if (sigma0 * B / A <= 1){
    portfolio <- (sigma0/sqrt(A)) * covar.inv %*% mean
  }
  else{
    portfolio <- (1/C) * (covar.inv%*%ones) + D * (covar.inv%*%mean - (B/C) * covar.inv%*%ones)
  }
  return(portfolio)
}
