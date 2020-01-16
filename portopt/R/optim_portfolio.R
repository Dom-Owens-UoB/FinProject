#' Optimise portolio weights via quadratic programming.
#'
#' @param returns A matrix of stock returns
#' @param covar An approximated covariance matrix
#'
#' @export
#'
optim.portfolio <- function(returns,covar,lambda){
  mean <- rowMeans(returns)
  p <- nrow(returns)
  constraints <- cbind(
    matrix(rep(1,p), nr=p ),
    diag(p)
  )
  portfolio <- quadprog::solve.QP(lambda*covar,mean,constraints
                        ,c(1,rep(0,p)),meq = 1)
  return(portfolio)
}
