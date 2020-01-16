#' Fit a factor model to the matrix of returns
#'
#' @param returns A matrix of stock returns.
#' @param k The number of factors to fit.
#'
#' @return An S4 object from the class 'facmodel'.
#' @export
#'
fit.factor.model <- function(returns,k){
  pca <- stats::prcomp(t(returns))
  loadings <- t(as.matrix(pca$rotation)[,1:k])
  scores <- solve(loadings%*%t(loadings),loadings %*% returns)
  model <- methods::new("facmodel",
                          loadings = loadings,
                          scores = scores
                        )
  return(model)
}

#' Factor Model S4 Class
#'
#' @slot loadings Fitted factor loadings.
#' @slot scores Fitted factor scores.
#'
#' @export
#'
methods::setClass("facmodel", slots = c(
    loadings = "matrix",
    scores = "matrix"
  )
)
