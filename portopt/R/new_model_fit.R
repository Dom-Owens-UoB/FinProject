#' Fit a factor model to the matrix of returns
#'
#' @param returns A matrix of stock returns.
#' @param k The number of factors to fit.
#' @param itr max no iterations.
#'
#' @return An S4 object from the class 'facmodel'.
#' @export
#'
fit.factor.model.new <- function(returns,k,itr){
  pca <- chemometrics::nipals(t(returns),k)
  scores <- pca$T
  loadings <- pca$P
  model <- methods::new("facmodel",
                        loadings = t(loadings),
                        scores = t(scores)
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
