#Define support vector machine function for multi-dimensional X


#' Support Vector Machine classifier
#'
#' @param X covariates
#' @param y response vector
#' @param max_it maximum iterations
#' @param eta_0 initial learning rate
#' @param alpha learning rate decay
#' @param c cost parameter
#'
#' @return numeric vector of coefficient parameter estimates for y
#' @export
#'
#' @examples 
#' SVM(X = X, y = y, max_it = 1e4, eta_0 = 10, alpha = 0.9, c = 0.9)
SVM <- function(X, y, max_it = 1e4, eta_0 = 10, alpha = 0.9, c = 0.9){
  
  n <- dim(X)[1] #observations
  d <- dim(X)[2] #dimensions
  w <- runif(d) #set initial weight vector randomly
  
    for (iter in 1:max_it){
      eta <- eta_0/(iter^alpha) #decrease step size
      samp <- sample(1:n, n) # index sample
      X1 <- X[samp,] #shuffle
      y1 <- y[samp] #shuffle
      
      for (i in 1:n) {
        x1 <- as.numeric(X1[i,])
        if (y1[i] * w %*% x1 <= 1) {
          w <- as.numeric(w + eta * (y1[i] * t(x1) - c * w))  }
      }
    }
  
  w
}

