#Define support vector machine function for multi-dimensional X


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

