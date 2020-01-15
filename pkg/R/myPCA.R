#Principal Component Analysis in S4

library(methods)

setClass("myPCA_object",
         slots = c(
           data  = "matrix",
           dimension = "integer",
           centre = "numeric",
           loadings = "matrix",
           projection = "matrix",
           projected = "matrix"
         )
)

#constructor
myPCA <- function(X, q) {
  pca <- new("myPCA_object",
             data  = X,
             dimension = q
  )
  return(pca)
}

setMethod("initialize", "myPCA_object",
          function(.Object, data, dimension){
  X <- data
  .Object@data <-  as.matrix(X)
  q <- as.integer(dimension)
  .Object@dimension <- q
    
            
  means <- colMeans(X) #get column means
  cX <- X - means #centre X
  .Object@centre <- means
  
  #cX <- matrix(cX)
  covX <- cov(cX) #sample covariance
  EV <- eigen(covX, symmetric = TRUE) #obtain eigendecomposition with eigen
  A <- EV$vectors #full projection matrix
  .Object@loadings <- A 
  Aq <- A[,1:q] #q-reduced projection matrix
  .Object@projection <- t(Aq) 
  Z <- t(Aq)%*%t(X) #project X onto Aq
  .Object@projected <- Z
  return(.Object)
          }
)


setMethod("project", "myPCA_object", function(X,Y) X@projection %*% Y ) #projection method


#oil_close_data <- read.csv("oil_close_2018.csv")
#close_data_2018 <- subset(oil_close_data, select = -c(X.1, DATE, DCOILWTICO, ADT)) 

#log_close <- log(close_data_2018) #take log
#diff_log <- diff(ts(log_close)) #diff series
#callPCA <- myPCA(diff_log, 10)

#project(callPCA, t(diff_log[,200:250] ))
