Y <- close_2018_diff
Y <- as.matrix(Y)
loadings <- PCA$loadings[,1:17]
loadings <- as.matrix(loadings)
factor_series <- (Y%*%loadings) %*% solve(t(loadings)%*%loadings)