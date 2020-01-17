library(tidyverse)
library(magrittr)
#Load the data and transform it such that it is ready to use.
close_prices <- read.csv("combined_close_data.csv")
close_prices <- close_prices[,-1]
close_prices_2017 <- close_prices %>% filter(as.Date(close_prices$date) >= "2017-06-01") %>%
  filter(as.Date(.$date) < "2018-01-01")
remove <- colnames(close_prices_2017)[colSums(is.na(close_prices_2017)) > 0]
close_prices_2017 %<>% select(-remove) #Remove ADT as it has missing values

#Transform the data to K time-series of the log return (We take a 2 day return)
log_close <- close_prices_2017[,-1] %>% log() %>% as.matrix %>% diff()
log_returns <- diff(log_close,lag = 2) %>% t()
ts.plot(as.ts(log_returns[6,]))

pca <- prcomp(t(log_returns))
plot(pca)
head(pca$sdev, 30)

#We arbitrarily decide to use an 10-factor model for now.
loadings <- pca$rotation[,1:10] %>% t()
Y <- as.matrix(log_returns)
loadings <- as.matrix(loadings)
factor_series <- solve(loadings%*%t(loadings)) %*% loadings %*% Y

#Estimate the error terms
y_mean <- rowSums(Y)/145
errors <- Y - y_mean %*% t(rep(1,145)) - t(loadings)%*%factor_series

#Estimate factor/loadings covar matrices
factor_covar <- factor_series%*%t(factor_series) * 1/dim(factor_series)[2]
error_covar <- errors%*%t(errors) * 1/dim(errors)[2]
error_covar <- diag(diag(error_covar))
covar <- t(loadings) %*% factor_covar %*% loadings + error_covar

a <- solve(error_covar + diag(1e-15,nrow=nrow(error_covar)))
a <- diag(diag(error_covar))
b <- solve(factor_covar)
inv_covar <- a - a%*%t(loadings) %*% solve(b + loadings%*%a%*%t(loadings),loadings %*% a)


#Portfolio Optimisation
#following (w.r.t notation) the analytical solution for portfolio optimisation...
sigma0 <- 1
A <- t(y_mean) %*% inv_covar %*% y_mean
B <- t(rep(1,414)) %*% inv_covar %*% y_mean
C <- t(rep(1,414)) %*% inv_covar %*% rep(1,414)
b <- (C*sigma0 - 1)/(A*C - B^2) %>% sqrt()
if (sqrt(sigma0) * B <= sqrt(A)){
  portfolio <- as.numeric(sqrt(sigma0)/(sqrt(A))) * inv_covar %*% y_mean 
} else{
  portfolio <- as.numeric(1/C) * inv_covar %*% rep(1,414) + 
    as.numeric(b)*(inv_covar%*%y_mean - as.numeric(B/C)*inv_covar%*%rep(1,414))
}

no.sl <- diag(1,nrow = 414) %>% cbind(rep(1,414))

library(quadprog)
portfolio <- solve.QP(2*covar,y_mean,const,c(1,rep(0,414)),meq = 1)$solution
close.matrix <- close_prices_2017[,-1] %>% as.matrix()
port.val <- t(portfolio) %*% t(close.matrix)
plot(1:148,port.val,type="l")


