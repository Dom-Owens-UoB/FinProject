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
ts.plot(as.ts(log_returns[6,])) #This is incorrect since as.ts acts on columns

pca <- prcomp(t(log_returns)) #Take the PCA of covariance matrix to identify factors
plot(pca)
head(pca$sdev, 30)

#We arbitrarily decide to use an 10-factor model for now.
loadings <- pca$rotation[,1:8] %>% t()
Y <- as.matrix(log_returns)
loadings <- as.matrix(loadings)
factor_series <- solve(loadings%*%t(loadings)) %*% loadings %*% Y

#Estimate the error terms
y_mean <- rowSums(Y)/145
errors <- Y - y_mean %*% t(rep(1,145)) - t(loadings)%*%factor_series

#Estimate factor/loadings covar matrices
factor_covar <- factor_series%*%t(factor_series) * 1/8
error_covar <- errors%*%t(errors) * 1/414
error_covar_diag <- diag(diag(error_covar))
covar <- t(loadings) %*% factor_covar %*% loadings + error_covar

a <- solve(error_covar_diag)
b <- solve(factor_covar)
inv_covar <- a - a%*%t(loadings) %*% solve(b + loadings%*%a%*%t(loadings)) %*% loadings %*% a


#Portfolio Optimisation
#following (w.r.t notation) the analytical solution for portfolio optimisation...
sigma0 <- 0.1
A <- t(y_mean) %*% inv_covar %*% y_mean
B <- t(rep(1,414)) %*% inv_covar %*% y_mean
C <- t(rep(1,414)) %*% inv_covar %*% rep(1,414)
b <- (C*sigma0^2 - 1)/(A*C - B^2) %>% sqrt()
if (sigma0 * B <= sqrt(A)){
  portfolio <- as.numeric(sigma0/(sqrt(A))) * inv_covar %*% y_mean 
} else{
  portfolio <- as.numeric(1/C) * inv_covar %*% rep(1,414) + 
    as.numeric(b)*(inv_covar%*%y_mean - as.numeric(B/C)*inv_covar%*%rep(1,414))
}

#Attempt to use the constrOptim function
return <- 1e-5
theta <- rep(0,414)
theta[5] <- 0.9
theta <- theta + rep(0.00001,414)

objective <- function(w){ #Variance of portfolio
  value <- t(w) %*% covar %*% w
  return(value)
}

grad <- function(w){
  value <- t(w) %*% covar
}

ui <- rbind(t(y_mean),rep(-1,414),diag(rep(1,414)))
ci <- c(return,-1,rep(0,414))

portfolio <- constrOptim(theta,objective,grad,ui,ci)
portfolio <- portfolio$par

close_prices_2018 <- close_prices %>% filter(as.Date(close_prices$date) >= "2018-01-01")
close_prices_2018 %<>% select(-remove)
close_prices_2018 %<>% select(-date)

close_prices_2017 %<>% select(-date)
as.matrix(close_prices_2018[250,]) %*% portfolio - as.matrix(close_prices_2017[148,]) %*% portfolio

vals <- rep(NA,250)
for (i in 1:250){
  vals[i] <- as.matrix(close_prices_2018[i,]) %*% portfolio - as.matrix(close_prices_2017[148,]) %*% portfolio
}

vals %>% as.ts %>% plot.ts

