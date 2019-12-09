library(tidyverse)
library(magrittr)
close_prices <- read.csv("combined_close_data.csv")
close_prices <- close_prices[,-1]
close_prices_2018 <- close_prices %>% filter(as.Date(close_prices$date) >= "2018-01-01")
close_prices_2018 %<>% select(-ADT) #Remove ADT as it has missing values

log_close <- close_prices_2018[,-1] %>% log()
covar <- cov(log_close)
pca <- princomp(covar) #Take the PCA of covariance matrix to identify factors

head(pca$sdev, 30)
#We arbitrarily decide to use an 11-factor model for now.
loadings <- pca$loadings
Y <- as.matrix(log_close)
loadings <- as.matrix(loadings)
factor_series <- (Y%*%loadings) %*% solve(t(loadings)%*%loadings)
factor_series_diff <- diff(factor_series)
plot(as.ts(factor_series_diff[,1:8]))
factor_series_diff[,11] %>% as.ts() %>% acf()
