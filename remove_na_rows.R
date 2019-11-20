close_prices <- read.csv("combined_close_data.csv")
#Note that while writting the csv file an extra column has been added to 
#combined_close_data.csv that needs to be removed.
close_prices <- close_prices[,-1]
close_prices <- close_prices[rowSums(is.na(close_prices)) != (ncol(close_prices) - 1),]
