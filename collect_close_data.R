library(tidyverse)
symbols <- read.csv("data/table-1.csv")
symbols <- symbols$Ticker.symbol

data <- read.csv("data/SP_500/AA.csv")

collect_close <- function(period){
  i = 1
  dates <- seq.Date(as.Date(period[1]),as.Date(period[2]),"day")
  combined_stock_data <- data.frame("date" = dates)
  for(symbol in symbols){
    file_destination <- paste("data/SP_500/",symbol,".csv",sep="")
    if(
      !file.exists(file_destination)
    ) next
    stock_data <- read.csv(file_destination)
    stock_data %<>% filter(as.Date(date) >= as.Date(period[1])
                          & as.Date(date) <= as.Date(period[2])
                          )
    if(length(stock_data$date) == 0) next
    stock_data$date <- stock_data$date %>% as.Date()
    combined_stock_data <- combined_stock_data %>% left_join(stock_data[,c(2,4)],by="date")
    colnames(combined_stock_data)[i + 1] <- symbol
    i = i + 1
  }
  return(combined_stock_data)
}

