library(tidyverse)
SP_500_wiki <- read.csv("table-1.csv")
SP_500_symbols <- SP_500_wiki$Ticker.symbol

for (symbol in SP_500_symbols){
  file_destination <- paste("full_history/",symbol,".csv",sep="")
  if(
  !file.exists(file_destination)
  ) next
  filesstrings::file.move(file_destination,"SP_500")
}
