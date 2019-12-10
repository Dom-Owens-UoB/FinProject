#We join oil and close data by date to handle missing entries, e.g. weekends 
library(dplyr)

close_data <- read.csv("combined_close_data.csv") #import data
close_data_2018 <- close_data[1:251,2:421] #select past year
oil_reverse <- read.csv("DCOILWTICO.csv") #read in oil data
oil <- rev(oil_reverse) #reverse input

oil_close_2018 <- inner_join(x = oil, y = close_data_2018, by = c("DATE" = "date")) #join by date

ts.plot(oil_close_2018$DCOILWTICO) #check oil series for missing data

write.csv(oil_close_2018, "oil_close_2018.csv") #write to csv
