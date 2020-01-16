#' Retreieve the close prices between specified dates
#' Columns containing missing values will
#' @param data A dataframe to be subsetted by date. The dataframe must have a column
#' with the name 'date'.
#' @param from A character string which gives the date of the earliest
#' entry e.g. 2018-01-01.
#' @param to A character string which gives the date of the latest
#' entry e.g. 2018-01-01.
#' @param missing Set equal to false if you want to retain missing values.
#'
#' @return A dataframe with observations between the specified dates.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
retrieve.close <- function(data,from,to,missing = TRUE){
  subset <- data %>% dplyr::filter(as.Date(data$date) >= from) %>%
    dplyr::filter(as.Date(.$date) < to)
  #Next identify the columns containing missing values.
  if (missing){
    to.remove <- colnames(subset)[colSums(is.na(subset)) > 0]
    subset %<>% dplyr::select(-to.remove) #Remove columns.
  }
  subset <- subset[,-1] #Remove first column otherwise output incorrect.
  subset$date <- as.Date(subset$date)
  subset <- dplyr::arrange(subset,date)
  return(subset)
}
