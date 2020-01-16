#' Convert close prices into returns
#'
#' The transpose of the matrix is returned so that this matrix can be used
#' with the rest of the package.
#' @param data A datafram of close prices.
#'
#' @export
#' @return
#'
returns <- function(data,lag,log = TRUE){
  data <- data[,-1] #Remove date column.
  if (log) data <- log(data)
  returns <- data %>% as.matrix %>% diff(lag = lag) %>% t()
  return(-returns)
}
