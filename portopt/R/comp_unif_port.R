#' Compare a portfolio to the uniform portfolio
#'
#' @param test A testing set to test a portfolio on.
#' @param portfolio A vector of portfolio weights.
#'
#' @export
#'
comp.unif.port <- function(test,portfolio){
  if(length(portfolio) != ncol(test)) return("Remove date column +stocks from test set that weren't in training set")
  p <- ncol(test)
  port.val <- t(portfolio) %*% t(test)
  port.val <- port.val / port.val[1]
  portfolio2 <- rep(1/p,p)
  port.val2 <- t(portfolio2) %*% t(test)
  port.val2 <- port.val2 / port.val2[1]
  plot(1:length(port.val),port.val,type="l",col="blue",xlab = "Time",ylab = "Portfolio Value")
  lines(1:length(port.val2),port.val2,type="l",col="red")
  legend("bottomright",legend = c("Factor","Uniform"),lty = c(1,1),col=c("blue","red"))
}
