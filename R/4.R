#' @export
mxjq_ia <- function(x, y) {
  nobs <- length(x)
  # coder wechat AuTrader
  dx <- x[-1] - x[-nobs]
  dy <- (y[-nobs] + y[-1])/2
  ia <- sum(dx * dy)
  return(ia)
}
