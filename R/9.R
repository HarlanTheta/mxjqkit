#' @export
mxjq_r01 <- function(x){
  r01 <- (x - min(x))/(max(x) - min(x))
  # coder wechat AuTrader
  return(r01)
}
