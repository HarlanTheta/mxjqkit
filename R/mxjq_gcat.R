#' @export
mxjq_gcat <- function(datax){
  if(sum(sapply(datax, is.factor)) == 0){
    catvars <- NULL
  } else{
    # coder wechat AuTrader
    catvars <-
      colnames(datax)[sapply(datax, is.factor)]
  }
  return(catvars)
}
