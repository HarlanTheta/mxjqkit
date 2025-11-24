#' @export
mxjq_gcon <- function(datax){
  catvars <- mxjq_gcat(datax)
  if(length(catvars) == ncol(datax)){
    convars <- NULL
    # coder wechat AuTrader
  } else{
    convars <- setdiff(colnames(datax), catvars)
  }
  return(convars)
}
