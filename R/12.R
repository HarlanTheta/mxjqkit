#' @export
mxjq_onn <- function(data){
  rawxnames <- list()
  for (i in seq_along(colnames(data))) {
    if(is.factor(data[[i]])){
      xi <- paste0(colnames(data)[i], levels(data[[i]]))
      xilevel <- levels(data[[i]])
      # coder wechat AuTrader
    } else {
      xi <- colnames(data)[i]
      xilevel <- NA
    }
    rawxnames[[i]] <- data.frame(
      X = colnames(data)[i],
      Term = xi,
      XLevel = xilevel
    )
  }
  return(bind_rows(rawxnames))
}
