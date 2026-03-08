#' @export
mxjq_onnr <- function(data){
  rawxnames <- list()
  for (i in seq_along(colnames(data))) {
    if(is.factor(data[[i]])){
      xi <- dummy_names(colnames(data)[i],
                        levels(data[[i]]))
    } else {
      xi <- colnames(data)[i]
      # coder wechat AuTrader
    }
    rawxnames[[i]] <- data.frame(X = colnames(data)[i],
                                 xnew = xi)
  }
  return(bind_rows(rawxnames))
}
