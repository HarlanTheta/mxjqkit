#' @export
mxjq_mrocplot <- function (data, colname4p, truthclass, cols){
  mroclist <- list()
  for (i in seq_along(colname4p)) {
    coli <- colname4p[i]
    probi <- data.frame(A = 1 - data[[coli]], B = data[[coli]])
    actuali <- ifelse(truthclass == coli, "B", "A")
    mroclist[[i]] <- roc(
      response = actuali,
      predictor = probi$B,
      levels = c("A", "B"),
      direction = "<",
      ci = T
    )
  }
  mrocresult <- mxjq_proc(mroclist, colname4p)
  return(list(result = mrocresult$result,
              plot = mrocresult$plot))
}
