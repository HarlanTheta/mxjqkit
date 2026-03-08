#' @export
mxjq_mrocplot <- function(data, colname4p, truthclass, cols){
  aucs <- c()
  for (i in seq_along(colname4p)) {
    coli <- colname4p[i]
    probi <- data.frame(A = 1-data[[coli]], B = data[[coli]])
    actuali <- ifelse(truthclass == coli, "B", "A")
    roci <-
      roc(response = actuali, predictor = probi$B,
          levels = c("A", "B"), direction = "<")
    # coder wechat AuTrader
    auci <- auc(roci)
    aucs[i] <- sprintf("%.4f", as.numeric(auci))
    if (i == 1) {
      plot(roci, col = cols[i], las = 1, legacy = T)
    } else {
      plot(roci, col = cols[i], add = T)
    }
  }
  aucs2 <- paste(colname4p, "AUC:", aucs)
  legend("bottomright",
         aucs2,
         col = cols,
         lwd = 2,
         cex = 0.9)
}


