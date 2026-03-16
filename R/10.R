#' @export
mxjq_mrocplot <- function (data, colname4p, truthclass, cols){
  rocdf <- data.frame()
  for (i in seq_along(colname4p)) {
    coli <- colname4p[i]
    probi <- data.frame(A = 1 - data[[coli]], B = data[[coli]])
    actuali <- ifelse(truthclass == coli, "B", "A")
    roci <- roc(response = actuali, predictor = probi$B, 
                levels = c("A", "B"), direction = "<")
    auci <- auc(roci)
    auci2 <- sprintf("%.4f", as.numeric(auci))
    rocdfi <- coords(roci)
    rocdfi$auc <- paste(coli, "AUC:", auci2)
    rocdf <- rbind(rocdf, rocdfi)
  }
  rocplot <- ggplot(rocdf, aes(x = 1 - specificity, y = sensitivity, color = auc)) +
    geom_path(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) + 
    labs(color = "") + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          legend.position = "inside", 
          legend.justification = c(1, 0), 
          legend.background = element_blank(), 
          legend.key = element_blank(), 
          text = element_text(family = "serif"))
  return(list(result = rocdf, plot = rocplot))
}
