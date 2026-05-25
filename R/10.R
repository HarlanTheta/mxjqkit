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
    rocdfi$class <- coli
    rocdfi$auc <- auci2
    rocdfi$dauc <- paste(coli, "AUC:", auci2)
    rocdf <- rbind(rocdf, rocdfi)
  }
  rocplot <- ggplot(rocdf, aes(x = 1 - specificity, y = sensitivity, color = dauc)) +
    geom_path(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2),
                       labels = c(0, seq(0.2, 0.8, by = 0.2), 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1),
                       breaks = seq(0, 1, by = 0.2),
                       labels = c(0, seq(0.2, 0.8, by = 0.2), 1)) +
    labs(x = "1-Specificity", y = "Sensitivity", color = "") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "inside",
          legend.justification = c(1, 0),
          legend.background = element_blank(),
          legend.key = element_blank(),
          text = element_text(family = "serif"))
  return(list(result = rocdf, plot = rocplot))
}
