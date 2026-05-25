#' @export
mxjq_proc <- function(roclist, names){
  rocres <- list()
  rocdat <- list()
  for(i in seq_along(roclist)){
    iname = names[i]
    roci <- roclist[[i]]
    rocresi <- data.frame(
      t(
        c(iname,
          roci$direction,
          sprintf("%.3f", as.numeric(ci.auc(roci))))
      )
    )
    colnames(rocresi) <-
      c("X", "direction", "auc1", "auc", "auc2")
    # coder wechat AuTrader
    rocdati <- pROC::coords(roci)
    roccori <- pROC::coords(roci, x = "best")
    rocresi$cutoff <- tail(roccori$threshold, 1)
    rocres[[i]] <- rocresi
    rocdati$X <- iname
    rocdati2 <- rocresi %>%
      left_join(rocdati, by = "X") %>%
      mutate(lab = paste0(X, " AUC=", auc,
                          "(", auc1, "-", auc2, ")"))
    rocdat[[i]] <- rocdati2
  }
  rocres2 <- bind_rows(rocres)
  rocplot <- bind_rows(rocdat) %>%
    mutate(lab = as_factor(lab)) %>%
    ggplot(aes(x = 1-specificity,
               y = sensitivity,
               color = lab)) +
    geom_path(linewidth = 1) +
    # coder wechat AuTrader
    geom_abline(slope = 1,
                intercept = 0,
                linetype = "dashed") +
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
          legend.justification = c(1,0),
          legend.background = element_blank(),
          legend.key = element_blank(),
          text = element_text(family = "serif"))
  return(list(result = rocres2,
              plot = rocplot))
}


