#' @export
mxjq_density <- function(pred, yrange){
  rawy <- data.frame(Y = yrange)
  qy <- quantile(yrange, c(0.25, 0.5, 0.75))
  densy <- density(yrange)
  dqy <- approx(densy$x, densy$y, xout = qy)$y
  dqy_df <- data.frame(Y = qy,
                       D = dqy,
                       L = names(qy))
  dpred <- approx(densy$x, densy$y, xout = pred)$y
  ggplot(rawy, aes(x = Y)) +
    geom_density(fill = "lightblue", alpha = 0.5, color = "blue") +
    geom_point(x = pred, y = dpred, color = "red", size = 3) +
    geom_label(x = pred, y = dpred, label = pred,
               hjust = -0.25) +
    geom_segment(data = dqy_df,
                 mapping = aes(x = Y, xend = Y, y = 0, yend = D),
                 color = "orange",
                 linetype = "dashed",
                 linewidth = 1) +
    geom_text(data = dqy_df,
              aes(x = Y, y = D, label = L),
              vjust = 0.25,
              hjust = -0.25) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(axis.text = element_text(size = 15))
}
