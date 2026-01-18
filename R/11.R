#' @export
mxjq_probbar <- function(prob, clas){
  clab <- paste0("P(", clas, ")=", scales::percent(prob, 0.01))
  df <- data.frame(
    id = "A",
    Class = forcats::as_factor(clas),
    Prob = prob,
    Clab = clab,
    Clabat = 1-(cumsum(prob) - prob/2)
  )
  ggplot(df, aes(y = id, x = Prob, fill = Class)) +
    geom_col(width = 0.15, show.legend = F) +
    ggrepel::geom_text_repel(
      aes(x = Clabat,
          y = as.numeric(as.factor(id)),
          label = Clab),
      nudge_y = 0.15,
      size = 5,
      color = "black"
    ) +
    theme_void()
}
