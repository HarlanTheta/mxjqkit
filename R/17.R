#' @export
mxjq_semicircle <- function(x, t=NULL) {
  par(mar = c(0.1, 0.3, 0.5, 0.3))
  plot.new()
  plot.window(xlim = c(-1.1, 1.1), ylim = c(-0.01, 1.1), asp = 1)
  theta <- seq(pi, 0, length.out = 100)
  xs <- cos(theta)
  ys <- sin(theta)
  lines(xs, ys, col = "black", lwd = 2)
  tick_theta <- seq(pi, 0, length.out = 101)
  tick_x <- cos(tick_theta)
  tick_y <- sin(tick_theta)
  for(i in 1:length(tick_theta)) {
    lines(c(tick_x[i]*0.97, tick_x[i]),
          c(tick_y[i]*0.97, tick_y[i]),
          col = "black",
          lwd = ifelse(i %% 10 == 1, 1.5, 0.8))
    if(i %% 10 == 1) {
      text(tick_x[i]*1.08,
           tick_y[i]*1.08,
           labels = round((i-1)/100, 1),
           cex = 1,
           adj = c(0.5, 0.5))
    }
  }
  split_theta <- pi * (1 - x)
  split_x <- cos(split_theta)
  split_y <- sin(split_theta)
  theta_fill <- seq(pi, 0, length.out = 200)
  x_fill <- cos(theta_fill)
  y_fill <- sin(theta_fill)
  if(x > 0) {
    start_idx <- 1
    end_idx <- which.min(abs(theta_fill - split_theta))
    polygon(c(0, x_fill[start_idx:end_idx], 0),
            c(0, y_fill[start_idx:end_idx], 0),
            col = rgb(1, 0.2, 0.2, 0.7), border = NA)
  }
  if(x < 1) {
    start_idx <- which.min(abs(theta_fill - split_theta))
    end_idx <- length(theta_fill)

    polygon(c(0, x_fill[start_idx:end_idx], 0),
            c(0, y_fill[start_idx:end_idx], 0),
            col = rgb(0.2, 1, 0.2, 0.7), border = NA)
  }
  lines(xs, ys, col = "black", lwd = 2)
  lines(c(0, split_x), c(0, split_y), col = "black", lwd = 2)
  arrow_start_ratio <- 0.1
  arrow_end_ratio <- 1
  arrow_start_x <- split_x * arrow_start_ratio
  arrow_start_y <- split_y * arrow_start_ratio
  arrow_end_x <- split_x * arrow_end_ratio
  arrow_end_y <- split_y * arrow_end_ratio
  arrows(arrow_start_x, arrow_start_y,
         arrow_end_x, arrow_end_y,
         length = 0.15,
         angle = 20,
         code = 2,
         lwd = 1.5,
         col = "black")
  points(split_x, split_y, col="red", pch=20, cex=1.2)
  rect(-0.35, 0.3, 0.35, 0.5,
       col = rgb(0, 0, 0, 0.6),
       border = rgb(0.2, 0.2, 0.2, 0.8),
       lwd = 1.5)
  if(is.null(t)){
    printx <- paste0("Prob=", scales::percent(x, 0.1))
  } else {
    printx <- ifelse(x > t, "High Risk", "Low Risk")
  }
  text(0, 0.4,
       printx,
       col = rgb(1, 1, 1),
       cex = 1.4,
       font = 2)

}
