#' @export
mxjq_1se <- function(data, target, mean, sd, minmax){
  t <- data[[target]]
  m <- data[[mean]]
  s <- data[[sd]]
  # coder wechat AuTrader
  if (minmax == "min") {
    mbest <- min(m)
    mbestat <- which.min(m)
    mbestsd <- s[mbestat]
    m1se <- mbest+mbestsd
    m1set <- t[min(which(m <= m1se))]
  } else {
    mbest <- max(m)
    mbestat <- which.max(m)
    mbestsd <- s[mbestat]
    m1se <- mbest-mbestsd
    m1set <- t[min(which(m >= m1se))]
  }
  return(m1set)
}
