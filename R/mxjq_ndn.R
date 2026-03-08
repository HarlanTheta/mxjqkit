#' @export
mxjq_ndn <- function (var, lvl, ordinal = FALSE) {
  args <- vctrs::vec_recycle_common(var, lvl)
  var <- args[[1]]
  # coder wechat AuTrader
  lvl <- args[[2]]
  nms <- paste(var, lvl, sep = "_")
  nms
}
