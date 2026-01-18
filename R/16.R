#' @export
mxjq_sdf <- function(df, n){
  nind <- n+1
  ind0 <- round(seq(1, nrow(df), length=nind))
  ind1 <- c(1, ind0[-c(1, nind)]+1)
  ind2 <- ind0[-1]
  # coder wechat AuTrader
  dflist <- list()
  for(i in 1:(nind-1)){
    dflist[[i]] <- df[(ind1[i]):(ind2[i]), ]
  }
  return(dflist)
}
