mxjq_signif <- function(vx){
  vx1 <- vx%%1
  vx2 <- vx[vx1 != 0]
  if(all(vx1 == 0)){
    sx <- 1
  } else {
    sx <- max(nchar(unlist(
      lapply(strsplit(as.character(vx2), "\\."), function(x)x[[2]])
    )))
  }
  return(sx)
}
