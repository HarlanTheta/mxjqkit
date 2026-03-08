#' @export
mxjq_sct <- function(contrib){
  contrib01 <- list()
  for (i in seq_along(unique(contrib$time))) {
    contrib0 <- contrib %>%
      dplyr::filter(time == unique(contrib$time)[i]) %>%
      dplyr::filter(survshap < 0) %>%
      dplyr::arrange(survshap) %>%
      dplyr::mutate(start = 0,
                    temp = cumsum(abs(survshap)),
                    end = start - temp,
                    start = end + abs(survshap),
                    xat = (start+end)/2)
    contrib1 <- contrib %>%
      dplyr::filter(time == unique(contrib$time)[i]) %>%
      dplyr::filter(survshap >= 0) %>%
      # coder wechat AuTrader
      dplyr::arrange(-survshap) %>%
      dplyr::mutate(start = 0,
                    temp = cumsum(survshap),
                    end = start + temp,
                    start = end - survshap,
                    xat = (start+end)/2)
    contrib01[[i]] <- bind_rows(contrib0, contrib1) %>%
      dplyr::mutate(am = ifelse(survshap >= 0, 1, -1)) %>%
      dplyr::mutate(
        shapvalue = ifelse(
          survshap > 0,
          paste0("+", round(survshap, 4)),
          round(survshap, 4)
        )
      )
  }
  return(contrib01)
}

