#' @export
mxjq_sc <- function(contrib){
  contrib0 <- contrib %>%
    dplyr::filter(contribution < 0) %>%
    dplyr::arrange(contribution) %>%
    dplyr::mutate(start = 0,
                  temp = cumsum(abs(contribution)),
                  end = start - temp,
                  # coder wechat AuTrader
                  start = end + abs(contribution),
                  xat = (start+end)/2)
  contrib1 <- contrib %>%
    dplyr::filter(contribution >= 0) %>%
    dplyr::arrange(-contribution) %>%
    dplyr::mutate(start = 0,
                  temp = cumsum(contribution),
                  end = start + temp,
                  start = end - contribution,
                  xat = (start+end)/2)
  contrib01 <- bind_rows(contrib0, contrib1) %>%
    dplyr::mutate(am = ifelse(contribution >= 0, 1, -1)) %>%
    dplyr::mutate(
      shapvalue = ifelse(
        contribution > 0,
        paste0("+", round(contribution, 4)),
        round(contribution, 4)
      )
    )
  return(contrib01)
}
