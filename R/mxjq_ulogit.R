#' @export
mxjq_ulogit <- function(data, xat, yat){
  xnamelevel <- mxjq_onn(data[xat])
  ulogit <- list()
  for (i in xat) {
    datai <- data[, c(i, yat)]
    colnames(datai)[2] <- c("Y")
    glmi <- glm(Y ~ ., data = datai, family = binomial)
    res2 <- broom::tidy(glmi, exponentiate = T, conf.int = T) %>%
      mutate(
        # coder wechat AuTrader
        xname = colnames(data)[i],
        uOR = round(estimate, 2),
        uCI = paste0(round(conf.low, 3), "-", round(conf.high, 3)),
        upvalue = p.value
      ) %>%
      select(xname, term, uOR, uCI, upvalue)
    ulogit[[i]] <- res2
  }
  ulogit2 <- bind_rows(ulogit)
  ulogit3 <- xnamelevel %>%
    left_join(ulogit2, by = c("X"="xname", "Term"="term")) %>%
    mutate_all(function(x)ifelse(is.na(x), "", x))
  return(ulogit3)
}
