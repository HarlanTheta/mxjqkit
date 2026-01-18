#' @export
mxjq_table0 <- function(data, varat){
  tab1 <- list()
  for (i in varat) {
    datai <- data[i]
    colnames(datai) <- "X"
    if (is.factor(datai[[1]])) {
      res1 <- datai %>%
        count(X) %>%
        mutate(p = n / sum(n)) %>%
        mutate(Stat = paste0(n, "(", scales::percent(p), ")"), 
               Xname = colnames(data)[i]) %>%
        select(5, 1, 4) %>%
        mutate(X = ifelse(is.na(X), "NA", as.character(X)),
               Note = ifelse(X=="NA", "NA-Ratio", ""))
      tab1[[i]] <- res1
    } else {
      resna <- data.frame()
      if(sum(is.na(datai[[1]])) > 0) {
        nan <- sum(is.na(datai))
        nap <- nan/nrow(datai)
        resna <- data.frame(
            Xname = colnames(data)[i],
            X = "NA",
            Stat = paste0(nan, "(", scales::percent(nap), ")"),
            Note = "NA-Ratio"
          )
      }
      datai2 <- na.omit(datai[[1]])
      if(length(datai2)>3000){
        set.seed(42)
        datai2 <- sample(datai2, size = 3000)
      }
      st <- shapiro.test(datai2)
      if (st$p > 0.1) {
        M = round(mean(datai[[1]], na.rm = T), 2)
        S = round(sd(datai[[1]], na.rm = T), 2)
        res1 <- data.frame(
          Xname = colnames(data)[i],
          X = "",
          Stat = paste0(M, "(", S, ")"),
          Note = "Normal"
        )
        tab1[[i]] <- bind_rows(res1, resna)
      } else {
        M = round(median(datai[[1]], na.rm = T), 2)
        S = quantile(datai[[1]], 0.75, na.rm = T) - 
                      quantile(datai[[1]], 0.25, na.rm = T)
        res1 <- data.frame(
          Xname = colnames(data)[i],
          X = "",
          Stat = paste0(M, "(", S, ")"),
          Note = "Non-Normal"
        )
        tab1[[i]] <- bind_rows(res1, resna)
      }
    }
  }
  tab12 <- bind_rows(tab1) %>%
    mutate_all(as.character) %>%
    mutate_all(function(x)ifelse(is.na(x),"",x))
  return(tab12)
}
