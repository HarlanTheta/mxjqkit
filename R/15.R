#' @export
mxjq_table1 <- function(data, varat, groupat){
  tab1 <- list()
  for (i in varat) {
    datai <- data[, c(i, groupat)]
    colnames(datai)[1:2] <- c("X", "Group")
    datai4test <- na.omit(datai)
    datai2 <- datai %>%
      mutate(Group = "ALLSample")
    datai3 <- datai %>%
      bind_rows(datai2)
    groupn <- datai3 %>%
      count(Group) %>%
      mutate(Groupname = colnames(data)[groupat]) %>%
      mutate(
        Grouplevel = ifelse(
          # coder wechat AuTrader
          Group == "ALLSample",
          paste0(Group, "(", n, ")"),
          paste0(Groupname, "=", Group, "(", n, ")")
        )
      )
    if (is.factor(datai[[1]])) {
      res1 <- datai3 %>%
        count(Group, X) %>%
        group_by(Group) %>%
        mutate(p = n / sum(n)) %>%
        ungroup() %>%
        mutate(np = paste0(n, "(", scales::percent(p), ")"), 
               Xname = colnames(data)[i]) %>%
        left_join(groupn[, c(1,4)], by = "Group") %>%
        select(6, 7, 2, 5) %>%
        pivot_wider(names_from = Grouplevel, 
                    values_from = np) %>%
        mutate(X = ifelse(is.na(X), "NA", as.character(X)))
      smcti <- 
        all(c(table(datai4test$X, datai4test$Group)) >= 5)
      cti <- chisq.test(
        datai4test$X, datai4test$Group,
        simulate.p.value = !smcti
      )
      res2 <- data.frame(
        Xname = colnames(data)[i],
        X = "",
        # coder wechat AuTrader
        test = ifelse(smcti, "Chisq.test", "Chisq.test.sim"),
        stat = as.numeric(cti$statistic),
        pvalue = as.numeric(cti$p.value)
      )
      res3 <- res1 %>%
        full_join(res2, by = c("Xname", "X")) %>%
        arrange(test)
      tab1[[i]] <- res3
    } else {
      resna <- data.frame()
      if(sum(is.na(datai[[1]])) > 0) {
        resna <- datai3 %>%
          filter(is.na(X)) %>%
          count(Group) %>%
          full_join(groupn, by = "Group") %>%
          mutate(
            Xname = colnames(data)[i],
            X = "NA",
            n.x = ifelse(is.na(n.x), 0, n.x),
            np = n.x/n.y,
            np2 = paste0(n.x, "(", scales::percent(np), ")")
          ) %>%
          select(6,7,5,9) %>%
          pivot_wider(names_from = Grouplevel, 
                      values_from = np2)
      }
      ming <- datai4test %>%
        count(Group) %>%
        pull(n) %>%
        min()
      st <- datai4test %>%
        group_by(Group) %>%
        sample_n(min(ming, 3000)) %>%
        rstatix::shapiro_test(X)
      if (all(st$p > 0.1)) {
        res1 <- datai3 %>%
          group_by(Group) %>%
          summarise(M = round(mean(X, na.rm = T), 2),
                    S = round(sd(X, na.rm = T), 2)) %>%
          ungroup() %>%
          mutate(ms = paste0(M, "(", S, ")"), 
                 Xname = colnames(data)[i],
                 X = "") %>%
          left_join(groupn[, c(1,4)], by = "Group") %>%
          select(5, 6, 7, 4) %>%
          pivot_wider(names_from = Grouplevel, 
                      values_from = ms)
        if (length(levels(datai$Group)) > 2) {
          ati <- rstatix::anova_test(datai4test, X ~ Group)
          res2 <- data.frame(
            Xname = colnames(data)[i],
            X = "",
            test = "anova",
            stat = as.numeric(ati$`F`),
            pvalue = as.numeric(ati$p)
          )
        } else {
          tti <- rstatix::t_test(datai4test, X ~ Group)
          res2 <- data.frame(
            Xname = colnames(data)[i],
            X = "",
            test = "t.test",
            stat = as.numeric(tti$statistic),
            pvalue = as.numeric(tti$p)
          )
        }
        res3 <- res1 %>%
          full_join(res2, by = c("Xname", "X"))
        tab1[[i]] <- bind_rows(res3, resna)
      } else {
        res1 <- datai3 %>%
          group_by(Group) %>%
          summarise(M = round(median(X, na.rm = T), 2),
                    S = quantile(X, 0.75, na.rm = T) - 
                      quantile(X, 0.25, na.rm = T)) %>%
          ungroup() %>%
          mutate(ms = paste0(M, "(", S, ")"), 
                 Xname = colnames(data)[i],
                 X = "") %>%
          left_join(groupn[, c(1,4)], by = "Group") %>%
          select(5, 6, 7, 4) %>%
          pivot_wider(names_from = Grouplevel,
                      values_from = ms)
        if (length(levels(datai$Group)) > 2) {
          kti <- rstatix::kruskal_test(datai4test, X ~ Group)
          res2 <- data.frame(
            Xname = colnames(data)[i],
            X = "",
            test = "kruskal.test",
            stat = as.numeric(kti$statistic),
            pvalue = as.numeric(kti$p)
          )
        } else {
          wti <- rstatix::wilcox_test(datai4test, X ~ Group)
          res2 <- data.frame(
            Xname = colnames(data)[i],
            X = "",
            test = "wilcox.test",
            stat = as.numeric(wti$statistic),
            pvalue = as.numeric(wti$p)
          )
        }
        res3 <- res1 %>%
          full_join(res2, by = c("Xname", "X"))
        tab1[[i]] <- bind_rows(res3, resna)
      }
    }
  }
  
  tab12 <- bind_rows(tab1) %>%
    mutate(stat = round(stat, 2),
           pvalue = round(pvalue, 4)) %>%
    mutate(pvalue = ifelse(pvalue < 0.001, "<0.001", pvalue)) %>%
    mutate_all(as.character) %>%
    mutate_all(function(x)ifelse(is.na(x),"",x))
  
  return(tab12)
}
