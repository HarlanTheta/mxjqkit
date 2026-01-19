#' @export
mxjq_glmnetplot3 <- function(glmnet_result, cvglmnet_result, labelsyat, labelyangle, fwr){
  lambda2 <- c(cvglmnet_result[["lambda.min"]],
               cvglmnet_result[["lambda.1se"]])
  llambda2 <- log(lambda2)
  lambda2at <- match(lambda2, cvglmnet_result[["lambda"]])
  nonzero2 <- cvglmnet_result[["nzero"]][lambda2at]
  nl <- length(glmnet_result[["beta"]])
  
  glmnet_result_2 <- mxjq_glmnetmc(glmnet_result)
  glmnet_result_3 <- data.frame(
    df = glmnet_result[["df"]],
    pdev = glmnet_result[["dev.ratio"]],
    lambda = glmnet_result[["lambda"]],
    lambdaid = paste0("s", 0:(length(glmnet_result[["lambda"]])-1))
  )
  glmnet_result1 <- glmnet_result_2 %>%
    left_join(glmnet_result_3, by = "lambdaid")
  p1 <- ggplot() +
    geom_path(
      data = glmnet_result1,
      mapping = aes(x = log(lambda), y = coef, color = X),
      linewidth = 1, 
      show.legend = F
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = llambda2, linetype = "dashed")
  if(!(is.null(labelsyat))){
    lambda2text2 <- data.frame(
      xat = rep(llambda2, nl),
      yat = rep(labelsyat, each = 2),
      txt = rep(c("lambda.min", "lambda.1se"), nl),
      ylevel = rep(names(glmnet_result[["beta"]]), each = 2)
    )
    p1 <- p1 +
      geom_label(data = lambda2text2,
                 mapping = aes(x = xat, y = yat, label = txt),
                 size = 4,
                 label.padding = unit(0, "lines"),
                 angle = labelyangle)
  }
  p1 +
    scale_x_continuous(
      sec.axis = dup_axis(breaks = llambda2,
                          labels = nonzero2,
                          name = "NonZero")
    ) +
    facet_wrap(~ylevel, nrow = fwr, scales = "free_y") +
    labs(color = "", y = "coefficient") +
    theme_bw() +
    theme(panel.grid = element_blank())
}
