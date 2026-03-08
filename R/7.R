#' @export
mxjq_glmnetplot2 <- function(glmnet_result, cvglmnet_result, labelyat, labelyangle){
  lambda2 <- c(cvglmnet_result[["lambda.min"]],
               cvglmnet_result[["lambda.1se"]])
  llambda2 <- log(lambda2)
  lambda2at <- match(lambda2, cvglmnet_result[["lambda"]])
  nonzero2 <- cvglmnet_result[["nzero"]][lambda2at]
  glmnet_result_1 <- glmnet_result[["beta"]] %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column("X") %>%
    pivot_longer(cols = -1, 
                 names_to = "lambdaid",
                 values_to = "coef")
  glmnet_result_2 <- data.frame(
    df = glmnet_result[["df"]],
    pdev = glmnet_result[["dev.ratio"]],
    lambda = glmnet_result[["lambda"]],
    lambdaid = paste0("s", 0:(length(glmnet_result[["lambda"]])-1))
  )
  glmnet_result <- glmnet_result_1 %>%
    left_join(glmnet_result_2, by = "lambdaid")
  p1 <- ggplot() +
    geom_path(
      data = glmnet_result,
      mapping = aes(x = log(lambda), y = coef, color = X),
      linewidth = 1, 
      show.legend = F
    ) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = llambda2, linetype = "dashed")
  if(!(is.null(labelyat))){
    lambda2text2 <- data.frame(
      xat = llambda2,
      yat = labelyat,
      txt = c("lambda.min", "lambda.1se")
    )
    p1 <- p1 + 
      geom_label(data = lambda2text2,
                 mapping = aes(x = xat, y = yat, label = txt),
                 angle = labelyangle)
  }
  p1 +
    scale_x_continuous(
      sec.axis = dup_axis(breaks = llambda2, 
                          labels = nonzero2, 
                          name = "NonZero")
    ) +
    labs(color = "", y = "coefficient") +
    theme_bw() +
    theme(panel.grid = element_blank())
}
