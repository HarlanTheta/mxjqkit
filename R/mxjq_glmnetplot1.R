#' @export
mxjq_glmnetplot1 <- 
  function(cvglmnet_result, labelyat, labelyangle){
    lambda2 <- c(cvglmnet_result[["lambda.min"]],
                 cvglmnet_result[["lambda.1se"]])
    llambda2 <- log(lambda2)
    lambda2at <- match(lambda2, cvglmnet_result[["lambda"]])
    nonzero2 <- cvglmnet_result[["nzero"]][lambda2at]
    cv_glmnet_result <- data.frame(
      lambda = cvglmnet_result[["lambda"]],
      cvm = cvglmnet_result[["cvm"]],
      cvsd = cvglmnet_result[["cvsd"]],
      cvup = cvglmnet_result[["cvup"]],
      cvlo = cvglmnet_result[["cvlo"]],
      nzero = cvglmnet_result[["nzero"]]
    ) %>%
      mutate(nx = paste(nzero, "variables"),
             nx = forcats::as_factor(nx))
    p1 <- cv_glmnet_result %>%
      ggplot(aes(x = log(lambda), y = cvm)) +
      geom_errorbar(aes(ymin = cvlo, 
                        ymax = cvup, 
                        color = nx),
                    linewidth = 1,
                    show.legend = F) +
      geom_point(color = "red") +
      geom_vline(xintercept = llambda2, linetype = "dashed")
    if(!(is.null(labelyat))){
      lambda2text <- data.frame(
        xat = llambda2,
        yat = labelyat,
        txt = c("lambda.min", "lambda.1se")
      )
      p1 <- p1 +
        geom_label(data = lambda2text,
                   mapping = aes(x = xat, y = yat,label = txt),
                   angle = labelyangle)
    }
    p1  +
      scale_x_continuous(
        sec.axis = dup_axis(breaks = llambda2, 
                            labels = nonzero2,
                            name = "NonZero")
      ) +
      labs(y = cvglmnet_result[["name"]], color = "") +
      theme_bw() +
      theme(panel.grid = element_blank())
  }
