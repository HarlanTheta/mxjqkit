mxjq_glmnetmc <- function(glmnetm_result){
  tempf <- function(x, y){
    x %>%
      as.matrix() %>%
      as.data.frame() %>%
      rownames_to_column("X") %>%
      pivot_longer(cols = -1, 
                   names_to = "lambdaid",
                   values_to = "coef") %>%
      mutate(ylevel = y)
  }
  result_1 <- map2(
    glmnetm_result[["beta"]], names(glmnetm_result[["beta"]]), tempf
  )
  result_2 <- bind_rows(result_1)
  return(result_2)
}
  
  
