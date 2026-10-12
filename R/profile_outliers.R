profile_outliers <- function(truth, probs) {
  
  results <- data.frame(
    observation = integer(),
    actual_class = character(),
    score_column = character(),
    score = numeric()
  )
  
  for (column in names(probs)) {
    
    values <- probs[[column]]
    
    q1 <- quantile(values, 0.25)
    q3 <- quantile(values, 0.75)
    
    iqr_value <- q3 - q1
    
    lower <- q1 - 1.5 * iqr_value
    upper <- q3 + 1.5 * iqr_value
    
    outliers <- which(values < lower | values > upper)
    
    if (length(outliers) > 0) {
      
      results <- rbind(
        results,
        data.frame(
          observation = outliers,
          actual_class = as.character(truth[outliers]),
          score_column = column,
          score = values[outliers]
        )
      )
    }
  }
  
  return(results)
}
