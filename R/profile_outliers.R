#' Profile multiclass prediction outliers
#'
#' @param truth True class labels.
#' @param probs Predicted class probabilities.
#' @param iqr_multiplier Multiplier used for IQR-based outlier detection.
#' @param lower Optional custom lower probability limit.
#' @param upper Optional custom upper probability limit.
#' @export
profile_outliers <- function(truth, probs,
                             iqr_multiplier = 1.5,
                             lower = NULL,
                             upper = NULL) {
  
  # Validate actual class labels
  if (!is.factor(truth) && !is.character(truth)) {
    stop("truth must be a factor or character vector.")
  }
  
  # Validate prediction scores
  if (!is.matrix(probs) && !is.data.frame(probs)) {
    stop("probs must be a numeric matrix or data frame.")
  }
  
  probs <- as.data.frame(probs)
  
  if (nrow(probs) != length(truth)) {
    stop("truth and probs must have the same number of observations.")
  }
  
  if (ncol(probs) < 3) {
    stop("At least three prediction-score columns are required.")
  }
  
  if (!all(vapply(probs, is.numeric, logical(1)))) {
    stop("All prediction scores must be numeric.")
  }
  
  if (anyNA(truth) || anyNA(probs)) {
    stop("Missing values detected. Please handle them first.")
  }
  
  if (any(!is.finite(as.matrix(probs)))) {
    stop("Prediction scores must be finite.")
  }
  
  if (any(as.matrix(probs) < 0 | as.matrix(probs) > 1)) {
    stop("Prediction probabilities must be between 0 and 1.")
  }
  
  if (!is.numeric(iqr_multiplier) ||
      length(iqr_multiplier) != 1 ||
      !is.finite(iqr_multiplier) ||
      iqr_multiplier < 0) {
    stop("iqr_multiplier must be a non-negative number.")
  }
  
  if (xor(is.null(lower), is.null(upper))) {
    stop("Provide both lower and upper, or neither.")
  }
  
  if (!is.null(lower)) {
    if (!is.numeric(lower) || !is.numeric(upper) ||
        length(lower) != 1 || length(upper) != 1 ||
        !is.finite(lower) || !is.finite(upper) ||
        lower < 0 || upper > 1 || lower >= upper) {
      stop("Custom limits must satisfy 0 <= lower < upper <= 1.")
    }
  }
  
  results <- data.frame(
    observation = integer(),
    actual_class = character(),
    score_column = character(),
    score = numeric(),
    reason = character()
  )
  
  for (column in names(probs)) {
    values <- probs[[column]]
    
    if (is.null(lower)) {
      q1 <- as.numeric(stats::quantile(values, 0.25))
      q3 <- as.numeric(stats::quantile(values, 0.75))
      iqr_value <- q3 - q1
      
      lower_limit <- q1 - iqr_multiplier * iqr_value
      upper_limit <- q3 + iqr_multiplier * iqr_value
    } else {
      lower_limit <- lower
      upper_limit <- upper
    }
    
    outliers <- which(
      values < lower_limit | values > upper_limit
    )
    
    if (length(outliers) > 0) {
      results <- rbind(
        results,
        data.frame(
          observation = as.integer(outliers),
          actual_class = as.character(truth[outliers]),
          score_column = column,
          score = as.numeric(values[outliers]),
          reason = "IQR outlier"
        )
      )
    }
  }
  
  # Detect high-confidence wrong predictions
  confidence_threshold <- 0.90
  
  if (!is.null(names(probs)) &&
      setequal(names(probs), unique(as.character(truth)))) {
    
    predicted_class <- names(probs)[max.col(probs, ties.method = "first")]
    confidence <- apply(probs, 1, max)
    
    wrong_high_confidence <- which(
      predicted_class != as.character(truth) &
        confidence >= confidence_threshold
    )
    
    if (length(wrong_high_confidence) > 0) {
      results <- rbind(
        results,
        data.frame(
          observation = as.integer(wrong_high_confidence),
          actual_class = as.character(truth[wrong_high_confidence]),
          score_column = predicted_class[wrong_high_confidence],
          score = as.numeric(confidence[wrong_high_confidence]),
          reason = "High-confidence wrong prediction"
        )
      )
    }
  }
  
  return(results)
}


# Calculate the influence of flagged observations on multiclass AUC

profile_auc_influence <- function(truth, probs,
                                  iqr_multiplier = 1.5,
                                  lower = NULL,
                                  upper = NULL) {
  
  flagged <- profile_outliers(
    truth, probs,
    iqr_multiplier = iqr_multiplier,
    lower = lower,
    upper = upper
  )
  
  probs <- as.matrix(probs)
  truth <- factor(truth)
  
  if (nlevels(truth) < 3) {
    stop("At least three classes are required.")
  }
  
  if (is.null(colnames(probs)) ||
      !setequal(colnames(probs), levels(truth))) {
    stop("Prediction columns must match the class labels.")
  }
  
  probs <- probs[, levels(truth), drop = FALSE]
  
  auc_value <- function(y, p) {
    as.numeric(pROC::multiclass.roc(
      response = y,
      predictor = p,
      levels = levels(truth),
      direction = ">"
    )$auc)
  }
  
  original_auc <- auc_value(truth, probs)
  
  observations <- unique(flagged$observation)
  
  results <- data.frame(
    observation = integer(),
    actual_class = character(),
    original_auc = numeric(),
    auc_without = numeric(),
    auc_change = numeric()
  )
  
  for (i in observations) {
    keep <- seq_along(truth) != i
    
    if (any(table(truth[keep]) == 0)) {
      next
    }
    
    new_auc <- auc_value(
      truth[keep],
      probs[keep, , drop = FALSE]
    )
    
    results <- rbind(
      results,
      data.frame(
        observation = as.integer(i),
        actual_class = as.character(truth[i]),
        original_auc = original_auc,
        auc_without = new_auc,
        auc_change = new_auc - original_auc
      )
    )
  }
  
  results[
    order(-abs(results$auc_change)),
    ,
    drop = FALSE
  ]
}
