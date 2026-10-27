# 3. Define mc_auc_ci()

mc_auc_ci <- function(truth,
                      probabilities,
                      n_boot = 1000,
                      conf_level = 0.95,
                      seed = NULL) {
  
  # Convert probabilities to a matrix
  probabilities <- as.matrix(probabilities)
  
  # Check number of observations
  if (length(truth) != nrow(probabilities)) {
    stop("The number of class labels must match the number of probability rows.")
  }
  
  # Check number of classes
  classes <- unique(as.character(truth))
  
  if (length(classes) < 3) {
    stop("At least 3 classes are required for multiclass AUC.")
  }
  # Check probability column names
  if (is.null(colnames(probabilities)) ||
      !setequal(colnames(probabilities), classes)) {
    stop("Probability column names must match the class labels in truth.")
  }
  
  # Check probabilities are numeric
  if (!is.numeric(probabilities)) {
    stop("Probabilities must be numeric.")
  }
  # Check missing values
  if (anyNA(truth) || anyNA(probabilities)) {
    stop("Missing values detected. Please handle missing data before running mc_auc_ci().")
  }
  # Check probabilities are between 0 and 1
  if (any(probabilities < 0 | probabilities > 1, na.rm = TRUE)) {
    stop("Probability values must be between 0 and 1.")
  }
  
  # Check n_boot
  if (length(n_boot) != 1 ||
      !is.numeric(n_boot) ||
      n_boot <= 0 ||
      n_boot != as.integer(n_boot)) {
    stop("n_boot must be a positive integer.")
  }
  
  # Check confidence level
  if (length(conf_level) != 1 ||
      !is.numeric(conf_level) ||
      conf_level <= 0 ||
      conf_level >= 1) {
    stop("conf_level must be between 0 and 1.")
  }
  
  # Set seed only when provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  
  
  # 4. Calculate the original Hand-Till multiclass AUC
  
  original_roc <- pROC::multiclass.roc(
    response = truth,
    predictor = probabilities
  )
  
  original_auc <- as.numeric(original_roc$auc)
  
  
  # 5. Stratified bootstrap resampling
  
  # Create a vector to store bootstrap AUC values
  bootstrap_values <- rep(NA_real_, n_boot)
  
  # Repeat the bootstrap process
  for (b in seq_len(n_boot)) {
    
    # Sample separately within each class
    bootstrap_index <- unlist(
      lapply(classes, function(class_name) {
        
        class_index <- which(as.character(truth) == class_name)
        
        sample(
          class_index,
          size = length(class_index),
          replace = TRUE
        )
      }),
      use.names = FALSE
    )
    
    # Apply the same sampled rows to labels and probabilities
    bootstrap_truth <- truth[bootstrap_index]
    
    bootstrap_probabilities <-
      probabilities[bootstrap_index, , drop = FALSE]
    
    # Calculate AUC for this bootstrap sample
    bootstrap_values[b] <- tryCatch({
      
      bootstrap_roc <- pROC::multiclass.roc(
        response = bootstrap_truth,
        predictor = bootstrap_probabilities
      )
      
      as.numeric(bootstrap_roc$auc)
      
    }, error = function(e) {
      
      NA_real_
      
    })
  }
  
  # Count successful and failed bootstrap replicates
  n_successful <- sum(!is.na(bootstrap_values))
  n_failed <- sum(is.na(bootstrap_values))
  
  # Keep successful bootstrap AUC values
  successful_values <- bootstrap_values[!is.na(bootstrap_values)]
  
  
  # 6. Calculate bootstrap standard error and confidence interval
  
  # Need at least two successful bootstrap values
  if (length(successful_values) < 2) {
    stop("Too few successful bootstrap replicates to calculate the confidence interval.")
  }
  
  # Bootstrap standard error
  bootstrap_se <- sd(successful_values)
  
  # Calculate alpha
  alpha <- 1 - conf_level
  
  # Percentile bootstrap confidence interval
  ci <- quantile(
    successful_values,
    probs = c(alpha / 2, 1 - alpha / 2),
    names = FALSE
  )
  
  lower_ci <- ci[1]
  upper_ci <- ci[2]
  
  
  # Return results
  
  
  return(list(
    original_auc = original_auc,
    standard_error = bootstrap_se,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    conf_level = conf_level,
    n_boot = n_boot,
    n_successful = n_successful,
    n_failed = n_failed,
    bootstrap_values = successful_values
  ))
}