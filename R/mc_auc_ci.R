#' Bootstrap Confidence Interval for Binary and Multiclass AUC
#'
#' Calculates AUC, bootstrap standard error, and a percentile
#' confidence interval using stratified bootstrap resampling.
#'
#' For binary classification, the second class is treated as positive.
#' For multiclass classification, Hand-Till AUC is used.
#'
#' @param truth True class labels.
#' @param probabilities Matrix of predicted class probabilities.
#' @param n_boot Number of bootstrap replicates (default: 1000).
#' @param conf_level Confidence level used to calculate the percentile
#'   bootstrap confidence interval. The default is 0.95 (95% CI),
#'   using the 2.5th and 97.5th percentiles of the bootstrap AUC values.
#'   For example, 0.90 produces a 90% confidence interval.
#' @param seed Optional random seed for reproducibility.
#' @param show_bootstrap_values Logical value controlling whether
#'   individual bootstrap AUC estimates are printed.
#'   The default is FALSE, which displays only the summary.
#'   If TRUE, up to 100 bootstrap AUC estimates are printed.
#'   Each value represents the AUC calculated from one bootstrap sample.
#'   All successful bootstrap AUC estimates are stored in
#'   the returned object's bootstrap_values component.
#'
#' @return An object of class mc_auc_ci containing:
#'   original_auc (AUC calculated from the original data),
#'   standard_error (standard deviation of bootstrap AUC estimates),
#'   lower_ci and upper_ci (percentile confidence interval bounds),
#'   conf_level (selected confidence level),
#'   n_boot (requested bootstrap replicates),
#'   n_successful and n_failed (bootstrap counts),
#'   bootstrap_values (successful bootstrap AUC estimates),
#'   and show_bootstrap_values (printing preference).
# 3. Define mc_auc_ci()

mc_auc_ci <- function(truth,
                      probabilities,
                      n_boot = 1000,
                      conf_level = 0.95,
                      seed = NULL,
                      show_bootstrap_values = FALSE) {
  
  # Convert probabilities to a matrix
  probabilities <- as.matrix(probabilities)
  
  # Check missing values
  if (anyNA(truth) || anyNA(probabilities)) {
    stop("Missing values detected. Please handle missing data before running mc_auc_ci().")
  }
  
  # Check number of observations
  if (length(truth) != nrow(probabilities)) {
    stop("The number of class labels must match the number of probability rows.")
  }
  
  # Check number of classes
  classes <- if (is.factor(truth)) {
    levels(droplevels(truth))
  } else {
    sort(unique(as.character(truth)))
  }
  
  if (length(classes) < 2) {
    stop("At least 2 classes are required for ROC/AUC analysis.")
  }
  
  # Check probability column names
  if (is.null(colnames(probabilities)) ||
      anyDuplicated(colnames(probabilities)) > 0 ||
      !setequal(colnames(probabilities), classes)) {
    stop("Probability column names must uniquely match the class labels in truth.")
  }
  
  # Check probabilities are numeric
  if (!is.numeric(probabilities)) {
    stop("Probabilities must be numeric.")
  }
  
  # Check finite probability values
  if (any(!is.finite(probabilities))) {
    stop("Probabilities must contain only finite numeric values.")
  }
  
  # Check probabilities are between 0 and 1
  if (any(probabilities < 0 | probabilities > 1)) {
    stop("Probability values must be between 0 and 1.")
  }
  
  # Check n_boot
  if (length(n_boot) != 1 || !is.numeric(n_boot)) {
    stop("n_boot must be an integer greater than or equal to 2.")
  }
  
  if (is.na(n_boot) || !is.finite(n_boot) ||
      n_boot < 2 || n_boot != floor(n_boot)) {
    stop("n_boot must be an integer greater than or equal to 2.")
  }
  
  # Check confidence level
  if (length(conf_level) != 1 || !is.numeric(conf_level)) {
    stop("conf_level must be between 0 and 1.")
  }
  
  if (is.na(conf_level) || !is.finite(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be between 0 and 1.")
  }
  
  # Check show_bootstrap_values
  if (length(show_bootstrap_values) != 1 ||
      !is.logical(show_bootstrap_values)) {
    stop("show_bootstrap_values must be TRUE or FALSE.")
  }
  
  if (is.na(show_bootstrap_values)) {
    stop("show_bootstrap_values must be TRUE or FALSE.")
  }
  
  # Check and set seed only when provided
  if (!is.null(seed)) {
    
    if (length(seed) != 1 ||
        !is.numeric(seed)) {
      stop("seed must be a single non-negative integer.")
    }
    
    if (is.na(seed) ||
        !is.finite(seed) ||
        seed < 0 ||
        seed != floor(seed) ||
        seed > .Machine$integer.max) {
      stop("seed must be a single non-negative integer.")
    }
    
    set.seed(seed)
  }
  
  # Calculate AUC for binary or multiclass classification
  calculate_auc <- function(y, probs) {
    
    if (length(classes) == 2) {
      
      # Binary AUC using the second class as positive
      positive_class <- classes[2]
      
      roc_result <- pROC::roc(
        response = factor(y, levels = classes),
        predictor = probs[, positive_class],
        levels = classes,
        direction = "<",
        quiet = TRUE
      )
      
      return(as.numeric(pROC::auc(roc_result)))
      
    } else {
      
      # Hand-Till multiclass AUC
      roc_result <- pROC::multiclass.roc(
        response = y,
        predictor = probs)
      
      return(as.numeric(roc_result$auc))
    }
  }
  
  
  # 4. Calculate the original AUC
  original_auc <- calculate_auc(
    y = truth,
    probs = probabilities)
  
  
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
      
      calculate_auc(
        y = bootstrap_truth,
        probs = bootstrap_probabilities
      )
      
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
  result <- list(
    original_auc = original_auc,
    standard_error = bootstrap_se,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    conf_level = conf_level,
    n_boot = n_boot,
    n_successful = n_successful,
    n_failed = n_failed,
    bootstrap_values = successful_values,
    show_bootstrap_values = show_bootstrap_values
  )
  
  class(result) <- "mc_auc_ci"
  
  return(result)
}

# Print method for mc_auc_ci results
print.mc_auc_ci <- function(x, ...) {
    
    cat("\nAUC Bootstrap Confidence Interval\n\n")
    
    cat("Original AUC:", round(x$original_auc, 6), "\n")
    cat("Standard Error:", round(x$standard_error, 6), "\n")
    
    cat(
      paste0(x$conf_level * 100, "% CI:"),
      "[",
      round(x$lower_ci, 6), ",",
      round(x$upper_ci, 6), "]\n"
    )
    
    cat("Bootstrap Samples:", x$n_boot, "\n")
    cat("Successful:", x$n_successful, "\n")
    cat("Failed:", x$n_failed, "\n")
    
    if (isTRUE(x$show_bootstrap_values)) {
      
      cat("\nBootstrap AUC Values:\n")
      
      if (length(x$bootstrap_values) > 100) {
        print(head(x$bootstrap_values, 100))
        cat(
          "\nShowing first 100 values out of",
          length(x$bootstrap_values),
          "successful bootstrap replicates.\n"
        )
        cat("Use result$bootstrap_values to access all values.\n")
      } else {
        print(x$bootstrap_values)
      }
    }
    
    invisible(x)
  }
  
