# mc_auc_ci() - Prototype Demo and Testing
# 1. Setup
library(pROC)

# 2. Example input data

# Actual class labels for 3 classes
truth <- factor(c(
  rep("A", 8),
  rep("B", 6),
  rep("C", 6)))

# Predicted probabilities for each class
probabilities <- data.frame(
  A = c(
    0.70, 0.65, 0.60, 0.55, 0.50, 0.45, 0.40, 0.35,
    0.20, 0.25, 0.30, 0.20, 0.35, 0.25,
    0.15, 0.20, 0.25, 0.30, 0.20, 0.35),
  
  B = c(
    0.20, 0.25, 0.25, 0.30, 0.30, 0.35, 0.35, 0.40,
    0.65, 0.60, 0.55, 0.50, 0.45, 0.50,
    0.20, 0.25, 0.20, 0.25, 0.30, 0.25),
  
  C = c(
    0.10, 0.10, 0.15, 0.15, 0.20, 0.20, 0.25, 0.25,
    0.15, 0.15, 0.15, 0.30, 0.20, 0.25,
    0.65, 0.55, 0.55, 0.45, 0.50, 0.40))
# Check the example data
table(truth)
head(probabilities)
dim(probabilities)
range(as.matrix(probabilities))
rowSums(probabilities)

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


# Test mc_auc_ci()

result <- mc_auc_ci(
  truth = truth,
  probabilities = probabilities,
  n_boot = 1000,
  conf_level = 0.95,
  seed = 42
)

# Check output type
class(result)

# Check main results
result$original_auc
result$standard_error
result$lower_ci
result$upper_ci
result$conf_level
result$n_boot 
result$n_successful
result$n_failed

# Check retained bootstrap values
length(result$bootstrap_values)
head(result$bootstrap_values)

# 7. Test input validation

# Test 1: Invalid probability value
bad_probabilities <- probabilities
bad_probabilities[1, 1] <- 1.2

try(
  mc_auc_ci(
    truth = truth,
    probabilities = bad_probabilities,
    n_boot = 100,
    conf_level = 0.95,
    seed = 42
  )
)


# Test 2: Invalid number of bootstrap replicates
try(
  mc_auc_ci(
    truth = truth,
    probabilities = probabilities,
    n_boot = 0,
    conf_level = 0.95,
    seed = 42
  )
)


# Test 3: Invalid confidence level
try(
  mc_auc_ci(
    truth = truth,
    probabilities = probabilities,
    n_boot = 100,
    conf_level = 1.2,
    seed = 42
  )
)


# Test 4: Mismatched number of observations
short_truth <- truth[-1]

try(
  mc_auc_ci(
    truth = short_truth,
    probabilities = probabilities,
    n_boot = 100,
    conf_level = 0.95,
    seed = 42
  )
)


# Test 5: Fewer than 3 classes
two_class_truth <- factor(
  ifelse(truth == "C", "B", as.character(truth))
)

try(
  mc_auc_ci(
    truth = two_class_truth,
    probabilities = probabilities,
    n_boot = 100,
    conf_level = 0.95,
    seed = 42
  )
)
# Test 6: Missing values
missing_probabilities <- probabilities
missing_probabilities[1, 1] <- NA

try(
  mc_auc_ci(
    truth = truth,
    probabilities = missing_probabilities,
    n_boot = 100,
    conf_level = 0.95,
    seed = 42
  )
)


# Test 7: Probability column names do not match class labels
wrong_names <- probabilities
colnames(wrong_names) <- c("A", "B", "D")

try(
  mc_auc_ci(
    truth = truth,
    probabilities = wrong_names,
    n_boot = 100,
    conf_level = 0.95,
    seed = 42
  )
)
# 8. Test reproducibility with seed

# Run the function twice using the same seed
result_seed_1 <- mc_auc_ci(
  truth = truth,
  probabilities = probabilities,
  n_boot = 100,
  conf_level = 0.95,
  seed = 42
)

result_seed_2 <- mc_auc_ci(
  truth = truth,
  probabilities = probabilities,
  n_boot = 100,
  conf_level = 0.95,
  seed = 42
)

# Check whether the bootstrap values are identical
identical(
  result_seed_1$bootstrap_values,
  result_seed_2$bootstrap_values
)

# Check whether the confidence intervals are identical
identical(
  c(result_seed_1$lower_ci, result_seed_1$upper_ci),
  c(result_seed_2$lower_ci, result_seed_2$upper_ci)
)

# Run without fixing the seed
result_no_seed_1 <- mc_auc_ci(
  truth = truth,
  probabilities = probabilities,
  n_boot = 100,
  conf_level = 0.95
)

result_no_seed_2 <- mc_auc_ci(
  truth = truth,
  probabilities = probabilities,
  n_boot = 100,
  conf_level = 0.95
)

# Results are not forced to be identical
identical(
  result_no_seed_1$bootstrap_values,
  result_no_seed_2$bootstrap_values
)

# 9. Final positive tests

# Test 1: Use a different confidence level
result_90 <- mc_auc_ci(
  truth = truth,
  probabilities = probabilities,
  n_boot = 500,
  conf_level = 0.90,
  seed = 42
)

result_90$conf_level
result_90$lower_ci
result_90$upper_ci


# Test 2: Use a different number of bootstrap replicates
result_500 <- mc_auc_ci(
  truth = truth,
  probabilities = probabilities,
  n_boot = 500,
  conf_level = 0.95,
  seed = 42
)

result_500$n_successful
result_500$n_failed
length(result_500$bootstrap_values)


# Test 3: Check that the main numerical outputs are valid

result$original_auc >= 0 && result$original_auc <= 1

result$lower_ci >= 0 && result$lower_ci <= 1

result$upper_ci >= 0 && result$upper_ci <= 1

result$lower_ci <= result$upper_ci

result$standard_error >= 0

# 10. Prototype Demo

# The mc_auc_ci() function estimates uncertainty around
# a Hand-Till multiclass AUC using stratified bootstrap resampling.


# 10.1 Input

# truth:
# A factor containing the true class label for each observation.

table(truth)

# probabilities:
# A data frame containing predicted probabilities.
# Each row represents one observation.
# Each column represents one class.
# Probability values must be between 0 and 1.

head(probabilities)

# Check the probability range
range(as.matrix(probabilities))

# 10.2 Run mc_auc_ci()

demo_result <- mc_auc_ci(
  truth = truth,
  probabilities = probabilities,
  n_boot = 1000,
  conf_level = 0.95,
  seed = 42
)


# 10.3 Main output


demo_summary <- data.frame(
  AUC = demo_result$original_auc,
  Standard_Error = demo_result$standard_error,
  CI_Lower = demo_result$lower_ci,
  CI_Upper = demo_result$upper_ci,
  Confidence_Level = demo_result$conf_level,
  Successful_Bootstraps = demo_result$n_successful,
  Failed_Bootstraps = demo_result$n_failed
)

demo_summary

# 10.4 Bootstrap values retained for visualisation

length(demo_result$bootstrap_values)

head(demo_result$bootstrap_values)