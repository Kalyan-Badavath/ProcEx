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