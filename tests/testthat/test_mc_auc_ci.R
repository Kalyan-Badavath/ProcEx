
# ProcEx - Automated tests for mc_auc_ci()
# File: tests/testthat/test_mc_auc_ci.R
#
# This file uses testthat and the functions loaded by ProcEx.
# No manual source() call or external dataset is required.

# ------------------------------------------------------------
# Test data helpers
# ------------------------------------------------------------

make_ci_multiclass_data <- function() {
  truth <- factor(c(rep("A", 8), rep("B", 6), rep("C", 6)))

  probabilities <- data.frame(
    A = c(
      0.70, 0.65, 0.60, 0.55, 0.50, 0.45, 0.40, 0.35,
      0.20, 0.25, 0.30, 0.20, 0.35, 0.25,
      0.15, 0.20, 0.25, 0.30, 0.20, 0.35
    ),
    B = c(
      0.20, 0.25, 0.25, 0.30, 0.30, 0.35, 0.35, 0.40,
      0.65, 0.60, 0.55, 0.50, 0.45, 0.50,
      0.20, 0.25, 0.20, 0.25, 0.30, 0.25
    ),
    C = c(
      0.10, 0.10, 0.15, 0.15, 0.20, 0.20, 0.25, 0.25,
      0.15, 0.15, 0.15, 0.30, 0.20, 0.25,
      0.65, 0.55, 0.55, 0.45, 0.50, 0.40
    )
  )

  list(
    truth = truth,
    probabilities = as.matrix(probabilities)
  )
}

make_ci_binary_data <- function() {
  set.seed(42)

  truth <- factor(
    rep(c("A", "B"), each = 50),
    levels = c("A", "B")
  )

  positive_probability <- c(
    runif(50, 0.05, 0.65),
    runif(50, 0.35, 0.95)
  )

  probabilities <- cbind(
    A = 1 - positive_probability,
    B = positive_probability
  )

  list(
    truth = truth,
    probabilities = probabilities
  )
}

# Check the structure and validity of a CI result.
check_ci_result <- function(x, expected_n,
                            expected_conf = 0.95) {

  expect_s3_class(x, "mc_auc_ci")

  expect_named(
    x,
    c(
      "original_auc",
      "standard_error",
      "lower_ci",
      "upper_ci",
      "conf_level",
      "n_boot",
      "n_successful",
      "n_failed",
      "bootstrap_values",
      "show_bootstrap_values"
    )
  )

  expect_true(is.finite(x$original_auc))
  expect_gte(x$original_auc, 0)
  expect_lte(x$original_auc, 1)

  expect_true(is.finite(x$standard_error))
  expect_gte(x$standard_error, 0)

  expect_true(is.finite(x$lower_ci))
  expect_true(is.finite(x$upper_ci))

  expect_gte(x$lower_ci, 0)
  expect_lte(x$upper_ci, 1)
  expect_lte(x$lower_ci, x$upper_ci)

  expect_equal(x$conf_level, expected_conf)
  expect_equal(x$n_boot, expected_n)

  expect_equal(
    x$n_successful + x$n_failed,
    expected_n
  )

  expect_gte(x$n_successful, 2)

  expect_length(
    x$bootstrap_values,
    x$n_successful
  )

  expect_true(all(is.finite(x$bootstrap_values)))
}

# ------------------------------------------------------------
# Test 1: Multiclass AUC and bootstrap confidence interval
# ------------------------------------------------------------

test_that("mc_auc_ci calculates multiclass AUC and CI", {
  d <- make_ci_multiclass_data()

  result <- mc_auc_ci(
    truth = d$truth,
    probabilities = d$probabilities,
    n_boot = 100,
    conf_level = 0.95,
    seed = 42
  )

  check_ci_result(result, expected_n = 100)

  expect_equal(result$n_successful, 100)
  expect_equal(result$n_failed, 0)

  expected_auc <- as.numeric(
    pROC::multiclass.roc(
      response = d$truth,
      predictor = d$probabilities
    )$auc
  )

  expect_equal(
    result$original_auc,
    expected_auc
  )

  expect_false(result$show_bootstrap_values)

  output <- capture.output(print(result))

  expect_true(
    any(grepl(
      "AUC Bootstrap Confidence Interval",
      output,
      fixed = TRUE
    ))
  )

  expect_false(
    any(grepl(
      "Bootstrap AUC Values:",
      output,
      fixed = TRUE
    ))
  )
})

# ------------------------------------------------------------
# Test 2: Binary classification support
# ------------------------------------------------------------

test_that("mc_auc_ci supports binary classification", {
  d <- make_ci_binary_data()

  result <- mc_auc_ci(
    truth = d$truth,
    probabilities = d$probabilities,
    n_boot = 100,
    seed = 42
  )

  check_ci_result(result, expected_n = 100)

  expect_equal(result$n_successful, 100)
  expect_equal(result$n_failed, 0)

  expected_auc <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = d$truth,
        predictor = d$probabilities[, "B"],
        levels = c("A", "B"),
        direction = "<",
        quiet = TRUE
      )
    )
  )

  expect_equal(
    result$original_auc,
    expected_auc
  )
})

# ------------------------------------------------------------
# Test 3: Optional bootstrap output
# ------------------------------------------------------------

test_that("bootstrap values can be displayed or hidden", {
  d <- make_ci_binary_data()

  hidden <- mc_auc_ci(
    d$truth,
    d$probabilities,
    n_boot = 10,
    seed = 42,
    show_bootstrap_values = FALSE
  )

  check_ci_result(hidden, expected_n = 10)

  hidden_output <- capture.output(print(hidden))

  expect_false(
    any(grepl(
      "Bootstrap AUC Values:",
      hidden_output,
      fixed = TRUE
    ))
  )

  visible <- mc_auc_ci(
    d$truth,
    d$probabilities,
    n_boot = 10,
    seed = 42,
    show_bootstrap_values = TRUE
  )

  check_ci_result(visible, expected_n = 10)

  expect_true(visible$show_bootstrap_values)

  visible_output <- capture.output(print(visible))

  expect_true(
    any(grepl(
      "Bootstrap AUC Values:",
      visible_output,
      fixed = TRUE
    ))
  )

  large_result <- mc_auc_ci(
    d$truth,
    d$probabilities,
    n_boot = 150,
    seed = 42,
    show_bootstrap_values = TRUE
  )

  check_ci_result(large_result, expected_n = 150)

  large_output <- capture.output(print(large_result))

  expect_true(
    any(grepl(
      "Showing first 100 values",
      large_output,
      fixed = TRUE
    ))
  )

  expect_length(large_result$bootstrap_values, 150)
})

# ------------------------------------------------------------
# Test 4: Input validation
# ------------------------------------------------------------

test_that("mc_auc_ci rejects invalid inputs", {
  d <- make_ci_multiclass_data()

  truth <- d$truth
  probabilities <- d$probabilities

  # Probability above 1
  bad_prob <- probabilities
  bad_prob[1, 1] <- 1.2

  expect_error(
    mc_auc_ci(truth, bad_prob, n_boot = 10),
    "between 0 and 1"
  )

  # Negative probability
  bad_negative <- probabilities
  bad_negative[1, 1] <- -0.1

  expect_error(
    mc_auc_ci(truth, bad_negative, n_boot = 10),
    "between 0 and 1"
  )

  # Invalid bootstrap counts
  expect_error(
    mc_auc_ci(truth, probabilities, n_boot = 0),
    "n_boot"
  )

  expect_error(
    mc_auc_ci(truth, probabilities, n_boot = 1),
    "n_boot"
  )

  expect_error(
    mc_auc_ci(truth, probabilities, n_boot = 2.5),
    "n_boot"
  )

  # Invalid confidence levels
  expect_error(
    mc_auc_ci(
      truth,
      probabilities,
      n_boot = 10,
      conf_level = 1.2
    ),
    "conf_level"
  )

  expect_error(
    mc_auc_ci(
      truth,
      probabilities,
      n_boot = 10,
      conf_level = 0
    ),
    "conf_level"
  )

  # Incorrect number of rows
  expect_error(
    mc_auc_ci(
      truth[-1],
      probabilities,
      n_boot = 10
    ),
    "number of class labels"
  )

  # Missing probability
  missing_prob <- probabilities
  missing_prob[1, 1] <- NA_real_

  expect_error(
    mc_auc_ci(truth, missing_prob, n_boot = 10),
    "Missing values"
  )

  # Infinite probability
  infinite_prob <- probabilities
  infinite_prob[1, 1] <- Inf

  expect_error(
    mc_auc_ci(truth, infinite_prob, n_boot = 10),
    "finite"
  )

  # Incorrect class names
  wrong_names <- probabilities
  colnames(wrong_names) <- c("A", "B", "D")

  expect_error(
    mc_auc_ci(truth, wrong_names, n_boot = 10),
    "column names"
  )

  # Duplicate class names
  duplicate_names <- probabilities
  colnames(duplicate_names) <- c("A", "A", "C")

  expect_error(
    mc_auc_ci(truth, duplicate_names, n_boot = 10),
    "column names"
  )

  # Invalid bootstrap display option
  expect_error(
    mc_auc_ci(
      truth,
      probabilities,
      n_boot = 10,
      show_bootstrap_values = NA
    ),
    "show_bootstrap_values"
  )

  # Invalid seed
  expect_error(
    mc_auc_ci(
      truth,
      probabilities,
      n_boot = 10,
      seed = -1
    ),
    "seed"
  )

  # Single-class input
  single_class_truth <- factor(
    rep("A", length(truth))
  )

  expect_error(
    mc_auc_ci(
      single_class_truth,
      probabilities,
      n_boot = 10
    ),
    "At least 2 classes"
  )

  # Missing class label
  missing_truth <- truth
  missing_truth[1] <- NA

  expect_error(
    mc_auc_ci(
      missing_truth,
      probabilities,
      n_boot = 10
    ),
    "Missing values"
  )
})

# ------------------------------------------------------------
# Test 5: Reproducibility
# ------------------------------------------------------------

test_that("same seed gives reproducible bootstrap results", {
  d <- make_ci_multiclass_data()

  result1 <- mc_auc_ci(
    d$truth,
    d$probabilities,
    n_boot = 100,
    seed = 42
  )

  result2 <- mc_auc_ci(
    d$truth,
    d$probabilities,
    n_boot = 100,
    seed = 42
  )

  check_ci_result(result1, expected_n = 100)
  check_ci_result(result2, expected_n = 100)

  expect_identical(
    result1$bootstrap_values,
    result2$bootstrap_values
  )

  expect_identical(
    result1$original_auc,
    result2$original_auc
  )

  expect_identical(
    result1$standard_error,
    result2$standard_error
  )

  expect_identical(
    result1$lower_ci,
    result2$lower_ci
  )

  expect_identical(
    result1$upper_ci,
    result2$upper_ci
  )
})

# ------------------------------------------------------------
# Test 6: Different confidence levels
# ------------------------------------------------------------

test_that("confidence level controls percentile CI", {
  d <- make_ci_multiclass_data()

  result90 <- mc_auc_ci(
    d$truth,
    d$probabilities,
    n_boot = 100,
    conf_level = 0.90,
    seed = 42
  )

  result95 <- mc_auc_ci(
    d$truth,
    d$probabilities,
    n_boot = 100,
    conf_level = 0.95,
    seed = 42
  )

  check_ci_result(
    result90,
    expected_n = 100,
    expected_conf = 0.90
  )

  check_ci_result(
    result95,
    expected_n = 100,
    expected_conf = 0.95
  )

  # The same seed should produce identical samples.
  expect_identical(
    result90$bootstrap_values,
    result95$bootstrap_values
  )

  # A 90% percentile CI should be inside the 95% CI.
  expect_gte(result90$lower_ci, result95$lower_ci)
  expect_lte(result90$upper_ci, result95$upper_ci)
})
