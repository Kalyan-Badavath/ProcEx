library(testthat)

# Create reproducible test data
set.seed(123)
truth <- factor(rep(c("A", "B", "C"), each = 20))
probs <- matrix(runif(180, 0.1, 0.9), ncol = 3)
probs <- probs / rowSums(probs)
colnames(probs) <- c("A", "B", "C")
probs[60, ] <- c(0.95, 0.03, 0.02)

test_that("IQR outlier detection works", {
  result <- profile_outliers(truth, probs)
  expect_true(60 %in% result$observation)
})

test_that("Custom limits work", {
  result <- profile_outliers(
    truth, probs, lower = 0.05, upper = 0.90
  )
  expect_true(60 %in% result$observation)
})

test_that("IQR multiplier is adjustable", {
  result <- profile_outliers(
    truth, probs, iqr_multiplier = 2
  )
  expect_true(is.data.frame(result))
})

test_that("AUC influence works", {
  result <- profile_auc_influence(truth, probs)
  expect_true(60 %in% result$observation)
  expect_true(all(is.finite(result$auc_change)))
})

test_that("AUC matches pROC", {
  expected <- as.numeric(pROC::multiclass.roc(
    response = truth,
    predictor = probs,
    levels = levels(truth),
    direction = ">"
  )$auc)
  
  result <- profile_auc_influence(truth, probs)
  expect_equal(result$original_auc[1], expected)
})

test_that("Missing values are rejected", {
  probs_na <- probs
  probs_na[5, 1] <- NA
  expect_error(
    profile_outliers(truth, probs_na),
    "Missing values"
  )
})

test_that("Incorrect row counts are rejected", {
  expect_error(
    profile_outliers(truth, probs[1:10, ]),
    "same number"
  )
})

test_that("Non-numeric scores are rejected", {
  bad <- as.data.frame(probs)
  bad$A <- as.character(bad$A)
  expect_error(profile_outliers(truth, bad))
})

test_that("Invalid probabilities are rejected", {
  bad <- probs
  bad[1, 1] <- 1.5
  expect_error(profile_outliers(truth, bad))
})

test_that("Invalid custom limits are rejected", {
  expect_error(
    profile_outliers(truth, probs, lower = 0.9, upper = 0.1)
  )
})

test_that("Uneven class sizes work", {
  uneven_truth <- factor(c(
    rep("A", 30), rep("B", 20), rep("C", 10)
  ))
  result <- profile_auc_influence(uneven_truth, probs)
  expect_true(is.data.frame(result))
})

test_that("No outliers returns an empty table", {
  even_probs <- data.frame(
    A = rep(0.3, 30),
    B = rep(0.4, 30),
    C = rep(0.3, 30)
  )
  even_truth <- factor(rep(c("A", "B", "C"), each = 10))
  result <- profile_outliers(even_truth, even_probs)
  expect_equal(nrow(result), 0L)
})

test_that("One observation per class is handled", {
  small_truth <- factor(c("A", "B", "C"))
  small_probs <- matrix(
    c(0.9, 0.05, 0.05,
      0.05, 0.9, 0.05,
      0.05, 0.05, 0.9),
    nrow = 3, byrow = TRUE,
    dimnames = list(NULL, c("A", "B", "C"))
  )
  
  result <- profile_auc_influence(
    small_truth, small_probs
  )
  expect_true(is.data.frame(result))
})

# Test the flag option
test_that("Flag option identifies outliers", {
  result <- profile_outliers(
    truth, probs,
    action = "flag"
  )
  
  expect_true(is.data.frame(result))
})

# Test the remove option
test_that("Remove option removes flagged observations", {
  flagged <- profile_outliers(truth, probs, action = "flag")
  
  cleaned <- profile_outliers(
    truth, probs,
    action = "remove"
  )
  
  expected_removed <- length(unique(flagged$observation))
  
  expect_equal(
    length(cleaned$removed_observations),
    expected_removed
  )
  
  expect_equal(
    nrow(cleaned$probs),
    nrow(probs) - expected_removed
  )
  
  expect_equal(
    length(cleaned$truth),
    nrow(cleaned$probs)
  )
})

# Test the keep option
test_that("Keep option retains all observations", {
  truth <- factor(rep(c("A", "B", "C"), each = 20))
  probs <- matrix(1/3, nrow = 60, ncol = 3)
  colnames(probs) <- c("A", "B", "C")
  
  kept <- profile_outliers(truth, probs, action = "keep")
  
  expect_equal(nrow(kept$probs), 60)
  expect_equal(length(kept$truth), 60)
})

# Test invalid action
test_that("Invalid action produces an error", {
  expect_error(
    profile_outliers(truth, probs, action = "delete")
  )
})

# Test AUC influence output
test_that("AUC influence returns expected columns", {
  result <- profile_auc_influence(truth, probs)
  
  expect_true(is.data.frame(result))
  
  expect_true(all(c(
    "observation",
    "actual_class",
    "original_auc",
    "auc_without",
    "auc_change"
  ) %in% names(result)))
})
test_that("AUC influence correctly detects improvement", {
  
  truth <- factor(rep(c("A", "B", "C"), each = 10))
  
  probs <- rbind(
    matrix(rep(c(0.90, 0.05, 0.05), 10),
           ncol = 3, byrow = TRUE),
    matrix(rep(c(0.05, 0.90, 0.05), 10),
           ncol = 3, byrow = TRUE),
    matrix(rep(c(0.05, 0.05, 0.90), 10),
           ncol = 3, byrow = TRUE)
  )
  
  colnames(probs) <- c("A", "B", "C")
  probs[5, ] <- c(0.01, 0.98, 0.01)
  
  result <- profile_auc_influence(truth, probs)
  
  expect_equal(result$original_auc[1], 0.95)
  expect_equal(result$auc_without[1], 1)
  expect_equal(result$auc_change[1], 0.05, tolerance = 1e-8)
})


# ==========================================
# TESTS FOR profile_auc_combinations()
# ==========================================

test_that("Combination function returns correct output", {
  
  result <- profile_auc_combinations(
    truth, probs,
    top_n = 5,
    warn_threshold = 1000
  )
  cat("\nSelected:", length(result$selected_observations))
  cat("\nCombinations:", nrow(result$combinations))
  cat("\nData rows:", length(truth), "\n")
  
  
  expect_true(is.list(result))
  
  expect_true(all(c(
    "individual",
    "combinations",
    "selected_observations",
    "flagged_count",
    "original_auc"
  ) %in% names(result)))
  
  expect_true(is.data.frame(result$combinations))
})


test_that("Combination count is correct", {
  
  result <- profile_auc_combinations(
    truth, probs,
    top_n = 5,
    warn_threshold = 1000
  )
  
  
  n <- length(result$selected_observations)
  
  # Maximum possible combinations for top 5
  expected <- 2^5 - 1
  
  expect_lte(length(result$selected_observations), 5)
  expect_lte(nrow(result$combinations), expected)
  expect_gt(nrow(result$combinations), 0)
  
})


test_that("Combination AUC values are valid", {
  
  result <- profile_auc_combinations(
    truth, probs,
    top_n = 5,
    warn_threshold = 1000
  )
  
  expect_true(all(is.finite(result$combinations$auc_change)))
  
  expect_true(all(
    result$combinations$auc_without >= 0 &
      result$combinations$auc_without <= 1
  ))
})


test_that("Combinations are ranked by AUC change", {
  
  result <- profile_auc_combinations(
    truth, probs,
    top_n = 5,
    warn_threshold = 1000
  )
  
  changes <- abs(result$combinations$auc_change)
  
  expect_true(
    all(diff(changes) <= 1e-10)
  )
})


test_that("Invalid top_n is rejected", {
  
  expect_error(
    profile_auc_combinations(
      truth, probs,
      top_n = -1
    ),
    "top_n"
  )
  
  expect_error(
    profile_auc_combinations(
      truth, probs,
      top_n = 11
    ),
    "top_n"
  )
})


test_that("Warning appears for many outliers", {
  
  expect_warning(
    profile_auc_combinations(
      truth, probs,
      top_n = 5,
      warn_threshold = 0
    ),
    "Influence analysis may take a long time"
  )
})
