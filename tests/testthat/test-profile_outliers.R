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
