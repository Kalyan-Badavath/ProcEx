test_that("valid multiclass data passes", {

  truth <- c(rep("A", 10), rep("B", 10), rep("C", 10))

  probs <- data.frame(
    A = rep(1/3, 30),
    B = rep(1/3, 30),
    C = rep(1/3, 30)
  )

  result <- validate_classification_data(truth, probs)

  expect_true(result$valid)
  expect_equal(result$status, "VALID")
  expect_length(result$errors, 0)
  expect_length(result$warnings, 0)
})


test_that("binary data is rejected", {

  truth <- c("A", "B", "A")

  probs <- data.frame(
    A = c(0.8, 0.2, 0.7),
    B = c(0.2, 0.8, 0.3)
  )

  result <- validate_classification_data(truth, probs)

  expect_false(result$valid)
  expect_equal(result$status, "INVALID")
  expect_true(any(grepl(
    "At least 3 classes are required",
    result$errors,
    fixed = TRUE
  )))
})


test_that("wrong class names are rejected", {

  truth <- c("A", "B", "C")

  probs <- data.frame(
    A = c(0.8, 0.1, 0.2),
    B = c(0.1, 0.7, 0.2),
    D = c(0.1, 0.2, 0.6)
  )

  result <- validate_classification_data(truth, probs)

  expect_false(result$valid)
  expect_true(any(grepl(
    "Probability column names must match the class labels.",
    result$errors,
    fixed = TRUE
  )))
})


test_that("invalid probabilities are rejected", {

  truth <- c("A", "B", "C")

  probs <- data.frame(
    A = c(1.2, 0.1, 0.2),
    B = c(0.1, 0.7, 0.2),
    C = c(0.1, 0.2, 0.6)
  )

  result <- validate_classification_data(truth, probs)

  expect_false(result$valid)
  expect_true(any(grepl(
    "Probability values must be between 0 and 1.",
    result$errors,
    fixed = TRUE
  )))
})


test_that("missing probabilities are rejected", {

  truth <- c("A", "B", "C")

  probs <- data.frame(
    A = c(0.8, NA, 0.2),
    B = c(0.1, 0.7, 0.2),
    C = c(0.1, 0.3, 0.6)
  )

  result <- validate_classification_data(truth, probs)

  expect_false(result$valid)
  expect_true(any(grepl(
    "Probability values cannot contain NA, NaN, or Inf.",
    result$errors,
    fixed = TRUE
  )))
})


test_that("small classes produce warning status", {

  truth <- c(rep("A", 5), rep("B", 5), rep("C", 2))

  probs <- data.frame(
    A = rep(1/3, 12),
    B = rep(1/3, 12),
    C = rep(1/3, 12)
  )

  result <- validate_classification_data(truth, probs)

  expect_true(result$valid)
  expect_equal(result$status, "VALID_WITH_WARNINGS")
  expect_true(any(grepl(
    "At least one class has fewer than 10 observations.",
    result$warnings,
    fixed = TRUE
  )))
})


test_that("empty data is rejected", {

  truth <- character(0)

  probs <- data.frame(
    A = numeric(0),
    B = numeric(0),
    C = numeric(0)
  )

  result <- validate_classification_data(truth, probs)

  expect_false(result$valid)
  expect_true(any(grepl(
    "truth cannot be empty.",
    result$errors,
    fixed = TRUE
  )))
})


test_that("missing true labels are rejected", {

  truth <- c("A", "B", NA, "C")

  probs <- data.frame(
    A = c(0.8, 0.1, 0.2, 0.1),
    B = c(0.1, 0.8, 0.2, 0.2),
    C = c(0.1, 0.1, 0.6, 0.7)
  )

  result <- validate_classification_data(truth, probs)

  expect_false(result$valid)
  expect_true(any(grepl(
    "truth cannot contain missing values.",
    result$errors,
    fixed = TRUE
  )))
})


test_that("duplicate probability column names are rejected", {

  truth <- c("A", "B", "C")

  probs <- data.frame(
    A = c(0.8, 0.1, 0.2),
    B = c(0.1, 0.7, 0.2),
    C = c(0.1, 0.2, 0.6),
    check.names = FALSE
  )

  names(probs) <- c("A", "A", "C")

  result <- validate_classification_data(truth, probs)

  expect_false(result$valid)
  expect_true(any(grepl(
    "Probability column names must be unique.",
    result$errors,
    fixed = TRUE
  )))
})


test_that("probability matrix without column names is rejected", {

  truth <- c("A", "B", "C")

  probs <- matrix(
    c(
      0.8, 0.1, 0.1,
      0.1, 0.8, 0.1,
      0.2, 0.2, 0.6
    ),
    nrow = 3,
    byrow = TRUE
  )

  result <- validate_classification_data(truth, probs)

  expect_false(result$valid)
  expect_true(any(grepl(
    "Probability columns must have class names.",
    result$errors,
    fixed = TRUE
  )))
})


test_that("non-numeric probability columns are rejected", {

  truth <- c("A", "B", "C")

  probs <- data.frame(
    A = c(0.8, 0.1, 0.2),
    B = c(0.1, 0.7, 0.2),
    C = c("0.1", "0.2", "0.6")
  )

  result <- validate_classification_data(truth, probs)

  expect_false(result$valid)
  expect_true(any(grepl(
    "All probability columns must be numeric.",
    result$errors,
    fixed = TRUE
  )))
})


test_that("invalid min_class_n is rejected", {

  truth <- c("A", "B", "C")

  probs <- data.frame(
    A = c(0.8, 0.1, 0.2),
    B = c(0.1, 0.7, 0.2),
    C = c(0.1, 0.2, 0.6)
  )

  result <- validate_classification_data(
    truth,
    probs,
    min_class_n = 0
  )

  expect_false(result$valid)
  expect_equal(result$status, "INVALID")
  expect_true(any(grepl(
    "min_class_n must be a single positive numeric value.",
    result$errors,
    fixed = TRUE
  )))
})


test_that("invalid imbalance_threshold is rejected", {

  truth <- c("A", "B", "C")

  probs <- data.frame(
    A = c(0.8, 0.1, 0.2),
    B = c(0.1, 0.7, 0.2),
    C = c(0.1, 0.2, 0.6)
  )

  result <- validate_classification_data(
    truth,
    probs,
    imbalance_threshold = 1.5
  )

  expect_false(result$valid)
  expect_equal(result$status, "INVALID")
  expect_true(any(grepl(
    "imbalance_threshold must be a single value between 0 and 1.",
    result$errors,
    fixed = TRUE
  )))
})
