test_that("remove method removes observations with missing probabilities", {

  x <- list(
    truth = factor(c("A", "B", "C", "A")),
    probs = data.frame(
      A = c(0.8, 0.1, 0.2, NA),
      B = c(0.1, 0.8, 0.2, 0.3),
      C = c(0.1, 0.1, 0.6, 0.7)
    )
  )

  class(x) <- "mc_roc"

  expect_warning(
    result <- handle_missing(x, method = "remove"),
    "1 observation(s) containing missing values were removed",
    fixed = TRUE
  )

  expect_equal(length(result$truth), 3)
  expect_equal(nrow(result$probs), 3)

  expect_false(anyNA(result$truth))
  expect_false(anyNA(result$probs))

  expect_equal(result$missing_info$n_affected, 1)
  expect_equal(result$missing_info$pct_affected, 25)
  expect_equal(result$missing_info$n_removed, 1)
  expect_equal(result$missing_info$n_imputed, 0)
  expect_equal(result$missing_info$method, "remove")

  expect_s3_class(result, "mc_roc")
})


test_that("mean method fills missing probability values", {

  x <- list(
    truth = factor(c("A", "B", "C", "A")),
    probs = data.frame(
      A = c(0.8, 0.1, 0.2, NA),
      B = c(0.1, 0.8, 0.2, 0.3),
      C = c(0.1, 0.1, 0.6, 0.7)
    )
  )

  class(x) <- "mc_roc"

  expect_warning(
    result <- handle_missing(x, method = "mean"),
    "1 observation(s) had missing probability values filled using mean imputation.",
    fixed = TRUE
  )

  expect_equal(length(result$truth), 4)
  expect_equal(nrow(result$probs), 4)

  expect_false(anyNA(result$probs))

  expect_equal(
    rowSums(result$probs),
    rep(1, 4),
    tolerance = 1e-10
  )

  expect_equal(result$missing_info$n_affected, 1)
  expect_equal(result$missing_info$pct_affected, 25)
  expect_equal(result$missing_info$n_removed, 0)
  expect_equal(result$missing_info$n_imputed, 1)
  expect_equal(result$missing_info$method, "mean")

  expect_s3_class(result, "mc_roc")
})


test_that("median method fills missing probability values", {

  x <- list(
    truth = factor(c("A", "B", "C", "A", "B")),
    probs = data.frame(
      A = c(0.8, 0.1, 0.2, NA, 0.3),
      B = c(0.1, 0.8, 0.2, 0.3, NA),
      C = c(0.1, 0.1, 0.6, 0.7, 0.4)
    )
  )

  class(x) <- "mc_roc"

  expect_warning(
    result <- handle_missing(x, method = "median"),
    "2 observation(s) had missing probability values filled using median imputation.",
    fixed = TRUE
  )

  expect_false(anyNA(result$probs))

  expect_equal(
    rowSums(result$probs),
    rep(1, 5),
    tolerance = 1e-10
  )

  expect_equal(result$missing_info$n_affected, 2)
  expect_equal(result$missing_info$pct_affected, 40)
  expect_equal(result$missing_info$n_removed, 0)
  expect_equal(result$missing_info$n_imputed, 2)
  expect_equal(result$missing_info$method, "median")

  expect_s3_class(result, "mc_roc")
})


test_that("missing true labels are removed instead of imputed", {

  x <- list(
    truth = factor(
      c("A", "B", NA, "C"),
      levels = c("A", "B", "C")
    ),
    probs = data.frame(
      A = c(0.8, 0.1, 0.2, 0.1),
      B = c(0.1, 0.8, 0.2, 0.2),
      C = c(0.1, 0.1, 0.6, 0.7)
    )
  )

  class(x) <- "mc_roc"

  expect_warning(
    result <- handle_missing(x, method = "mean"),
    "1 observation(s) with missing true class labels were removed.",
    fixed = TRUE
  )

  expect_equal(length(result$truth), 3)
  expect_equal(nrow(result$probs), 3)

  expect_false(anyNA(result$truth))
  expect_false(anyNA(result$probs))

  expect_equal(result$missing_info$n_affected, 1)
  expect_equal(result$missing_info$pct_affected, 25)
  expect_equal(result$missing_info$n_removed, 1)
  expect_equal(result$missing_info$n_imputed, 0)

  expect_s3_class(result, "mc_roc")
})


test_that("data without missing values is returned unchanged", {

  x <- list(
    truth = factor(c("A", "B", "C")),
    probs = data.frame(
      A = c(0.8, 0.1, 0.2),
      B = c(0.1, 0.8, 0.2),
      C = c(0.1, 0.1, 0.6)
    )
  )

  class(x) <- "mc_roc"

  result <- handle_missing(x, method = "remove")

  expect_equal(result$truth, x$truth)
  expect_equal(result$probs, x$probs)

  expect_equal(result$missing_info$n_affected, 0)
  expect_equal(result$missing_info$pct_affected, 0)
  expect_equal(result$missing_info$n_removed, 0)
  expect_equal(result$missing_info$n_imputed, 0)

  expect_s3_class(result, "mc_roc")
})


test_that("non mc_roc objects are rejected", {

  x <- list(
    truth = c("A", "B", "C"),
    probs = data.frame(
      A = c(0.8, 0.1, 0.2),
      B = c(0.1, 0.8, 0.2),
      C = c(0.1, 0.1, 0.6)
    )
  )

  expect_error(
    handle_missing(x),
    "x must be an object of class 'mc_roc'.",
    fixed = TRUE
  )
})


test_that("mc_roc object must contain truth and probs", {

  x <- list(
    truth = factor(c("A", "B", "C"))
  )

  class(x) <- "mc_roc"

  expect_error(
    handle_missing(x),
    "x must contain 'truth' and 'probs' components.",
    fixed = TRUE
  )
})


test_that("invalid missing value method is rejected", {

  x <- list(
    truth = factor(c("A", "B", "C")),
    probs = data.frame(
      A = c(0.8, 0.1, 0.2),
      B = c(0.1, 0.8, 0.2),
      C = c(0.1, 0.1, 0.6)
    )
  )

  class(x) <- "mc_roc"

  expect_error(
    handle_missing(x, method = "unknown")
  )
})


test_that("truth and probs must have the same number of observations", {

  x <- list(
    truth = factor(c("A", "B", "C", "A")),
    probs = data.frame(
      A = c(0.8, 0.1, 0.2),
      B = c(0.1, 0.8, 0.2),
      C = c(0.1, 0.1, 0.6)
    )
  )

  class(x) <- "mc_roc"

  expect_error(
    handle_missing(x),
    "x$truth and x$probs must contain the same number of observations.",
    fixed = TRUE
  )
})
