
# Tests for profile_outliers()

testthat::test_that("Unusual prediction scores are detected", {

  truth <- c(rep("A", 10), rep("B", 10), rep("C", 10))

  probs <- data.frame(
    A = c(rep(0.4, 29), 0.95),
    B = c(rep(0.3, 29), 0.03),
    C = c(rep(0.3, 29), 0.02)
  )

  result <- profile_outliers(truth, probs)

  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_true(all(result$observation == 30))
  testthat::expect_true(all(result$actual_class == "C"))
})

testthat::test_that("No unusual scores returns an empty table", {

  truth <- rep(c("A", "B", "C"), each = 10)

  probs <- data.frame(
    A = rep(0.4, 30),
    B = rep(0.3, 30),
    C = rep(0.3, 30)
  )

  result <- profile_outliers(truth, probs)

  testthat::expect_equal(nrow(result), 0L)
})
