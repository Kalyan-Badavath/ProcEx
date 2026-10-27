test_that("mc_auc_ci returns valid results", {

  truth <- factor(c(
    rep("A", 8),
    rep("B", 6),
    rep("C", 6)
  ))

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

  result <- mc_auc_ci(
    truth,
    probabilities,
    n_boot = 100,
    conf_level = 0.95,
    seed = 42
  )

  expect_true(result$original_auc >= 0)
  expect_true(result$original_auc <= 1)
  expect_true(result$lower_ci <= result$upper_ci)
  expect_equal(result$n_boot, 100)
  expect_equal(result$n_failed, 0)
})


test_that("invalid probability values are rejected", {

  truth <- factor(c(rep("A", 8), rep("B", 6), rep("C", 6)))

  probabilities <- matrix(
    1 / 3,
    nrow = 20,
    ncol = 3,
    dimnames = list(NULL, c("A", "B", "C"))
  )

  probabilities[1, 1] <- 1.2

  expect_error(
    mc_auc_ci(truth, probabilities, n_boot = 100),
    "Probability values must be between 0 and 1."
  )
})


test_that("invalid n_boot is rejected", {

  truth <- factor(c(rep("A", 8), rep("B", 6), rep("C", 6)))

  probabilities <- matrix(
    1 / 3,
    nrow = 20,
    ncol = 3,
    dimnames = list(NULL, c("A", "B", "C"))
  )

  expect_error(
    mc_auc_ci(truth, probabilities, n_boot = 0),
    "n_boot must be a positive integer."
  )
})


test_that("invalid confidence level is rejected", {

  truth <- factor(c(rep("A", 8), rep("B", 6), rep("C", 6)))

  probabilities <- matrix(
    1 / 3,
    nrow = 20,
    ncol = 3,
    dimnames = list(NULL, c("A", "B", "C"))
  )

  expect_error(
    mc_auc_ci(truth, probabilities, conf_level = 1.2),
    "conf_level must be between 0 and 1."
  )
})


test_that("row mismatch is rejected", {

  truth <- factor(c(rep("A", 8), rep("B", 6), rep("C", 6)))

  probabilities <- matrix(
    1 / 3,
    nrow = 19,
    ncol = 3,
    dimnames = list(NULL, c("A", "B", "C"))
  )

  expect_error(
    mc_auc_ci(truth, probabilities),
    "The number of class labels must match the number of probability rows."
  )
})


test_that("missing values are rejected", {

  truth <- factor(c(rep("A", 8), rep("B", 6), rep("C", 6)))

  probabilities <- matrix(
    1 / 3,
    nrow = 20,
    ncol = 3,
    dimnames = list(NULL, c("A", "B", "C"))
  )

  probabilities[1, 1] <- NA

  expect_error(
    mc_auc_ci(truth, probabilities),
    "Missing values detected."
  )
})


test_that("wrong probability column names are rejected", {

  truth <- factor(c(rep("A", 8), rep("B", 6), rep("C", 6)))

  probabilities <- matrix(
    1 / 3,
    nrow = 20,
    ncol = 3,
    dimnames = list(NULL, c("A", "B", "D"))
  )

  expect_error(
    mc_auc_ci(truth, probabilities),
    "Probability column names must match"
  )
})


test_that("same seed gives reproducible bootstrap results", {

  truth <- factor(c(
    rep("A", 8),
    rep("B", 6),
    rep("C", 6)
  ))

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

  result1 <- mc_auc_ci(
    truth,
    probabilities,
    n_boot = 100,
    seed = 42
  )

  result2 <- mc_auc_ci(
    truth,
    probabilities,
    n_boot = 100,
    seed = 42
  )

  expect_equal(
    result1$bootstrap_values,
    result2$bootstrap_values
  )
})
