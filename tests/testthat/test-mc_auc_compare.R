# Tests for mc_auc_compare().

#   signal   how separable the classes are (0 = no information)
#   weights  class proportions, e.g. c(0.7, 0.2, 0.1) for imbalance
sim_mc <- function(n = 300, k = 3, signal = 1.5, seed = 1,
                   weights = rep(1, k)) {
  set.seed(seed)
  y <- factor(sample(letters[seq_len(k)], n, replace = TRUE, prob = weights),
              levels = letters[seq_len(k)])
  z <- matrix(stats::rnorm(n * k), n, k)
  idx <- cbind(seq_len(n), as.integer(y))
  z[idx] <- z[idx] + signal
  p <- exp(z) / rowSums(exp(z))
  colnames(p) <- levels(y)
  list(y = y, p = p)
}

# Independent Hand & Till (2001) implementation, written from the paper.
# A(i|j) = P(a class-i row scores higher on P(class i) than a class-j row).
ht_A <- function(y, p, i, j) {
  si <- p[y == i, i]; sj <- p[y == j, i]
  mean(outer(si, sj, ">") + 0.5 * outer(si, sj, "=="))
}
hand_till <- function(y, p) {
  pr <- utils::combn(levels(y), 2)
  mean(apply(pr, 2, function(q) (ht_A(y, p, q[1], q[2]) + ht_A(y, p, q[2], q[1])) / 2))
}


# Tests for mc_auc_compare()
# Argument names: truth, probs1, probs2, levels, n_boot, conf_level, seed.
# sim_mc() and hand_till() come from helper-sim.R.

# Two iris models: "strong" uses the petals, "weak" uses the sepals.
make_iris_pair <- function() {
  set.seed(1)
  strong <- nnet::multinom(Species ~ Petal.Length + Petal.Width,
                           data = iris, trace = FALSE)
  weak   <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
                           data = iris, trace = FALSE)
  list(
    truth  = iris$Species,
    strong = stats::predict(strong, type = "probs"),
    weak   = stats::predict(weak,   type = "probs")
  )
}



test_that("returns a well-formed mc_compare object", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 42)

  expect_s3_class(res, "mc_compare")
  expect_named(res, c("auc1", "auc2", "difference", "lower", "upper", "se",
                      "p_value", "conf_level", "n_boot", "n_failed",
                      "replicates"))
  expect_equal(res$n_boot + res$n_failed, 100)
  expect_length(res$replicates, res$n_boot)
  expect_lte(res$lower, res$upper)
  expect_true(res$auc1 >= 0 && res$auc1 <= 1)
  expect_true(res$auc2 >= 0 && res$auc2 <= 1)
  expect_equal(res$difference, res$auc1 - res$auc2)
  expect_equal(res$se, stats::sd(res$replicates))
  expect_true(res$p_value >= 0 && res$p_value <= 1)
  expect_equal(res$conf_level, 0.95)
})

test_that("each AUC matches pROC and an independent Hand & Till calculation", {
  d <- sim_mc(n = 200, k = 4, signal = 1.5, seed = 3)
  e <- sim_mc(n = 200, k = 4, signal = 0.5, seed = 4)
  res <- mc_auc_compare(d$y, d$p, e$p, n_boot = 20, seed = 1)

  expect_equal(res$auc1,
               as.numeric(suppressMessages(pROC::multiclass.roc(d$y, d$p))$auc))
  expect_equal(res$auc2,
               as.numeric(suppressMessages(pROC::multiclass.roc(d$y, e$p))$auc))
  expect_equal(res$auc1, hand_till(d$y, d$p))
  expect_equal(res$auc2, hand_till(d$y, e$p))
})


test_that("comparing a model with itself gives exactly zero", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$strong, n_boot = 100, seed = 1)

  expect_true(all(res$replicates == 0))
  expect_equal(res$difference, 0)
  expect_equal(res$lower, 0)
  expect_equal(res$upper, 0)
  expect_equal(res$se, 0)
  expect_equal(res$p_value, 1)
})

test_that("swapping the models flips the sign (the bootstrap is paired)", {
  d <- make_iris_pair()
  a <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 5)
  b <- mc_auc_compare(d$truth, d$weak, d$strong, n_boot = 100, seed = 5)

  expect_equal(b$difference, -a$difference)
  expect_equal(b$replicates, -a$replicates)
  expect_equal(b$lower, -a$upper)
  expect_equal(b$upper, -a$lower)
  expect_equal(b$p_value, a$p_value)
})

test_that("the same seed reproduces the result; a different seed does not", {
  d <- make_iris_pair()
  a <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 99)
  b <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 99)
  c <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 100)

  expect_identical(a$replicates, b$replicates)
  expect_identical(a$lower, b$lower)
  expect_false(identical(a$replicates, c$replicates))
})

test_that("a better model is detected", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 200, seed = 42)

  expect_gt(res$difference, 0)
  expect_gt(res$lower, 0)
  expect_lt(res$p_value, 0.05)
})

test_that("two equally useless models are not called different", {
  y  <- sim_mc(n = 300, k = 4, signal = 2, seed = 1)$y
  p1 <- sim_mc(n = 300, k = 4, signal = 0, seed = 5)$p   # pure noise
  p2 <- sim_mc(n = 300, k = 4, signal = 0, seed = 6)$p   # pure noise
  res <- mc_auc_compare(y, p1, p2, n_boot = 200, seed = 11)

  expect_lt(res$lower, 0)
  expect_gt(res$upper, 0)
  expect_gt(res$p_value, 0.05)
})

test_that("a higher conf_level gives a wider interval", {
  d <- make_iris_pair()
  narrow <- mc_auc_compare(d$truth, d$strong, d$weak,
                           n_boot = 200, conf_level = 0.80, seed = 7)
  wide   <- mc_auc_compare(d$truth, d$strong, d$weak,
                           n_boot = 200, conf_level = 0.99, seed = 7)

  expect_gt(wide$upper - wide$lower, narrow$upper - narrow$lower)
  expect_equal(wide$conf_level, 0.99)
})

test_that("a class with a single row stays in every bootstrap sample", {
  # Regression test for the sample() pitfall: sample(i, 1) with a single
  # index i draws from 1:i, so the one-row class vanished from the replicate
  # and pROC warned "No observation for response level(s): d".
  d <- sim_mc(n = 120, k = 4, signal = 2, seed = 1)
  e <- sim_mc(n = 120, k = 4, signal = 1, seed = 1)
  y <- d$y
  y[y == "d"] <- "c"
  y[60] <- "d"                                   # class d has exactly 1 row

  expect_no_warning(
    res <- mc_auc_compare(y, d$p, e$p, n_boot = 50, seed = 1)
  )
  expect_equal(res$n_failed, 0)
})


# Accepted input formats

test_that("data frames, character labels and column order give the same answer", {
  d <- make_iris_pair()
  ref <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 50, seed = 3)

  as_df <- mc_auc_compare(d$truth, as.data.frame(d$strong),
                          as.data.frame(d$weak), n_boot = 50, seed = 3)
  as_chr <- mc_auc_compare(as.character(d$truth), d$strong, d$weak,
                           n_boot = 50, seed = 3)
  shuffled <- mc_auc_compare(d$truth, d$strong, d$weak[, c(3, 1, 2)],
                             n_boot = 50, seed = 3)
  given_levels <- mc_auc_compare(d$truth, d$strong, d$weak,
                                 levels = levels(d$truth),
                                 n_boot = 50, seed = 3)

  expect_equal(as_df$replicates, ref$replicates)
  expect_equal(as_chr$replicates, ref$replicates)
  expect_equal(shuffled$auc2, ref$auc2)
  expect_equal(shuffled$replicates, ref$replicates)
  expect_identical(given_levels$replicates, ref$replicates)
})

test_that("mc_roc objects can be passed in place of truth and probabilities", {
  d <- make_iris_pair()
  fit_strong <- mc_roc(d$truth, d$strong)
  fit_weak   <- mc_roc(d$truth, d$weak)
  ref <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 50, seed = 8)

  two_fits   <- mc_auc_compare(fit_strong, fit_weak, n_boot = 50, seed = 8)
  fit_matrix <- mc_auc_compare(fit_strong, d$weak, n_boot = 50, seed = 8)
  named_arg  <- mc_auc_compare(fit_strong, probs2 = fit_weak,
                               n_boot = 50, seed = 8)
  fit_second <- mc_auc_compare(d$truth, d$strong, fit_weak,
                               n_boot = 50, seed = 8)

  for (res in list(two_fits, fit_matrix, named_arg, fit_second)) {
    expect_equal(res$auc1, ref$auc1)
    expect_equal(res$auc2, ref$auc2)
    expect_identical(res$replicates, ref$replicates)
  }
  expect_equal(two_fits$auc1, fit_strong$auc)
  expect_equal(two_fits$auc2, fit_weak$auc)
})


# Clear message for bad input

test_that("fewer than three classes is rejected", {
  two   <- iris$Species != "virginica"
  truth <- droplevels(iris$Species[two])
  probs <- matrix(stats::runif(sum(two) * 2), ncol = 2,
                  dimnames = list(NULL, levels(truth)))

  expect_error(mc_auc_compare(truth, probs, probs), "at least 3 classes",
               ignore.case = TRUE)
})

test_that("mismatched row counts are rejected", {
  d <- make_iris_pair()

  expect_error(mc_auc_compare(d$truth, d$strong[1:140, ], d$weak),
               "probs1 has 140 rows but truth has 150 values")
  expect_error(mc_auc_compare(d$truth, d$strong, d$weak[1:140, ]),
               "probs2 has 140 rows but truth has 150 values")
})

test_that("probabilities that are not a matrix or data frame are rejected", {
  d <- make_iris_pair()

  expect_error(mc_auc_compare(d$truth, "not a matrix", d$weak),
               "probs1 must be a matrix or data frame")
  expect_error(mc_auc_compare(d$truth, d$strong, 1:150),
               "probs2 must be a matrix or data frame")
})

test_that("non-numeric probabilities are rejected", {
  d <- make_iris_pair()
  text <- matrix(as.character(d$weak), ncol = 3,
                 dimnames = list(NULL, colnames(d$weak)))

  expect_error(mc_auc_compare(d$truth, d$strong, text),
               "must contain numeric values")
})

test_that("invalid n_boot values are rejected", {
  d <- make_iris_pair()
  for (bad in list(0, -5, 2.5, "100", c(10, 20), NA_real_)) {
    expect_error(
      mc_auc_compare(d$truth, d$strong, d$weak, n_boot = bad),
      "n_boot must be a single positive integer",
      info = paste("n_boot =", deparse(bad))
    )
  }
})

test_that("invalid conf_level values are rejected", {
  d <- make_iris_pair()
  for (bad in list(0, 1, 1.5, -0.1, NA_real_, c(0.9, 0.95), "0.95")) {
    expect_error(
      mc_auc_compare(d$truth, d$strong, d$weak, conf_level = bad),
      "conf_level must be a single value between 0 and 1",
      info = paste("conf_level =", deparse(bad))
    )
  }
})


#  Failed bootstrap replicates

test_that("failed replicates are counted and warned about", {
  d <- make_iris_pair()
  real <- pROC::multiclass.roc
  calls <- 0
  # Let the two observed AUCs through, then fail every 4th call.
  testthat::local_mocked_bindings(
    multiclass.roc = function(...) {
      calls <<- calls + 1
      if (calls > 2 && calls %% 4 == 0) stop("simulated failure")
      real(...)
    },
    .package = "pROC"
  )

  expect_warning(
    res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 40, seed = 1),
    "bootstrap replicates failed"
  )
  expect_gt(res$n_failed, 0)
  expect_equal(res$n_boot + res$n_failed, 40)
  expect_length(res$replicates, res$n_boot)
})

test_that("it stops when almost every replicate fails", {
  d <- make_iris_pair()
  real <- pROC::multiclass.roc
  calls <- 0
  testthat::local_mocked_bindings(
    multiclass.roc = function(...) {
      calls <<- calls + 1
      if (calls > 2) stop("simulated failure")
      real(...)
    },
    .package = "pROC"
  )

  expect_error(mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 10, seed = 1),
               "Too few successful bootstrap replicates")
})



test_that("print() shows the results and returns its input invisibly", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 42)

  expect_output(print(res), "Multiclass AUC comparison")
  expect_output(print(res), "Model 1 AUC")
  expect_output(print(res), "95% CI", fixed = TRUE)
  expect_output(print(res), "100 successful, 0 failed", fixed = TRUE)

  utils::capture.output(shown <- withVisible(print(res)))
  expect_false(shown$visible)
  expect_identical(shown$value, res)
})

test_that("print() shows a tiny p-value as '< 1/n_boot', not 0.0000", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 42)
  expect_equal(res$p_value, 0)
  expect_output(print(res), "p-value      : < 0.0100", fixed = TRUE)

  same <- mc_auc_compare(d$truth, d$strong, d$strong, n_boot = 100, seed = 1)
  expect_output(print(same), "p-value      : 1.0000", fixed = TRUE)
})
