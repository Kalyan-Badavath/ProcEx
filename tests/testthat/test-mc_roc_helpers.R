# sim_mc() and hand_till() live in helper-sim.R

test_that(".mc_auc_fast() equals pROC's AUC, including heavy ties", {
  for (s in 1:60) {
    set.seed(1000 + s)
    k <- sample(3:7, 1)
    d <- sim_mc(n = sample(30:400, 1), k = k, signal = stats::runif(1, -1, 3),
                seed = s)
    if (s %% 3 == 0) d$p <- round(d$p, 1)          # many tied scores
    fit_auc <- .mc_fit(d$y, d$p)$auc
    expect_equal(.mc_auc_fast(d$y, d$p), fit_auc, tolerance = 1e-12)
  }
})

test_that("mc_boot_index() resamples within each class only", {
  y <- factor(c("a", "b", "b", "c", "c", "c", "a", "b", "c"))
  set.seed(1)
  for (r in 1:50) {
    i <- mc_boot_index(y)
    expect_length(i, length(y))
    expect_equal(as.vector(table(y[i])), as.vector(table(y)))
  }
})

test_that("a class with one observation is never mixed with other rows", {
  # sample(5, 1, replace = TRUE) would draw from 1:5; sample.int() avoids it
  y <- factor(c("a", "a", "a", "a", "b", "c", "c"))
  set.seed(2)
  for (r in 1:50) {
    i <- mc_boot_index(y)
    expect_true(all(i[y[i] == "b"] == 5L))
  }
})

test_that("mc_boot_auc() is paired and labels its columns", {
  d <- sim_mc()
  m1 <- mc_roc(d$y, d$p, label = "rf")
  m2 <- mc_roc(d$y, d$p, label = "rf")             # identical model
  set.seed(3)
  b <- mc_boot_auc(list(m1, m2), n_boot = 50)
  expect_equal(dim(b), c(50L, 2L))
  expect_identical(colnames(b), c("rf", "rf.1"))
  expect_true(all(b[, 1] == b[, 2]))               # same rows each replicate
  expect_equal(attr(b, "rows_used"), 300L)

  one <- mc_boot_auc(m1, n_boot = 20)
  expect_equal(dim(one), c(20L, 1L))
  expect_false(anyNA(one))
})

test_that("mc_boot_auc() is reproducible with a seed set by the caller", {
  d <- sim_mc()
  m <- mc_roc(d$y, d$p)
  set.seed(42); b1 <- mc_boot_auc(m, 30)
  set.seed(42); b2 <- mc_boot_auc(m, 30)
  expect_identical(b1, b2)
})

test_that("mc_boot_auc() uses rows complete in every model", {
  d <- sim_mc()
  p1 <- d$p; p1[1:3, "a"] <- NA
  p2 <- d$p; p2[4:5, "b"] <- NA
  m1 <- suppressWarnings(mc_roc(d$y, p1))
  m2 <- suppressWarnings(mc_roc(d$y, p2))
  b <- mc_boot_auc(list(m1, m2), n_boot = 5)
  expect_equal(attr(b, "rows_used"), 295L)
})

test_that("mc_boot_auc() refuses models scored on different data", {
  d <- sim_mc(); e <- sim_mc(seed = 99)
  expect_error(mc_boot_auc(list(mc_roc(d$y, d$p), mc_roc(e$y, e$p)), 5),
               "same observations")
  expect_error(mc_boot_auc(list(1, 2), 5), "mc_roc object")
  expect_error(mc_boot_auc(mc_roc(d$y, d$p), 0), "positive whole number")
})

test_that("a missing class gives a clear error, not a pROC pair-order error", {
  d <- sim_mc()
  keep <- d$y != "c"
  expect_error(.mc_fit(d$y[keep], d$p[keep, ]), "No observations for class\\(es\\): c")
  expect_error(.mc_auc_fast(d$y[keep], d$p[keep, ]), "No observations")
})

test_that("mc_curves() gives a flat list named for ggroc()", {
  d <- sim_mc()
  cv <- mc_curves(mc_roc(d$y, d$p))
  expect_length(cv, 6L)
  expect_identical(names(cv), c("a|b", "b|a", "a|c", "c|a", "b|c", "c|b"))
  expect_true(all(vapply(cv, inherits, logical(1), what = "roc")))
})
