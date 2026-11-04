# sim_mc(), ht_A() and hand_till() live in helper-sim.R

test_that("AUC matches an independent Hand & Till calculation", {
  for (k in 3:5) {
    d <- sim_mc(k = k, seed = k)
    fit <- mc_roc(d$y, d$p)
    expect_equal(fit$auc, hand_till(d$y, d$p))
    expect_equal(fit$auc, mean(fit$pairwise$auc))
    expect_equal(nrow(fit$pairwise), choose(k, 2))
  }
})

test_that("each one-way pairwise AUC is correct, not just their mean", {
  d <- sim_mc(k = 4, seed = 9)
  pw <- mc_roc(d$y, d$p)$pairwise
  for (r in seq_len(nrow(pw))) {
    expect_equal(pw$auc_1v2[r], ht_A(d$y, d$p, pw$class1[r], pw$class2[r]))
    expect_equal(pw$auc_2v1[r], ht_A(d$y, d$p, pw$class2[r], pw$class1[r]))
  }
})

test_that("column order, data frames, character and integer labels give the same AUC", {
  d <- sim_mc()
  ref <- mc_roc(d$y, d$p)$auc
  expect_equal(mc_roc(d$y, d$p[, c("c", "a", "b")])$auc, ref)
  expect_equal(mc_roc(d$y, as.data.frame(d$p))$auc, ref)
  expect_equal(mc_roc(as.character(d$y), d$p)$auc, ref)
  pi <- d$p; colnames(pi) <- c("1", "2", "3")
  expect_equal(mc_roc(as.integer(d$y), pi)$auc, ref)
  y2 <- factor(d$y, levels = c("c", "a", "b"))
  fit2 <- mc_roc(y2, d$p)
  expect_equal(fit2$auc, ref)
  expect_identical(fit2$levels, c("c", "a", "b"))
})

test_that("a model worse than chance scores below 0.5 (no direction flipping)", {
  d <- sim_mc()
  bad <- (1 - d$p) / rowSums(1 - d$p)          # still valid probabilities
  expect_lt(mc_roc(d$y, bad)$auc, 0.5)
})

test_that("object follows the agreed contract", {
  d <- sim_mc()
  fit <- mc_roc(d$y, d$p)
  expect_s3_class(fit, "mc_roc")
  expect_named(fit, c("truth", "probs", "levels", "complete", "n",
                      "n_used", "class_counts", "auc", "pairwise", "roc",
                      "label", "call", "checks", "missing_info", "boot",
                      "pROC_version"))
  expect_true(is.factor(fit$truth))
  expect_true(is.matrix(fit$probs))
  expect_identical(colnames(fit$probs), fit$levels)
  expect_null(fit$boot)
  expect_null(fit$missing_info)
  expect_identical(fit$checks$status, "VALID")
  expect_identical(fit$label, NULL)               # d$p is not a plain name
  probs <- d$p
  expect_identical(mc_roc(d$y, probs)$label, "probs")
  expect_identical(names(fit$roc$rocs), c("a vs b", "a vs c", "b vs c"))
  expect_identical(names(fit$roc$rocs[["a vs c"]]), c("a|c", "c|a"))
})

test_that("unused factor levels are dropped, as in validate_classification_data()", {
  d <- sim_mc()
  y <- factor(d$y, levels = c("a", "b", "c", "zzz"))
  fit <- mc_roc(y, d$p)
  expect_identical(fit$levels, c("a", "b", "c"))
  expect_equal(fit$auc, mc_roc(d$y, d$p)$auc)
})

test_that("missing rows are left out, counted with a percentage, and warned about", {
  d <- sim_mc()
  p <- d$p; y <- d$y
  p[1, 1] <- NA; y[2] <- NA
  expect_warning(fit <- mc_roc(y, p), "2 of 300 rows \\(0.7%\\) have missing values")
  expect_equal(fit$n, 300L)
  expect_equal(fit$n_used, 298L)
  expect_equal(fit$auc, hand_till(d$y[-(1:2)], d$p[-(1:2), ]))
  expect_equal(length(mc_data(fit)$truth), 298L)
  expect_output(print(fit), "2 rows, 0.7%")
})

test_that("a class with no complete rows stops with a clear message", {
  d <- sim_mc()
  p <- d$p; p[d$y == "c", "a"] <- NA
  expect_error(mc_roc(d$y, p), "No complete rows for class\\(es\\): c")
})

test_that("format problems stop with clear messages", {
  d <- sim_mc()
  expect_error(mc_roc(d$y[-1], d$p), "300 rows")
  expect_error(mc_roc(d$y, unname(d$p)), "column names")
  expect_error(mc_roc(d$y, list(1, 2)), "matrix or data frame")
  df <- as.data.frame(d$p); df$a <- as.character(df$a)
  expect_error(mc_roc(d$y, df), "not numeric: a")
})

test_that("content problems are decided by validate_classification_data()", {
  d <- sim_mc()
  two <- sim_mc(k = 2)
  expect_error(mc_roc(two$y, two$p), "At least 3 classes are required")
  extra <- cbind(d$p, d = 0)
  expect_error(mc_roc(d$y, extra), "column names must match the class labels")
  bad <- d$p; bad[1, ] <- c(1.2, -0.1, -0.1)
  expect_error(mc_roc(d$y, bad), "between 0 and 1")
  # all errors are listed together, once
  bad2 <- bad; colnames(bad2)[3] <- "x"
  err <- tryCatch(mc_roc(d$y, bad2), error = conditionMessage)
  expect_match(err, "between 0 and 1")
  expect_match(err, "must match the class labels")
})

test_that("validator warnings pass through and thresholds can be set", {
  d <- sim_mc(n = 300, weights = c(0.9, 0.08, 0.02), seed = 3)
  msgs <- character()
  fit <- withCallingHandlers(mc_roc(d$y, d$p), warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_true(any(grepl("fewer than 10", msgs)))
  expect_true(any(grepl("imbalance ratio", msgs)))
  expect_identical(fit$checks$status, "VALID_WITH_WARNINGS")
  expect_no_warning(mc_roc(d$y, d$p, min_class_n = 1, imbalance_ratio = 1000))
})

test_that("mc_refit() rebuilds after handle_missing(), keeping missing_info", {
  d <- sim_mc()
  p <- d$p; p[c(5, 50, 120), "b"] <- NA
  fit <- suppressWarnings(mc_roc(d$y, p, label = "m1"))
  fit$boot <- list(lower_ci = 0.1, upper_ci = 0.2, conf_level = 0.95)

  cleaned <- suppressWarnings(handle_missing(fit, method = "mean"))
  expect_identical(cleaned$auc, fit$auc)          # stale until refitted
  refit <- mc_refit(cleaned)

  expect_equal(refit$n, 300L)
  expect_equal(refit$n_used, 300L)
  expect_equal(refit$auc, hand_till(refit$truth, refit$probs))
  expect_identical(refit$label, "m1")
  expect_equal(refit$missing_info$n_imputed, 3)
  expect_null(refit$boot)                         # old CI no longer applies
  expect_output(print(refit), "handled by handle_missing\\(\"mean\"\\)")

  removed <- mc_refit(suppressWarnings(handle_missing(fit, method = "remove")))
  expect_equal(removed$n, 297L)
  expect_equal(removed$auc, fit$auc)              # same rows as complete-case
})

test_that("print() and summary() are readable and return invisibly", {
  d <- sim_mc()
  fit <- mc_roc(d$y, d$p, label = "rf")
  out <- capture.output(res <- print(fit))
  expect_identical(res, fit)
  expect_true(any(grepl("AUC", out)))
  expect_true(any(grepl("no CI yet", out)))
  expect_lt(length(out), 15)                       # no wall of raw data
  s <- summary(fit)
  expect_s3_class(s, "summary.mc_roc")
  expect_output(print(s), "Pairwise AUCs")
  expect_output(print(s), "Data checks")
})

test_that("print() shows the interval once mc_auc_ci() fills $boot", {
  d <- sim_mc()
  fit <- mc_roc(d$y, d$p)
  fit$boot <- list(original_auc = fit$auc, standard_error = 0.012,
                   lower_ci = 0.81, upper_ci = 0.86, conf_level = 0.95,
                   n_boot = 1000, n_successful = 1000, n_failed = 0,
                   bootstrap_values = rep(fit$auc, 1000))
  expect_output(print(fit), "95% CI \\[0.810, 0.860\\]")
  expect_output(print(summary(fit)), "1000 successful / 0 failed")
})
