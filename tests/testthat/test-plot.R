# Tests for plot.mc_roc(), plot.mc_auc_ci(), plot.mc_auc_compare() and the
# per-pair AUCs that mc_auc_compare() stores for plotting. sim_mc() comes
# from helper-sim.R.

# Draw to a throwaway device so tests leave no files behind.
draw <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

iris_models <- function() {
  set.seed(1)
  strong <- nnet::multinom(Species ~ Petal.Length + Petal.Width,
                           data = iris, trace = FALSE)
  weak   <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
                           data = iris, trace = FALSE)
  list(truth  = iris$Species,
       strong = stats::predict(strong, type = "probs"),
       weak   = stats::predict(weak,   type = "probs"))
}

# plot.mc_roc

test_that("plot.mc_roc draws both types and returns the object invisibly", {
  d <- sim_mc(n = 300, k = 4, signal = 1.5, seed = 2)
  fit <- mc_roc(d$y, d$p)

  draw(expect_invisible(plot(fit)))
  draw(expect_identical(plot(fit), fit))
  draw(expect_invisible(plot(fit, type = "heatmap")))
  draw(expect_invisible(plot(fit, type = "heatmap", scale = "full")))
})

test_that("plot.mc_roc restores the user's graphics settings", {
  d <- sim_mc(n = 300, k = 4, signal = 1.5, seed = 2)
  fit <- mc_roc(d$y, d$p)

  draw({
    graphics::par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
    before <- graphics::par(c("mfrow", "mar", "oma", "pty"))
    plot(fit)
    expect_identical(graphics::par(c("mfrow", "mar", "oma", "pty")), before)
    plot(fit, type = "heatmap")
    expect_identical(graphics::par(c("mfrow", "mar", "oma", "pty")), before)
  })
})

test_that("pairs can be chosen by name or row number", {
  d <- sim_mc(n = 300, k = 4, signal = 1.5, seed = 2)
  fit <- mc_roc(d$y, d$p)

  expect_identical(.mc_select_pairs(fit$pairwise, c("a vs c", "b vs d")),
                   match(c("a vs c", "b vs d"), fit$pairwise$pair))
  expect_identical(.mc_select_pairs(fit$pairwise, c(2, 5)), c(2L, 5L))
  expect_identical(.mc_select_pairs(fit$pairwise, NULL),
                   seq_len(nrow(fit$pairwise)))
  draw(expect_invisible(plot(fit, pairs = "a vs b")))
})

test_that("unknown pairs give a helpful error", {
  d <- sim_mc(n = 300, k = 4, signal = 1.5, seed = 2)
  fit <- mc_roc(d$y, d$p)

  draw(expect_error(plot(fit, pairs = "a vs z"), "Unknown pair.*Available"))
  draw(expect_error(plot(fit, pairs = 99), "between 1 and 6"))
  draw(expect_error(plot(fit, pairs = list(1)), "pairs must be"))
})

test_that("more than 25 pairs shows the 25 weakest, with a message", {
  d <- sim_mc(n = 900, k = 9, signal = 1, seed = 3)   # 36 pairs
  fit <- mc_roc(d$y, d$p)

  expect_message(idx <- .mc_select_pairs(fit$pairwise, NULL),
                 "25 lowest-AUC of 36")
  expect_length(idx, 25)
  kept    <- fit$pairwise$auc[idx]
  dropped <- fit$pairwise$auc[-idx]
  expect_true(max(kept) <= min(dropped))
  draw(expect_message(plot(fit), "lowest-AUC"))
})

test_that("bad arguments are rejected", {
  d <- sim_mc(n = 300, k = 3, signal = 1.5, seed = 2)
  fit <- mc_roc(d$y, d$p)

  draw(expect_error(plot(fit, type = "curves"), "type must be one of"))
  draw(expect_error(plot(fit, type = "heatmap", scale = "log"),
                    "scale must be one of"))
  draw(expect_warning(plot(fit, type = "heatmap", pairs = 1),
                      "pairs is ignored"))
})

test_that("a model worse than chance can be plotted", {
  m <- iris_models()
  bad <- m$weak[, c("versicolor", "virginica", "setosa")]
  colnames(bad) <- colnames(m$weak)
  fit <- mc_roc(m$truth, bad)
  expect_lt(fit$auc, 0.5)

  draw(expect_invisible(plot(fit)))
  draw(expect_invisible(plot(fit, type = "heatmap")))
})

test_that("AUC colours run red below chance, grey at 0.5, blue toward 1", {
  ramp <- .mc_auc_colours(101)
  expect_length(ramp, 101)
  rgb <- grDevices::col2rgb(ramp[c(1, 51, 101)])
  expect_gt(rgb["red", 1], rgb["blue", 1])     # 0: red
  expect_equal(toupper(ramp[51]), "#F0EFEC")    # 0.5: neutral grey
  expect_gt(rgb["blue", 3], rgb["red", 3])     # 1: blue
})

# mc_auc_compare per-pair AUCs

test_that("mc_auc_compare stores per-pair AUCs that average to the overall AUC", {
  m <- iris_models()
  res <- mc_auc_compare(m$truth, m$strong, m$weak, n_boot = 20, seed = 1)
  pw  <- res$pairwise

  expect_s3_class(pw, "data.frame")
  expect_named(pw, c("pair", "class1", "class2", "auc1", "auc2", "difference"))
  expect_equal(nrow(pw), choose(3, 2))
  expect_equal(mean(pw$auc1), res$auc1)
  expect_equal(mean(pw$auc2), res$auc2)
  expect_equal(pw$difference, pw$auc1 - pw$auc2)
})

test_that("per-pair AUCs match mc_roc() and ignore column order", {
  d <- sim_mc(n = 300, k = 4, signal = 1.2, seed = 5)
  e <- sim_mc(n = 300, k = 4, signal = 0.6, seed = 6)
  res <- mc_auc_compare(d$y, d$p, e$p, n_boot = 10, seed = 1)

  expect_equal(res$pairwise$auc1, mc_roc(d$y, d$p)$pairwise$auc)
  expect_equal(res$pairwise$auc2, mc_roc(d$y, e$p)$pairwise$auc)

  shuffled <- mc_auc_compare(d$y, d$p[, c(3, 1, 4, 2)], e$p, n_boot = 10,
                             seed = 1)
  expect_equal(shuffled$pairwise, res$pairwise)
})

# plot.mc_auc_compare

test_that("plot.mc_auc_compare draws both types and returns invisibly", {
  m <- iris_models()
  res <- mc_auc_compare(m$truth, m$strong, m$weak, n_boot = 50, seed = 1)

  draw(expect_invisible(plot(res)))
  draw(expect_identical(plot(res), res))
  draw(expect_invisible(plot(res, type = "pairs",
                             labels = c("Petals", "Sepals"))))
})

test_that("plot.mc_auc_compare handles a model compared with itself", {
  m <- iris_models()
  res <- mc_auc_compare(m$truth, m$strong, m$strong, n_boot = 30, seed = 1)
  expect_true(all(res$bootstrap_values == 0))

  draw(expect_invisible(plot(res)))
  draw(expect_invisible(plot(res, type = "pairs")))
})

test_that("plot.mc_auc_compare restores the user's graphics settings", {
  m <- iris_models()
  res <- mc_auc_compare(m$truth, m$strong, m$weak, n_boot = 30, seed = 1)

  draw({
    before <- graphics::par(c("mfrow", "mar", "oma"))
    plot(res)
    plot(res, type = "pairs")
    expect_identical(graphics::par(c("mfrow", "mar", "oma")), before)
  })
})

test_that("plot.mc_auc_compare rejects bad input", {
  m <- iris_models()
  res <- mc_auc_compare(m$truth, m$strong, m$weak, n_boot = 30, seed = 1)

  draw(expect_error(plot(res, type = "roc"), "type must be one of"))
  draw(expect_error(plot(res, labels = "only one"), "labels must be"))

  old <- res
  old$pairwise <- NULL
  draw(expect_error(plot(old, type = "pairs"), "no per-pair AUCs"))
  draw(expect_invisible(plot(old)))   # the bootstrap view still works

  expect_error(plot.mc_auc_compare(list()), "mc_auc_compare")
})


# plot.mc_auc_ci

test_that("plot.mc_auc_ci draws the bootstrap AUCs and returns invisibly", {
  m <- iris_models()
  ci <- mc_auc_ci(m$truth, m$weak, n_boot = 50, seed = 1)

  draw(expect_invisible(plot(ci)))
  draw(expect_identical(plot(ci), ci))
  draw(expect_invisible(plot(ci, main = "Sepal model")))
})

test_that("plot.mc_auc_ci works for a binary result", {
  set.seed(3)
  y <- factor(rep(c("no", "yes"), each = 40))
  p <- c(stats::runif(40, 0, 0.6), stats::runif(40, 0.4, 1))
  ci <- mc_auc_ci(y, cbind(no = 1 - p, yes = p), n_boot = 40, seed = 1)

  draw(expect_invisible(plot(ci)))
})

test_that("plot.mc_auc_ci restores the user's graphics settings", {
  m <- iris_models()
  ci <- mc_auc_ci(m$truth, m$weak, n_boot = 30, seed = 1)

  draw({
    before <- graphics::par(c("mfrow", "mar", "oma"))
    plot(ci)
    expect_identical(graphics::par(c("mfrow", "mar", "oma")), before)
  })
})

test_that("plot.mc_auc_ci rejects bad input", {
  m <- iris_models()
  ci <- mc_auc_ci(m$truth, m$weak, n_boot = 30, seed = 1)

  expect_error(plot.mc_auc_ci(list()), "mc_auc_ci")
  one <- ci
  one$bootstrap_values <- one$bootstrap_values[1]
  draw(expect_error(plot(one), "fewer than two bootstrap values"))
})
