# mc_roc_helpers.R -- small shared tools for the functions that receive an
# mc_roc object. None are exported; call them from inside the package.
#
#   mc_data(x)            complete-case truth + probs (aligned, ready to use)
#   mc_curves(x)          flat list of the 2 x choose(K, 2) pairwise curves,
#                         named "a|b", ready for pROC::ggroc() in plot()
#   .mc_auc_fast(t, p)    the same Hand & Till AUC as x$auc, by ranks,
#                         5-17x faster than pROC; for bootstrap loops
#   mc_boot_index(truth)  one stratified resample: row indices drawn with
#                         replacement WITHIN each class
#   mc_boot_auc(x, R)     the shared bootstrap loop. One mc_roc object for
#                         mc_auc_ci(), a list of two for mc_auc_compare()
#                         (same rows for both models = paired)
#
# The caller sets the seed; these helpers never call set.seed().

#' Complete-case data of an mc_roc object
#' @param x An `mc_roc` object.
#' @return list(truth = factor, probs = numeric matrix, columns in class order)
#' @noRd
mc_data <- function(x) {
  .mc_stop_if_not_fit(x)
  list(truth = x$truth[x$complete],
       probs = x$probs[x$complete, , drop = FALSE])
}

#' Pairwise ROC curves as one flat, named list
#'
#' pROC's ggroc() has no method for multiclass objects, and flattening
#' `x$roc$rocs` by hand gives names like "a vs b.a|b". This returns the
#' curves named "a|b" (ranks by P(a), a vs b) and "b|a", so
#' `pROC::ggroc(mc_curves(fit))` labels every line properly.
#' @noRd
mc_curves <- function(x) {
  .mc_stop_if_not_fit(x)
  do.call(c, unname(x$roc$rocs))
}

#' Hand & Till AUC by ranks (Mann-Whitney form)
#'
#' Identical to pROC's value (ties count 1/2), which the tests check on
#' random data with heavy ties. Use it inside bootstrap loops; use mc_roc()
#' (pROC) for the reported estimate.
#'
#' @param truth factor, no NA, every level observed.
#' @param probs numeric matrix, no NA, columns in levels(truth) order.
#' @return numeric(1)
#' @noRd
.mc_auc_fast <- function(truth, probs) {
  .mc_require_all_classes(truth)
  idx <- split(seq_along(truth), truth)
  k <- length(idx)
  pr <- utils::combn(k, 2)
  # A(i|j): probability a class-i row scores higher on P(class i) than a
  # class-j row does.
  a <- function(i, j) {
    ri <- idx[[i]]; rj <- idx[[j]]
    ni <- length(ri); nj <- length(rj)
    r <- rank(probs[c(ri, rj), i])
    (sum(r[seq_len(ni)]) - ni * (ni + 1) / 2) / (ni * nj)
  }
  a12 <- vapply(seq_len(ncol(pr)), function(p) a(pr[1, p], pr[2, p]), numeric(1))
  a21 <- vapply(seq_len(ncol(pr)), function(p) a(pr[2, p], pr[1, p]), numeric(1))
  mean((a12 + a21) / 2)
}

#' One stratified bootstrap resample
#'
#' Draws, within each class, as many rows as that class has, with
#' replacement, so no class ever disappears and class sizes stay fixed.
#' Uses sample.int(), because sample(i, ...) with a single row index i draws
#' from 1:i instead (a class with one observation would pull in other rows).
#'
#' @param truth factor of class labels (no NA).
#' @return integer vector of row indices, length(truth) long.
#' @noRd
mc_boot_index <- function(truth) {
  idx <- split(seq_along(truth), truth, drop = TRUE)
  unlist(lapply(idx, function(i) i[sample.int(length(i), length(i), replace = TRUE)]),
         use.names = FALSE)
}

#' Shared bootstrap engine
#'
#' Resamples rows within each class and recomputes the Hand & Till AUC of
#' every model on the SAME rows each time, so two models are compared paired.
#' A failed replicate is recorded as NA rather than stopping the loop.
#'
#' @param x One `mc_roc` object, or a list of them fitted on the same
#'   `truth`. With several models, only rows complete in all of them are
#'   used.
#' @param n_boot Number of bootstrap replicates.
#' @return An n_boot x M numeric matrix of bootstrap AUCs (columns named by
#'   model label, or "model1", "model2", ...), with attribute `rows_used`
#'   (the number of rows resampled).
#' @examples
#' # inside mc_auc_ci():       aucs <- mc_boot_auc(x, n_boot)[, 1]
#' # inside mc_auc_compare():  b <- mc_boot_auc(list(x1, x2), n_boot)
#' #                           diffs <- b[, 1] - b[, 2]
#' @noRd
mc_boot_auc <- function(x, n_boot = 1000) {
  fits <- if (inherits(x, "mc_roc")) list(x) else x
  if (!is.list(fits) || !length(fits) ||
      !all(vapply(fits, inherits, logical(1), what = "mc_roc"))) {
    stop("x must be an mc_roc object or a list of mc_roc objects.", call. = FALSE)
  }
  if (length(n_boot) != 1 || !is.numeric(n_boot) || n_boot < 1 ||
      n_boot != round(n_boot)) {
    stop("n_boot must be a positive whole number.", call. = FALSE)
  }

  ref <- fits[[1]]
  for (f in fits[-1]) {
    if (!identical(as.character(f$truth), as.character(ref$truth)) ||
        !identical(f$levels, ref$levels)) {
      stop("All models must be scored on the same observations, in the same ",
           "order, with the same classes.", call. = FALSE)
    }
  }
  keep <- Reduce(`&`, lapply(fits, `[[`, "complete"))
  truth <- ref$truth[keep]
  .mc_require_all_classes(truth)   # columns of probs follow these levels
  probs <- lapply(fits, function(f) f$probs[keep, , drop = FALSE])

  labels <- vapply(seq_along(fits), function(m) {
    l <- fits[[m]]$label
    if (is.null(l) || !nzchar(l)) paste0("model", m) else l
  }, character(1))

  out <- matrix(NA_real_, n_boot, length(fits),
                dimnames = list(NULL, make.unique(labels)))
  for (b in seq_len(n_boot)) {
    i <- mc_boot_index(truth)
    tb <- truth[i]
    for (m in seq_along(probs)) {
      out[b, m] <- tryCatch(.mc_auc_fast(tb, probs[[m]][i, , drop = FALSE]),
                            error = function(e) NA_real_)
    }
  }
  attr(out, "rows_used") <- length(truth)
  out
}

.mc_stop_if_not_fit <- function(x) {
  if (!inherits(x, "mc_roc")) {
    stop("x must be an object of class 'mc_roc'.", call. = FALSE)
  }
  invisible(x)
}
