# mc_roc.R -- the core object every other ProcEx function receives.
#
#   mc_roc()          user-facing: read the inputs, validate them, build the object
#   new_mc_roc()      internal constructor (assumes inputs are already clean)
#   mc_refit()        rebuild an object after the data inside it changed;
#                     handle_missing() should call this as its last step
#   .mc_fit()         the AUC via pROC: overall, pairwise table, named curves
#
# Shared helpers for the other functions live in mc_roc_helpers.R.

#' Build a multiclass ROC object
#'
#' Computes the Hand & Till (2001) multiclass AUC with
#' [pROC::multiclass.roc()] and stores everything the other ProcEx functions
#' need in a single `mc_roc` object. Every other function
#' ([handle_missing()], `mc_auc_ci()`, `mc_auc_compare()`, `plot()`) receives
#' this object.
#'
#' @details
#' The inputs are checked in two stages. `mc_roc()` first checks that it can
#' read them at all (shape, type, column names), then passes the complete rows
#' to [validate_classification_data()], which decides what stops the analysis
#' and what only warns.
#'
#' Rows with a missing label or a missing probability are kept in the object
#' but left out of the AUC, with a warning giving the count and percentage.
#' Pass the object to [handle_missing()] to remove or fill them.
#'
#' Factor levels with no observations are dropped, matching
#' [validate_classification_data()], which only counts observed classes.
#'
#' @param truth True class labels: a factor, character or integer vector of
#'   length n. Factor level order sets the class order in all output.
#' @param probs An n x K matrix or data frame of predicted probabilities, one
#'   numeric column per class, named exactly as the class labels (any column
#'   order).
#' @param label Optional model name used by `print()`, `plot()` and
#'   `mc_auc_compare()`. Defaults to the name of `probs` when it is a plain
#'   variable name.
#' @param ... Passed to [validate_classification_data()], e.g. `min_class_n`
#'   or `imbalance_ratio`.
#'
#' @return An object of class `mc_roc`; see the "Object structure" section.
#'
#' @section Object structure:
#' \describe{
#'   \item{truth}{factor, length n, all rows including incomplete ones}
#'   \item{probs}{numeric matrix, n x K, columns in `levels` order}
#'   \item{levels}{character, the K class labels}
#'   \item{complete}{logical, length n: rows used for the AUC}
#'   \item{n, n_used}{integers: total rows and rows used}
#'   \item{class_counts}{named integer: observations per class (rows used)}
#'   \item{auc}{numeric: Hand & Till multiclass AUC}
#'   \item{pairwise}{data frame, one row per class pair: pair, class1,
#'     class2, auc_1v2 = A(class1|class2), auc_2v1 = A(class2|class1), auc}
#'   \item{roc}{the pROC `mv.multiclass.roc` object; `roc$rocs` is named by
#'     pair ("a vs b") and each pair's two curves are named "a|b" and "b|a"}
#'   \item{label, call}{model label and the original call}
#'   \item{checks}{list returned by [validate_classification_data()] for the
#'     rows used}
#'   \item{missing_info}{NULL; filled by [handle_missing()]}
#'   \item{boot}{NULL; filled by `mc_auc_ci()` with its result list
#'     (`original_auc`, `standard_error`, `lower_ci`, `upper_ci`,
#'     `conf_level`, `n_boot`, `n_successful`, `n_failed`,
#'     `bootstrap_values`)}
#'   \item{pROC_version}{character, for reproducibility}
#' }
#'
#' @references Hand, D. J. & Till, R. J. (2001). A simple generalisation of the
#'   area under the ROC curve for multiple class classification problems.
#'   Machine Learning, 45, 171-186.
#'
#' @examples
#' set.seed(1)
#' truth  <- factor(sample(c("a", "b", "c"), 150, replace = TRUE))
#' scores <- matrix(stats::rnorm(450), 150, 3) +
#'   1.5 * outer(as.integer(truth), 1:3, "==")
#' probs  <- exp(scores) / rowSums(exp(scores))
#' colnames(probs) <- levels(truth)
#'
#' fit <- mc_roc(truth, probs, label = "toy model")
#' fit
#' summary(fit)
#' @export
mc_roc <- function(truth, probs, label = NULL, ...) {
  call <- match.call()
  if (is.null(label)) label <- .mc_default_label(substitute(probs))

  inp <- .mc_prepare(truth, probs)                     # can we read it?
  checks <- .mc_check(inp$truth, inp$probs, ...)       # is it good? (stop/warn)
  probs <- .mc_align(inp$truth, inp$probs)             # columns in class order

  new_mc_roc(inp$truth, probs, label = label, checks = checks, call = call)
}

#' Internal constructor. Assumes inputs are already validated: `truth` a
#' factor, `probs` a numeric matrix whose column names equal levels(truth) in
#' the same order. `boot` always starts empty: a CI from other data is invalid.
#' @noRd
new_mc_roc <- function(truth, probs, label = NULL, checks = NULL, call = NULL,
                       missing_info = NULL) {
  stopifnot(is.factor(truth), is.matrix(probs), is.numeric(probs),
            identical(colnames(probs), levels(truth)),
            nrow(probs) == length(truth))

  # pROC silently drops incomplete rows, so we do it ourselves and record it.
  complete <- .mc_complete(truth, probs)
  fit <- .mc_fit(truth[complete], probs[complete, , drop = FALSE])

  structure(
    list(
      truth        = truth,
      probs        = probs,
      levels       = levels(truth),
      complete     = complete,
      n            = length(truth),
      n_used       = sum(complete),
      class_counts = c(table(truth[complete])),
      auc          = fit$auc,
      pairwise     = fit$pairwise,
      roc          = fit$roc,
      label        = label,
      call         = call,
      checks       = checks,
      missing_info = missing_info,
      boot         = NULL,
      pROC_version = as.character(utils::packageVersion("pROC"))
    ),
    class = "mc_roc"
  )
}

#' Rebuild an mc_roc object after its data changed
#'
#' Re-runs the checks and the AUC on `x$truth` and `x$probs`. Keeps `label`,
#' `call` and `missing_info`; clears `boot`, because an interval computed on
#' the old data no longer applies. handle_missing() should end with
#' `mc_refit(x)` so the AUC matches the cleaned data.
#'
#' @param x An `mc_roc` object whose `truth` and/or `probs` were modified.
#' @param ... Passed to validate_classification_data().
#' @return The rebuilt `mc_roc` object.
#' @noRd
mc_refit <- function(x, ...) {
  if (!inherits(x, "mc_roc")) {
    stop("x must be an object of class 'mc_roc'.", call. = FALSE)
  }
  inp <- .mc_prepare(x$truth, x$probs)
  checks <- .mc_check(inp$truth, inp$probs, ...)
  probs <- .mc_align(inp$truth, inp$probs)
  new_mc_roc(inp$truth, probs, label = x$label, checks = checks,
             call = x$call, missing_info = x$missing_info)
}

#' The AUC via pROC. Takes complete-case, aligned data and returns the
#' multiclass AUC, the pairwise table and the pROC object with named curves.
#' @noRd
.mc_fit <- function(truth, probs) {
  .mc_require_all_classes(truth)
  lv <- levels(truth)
  pairs <- utils::combn(lv, 2)

  # direction ">" is pROC's default for probability tables; pinned explicitly
  # so a bad model can score below 0.5 (no auto-flipping).
  # Note: pROC takes the class order from the COLUMN order of `probs` and
  # ignores `levels=`, which is why .mc_align() must run first.
  roc <- pROC::multiclass.roc(truth, probs, levels = lv,
                              direction = ">", percent = FALSE)

  # Guard against pROC changing its pair order in a future version.
  if (!is.null(names(roc$rocs)) &&
      !identical(names(roc$rocs), paste(pairs[1, ], pairs[2, ], sep = "/"))) {
    stop("pROC returned class pairs in an unexpected order.", call. = FALSE)
  }

  pair_names <- paste(pairs[1, ], "vs", pairs[2, ])
  names(roc$rocs) <- pair_names
  for (p in seq_along(roc$rocs)) {
    # Curve 1 ranks by P(class1), curve 2 by P(class2).
    names(roc$rocs[[p]]) <- c(paste0(pairs[1, p], "|", pairs[2, p]),
                              paste0(pairs[2, p], "|", pairs[1, p]))
  }

  a12 <- vapply(roc$rocs, function(r) as.numeric(pROC::auc(r[[1]])), numeric(1))
  a21 <- vapply(roc$rocs, function(r) as.numeric(pROC::auc(r[[2]])), numeric(1))

  pairwise <- data.frame(
    pair = pair_names, class1 = pairs[1, ], class2 = pairs[2, ],
    auc_1v2 = unname(a12), auc_2v1 = unname(a21),
    auc = unname((a12 + a21) / 2),
    stringsAsFactors = FALSE
  )

  list(auc = as.numeric(roc$auc), pairwise = pairwise, roc = roc)
}

# ---- input handling ---------------------------------------------------------

.mc_complete <- function(truth, probs) {
  !is.na(truth) & stats::complete.cases(probs)
}

# Shape and type only: what mc_roc() needs to be able to read the inputs.
# Content rules (>= 3 classes, names match, [0, 1], sums, small classes,
# imbalance) belong to validate_classification_data().
.mc_prepare <- function(truth, probs) {
  if (is.data.frame(truth)) {
    if (ncol(truth) != 1L) {
      stop("`truth` must be a single vector of class labels.", call. = FALSE)
    }
    truth <- truth[[1L]]
  }
  if (!is.atomic(truth) || !is.null(dim(truth)) || length(truth) == 0L) {
    stop("`truth` must be a non-empty vector of class labels.", call. = FALSE)
  }
  # Only observed classes count, as in validate_classification_data().
  truth <- droplevels(factor(truth))

  if (!is.matrix(probs) && !is.data.frame(probs)) {
    stop("`probs` must be a matrix or data frame of class probabilities, ",
         "one column per class.", call. = FALSE)
  }
  if (is.data.frame(probs)) {
    bad <- names(probs)[!vapply(probs, is.numeric, logical(1))]
    if (length(bad)) {
      stop("These `probs` columns are not numeric: ",
           paste(bad, collapse = ", "), ".", call. = FALSE)
    }
    probs <- as.matrix(probs)
  }
  if (!is.numeric(probs)) {
    stop("`probs` must contain numeric probabilities.", call. = FALSE)
  }
  storage.mode(probs) <- "double"

  if (nrow(probs) != length(truth)) {
    stop(sprintf("`truth` has %d labels but `probs` has %d rows.",
                 length(truth), nrow(probs)), call. = FALSE)
  }
  cn <- colnames(probs)
  if (is.null(cn) || anyNA(cn) || any(cn == "")) {
    stop("`probs` needs column names equal to the class labels.",
         call. = FALSE)
  }
  if (anyDuplicated(cn)) {
    stop("Duplicate column names in `probs`: ",
         paste(unique(cn[duplicated(cn)]), collapse = ", "), ".", call. = FALSE)
  }
  list(truth = truth, probs = probs)
}

# Runs validate_classification_data() on the complete rows, so missing values
# are reported here (count + percentage) and left for handle_missing(), rather
# than stopping the analysis. Stops once, listing every error; warns for each
# validator warning. Returns the validator's list.
.mc_check <- function(truth, probs, ...) {
  complete <- .mc_complete(truth, probs)
  n <- length(truth)
  n_missing <- n - sum(complete)

  if (!any(complete)) {
    stop("Every row has a missing label or probability, so there is nothing ",
         "to analyse.", call. = FALSE)
  }
  lost <- setdiff(levels(truth), unique(as.character(truth[complete])))
  if (length(lost)) {
    stop("No complete rows for class(es): ", paste(lost, collapse = ", "),
         ". Fill the missing probabilities before calling mc_roc().",
         call. = FALSE)
  }

  checks <- validate_classification_data(truth[complete],
                                         probs[complete, , drop = FALSE], ...)
  if (!isTRUE(checks$valid)) {
    stop("Data checks failed:\n",
         paste0("  - ", checks$errors, collapse = "\n"), call. = FALSE)
  }
  for (w in checks$warnings) warning(w, call. = FALSE)

  if (n_missing > 0) {
    warning(sprintf(paste0(
      "%d of %d rows (%.1f%%) have missing values and were left out of the ",
      "AUC. Use handle_missing() to remove or fill them."),
      n_missing, n, 100 * n_missing / n), call. = FALSE)
  }
  checks
}

# Safety net: the validator should already have stopped on a mismatch, but
# reordering columns is impossible without it, so fail clearly here too.
.mc_align <- function(truth, probs) {
  lv <- levels(truth)
  no_col <- setdiff(lv, colnames(probs))
  no_class <- setdiff(colnames(probs), lv)
  if (length(no_col) || length(no_class)) {
    stop("Class labels and `probs` column names do not match.",
         if (length(no_col)) paste0(" No column for: ", paste(no_col, collapse = ", "), "."),
         if (length(no_class)) paste0(" No class for column: ", paste(no_class, collapse = ", "), "."),
         call. = FALSE)
  }
  probs[, lv, drop = FALSE]
}

.mc_require_all_classes <- function(truth) {
  counts <- tabulate(truth, nbins = nlevels(truth))
  if (any(counts == 0L)) {
    stop("No observations for class(es): ",
         paste(levels(truth)[counts == 0L], collapse = ", "),
         ". The multiclass AUC needs every class. Inside a bootstrap, ",
         "resample within each class (see mc_boot_index()).", call. = FALSE)
  }
  invisible(counts)
}

.mc_default_label <- function(expr) {
  if (is.name(expr)) as.character(expr) else NULL
}

# ---- print and summary ------------------------------------------------------

.mc_fmt <- function(x, digits) formatC(x, format = "f", digits = digits)

.mc_fmt_classes <- function(lv, max = 6L) {
  if (length(lv) <= max) return(paste(lv, collapse = ", "))
  paste0(paste(lv[seq_len(max)], collapse = ", "), ", ... (+",
         length(lv) - max, " more)")
}

# Reads the result list of mc_auc_ci() stored in x$boot.
.mc_fmt_ci <- function(boot, digits) {
  if (is.null(boot) || is.null(boot$lower_ci)) {
    return("  (no CI yet: run mc_auc_ci())")
  }
  lvl <- if (is.null(boot$conf_level)) "" else
    paste0(round(100 * boot$conf_level), "% ")
  sprintf("  %sCI [%s, %s]", lvl, .mc_fmt(boot$lower_ci, digits),
          .mc_fmt(boot$upper_ci, digits))
}

.mc_fmt_checks <- function(checks) {
  if (is.null(checks)) return("not run")
  nw <- length(checks$warnings)
  if (nw == 0) return(checks$status)
  sprintf("%s (%d warning%s, see summary())", checks$status, nw,
          if (nw == 1) "" else "s")
}

#' @rdname mc_roc
#' @param x An `mc_roc` object (for `print()`), or a `summary.mc_roc` object.
#' @param digits Number of decimal places to show.
#' @export
print.mc_roc <- function(x, digits = 3, ...) {
  cat("Multiclass ROC", if (!is.null(x$label)) paste0(": ", x$label), "\n", sep = "")
  cat("  Method       : Hand & Till (2001) multiclass AUC, via pROC ",
      x$pROC_version, "\n", sep = "")
  cat(sprintf("  Classes      : %d (%s)\n", length(x$levels),
              .mc_fmt_classes(x$levels)))
  n_out <- x$n - x$n_used
  obs <- if (n_out > 0) {
    sprintf("%d of %d (%d rows, %.1f%%, with missing values left out)",
            x$n_used, x$n, n_out, 100 * n_out / x$n)
  } else {
    as.character(x$n)
  }
  cat("  Observations : ", obs, "\n", sep = "")
  if (!is.null(x$missing_info) && isTRUE(x$missing_info$n_affected > 0)) {
    cat(sprintf("  Missing data : %d rows (%.1f%%) handled by handle_missing(\"%s\")\n",
                as.integer(x$missing_info$n_affected),
                x$missing_info$pct_affected, x$missing_info$method))
  }
  cat("  AUC          : ", .mc_fmt(x$auc, digits), .mc_fmt_ci(x$boot, digits),
      "\n", sep = "")
  pw <- x$pairwise
  lo <- which.min(pw$auc); hi <- which.max(pw$auc)
  cat(sprintf("  Pairwise AUC : %d pairs, lowest %s (%s), highest %s (%s)\n",
              nrow(pw), .mc_fmt(pw$auc[lo], digits), pw$pair[lo],
              .mc_fmt(pw$auc[hi], digits), pw$pair[hi]))
  cat("  Data checks  : ", .mc_fmt_checks(x$checks), "\n", sep = "")
  cat("\nsummary() shows class counts, every pairwise AUC and the data checks.\n")
  invisible(x)
}

#' @rdname mc_roc
#' @param object An `mc_roc` object.
#' @export
summary.mc_roc <- function(object, ...) {
  counts <- object$class_counts
  out <- list(
    label    = object$label,
    auc      = object$auc,
    n        = object$n,
    n_used   = object$n_used,
    classes  = data.frame(class = names(counts), n = as.integer(counts),
                          percent = 100 * as.numeric(counts) / sum(counts),
                          stringsAsFactors = FALSE),
    imbalance_ratio = max(counts) / min(counts),
    pairwise = object$pairwise,
    checks   = object$checks,
    missing_info = object$missing_info,
    boot     = object$boot
  )
  class(out) <- "summary.mc_roc"
  out
}

#' @rdname mc_roc
#' @export
print.summary.mc_roc <- function(x, digits = 3, ...) {
  cat("Multiclass ROC summary", if (!is.null(x$label)) paste0(": ", x$label),
      "\n\n", sep = "")
  cat("Multiclass AUC (Hand & Till): ", .mc_fmt(x$auc, digits),
      .mc_fmt_ci(x$boot, digits), "\n", sep = "")
  b <- x$boot
  if (!is.null(b) && !is.null(b$standard_error)) {
    cat(sprintf("Bootstrap: SE %s, %d successful / %d failed replicates\n",
                .mc_fmt(b$standard_error, digits),
                as.integer(b$n_successful), as.integer(b$n_failed)))
  }
  n_out <- x$n - x$n_used
  cat(sprintf("Observations used: %d of %d", x$n_used, x$n),
      if (n_out > 0) sprintf(" (%.1f%% left out for missing values)",
                             100 * n_out / x$n),
      "\n", sep = "")
  mi <- x$missing_info
  if (!is.null(mi) && isTRUE(mi$n_affected > 0)) {
    cat(sprintf(paste0("Missing data: %d rows (%.1f%%) handled by ",
                       "handle_missing(\"%s\"): %d removed, %d imputed\n"),
                as.integer(mi$n_affected), mi$pct_affected, mi$method,
                as.integer(mi$n_removed), as.integer(mi$n_imputed)))
  }

  cat(sprintf("\nClass counts (largest / smallest = %.2f):\n", x$imbalance_ratio))
  cl <- x$classes
  cl$percent <- .mc_fmt(cl$percent, 1)
  print(cl, row.names = FALSE)

  cat("\nPairwise AUCs  (A(i|j) ranks by P(class i); pair AUC = mean of both)\n")
  pw <- x$pairwise
  tab <- data.frame(pair = pw$pair,
                    `A(i|j)` = .mc_fmt(pw$auc_1v2, digits),
                    `A(j|i)` = .mc_fmt(pw$auc_2v1, digits),
                    AUC = .mc_fmt(pw$auc, digits),
                    check.names = FALSE)
  print(tab, row.names = FALSE)

  ck <- x$checks
  if (!is.null(ck)) {
    cat("\nData checks (validate_classification_data): ", ck$status, "\n",
        sep = "")
    # Forward compatible: if the validator adds a summary table, show it.
    if (is.data.frame(ck$table)) print(ck$table, row.names = FALSE)
    for (w in ck$warnings) cat("  - warning: ", w, "\n", sep = "")
  }
  invisible(x)
}
