#' Compares the Hand & Till multiclass AUC of two classification models
#' using a paired stratified bootstrap to quantify uncertainty in their
#' AUC difference.
#' Both models must be scored on the same observations, in the same order.
#' Each bootstrap replicate resamples observations within each class and
#' applies the SAME sampled rows to both models. This preserves the paired
#' structure because both models are evaluated on identical bootstrap samples.
#'
#' #' Hand & Till multiclass AUC values are computed using
#' [pROC::multiclass.roc()] for the observed data and each bootstrap
#' replicate, consistent with the AUC calculation used by [mc_roc()].
#'
#'
#' @param truth True class labels (factor or character), or an `mc_roc`
#'   object. At least three classes are required.
#' @param probs1 Predicted class probabilities from the first model: a
#'   numeric matrix or data frame, one row per observation and one column
#'   per class, column names matching the class labels. May also be an
#'   `mc_roc` object. When `truth` is an `mc_roc` object, this argument may
#'   instead provide the predictions or `mc_roc` object for the second model.
#' @param probs2 Predicted class probabilities from the second model, or a
#'   second `mc_roc` object, scored on the same observations in the same
#'   order as `probs1`.
#' @param levels The full set of class labels. Defaults to the levels of
#'   `truth`.
#' @param n_boot Number of bootstrap replicates. A positive whole number.
#'   Default 1000.
#' @param conf_level Confidence level for the interval, strictly between 0
#'   and 1. Default 0.95.
#' @param seed Optional integer, for a reproducible bootstrap.
#'
#' @return An object of class `mc_compare`, a list containing:
#'   \item{auc1, auc2}{the two multiclass AUCs}
#'   \item{difference}{auc1 - auc2 on the observed data}
#'   \item{lower, upper}{confidence interval for the difference}
#'   \item{se}{bootstrap standard error of the difference}
#'   \item{p_value}{two-sided bootstrap p-value}
#'   \item{conf_level}{the confidence level used}
#'   \item{n_boot, n_failed}{successful and failed replicates}
#'   \item{replicates}{the bootstrap differences}
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' fit1 <- nnet::multinom(Species ~ Petal.Length + Petal.Width,
#'                        data = iris, trace = FALSE)
#' fit2 <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
#'                        data = iris, trace = FALSE)
#' mc_auc_compare(iris$Species,
#'                predict(fit1, type = "probs"),
#'                predict(fit2, type = "probs"),
#'                n_boot = 100, seed = 42)
#' }
#'
#' @seealso [mc_auc_ci()] for one model, [mc_roc()] to build the object.
#' @export
mc_auc_compare <- function(truth,
                           probs1,
                           probs2,
                           levels     = NULL,
                           n_boot     = 1000,
                           conf_level = 0.95,
                           seed       = NULL) {

  if (!is.numeric(n_boot) || length(n_boot) != 1 || is.na(n_boot) ||
      n_boot < 1 || n_boot != round(n_boot))
    stop("n_boot must be a single positive integer.", call. = FALSE)

  if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop("conf_level must be a single value between 0 and 1.", call. = FALSE)

  # An mc_roc object may stand in for truth, probs1 or probs2. Only its
  # stored truth and probs are read.
  if (inherits(truth, "mc_roc")) {
    if (missing(probs2) && !missing(probs1)) probs2 <- probs1
    probs1 <- truth$probs
    truth  <- truth$truth
  }
  if (inherits(probs1, "mc_roc")) probs1 <- probs1$probs
  if (inherits(probs2, "mc_roc")) probs2 <- probs2$probs

  if (is.null(levels)) levels <- base::levels(as.factor(truth))

  if (!is.matrix(probs1) && !is.data.frame(probs1))
    stop("probs1 must be a matrix or data frame.", call. = FALSE)
  if (!is.matrix(probs2) && !is.data.frame(probs2))
    stop("probs2 must be a matrix or data frame.", call. = FALSE)

  probs1 <- as.matrix(probs1)
  probs2 <- as.matrix(probs2)

  if (!is.numeric(probs1) || !is.numeric(probs2))
    stop("probs1 and probs2 must contain numeric values.", call. = FALSE)

  if (nrow(probs1) != length(truth))
    stop("probs1 has ", nrow(probs1), " rows but truth has ",
         length(truth), " values.", call. = FALSE)
  if (nrow(probs2) != length(truth))
    stop("probs2 has ", nrow(probs2), " rows but truth has ",
         length(truth), " values.", call. = FALSE)

  if (length(levels) < 3)
    stop("Need at least 3 classes for multiclass ROC; found ",
         length(levels), ".", call. = FALSE)

  truth <- factor(truth, levels = levels)
  if (!is.null(seed)) set.seed(seed)

  auc_of <- function(y, p) {
    as.numeric(suppressMessages(
      pROC::multiclass.roc(y, p, levels = levels))$auc)
  }

  auc1 <- auc_of(truth, probs1)
  auc2 <- auc_of(truth, probs2)

  class_rows <- split(seq_along(truth), truth, drop = TRUE)
  boot_differences <- rep(NA_real_, n_boot)

  for (b in seq_len(n_boot)) {

    # Resample within each class so class sizes never change.

    rows <- unlist(
      lapply(class_rows,
             function(i) i[sample.int(length(i), length(i), replace = TRUE)]),
      use.names = FALSE
    )

# Apply the same sampled rows to both models to preserve the paired comparison.
    boot_truth <- truth[rows]

    boot_differences[b] <- tryCatch(
      auc_of(boot_truth, probs1[rows, , drop = FALSE]) -
        auc_of(boot_truth, probs2[rows, , drop = FALSE]),
      error = function(e) NA_real_
    )
  }

  ok       <- boot_differences[!is.na(boot_differences)]
  n_ok     <- length(ok)
  n_failed <- n_boot - n_ok

  if (n_ok < 2)
    stop("Too few successful bootstrap replicates to summarise.", call. = FALSE)
  if (n_ok < n_boot * 0.9)
    warning(n_failed, " of ", n_boot, " bootstrap replicates failed.",
            call. = FALSE)

  alpha <- 1 - conf_level
  ci    <- stats::quantile(ok, c(alpha / 2, 1 - alpha / 2), names = FALSE)

  structure(
    list(auc1       = auc1,
         auc2       = auc2,
         difference = auc1 - auc2,
         lower      = ci[1],
         upper      = ci[2],
         se         = stats::sd(ok),
         p_value    = min(2 * min(mean(ok >= 0), mean(ok <= 0)), 1),
         conf_level = conf_level,
         n_boot     = n_ok,
         n_failed   = n_failed,
         replicates = ok),
    class = "mc_compare"
  )
}


#' @param x An `mc_compare` object.
#' @param ... Ignored.
#' @rdname mc_auc_compare
#' @export
print.mc_compare <- function(x, ...) {
  cat("\nMulticlass AUC comparison (paired stratified bootstrap)\n")
  cat(sprintf("  Model 1 AUC  : %.4f\n", x$auc1))
  cat(sprintf("  Model 2 AUC  : %.4f\n", x$auc2))
  cat(sprintf("  Difference   : %.4f\n", x$difference))
  cat(sprintf("  %.0f%% CI       : %.4f to %.4f\n",
              x$conf_level * 100, x$lower, x$upper))
  cat(sprintf("  Bootstrap SE : %.4f\n", x$se))

  # For display, an empirical p-value of zero is reported as < 1 / n_boot.
  if (x$p_value < 1 / x$n_boot) {
    cat(sprintf("  p-value      : < %.4f\n", 1 / x$n_boot))
  } else {
    cat(sprintf("  p-value      : %.4f\n", x$p_value))
  }

  cat(sprintf("  Replicates   : %d successful, %d failed\n",
              x$n_boot, x$n_failed))
  cat("\n")
  invisible(x)
}
