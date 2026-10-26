#' Compare two multiclass AUCs using a paired stratified bootstrap
#'
#' Tests whether two classification models differ in their Hand-Till
#' multiclass AUC. pROC computes the multiclass AUC but its
#' documentation states that comparison tests are not implemented for
#' multiclass ROC objects.
#'
#' Both models must be scored on the same observations, in the same
#' order. Each bootstrap replicate resamples observations within each
#' class (so class sizes are preserved) and applies the same resampled
#' rows to both models, which keeps the comparison paired.
#'
#' @param response A factor or vector of true class labels. At least
#'   three classes are required.
#' @param predictor1 A matrix or data frame of predicted class
#'   probabilities from the first model, with one row per observation
#'   and one column per class.
#' @param predictor2 A matrix or data frame of predicted class
#'   probabilities from the second model, scored on the same
#'   observations in the same order as \code{predictor1}.
#' @param levels The full set of class labels. Defaults to the levels
#'   of \code{response}.
#' @param boot.n Number of bootstrap replicates. Default 1000.
#' @param conf.level Confidence level for the interval. Default 0.95.
#' @param seed Optional integer used to make the bootstrap
#'   reproducible. Default \code{NULL}.
#'
#' @return An object of class \code{mc_compare}: a list containing the
#'   two AUCs, their observed difference, the confidence interval
#'   bounds, the bootstrap standard error, a two-sided bootstrap
#'   p-value, the confidence level, the numbers of successful and
#'   failed replicates, and the vector of bootstrap differences.
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' fit_strong <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)
#' fit_weak   <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
#'                              data = iris, trace = FALSE)
#' mc_auc_compare(iris$Species,
#'                stats::predict(fit_strong, type = "probs"),
#'                stats::predict(fit_weak,   type = "probs"),
#'                boot.n = 100, seed = 42)
#' }
#'
#' @export
mc_auc_compare <- function(response,
                           predictor1,
                           predictor2,
                           levels     = base::levels(as.factor(response)),
                           boot.n     = 1000,
                           conf.level = 0.95,
                           seed       = NULL) {

  # basic input checks
  stopifnot(is.matrix(predictor1) || is.data.frame(predictor1))
  stopifnot(is.matrix(predictor2) || is.data.frame(predictor2))
  predictor1 <- as.matrix(predictor1)
  predictor2 <- as.matrix(predictor2)

  if (nrow(predictor1) != length(response))
    stop("predictor1 has ", nrow(predictor1), " rows but response has ",
         length(response), " values.")
  if (nrow(predictor2) != length(response))
    stop("predictor2 has ", nrow(predictor2), " rows but response has ",
         length(response), " values.")
  if (length(levels) < 3)
    stop("Need at least 3 classes for multiclass ROC; found ",
         length(levels), ".")

  response <- factor(response, levels = levels)
  if (!is.null(seed)) set.seed(seed)

  # observed difference
  auc1 <- as.numeric(suppressMessages(
    pROC::multiclass.roc(response, predictor1, levels = levels))$auc)
  auc2 <- as.numeric(suppressMessages(
    pROC::multiclass.roc(response, predictor2, levels = levels))$auc)
  observed_difference <- auc1 - auc2

  #  paired stratified bootstrap
  boot_differences <- rep(NA_real_, boot.n)

  for (b in seq_len(boot.n)) {

    # resample WITHIN each class
    boot_index <- unlist(
      lapply(levels, function(class_name) {
        class_index <- which(response == class_name)
        sample(class_index, size = length(class_index), replace = TRUE)
      }),
      use.names = FALSE
    )

    boot_truth <- response[boot_index]

    boot_differences[b] <- tryCatch({
      r1 <- suppressMessages(pROC::multiclass.roc(
        boot_truth, predictor1[boot_index, , drop = FALSE], levels = levels))
      r2 <- suppressMessages(pROC::multiclass.roc(
        boot_truth, predictor2[boot_index, , drop = FALSE], levels = levels))
      as.numeric(r1$auc) - as.numeric(r2$auc)
    }, error = function(e) NA_real_)
  }

  ok       <- boot_differences[!is.na(boot_differences)]
  n_ok     <- length(ok)
  n_failed <- boot.n - n_ok

  if (n_ok < 2)
    stop("Too few successful bootstrap replicates to summarise.")

  if (n_ok < boot.n * 0.9)
    warning(n_failed, " of ", boot.n, " bootstrap replicates failed.")

  alpha <- 1 - conf.level
  ci    <- stats::quantile(ok, c(alpha / 2, 1 - alpha / 2), names = FALSE)

  # two-sided bootstrap p-value: how often does the difference cross zero?
  p_value <- min(2 * min(mean(ok >= 0), mean(ok <= 0)), 1)

  structure(
    list(auc1       = auc1,
         auc2       = auc2,
         difference = observed_difference,
         lower      = ci[1],
         upper      = ci[2],
         se         = stats::sd(ok),
         p.value    = p_value,
         conf.level = conf.level,
         boot.n     = n_ok,
         n_failed   = n_failed,
         replicates = ok),
    class = "mc_compare"
  )
}


#' Print a multiclass AUC comparison
#'
#' @param x An object of class \code{mc_compare}, as returned by
#'   \code{\link{mc_auc_compare}}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return \code{x}, invisibly. Called for the printed output.
#'
#' @export
print.mc_compare <- function(x, ...) {
  cat("\nMulticlass AUC comparison (paired stratified bootstrap)\n")
  cat(sprintf("  Model 1 AUC : %.4f\n", x$auc1))
  cat(sprintf("  Model 2 AUC : %.4f\n", x$auc2))
  cat(sprintf("  Difference  : %.4f\n", x$difference))
  cat(sprintf("  %.0f%% CI      : %.4f to %.4f\n",
              x$conf.level * 100, x$lower, x$upper))
  cat(sprintf("  Bootstrap SE: %.4f\n", x$se))
  cat(sprintf("  p-value     : %.4f\n", x$p.value))
  cat(sprintf("  Replicates  : %d successful, %d failed\n",
              x$boot.n, x$n_failed))
  cat("\n")
  invisible(x)
}
