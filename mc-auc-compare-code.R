
# Generalise the comparison + check it is calibrated
#
# PART 1  reusable function + realistic iris demo
# PART 2  calibration study, run in batches


library(pROC)
library(nnet)



mc_auc_compare <- function(response,
                           predictor1,
                           predictor2,
                           levels     = base::levels(as.factor(response)),
                           boot.n     = 1000,
                           conf.level = 0.95,
                           seed       = NULL) {

  #  basic input checks
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
    stop("Need at least 3 classes for multiclass ROC; found ", length(levels), ".")

  response <- factor(response, levels = levels)
  if (!is.null(seed)) set.seed(seed)

  # observed difference on the full data
  auc1 <- as.numeric(suppressMessages(
    multiclass.roc(response, predictor1, levels = levels))$auc)
  auc2 <- as.numeric(suppressMessages(
    multiclass.roc(response, predictor2, levels = levels))$auc)
  observed_difference <- auc1 - auc2

  #  paired stratified bootstrap
  boot_differences <- rep(NA_real_, boot.n)

  for (b in seq_len(boot.n)) {

    # resample WITHIN each class, so class sizes never change
    boot_index <- unlist(
      lapply(levels, function(class_name) {
        class_index <- which(response == class_name)
        sample(class_index, size = length(class_index), replace = TRUE)
      }),
      use.names = FALSE
    )

    # the SAME rows go to both models which makes it paired
    boot_truth <- response[boot_index]

    boot_differences[b] <- tryCatch({
      r1 <- suppressMessages(multiclass.roc(
        boot_truth, predictor1[boot_index, , drop = FALSE], levels = levels))
      r2 <- suppressMessages(multiclass.roc(
        boot_truth, predictor2[boot_index, , drop = FALSE], levels = levels))
      as.numeric(r1$auc) - as.numeric(r2$auc)
    }, error = function(e) NA_real_)   # survive a bad replicate instead of dying
  }

  #  summarise
  ok       <- boot_differences[!is.na(boot_differences)]
  n_ok     <- length(ok)
  n_failed <- boot.n - n_ok

  if (n_ok < boot.n * 0.9)
    warning(n_failed, " of ", boot.n, " bootstrap replicates failed.")

  alpha <- 1 - conf.level
  ci    <- quantile(ok, c(alpha / 2, 1 - alpha / 2), names = FALSE)

  # two-sided bootstrap p-value: how often does the difference cross zero?
  p_value <- min(2 * min(mean(ok >= 0), mean(ok <= 0)), 1)

  structure(
    list(auc1 = auc1, auc2 = auc2,
         difference = observed_difference,
         lower = ci[1], upper = ci[2],
         se = sd(ok), p.value = p_value,
         conf.level = conf.level,
         boot.n = n_ok, n_failed = n_failed,
         replicates = ok),
    class = "mc_compare"
  )
}


print.mc_compare <- function(x, ...) {
  cat("\nMulticlass AUC comparison (paired stratified bootstrap)\n")
  cat("------------------------------------------------------\n")
  cat(sprintf("  Model 1 AUC : %.4f\n", x$auc1))
  cat(sprintf("  Model 2 AUC : %.4f\n", x$auc2))
  cat(sprintf("  Difference  : %.4f\n", x$difference))
  cat(sprintf("  %.0f%% CI      : %.4f to %.4f\n",
              x$conf.level * 100, x$lower, x$upper))
  cat(sprintf("  Bootstrap SE: %.4f\n", x$se))
  cat(sprintf("  p-value     : %.4f\n", x$p.value))
  cat(sprintf("  Replicates  : %d successful, %d failed\n", x$boot.n, x$n_failed))
  cat("\n")
  invisible(x)
}


# ============================================================
# PART 1B: realistic demo on iris
#
# Two genuinely different models on the SAME rows:
#   strong = all four measurements
#   weak   = sepal measurements only
# We EXPECT the strong model to win clearly.
# ============================================================

set.seed(1)

fit_strong <- multinom(Species ~ ., data = iris, trace = FALSE)
fit_weak   <- multinom(Species ~ Sepal.Length + Sepal.Width,
                       data = iris, trace = FALSE)

probs_strong <- predict(fit_strong, type = "probs")
probs_weak   <- predict(fit_weak,   type = "probs")

result <- mc_auc_compare(
  response   = iris$Species,
  predictor1 = probs_strong,
  predictor2 = probs_weak,
  boot.n     = 1000,
  seed       = 42
)

print(result)

hist(result$replicates,
     main = "Bootstrap distribution: strong model - weak model",
     xlab = "AUC difference", breaks = 30)
abline(v = 0, lty = 2, lwd = 2)

# Sanity check 1: a model compared with itself must give exactly zero
same <- mc_auc_compare(iris$Species, probs_strong, probs_strong,
                       boot.n = 200, seed = 1)
cat("Self-comparison -- all differences exactly zero? ",
    all(same$replicates == 0), "\n")


# PART 2: CALIBRATION STUDY  (slow -- about 3 minutes)
#
# The self-comparison above proves the PAIRING works.
# It does NOT prove the test is calibrated, because identical
# models can never disagree.
#
# When two models are genuinely DIFFERENT but equally good, how often does the test wrongly declare a winner?
# It should be about 5% of the time.
# Build two different-but-equally-good models:
# one latent signal, read twice with independent noise.
make_null_data <- function(n_per_class = 50, noise = 1.5) {
  classes <- c("A", "B", "C")
  truth   <- factor(rep(classes, each = n_per_class), levels = classes)
  signal  <- as.numeric(truth)

  to_probs <- function(s) {
    m <- cbind(A = dnorm(s, 1, noise),
               B = dnorm(s, 2, noise),
               C = dnorm(s, 3, noise))
    m / rowSums(m)
  }

  list(truth = truth,
       p1 = to_probs(signal + rnorm(length(signal), 0, noise)),
       p2 = to_probs(signal + rnorm(length(signal), 0, noise)))
}


run_calibration_batch <- function(n_rep = 60, boot.n = 200, seed = 1) {
  set.seed(seed)
  rejected <- logical(n_rep)
  for (i in seq_len(n_rep)) {
    d <- make_null_data()
    r <- mc_auc_compare(d$truth, d$p1, d$p2,
                        levels = c("A", "B", "C"), boot.n = boot.n)
    # "rejected" = CI excludes zero = test claims a real difference
    rejected[i] <- (r$lower > 0) || (r$upper < 0)
    if (i %% 10 == 0) cat("  ...", i, "of", n_rep, "\n")
  }
  rejected
}

cat("\nRunning calibration batch 1...\n")
batch1 <- run_calibration_batch(seed = 2026)

cat("\nRunning calibration batch 2...\n")
batch2 <- run_calibration_batch(seed = 7777)

# The POOLED result is important, not either batch alone.
# Single batches are very noisy at this size.
pooled <- c(batch1, batch2)

cat("\n=========================================\n")
cat(sprintf("Batch 1 : %d / %d = %.1f%%\n",
            sum(batch1), length(batch1), 100 * mean(batch1)))
cat(sprintf("Batch 2 : %d / %d = %.1f%%\n",
            sum(batch2), length(batch2), 100 * mean(batch2)))
cat(sprintf("POOLED  : %d / %d = %.1f%%   (target ~5%%)\n",
            sum(pooled), length(pooled), 100 * mean(pooled)))
cat("=========================================\n")
