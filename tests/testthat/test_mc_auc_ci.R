# ProcEx - mc_auc_ci() verification tests
# Matched to mc_auc_ci(20261111-120657).R
# Run from a folder containing mc_auc_ci.R and this script.
# Required package: pROC
# Optional real-data demo: Dry_Bean_Dataset.xlsx, readxl, randomForest

function_file <- "mc_auc_ci.R"
if (!file.exists(function_file)) {
  stop("Cannot find mc_auc_ci.R. Place it in the working directory, or set function_file to its path.")
}
if (!requireNamespace("pROC", quietly = TRUE)) {
  stop("Install pROC first: install.packages('pROC')")
}
source(function_file)
stopifnot(is.function(mc_auc_ci), is.function(print.mc_auc_ci))

# An invalid input passes only when the function raises an error.
expect_error <- function(expr, label) {
  raised <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  if (!raised) stop("FAIL: expected an error for ", label)
  cat("PASS (expected error): ", label, "\n", sep = "")
}

check_result <- function(x, expected_n, expected_conf = 0.95) {
  stopifnot(
    inherits(x, "mc_auc_ci"),
    is.finite(x$original_auc), x$original_auc >= 0, x$original_auc <= 1,
    is.finite(x$standard_error), x$standard_error >= 0,
    is.finite(x$lower_ci), is.finite(x$upper_ci),
    x$lower_ci >= 0, x$upper_ci <= 1, x$lower_ci <= x$upper_ci,
    identical(x$conf_level, expected_conf),
    x$n_boot == expected_n,
    x$n_successful + x$n_failed == expected_n,
    x$n_successful >= 2,
    length(x$bootstrap_values) == x$n_successful,
    all(is.finite(x$bootstrap_values))
  )
  invisible(TRUE)
}

# ------------------------------------------------------------
# 1. Small 3-class example from the previous test setup
# ------------------------------------------------------------
truth <- factor(c(rep("A", 8), rep("B", 6), rep("C", 6)))
probabilities <- data.frame(
  A = c(0.70,0.65,0.60,0.55,0.50,0.45,0.40,0.35,
        0.20,0.25,0.30,0.20,0.35,0.25,
        0.15,0.20,0.25,0.30,0.20,0.35),
  B = c(0.20,0.25,0.25,0.30,0.30,0.35,0.35,0.40,
        0.65,0.60,0.55,0.50,0.45,0.50,
        0.20,0.25,0.20,0.25,0.30,0.25),
  C = c(0.10,0.10,0.15,0.15,0.20,0.20,0.25,0.25,
        0.15,0.15,0.15,0.30,0.20,0.25,
        0.65,0.55,0.55,0.45,0.50,0.40)
)
stopifnot(length(truth) == nrow(probabilities),
          setequal(levels(truth), colnames(probabilities)))

cat("\n=== TEST 1: Multiclass AUC and clean print ===\n")
result <- mc_auc_ci(truth, probabilities, n_boot = 1000,
                    conf_level = 0.95, seed = 42)
check_result(result, 1000)
stopifnot(result$n_successful == 1000, result$n_failed == 0)
print(result)
summary_output <- capture.output(print(result))
stopifnot(!any(grepl("Bootstrap AUC Values:", summary_output, fixed = TRUE)))
# Verify multiclass point estimate directly against pROC's Hand-Till AUC.
expected_auc <- as.numeric(pROC::multiclass.roc(truth, as.matrix(probabilities))$auc)
stopifnot(isTRUE(all.equal(result$original_auc, expected_auc)))
cat("PASS: multiclass AUC, CI, and concise default output\n")

# ------------------------------------------------------------
# 2. Supervisor feedback: 2-class data must WORK, not error
# ------------------------------------------------------------
cat("\n=== TEST 2: Binary classification ===\n")
set.seed(42)
binary_truth <- factor(rep(c("A", "B"), each = 50), levels = c("A", "B"))
# Deliberately overlapping predictions: AUC should be finite.
binary_p <- c(runif(50, 0.05, 0.65), runif(50, 0.35, 0.95))
binary_probs <- cbind(A = 1 - binary_p, B = binary_p)
binary_result <- mc_auc_ci(binary_truth, binary_probs,
                           n_boot = 100, seed = 42)
check_result(binary_result, 100)
stopifnot(binary_result$n_successful == 100, binary_result$n_failed == 0)
expected_binary_auc <- as.numeric(pROC::auc(pROC::roc(
  response = binary_truth, predictor = binary_probs[, "B"],
  levels = c("A", "B"), direction = "<", quiet = TRUE)))
stopifnot(isTRUE(all.equal(binary_result$original_auc, expected_binary_auc)))
print(binary_result)
cat("PASS: 2-class input works and matches pROC binary AUC\n")

# ------------------------------------------------------------
# 3. Supervisor feedback: optional bootstrap values display
# ------------------------------------------------------------
cat("\n=== TEST 3: Bootstrap output toggle ===\n")
detail <- mc_auc_ci(binary_truth, binary_probs, n_boot = 10,
                    seed = 42, show_bootstrap_values = TRUE)
check_result(detail, 10)
detail_output <- capture.output(print(detail))
stopifnot(any(grepl("Bootstrap AUC Values:", detail_output, fixed = TRUE)))
print(detail)

large_detail <- mc_auc_ci(binary_truth, binary_probs, n_boot = 150,
                          seed = 42, show_bootstrap_values = TRUE)
large_output <- capture.output(print(large_detail))
stopifnot(any(grepl("Showing first 100 values", large_output, fixed = TRUE)),
          length(large_detail$bootstrap_values) == 150)
cat("PASS: default hides values; TRUE shows values; >100 are truncated in print only\n")

# ------------------------------------------------------------
# 4. Validation: errors must actually occur
# ------------------------------------------------------------
cat("\n=== TEST 4: Input validation ===\n")
bad_prob <- probabilities; bad_prob[1, 1] <- 1.2
expect_error(mc_auc_ci(truth, bad_prob, n_boot = 10), "probability > 1")
expect_error(mc_auc_ci(truth, probabilities, n_boot = 0), "n_boot = 0")
expect_error(mc_auc_ci(truth, probabilities, n_boot = 1), "n_boot = 1")
expect_error(mc_auc_ci(truth, probabilities, conf_level = 1.2), "conf_level > 1")
expect_error(mc_auc_ci(truth[-1], probabilities, n_boot = 10), "row count mismatch")
missing_prob <- probabilities; missing_prob[1, 1] <- NA_real_
expect_error(mc_auc_ci(truth, missing_prob, n_boot = 10), "missing probability")
wrong_names <- probabilities; names(wrong_names) <- c("A", "B", "D")
expect_error(mc_auc_ci(truth, wrong_names, n_boot = 10), "wrong class names")
duplicate_names <- probabilities; names(duplicate_names) <- c("A", "A", "C")
expect_error(mc_auc_ci(truth, duplicate_names, n_boot = 10), "duplicate class names")
expect_error(mc_auc_ci(truth, probabilities, n_boot = 10,
                       show_bootstrap_values = NA), "invalid show_bootstrap_values")
expect_error(mc_auc_ci(truth, probabilities, n_boot = 10,
                       seed = -1), "invalid seed")
expect_error(mc_auc_ci(factor(rep("A", length(truth))), probabilities,
                       n_boot = 10), "only one class")
cat("PASS: validation tests\n")

# ------------------------------------------------------------
# 5. Reproducibility and confidence levels
# ------------------------------------------------------------
cat("\n=== TEST 5: Reproducibility and CI settings ===\n")
r1 <- mc_auc_ci(truth, probabilities, n_boot = 100, seed = 42)
r2 <- mc_auc_ci(truth, probabilities, n_boot = 100, seed = 42)
stopifnot(identical(r1$bootstrap_values, r2$bootstrap_values),
          identical(c(r1$lower_ci, r1$upper_ci), c(r2$lower_ci, r2$upper_ci)))
r90 <- mc_auc_ci(truth, probabilities, n_boot = 500,
                 conf_level = 0.90, seed = 42)
check_result(r90, 500, 0.90)
r95 <- mc_auc_ci(truth, probabilities, n_boot = 500,
                 conf_level = 0.95, seed = 42)
stopifnot(r90$lower_ci >= r95$lower_ci,
          r90$upper_ci <= r95$upper_ci)
cat("PASS: same seed reproduces results; 90% CI lies within 95% CI\n")

# ------------------------------------------------------------
# 6. Optional REAL Dry Bean dataset (7 classes)
# ------------------------------------------------------------
# Place Dry_Bean_Dataset.xlsx in the same folder as this test script.
# Requires: install.packages(c("readxl", "randomForest"))
cat("\n=== TEST 6: Real Dry Bean dataset (optional) ===\n")
# Choose the actual path to your Dry Bean dataset here.
# Supported: .xlsx, .csv
bean_file <- "Dry_Bean_Dataset.xlsx"
# Alternatively: bean_file <- file.choose()
if (!file.exists(bean_file)) {
  cat("SKIPPED: Dry Bean Excel file not found at:", bean_file, "\n")
  cat("Update bean_file and rerun this section to test the real dataset.\n")
} else if ((!grepl("\\.csv$", bean_file, ignore.case = TRUE) &&
            !requireNamespace("readxl", quietly = TRUE)) ||
           !requireNamespace("randomForest", quietly = TRUE)) {
  cat("SKIPPED: install randomForest (and readxl for Excel input).\n")
} else {
  if (grepl("\\.csv$", bean_file, ignore.case = TRUE)) {
    dry_bean <- read.csv(bean_file, check.names = FALSE)
  } else {
    dry_bean <- as.data.frame(readxl::read_excel(bean_file))
  }
  stopifnot("Class" %in% names(dry_bean))
  dry_bean$Class <- factor(dry_bean$Class)
  stopifnot(nlevels(dry_bean$Class) == 7, !anyNA(dry_bean))

  # Same 80/20 split and 300-tree random forest as the previous demo.
  set.seed(42)
  train_index <- sample(seq_len(nrow(dry_bean)),
                        size = floor(0.8 * nrow(dry_bean)))
  train_data <- dry_bean[train_index, , drop = FALSE]
  test_data <- dry_bean[-train_index, , drop = FALSE]
  set.seed(42)
  bean_model <- randomForest::randomForest(Class ~ ., data = train_data,
                                           ntree = 300)
  bean_probabilities <- predict(bean_model, newdata = test_data, type = "prob")
  bean_truth <- droplevels(test_data$Class)
  stopifnot(nlevels(bean_truth) == 7,
            nrow(bean_probabilities) == length(bean_truth),
            setequal(colnames(bean_probabilities), levels(bean_truth)))

  bean_result <- mc_auc_ci(bean_truth, bean_probabilities,
                           n_boot = 1000, conf_level = 0.95,
                           seed = 42, show_bootstrap_values = FALSE)
  check_result(bean_result, 1000)
  expected_bean_auc <- as.numeric(pROC::multiclass.roc(
    bean_truth, bean_probabilities)$auc)
  stopifnot(isTRUE(all.equal(bean_result$original_auc, expected_bean_auc)))
  print(bean_result)
  # Verify that the default print does not flood the console.
  bean_print <- capture.output(print(bean_result))
  stopifnot(!any(grepl("Bootstrap AUC Values:", bean_print, fixed = TRUE)))
  # Verify reproducibility with the same seed (100 replicates to keep it quick).
  bean_repeat_1 <- mc_auc_ci(bean_truth, bean_probabilities,
                             n_boot = 100, seed = 42)
  bean_repeat_2 <- mc_auc_ci(bean_truth, bean_probabilities,
                             n_boot = 100, seed = 42)
  stopifnot(identical(bean_repeat_1$bootstrap_values,
                      bean_repeat_2$bootstrap_values))
  bean_summary <- data.frame(
    AUC = bean_result$original_auc,
    Standard_Error = bean_result$standard_error,
    CI_Lower = bean_result$lower_ci,
    CI_Upper = bean_result$upper_ci,
    Confidence_Level = bean_result$conf_level,
    Bootstrap_Replicates = bean_result$n_boot,
    Successful_Bootstraps = bean_result$n_successful,
    Failed_Bootstraps = bean_result$n_failed
  )
  print(bean_summary)
  cat("PASS: 7-class Dry Bean AUC, 95% CI, clean print, and reproducibility\n")
}

cat("\nCompleted all available tests. Check above for any SKIPPED section.\n")
