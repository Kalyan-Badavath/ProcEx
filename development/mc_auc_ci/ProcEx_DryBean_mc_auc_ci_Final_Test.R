required <- c("devtools", "readxl", "nnet", "pROC")
missing_packages <- required[!vapply(required, requireNamespace,
                                      quietly = TRUE, FUN.VALUE = logical(1))]
if (length(missing_packages) > 0L) {
  stop("Install the missing packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}
if (!file.exists("DESCRIPTION") || !file.exists("R/mc_auc_ci.R")) {
  stop("Open the ProcEx RStudio PROJECT and run this script from its project root.")
}

# Use the source code of the currently open ProcEx project, not a possibly
# older installed version. This includes your final R/mc_auc_ci.R.
devtools::load_all(".", quiet = TRUE)
cat("\n=== ProcEx: Dry Bean practical validation ===\n")
cat("Loaded mc_auc_ci from current project.\n")

# ---------------------------------------------------------------------------
# A. Read the real Dry Bean data directly from the supplied ZIP archive
# ---------------------------------------------------------------------------
cat("\nChoose dry+bean+dataset.zip when the file selector appears.\n")
zip_path <- file.choose()
if (!grepl("\\.zip$", zip_path, ignore.case = TRUE)) {
  stop("Please select the Dry Bean ZIP file (dry+bean+dataset.zip).")
}
zip_listing <- utils::unzip(zip_path, list = TRUE)
xlsx_in_zip <- zip_listing$Name[
  grepl("(^|/)Dry_Bean_Dataset\\.xlsx$", zip_listing$Name)
]
if (length(xlsx_in_zip) != 1L) {
  stop("Expected exactly one Dry_Bean_Dataset.xlsx inside the ZIP.")
}
extract_dir <- file.path(tempdir(), "procex_drybean_validation")
dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
utils::unzip(zip_path, files = xlsx_in_zip, exdir = extract_dir)
bean_path <- file.path(extract_dir, xlsx_in_zip)
bean <- as.data.frame(readxl::read_excel(bean_path))
if (!"Class" %in% names(bean)) stop("Missing target column: Class.")
bean$Class <- factor(bean$Class)
predictors <- setdiff(names(bean), "Class")
if (!all(vapply(bean[predictors], is.numeric, logical(1)))) {
  stop("The 16 predictor columns must be numeric.")
}
if (anyNA(bean) || any(!is.finite(as.matrix(bean[predictors])))) {
  stop("Missing or non-finite values found in the Dry Bean dataset.")
}
cat("\n=== Dataset verification ===\n")
cat("Observations:", nrow(bean), "\n")
cat("Predictors:", length(predictors), "\n")
cat("Class count:", nlevels(bean$Class), "\n")
print(table(bean$Class))
stopifnot(nrow(bean) == 13611L, length(predictors) == 16L,
          nlevels(bean$Class) == 7L)

# ---------------------------------------------------------------------------
# B. Stratified 80:20 train-test split (seed fixed for reproducibility)
#    Scale using TRAINING statistics only to avoid data leakage.
# ---------------------------------------------------------------------------
set.seed(42)
by_class <- split(seq_len(nrow(bean)), bean$Class)
train_idx <- sort(unlist(lapply(by_class, function(ids) {
  sample(ids, size = floor(0.80 * length(ids)), replace = FALSE)
}), use.names = FALSE))
test_idx <- setdiff(seq_len(nrow(bean)), train_idx)

raw_x <- as.matrix(bean[predictors])
train_mean <- colMeans(raw_x[train_idx, , drop = FALSE])
train_sd <- apply(raw_x[train_idx, , drop = FALSE], 2, stats::sd)
if (any(!is.finite(train_sd)) || any(train_sd == 0)) {
  stop("At least one predictor has zero/invalid training SD.")
}
scaled_x <- sweep(sweep(raw_x, 2, train_mean, "-"), 2, train_sd, "/")
model_data <- as.data.frame(scaled_x)
model_data$Class <- bean$Class
train <- model_data[train_idx, , drop = FALSE]
test <- model_data[test_idx, , drop = FALSE]
cat("\nTrain rows:", nrow(train), "| Test rows:", nrow(test), "\n")
stopifnot(nrow(train) == 10885L, nrow(test) == 2726L)

# ---------------------------------------------------------------------------
# C. Real-world multiclass test: 7 Dry Bean classes
# ---------------------------------------------------------------------------
cat("\n=== Training a 7-class multinomial model ===\n")

model7 <- nnet::multinom(
  Class ~ .,
  data = train,
  trace = FALSE,
  maxit = 1500,
  decay = 0.001,
  MaxNWts = 2000
)

cat("Model convergence code (0 means converged):", model7$convergence, "\n")
if (is.null(model7$convergence) || model7$convergence != 0) {
  warning("The seven-class model did not report convergence; review model fitting before reporting its AUC.")
}
truth7 <- droplevels(test$Class)
probs7 <- as.matrix(predict(model7, newdata = test, type = "probs"))
stopifnot(nrow(probs7) == length(truth7),
          setequal(colnames(probs7), levels(truth7)),
          !anyNA(probs7), all(is.finite(probs7)),
          all(probs7 >= 0 & probs7 <= 1),
          all(abs(rowSums(probs7) - 1) < 1e-6))

cat("\n=== DRY BEAN: SEVEN-CLASS AUC + 95% CI ===\n")
bean_ci7 <- mc_auc_ci(
  truth = truth7,
  probs = probs7,
  n_boot = 100,
  conf_level = 0.95,
  seed = 42
)
print(bean_ci7)
ref_auc7 <- as.numeric(pROC::multiclass.roc(
  response = truth7, predictor = probs7)$auc)
cat("Reference pROC multiclass AUC:", sprintf("%.8f", ref_auc7), "\n")
cat("AUC matches pROC:",
    isTRUE(all.equal(bean_ci7$original_auc, ref_auc7)), "\n")
cat("Valid bootstrap counts:",
    bean_ci7$n_successful + bean_ci7$n_failed == bean_ci7$n_boot, "\n")

bean_ci7_repeat <- mc_auc_ci(truth7, probs7, n_boot = 100,
                             conf_level = 0.95, seed = 42)
cat("Repeated bootstrap results identical:",
    identical(bean_ci7$bootstrap_values, bean_ci7_repeat$bootstrap_values), "\n")
stopifnot(isTRUE(all.equal(bean_ci7$original_auc, ref_auc7)),
          bean_ci7$n_successful + bean_ci7$n_failed == 100,
          identical(bean_ci7$bootstrap_values, bean_ci7_repeat$bootstrap_values))

# ---------------------------------------------------------------------------
# D. Additional real-world binary test: DERMASON vs SIRA
#    Keep the original training/test partition. For two-class multinom,
#    predict(type = 'probs') is the P(second class) numeric vector.
# ---------------------------------------------------------------------------
binary_classes <- c("DERMASON", "SIRA")
train2 <- train[train$Class %in% binary_classes, , drop = FALSE]
test2 <- test[test$Class %in% binary_classes, , drop = FALSE]
train2$Class <- factor(as.character(train2$Class), levels = binary_classes)
test2$Class <- factor(as.character(test2$Class), levels = binary_classes)

cat("\n=== Training a two-class multinomial model ===\n")

model2 <- nnet::multinom(
  Class ~ .,
  data = train2,
  trace = FALSE,
  maxit = 1500,
  decay = 0.001,
  MaxNWts = 2000
)

cat("Binary model convergence code (0 means converged):", model2$convergence, "\n")
if (is.null(model2$convergence) || model2$convergence != 0) {
  warning("The binary model did not report convergence; review model fitting before reporting its AUC.")
}
positive_prob <- as.numeric(predict(model2, newdata = test2, type = "probs"))
probs2 <- cbind(DERMASON = 1 - positive_prob, SIRA = positive_prob)
truth2 <- test2$Class
stopifnot(nrow(probs2) == length(truth2),
          setequal(colnames(probs2), levels(truth2)),
          !anyNA(probs2), all(is.finite(probs2)),
          all(probs2 >= 0 & probs2 <= 1),
          all(abs(rowSums(probs2) - 1) < 1e-6))

cat("\n=== DRY BEAN: TWO-CLASS AUC + 95% CI ===\n")
bean_ci2 <- mc_auc_ci(
  truth = truth2,
  probs = probs2,
  n_boot = 100,
  conf_level = 0.95,
  seed = 42
)
print(bean_ci2)
ref_auc2 <- as.numeric(pROC::auc(pROC::roc(
  response = truth2, predictor = probs2[, "SIRA"],
  levels = binary_classes, direction = "<", quiet = TRUE)))
cat("Reference pROC binary AUC:", sprintf("%.8f", ref_auc2), "\n")
cat("AUC matches pROC:",
    isTRUE(all.equal(bean_ci2$original_auc, ref_auc2)), "\n")
stopifnot(isTRUE(all.equal(bean_ci2$original_auc, ref_auc2)))

cat("\n=== FINAL RESULTS: Dry Bean practical validation completed ===\n")
cat("7-class model: ", length(truth7), " test rows; AUC = ",
    sprintf("%.6f", bean_ci7$original_auc),
    "; CI = [", sprintf("%.6f", bean_ci7$lower_ci), ", ",
    sprintf("%.6f", bean_ci7$upper_ci), "]\n", sep = "")
cat("2-class model: ", length(truth2), " test rows; AUC = ",
    sprintf("%.6f", bean_ci2$original_auc),
    "; CI = [", sprintf("%.6f", bean_ci2$lower_ci), ", ",
    sprintf("%.6f", bean_ci2$upper_ci), "]\n", sep = "")
cat("Check the model convergence codes above before reporting values.\n")
cat("Use print(bean_ci7), print(bean_ci2), and plot(bean_ci7) for report figures.\n")

# Display the 7-class distribution in the RStudio Plots pane.
plot(bean_ci7)


# Seven-class results
print(bean_ci7)

# Two-class results
print(bean_ci2)

# Seven-class bootstrap histogram
plot(bean_ci7)


# 1. Verify 7-class AUC against pROC
all.equal(bean_ci7$original_auc, ref_auc7)

# 2. Verify 2-class AUC against pROC
all.equal(bean_ci2$original_auc, ref_auc2)

# 3. Verify bootstrap reproducibility
identical(
  bean_ci7$bootstrap_values,
  bean_ci7_repeat$bootstrap_values
)

# 4. Check model convergence
c(
  multiclass = model7$convergence,
  binary = model2$convergence
)
