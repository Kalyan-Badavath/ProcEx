# ProcEx

ProcEx is an R package for evaluating classification models using ROC analysis, Hand–Till multiclass AUC, bootstrap confidence intervals, model comparison, plots of pairwise ROC curves and AUCs, data validation, missing-value handling, and prediction outlier detection.

The package focuses on multiclass classification analysis and provides tools for assessing model performance, identifying data-quality issues, and understanding statistical uncertainty.

## Installation

ProcEx can be installed directly from GitHub.

First, install the `remotes` package if it is not already installed:

```r
install.packages("remotes")
```

Install ProcEx:

```r
remotes::install_github("Kalyan-Badavath/ProcEx")
```

To also install the introductory vignette, build it during installation:

```r
remotes::install_github("Kalyan-Badavath/ProcEx", build_vignettes = TRUE)
```

Load the package:

```r
library(ProcEx)
```

Open the vignette (if it was built):

```r
vignette("ProcEx")
```

Check the installed version:

```r
packageVersion("ProcEx")
```

View the exported functions:

```r
getNamespaceExports("ProcEx")
```

## Main Functions

ProcEx provides six main user-facing functions:

- `validate_classification_data()` – Checks class labels, predicted probabilities, structural errors, and reliability warnings.
- `mc_roc()` – Calculates Hand–Till multiclass AUC and pairwise AUC results.
- `handle_missing()` – Handles missing observations in an `mc_roc` object using removal, mean imputation, or median imputation.
- `profile_outliers()` – Identifies unusual prediction probabilities and high-confidence incorrect predictions.
- `mc_auc_ci()` – Estimates bootstrap confidence intervals and standard errors for binary or multiclass AUC.
- `mc_auc_compare()` – Compares two multiclass classification models using paired stratified bootstrap resampling.

Results can be plotted:

- `plot()` on an `mc_roc` object – ROC curves for every pair of classes, or a heatmap of the pairwise AUCs.
- `plot()` on an `mc_auc_ci` result – the bootstrap AUCs, with the confidence interval shaded.
- `plot()` on an `mc_auc_compare` result – the bootstrap distribution of the AUC difference, or each class pair's AUC under both models.

Additional exported utilities are available for investigating the influence of unusual observations on multiclass AUC.

## Example: Dry Bean Classification Dataset

The following example demonstrates ProcEx using the Dry Bean Dataset, which contains 13,611 observations, 16 predictor variables, and 7 bean classes.

The example requires the `readxl` and `nnet` packages:

```r
install.packages(c("readxl", "nnet"))
```

Download the Dry Bean Dataset separately and place `Dry_Bean_Dataset.xlsx` in the current R working directory.

Load the dataset:

```r
library(ProcEx)
library(readxl)

data <- read_excel("Dry_Bean_Dataset.xlsx")

truth <- factor(data$Class)
```

### Train and Test Split

Create a stratified training and testing split:

```r
set.seed(42)

train_index <- unlist(
  lapply(split(seq_len(nrow(data)), data$Class), function(i) {
    sample(i, floor(0.8 * length(i)))
  })
)

train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

truth_test <- factor(
  test_data$Class,
  levels = levels(truth)
)
```

Train a multinomial classification model:

```r
model1 <- nnet::multinom(
  Class ~ .,
  data = train_data,
  trace = FALSE
)

probs1 <- predict(
  model1,
  newdata = test_data,
  type = "probs"
)
```

The predicted probabilities are used as inputs for the ProcEx functions.

## Validate Classification Data

The `validate_classification_data()` function checks whether class labels and predicted probabilities are suitable for multiclass analysis.

```r
validation_result <- validate_classification_data(
  truth = truth_test,
  probs = probs1
)

validation_result
```

The returned list contains:

- `status`: Overall validation status.
- `errors`: Structural or input errors.
- `warnings`: Reliability warnings.
- `n_obs`: Number of observations.
- `n_classes`: Number of observed classes.
- `class_counts`: Number of observations in each class.
- `class_proportions`: Proportion of observations in each class.

The possible validation statuses are:

| Status | Meaning |
|---|---|
| `VALID` | Inputs pass validation without reliability warnings. |
| `VALID_WITH_WARNINGS` | Inputs are structurally valid but have reliability warnings. |
| `NOT_MULTICLASS` | Fewer than three observed classes are present. |
| `INVALID` | Structural or input errors were detected. |

For datasets containing one or two observed classes, the result also includes a `concern` field explaining the multiclass limitation.

The function also supports configurable reliability thresholds:

```r
validation_result <- validate_classification_data(
  truth = truth_test,
  probs = probs1,
  min_class_n = 10,
  imbalance_ratio = 10
)
```

## Multiclass ROC and AUC

The `mc_roc()` function calculates the Hand–Till multiclass AUC using the `pROC` package.

```r
roc_fit <- mc_roc(
  truth = truth_test,
  probs = probs1,
  label = "Dry Bean Model 1"
)

roc_fit
summary(roc_fit)
```

The returned `mc_roc` object contains the overall multiclass AUC, pairwise AUC information, input data, class information, and validation results.

The overall AUC can be accessed using:

```r
roc_fit$auc
```

The pairwise comparison results can be viewed using:

```r
roc_fit$pairwise
```

### Plot the pairwise ROC curves

The multiclass AUC is an average over every pair of classes, so one weak pair can hide behind a high overall value. `plot()` shows each pair:

```r
plot(roc_fit)
```

Each panel shows one pair of classes with its two Hand–Till curves (one ranked by each class's probability) and their AUCs. With more than 25 pairs, the 25 pairs with the lowest AUC are drawn; choose others with `pairs`:

```r
plot(roc_fit, pairs = c("DERMASON vs SIRA", "BARBUNYA vs CALI"))
```

For many classes, a heatmap shows every pair at once. By default the colour scale stretches over the observed AUCs, so the weakest pairs stand out even when all AUCs are high:

```r
plot(roc_fit, type = "heatmap")
```

## Handle Missing Values

The `handle_missing()` function supports missing-data handling for an `mc_roc` object.

Available methods include:

- `"remove"`: Remove observations containing missing values.
- `"mean"`: Replace missing prediction values using column means.
- `"median"`: Replace missing prediction values using column medians.

Missing true class labels are removed rather than imputed.

Example:

```r
probs_missing <- probs1

probs_missing[1, 1] <- NA
probs_missing[10, 3] <- NA
probs_missing[20, 5] <- NA

roc_missing <- mc_roc(
  truth = truth_test,
  probs = probs_missing,
  label = "Dry Bean Missing Test"
)

cleaned_fit <- handle_missing(
  roc_missing,
  method = "remove"
)

cleaned_fit$missing_info
```

The `missing_info` component reports the number and percentage of affected observations, the number removed or imputed, and the selected method.

## Profile Prediction Outliers

The `profile_outliers()` function identifies unusual predicted probability values using the interquartile range (IQR) method.

It also identifies high-confidence incorrect predictions.

```r
outliers <- profile_outliers(
  truth = truth_test,
  probs = probs1
)

head(outliers)
table(outliers$reason)
```

The default action is `"flag"`, which returns information about detected outliers without removing observations.

Users can also select `"remove"` or `"keep"`:

```r
outlier_result <- profile_outliers(
  truth = truth_test,
  probs = probs1,
  action = "keep"
)
```

The function supports configurable IQR thresholds and optional custom probability limits.

## AUC Confidence Intervals

The `mc_auc_ci()` function estimates uncertainty in AUC using stratified bootstrap resampling.

It supports binary and multiclass classification. For multiclass data, the Hand–Till AUC approach is used.

The function calculates the original AUC, bootstrap standard error, and percentile confidence interval.

```r
ci_result <- mc_auc_ci(
  truth = truth_test,
  probs = probs1,
  n_boot = 200,
  conf_level = 0.95,
  seed = 42
)
```

View the main results:

```r
ci_result$original_auc
ci_result$standard_error
ci_result$lower_ci
ci_result$upper_ci
ci_result$n_successful
ci_result$n_failed
```

Plot the bootstrap AUCs, with the confidence interval shaded and the AUC of the original data marked:

```r
plot(ci_result)
```

Users can control the number of bootstrap replicates, confidence level, and random seed.

The function also supports the `show_bootstrap_values` parameter.

For example:

```r
ci_result <- mc_auc_ci(
  truth = truth_test,
  probs = probs1,
  n_boot = 200,
  conf_level = 0.95,
  seed = 42,
  show_bootstrap_values = TRUE
)
```

All successful bootstrap AUC estimates are stored in:

```r
ci_result$bootstrap_values
```

## Compare Two Multiclass Models

The `mc_auc_compare()` function compares two multiclass classification models evaluated on the same observations.

It uses paired stratified bootstrap resampling, applying the same sampled observations to both models in each replicate.

Train a second model using a smaller subset of predictors:

```r
model2 <- nnet::multinom(
  Class ~ Area + Perimeter + MajorAxisLength + MinorAxisLength,
  data = train_data,
  trace = FALSE
)

probs2 <- predict(
  model2,
  newdata = test_data,
  type = "probs"
)
```

Compare the models:

```r
compare_result <- mc_auc_compare(
  truth = truth_test,
  probs1 = probs1,
  probs2 = probs2,
  n_boot = 200,
  conf_level = 0.95,
  seed = 42
)

compare_result
```

The main results:

```r
compare_result$difference
compare_result$lower_ci
compare_result$upper_ci
compare_result$standard_error
compare_result$p_value
```

The results include:

- `auc1`, `auc2`: AUC for each model.
- `difference`: `auc1 - auc2`.
- `lower_ci`, `upper_ci`: bootstrap confidence interval for the difference.
- `standard_error`: bootstrap standard error.
- `p_value`: two-sided bootstrap p-value.
- `n_boot`, `n_successful`, `n_failed`: replicates requested, successful and failed.
- `bootstrap_values`: the bootstrap differences.
- `pairwise`: each class pair's AUC under both models.

These names match `mc_auc_ci()`, so both bootstrap results are read the same way.

The function reports very small empirical bootstrap p-values as less than `1 / n_successful`, reflecting the resolution of the resampling procedure.

Plot the comparison:

```r
plot(compare_result, labels = c("All features", "Size features"))
plot(compare_result, type = "pairs", labels = c("All features", "Size features"))
```

The first plot shows the bootstrap differences with the confidence interval shaded and zero marked: if the shaded interval does not cover zero, the models differ at that confidence level. The second shows which pairs of classes each model separates better.

## Important Considerations

ProcEx functions have specific input requirements.

Predicted probabilities should correspond to the supplied true class labels, with one probability column per class, named exactly as the class labels (any order). `mc_roc()` also checks that values are between 0 and 1 and that each row sums to 1, and rejects other scores such as SVM decision values or logits.

The main multiclass ROC and model comparison functions require at least three classes, while `mc_auc_ci()` also supports binary AUC estimation.

Missing observations should be considered carefully because removal or imputation can influence subsequent results.

Bootstrap confidence intervals provide estimates of statistical uncertainty, but their reliability depends on the available data and the resampling process.

Data validation and outlier detection identify potential issues but do not establish that a classification model is fair, unbiased, or suitable for every application.

## Development and Testing

ProcEx is developed as a structured R package with source code, function documentation, and automated tests.

Automated tests are implemented using `testthat`.

For version 0.0.0.9001, the package test run produced 134 tests with 851 passing expectations, with zero failures, zero warnings, and zero skipped tests.

The recorded R CMD check, including building the vignette, also completed with zero errors, zero warnings, and zero notes.

These results describe the tested package version and should be updated if subsequent changes are made.

## Changes and Known Issues

See `NEWS.md` for what changed in each version and for known issues still to be fixed.

## Project Repository

Source code, documentation, and development history are available at:

https://github.com/Kalyan-Badavath/ProcEx

ProcEx was developed as a collaborative capstone project by a six-member student team.
