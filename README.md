# ProcEx

ProcEx is an R package for evaluating multiclass classification models using ROC analysis, Hand–Till multiclass AUC, bootstrap confidence intervals, model comparison, data validation, missing-value handling, and prediction outlier detection.

## Main Functions

ProcEx provides six main user-facing functions:

- `validate_classification_data()` – validates multiclass labels and predicted probability data.
- `mc_roc()` – calculates Hand–Till multiclass AUC and pairwise AUC values.
- `handle_missing()` – handles missing values in multiclass ROC data.
- `profile_outliers()` – identifies unusual prediction scores and high-confidence incorrect predictions.
- `mc_auc_ci()` – estimates a confidence interval for multiclass AUC using stratified bootstrap resampling.
- `mc_auc_compare()` – compares the multiclass AUC of two models using paired stratified bootstrap resampling.

## Example: Dry Bean Classification Dataset

The package was tested using the Dry Bean Dataset containing 13,611 observations, 16 predictor variables, and 7 bean classes.

```r
library(readxl)

data <- read_excel("Dry_Bean_Dataset.xlsx")

truth <- factor(data$Class)
```

### Train and Test Split

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

A multinomial classification model can be used to generate class probabilities:

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

The resulting test set contains 2,726 observations and predicted probabilities for all 7 classes.

## Validate Classification Data

```r
validation_result <- validate_classification_data(
  truth = truth_test,
  probs = probs1
)

validation_result
```

Example result:

```text
valid: TRUE
status: VALID
observations: 2726
classes: 7
```

## Multiclass ROC and AUC

```r
roc_fit <- mc_roc(
  truth = truth_test,
  probs = probs1,
  label = "Dry Bean Model 1"
)

roc_fit
summary(roc_fit)
```

Example result:

```text
Multiclass AUC: 0.996
Classes: 7
Observations: 2726
Data checks: VALID
```

The function also reports pairwise AUC values for every pair of classes.

## Handle Missing Values

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

Example result:

```text
Affected observations: 3
Removed observations: 3
Imputed observations: 0
Method: remove
```

## Profile Prediction Outliers

```r
outliers <- profile_outliers(
  truth = truth_test,
  probs = probs1
)

head(outliers)
table(outliers$reason)
```

Example result:

```text
High-confidence wrong predictions: 31
IQR outliers: 3159
```

## Multiclass AUC Confidence Interval

```r
ci_result <- mc_auc_ci(
  truth = truth_test,
  probabilities = probs1,
  n_boot = 200,
  conf_level = 0.95,
  seed = 42
)
```

Example result:

```text
AUC: 0.9962
Bootstrap SE: 0.00048
95% CI: 0.9952 to 0.9970
Successful bootstrap replicates: 200
Failed bootstrap replicates: 0
```

## Compare Two Multiclass Models

A second model can be trained using a smaller subset of predictors:

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

The two models can then be compared:

```r
compare_result <- mc_auc_compare(
  response = truth_test,
  predictor1 = probs1,
  predictor2 = probs2,
  boot.n = 200,
  conf.level = 0.95,
  seed = 42
)

compare_result
```

Example result:

```text
Model 1 AUC: 0.9962
Model 2 AUC: 0.9949
Difference: 0.0013
95% CI: 0.0008 to 0.0018
Bootstrap SE: 0.0003
Successful bootstrap replicates: 200
Failed bootstrap replicates: 0
```
