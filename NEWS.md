# ProcEx 0.0.0.9001

## Breaking changes

* `mc_auc_compare()` results now use the same names as `mc_auc_ci()`:

  | Before | Now |
  |---|---|
  | class `mc_compare` | class `mc_auc_compare` |
  | `lower`, `upper` | `lower_ci`, `upper_ci` |
  | `se` | `standard_error` |
  | `replicates` | `bootstrap_values` |
  | `n_boot` (successful replicates) | `n_boot` (requested) and `n_successful` |

  Code that reads the old names, such as `res$lower`, needs updating. The
  calculation itself is unchanged.

* `mc_auc_ci()`: the argument `probabilities` is now `probs`, the name every
  other function uses. Calls such as `mc_auc_ci(truth, probabilities = p)`
  need to become `mc_auc_ci(truth, probs = p)`; calls that pass the
  probabilities by position, such as `mc_auc_ci(truth, p)`, are unaffected.
  Nothing else in the function changed.

## New features

* `plot()` for `mc_roc` objects: ROC curves for every pair of classes
  (`type = "pairs"`), or a heatmap of the pairwise AUCs
  (`type = "heatmap"`). pROC has no plot method for multiclass objects.
* `plot()` for `mc_auc_ci()` results: a histogram of the bootstrap AUCs with
  the confidence interval shaded and the original AUC marked.
* `plot()` for `mc_auc_compare()` results: the bootstrap distribution of the
  AUC difference (`type = "bootstrap"`), or each class pair's AUC under both
  models (`type = "pairs"`).
* `mc_auc_compare()` also returns `pairwise`, each class pair's AUC under
  both models, which the pairs plot uses.
* New vignette, "Getting started with ProcEx": `vignette("ProcEx")`.

## Documentation and packaging

* README: plotting section, updated comparison and confidence interval
  sections, and test results.
* The Dry Bean zip in `inst/extdata` (4.6 MB, not used by any code) is
  excluded from the built package. The previous `.Rbuildignore` entry did
  not match its location.
* DESCRIPTION: `URL`, `BugReports`, vignette support, and `graphics` and
  `grDevices` in Imports (used by the plots).

## Known issues (not changed in this version)

* `handle_missing()` changes the data but does not recalculate the AUC, so
  `$auc`, `$pairwise`, `$n`, `$n_used` and `$complete` describe the data
  before handling.
* `profile_outliers()` applies the IQR rule to each probability column
  across all rows. With four or more classes this flags most confident,
  correct predictions (77% of Dry Bean test rows), and
  `profile_auc_influence()` and `profile_auc_combinations()` start from
  these flags.
* `mc_auc_ci()` gives a wrong interval when a class has a single row, and
  its `seed` argument resets the caller's random-number stream.
* `mc_auc_compare()` does not run the input checks that `mc_roc()` runs,
  passes missing values to pROC (which drops them separately for each pair
  of classes, without a warning), and its `seed` argument resets the
  caller's random-number stream.
* `validate_classification_data()` requires rows to sum to 1 within 1e-6, so
  probabilities rounded to 3 decimals are rejected by `mc_roc()`.
* Pairwise confidence intervals are not implemented yet.

# ProcEx 0.0.0.9000

* First development version.
