# Integrating `mc_roc()`: notes for the team

`mc_roc()` builds the object that every other ProcEx function receives. This
change lines it up with the code already on `main` and adds a few shared
helpers. **No teammate's file was edited.** Each section below lists what that
function needs so the pieces fit together, with code you can copy.

Checked on R 4.3.3 / pROC 1.18.5:

- **Tests:** `devtools::test()` passes 415/415 expectations, everyone's
  existing tests included.
- **`R CMD check`:** 0 errors. There is 1 NOTE, about `sd` and `quantile` in
  `mc_auc_ci.R` and `profile_outliers.R`. There is also 1 WARNING that comes
  only from the test machine's locale settings.

---

## 1. What changed in `mc_roc()`

| Before | Now | Why |
|---|---|---|
| `mc_roc(response, predictor)` | `mc_roc(truth, probs, label, ...)` | Same names as `validate_classification_data()`, `handle_missing()` and `profile_outliers()` |
| fields `response`, `predictor`, `missing` | `truth`, `probs`, `missing_info` | `handle_missing()` reads and writes exactly these |
| placeholder `check_mc_data()` stub | calls `validate_classification_data()` directly | stub deleted; `...` passes `min_class_n` / `imbalance_ratio` through |
| `boot$ci`, `boot$level` | `boot` = the list `mc_auc_ci()` already returns | `print(fit)` shows the CI as soon as `mc_auc_ci()` fills it |
| — | `R/mc_roc_helpers.R` | shared tools, section 3 |

**How missing values flow now.** `validate_classification_data()` treats any
NA as an error. If `mc_roc()` stopped on NA, an `mc_roc` object could never
hold missing values, and `handle_missing()` would have nothing to work on.
So `mc_roc()` runs the validator on the complete rows only. It keeps every
row in the object, computes the AUC on the complete rows, and warns:

```
2 of 50 rows (4.0%) have missing values and were left out of the AUC.
Use handle_missing() to remove or fill them.
```

The full flow is then `fit |> handle_missing("mean") |> mc_refit()`.

**Unused factor levels are dropped**, matching the validator, which only
counts observed classes.

## 2. The object contract

| Field | Type | Filled by |
|---|---|---|
| `truth` | factor, length n, all rows | `mc_roc()` |
| `probs` | numeric matrix n x K, columns in `levels` order | `mc_roc()` |
| `levels` | character, the K classes | `mc_roc()` |
| `complete`, `n`, `n_used` | rows used for the AUC | `mc_roc()` |
| `class_counts` | named integer | `mc_roc()` |
| `auc` | Hand & Till multiclass AUC (pROC) | `mc_roc()` |
| `pairwise` | data frame: `pair`, `class1`, `class2`, `auc_1v2`, `auc_2v1`, `auc` | `mc_roc()` |
| `roc` | pROC object; `roc$rocs` named `"a vs b"`, curves `"a|b"`, `"b|a"` | `mc_roc()` |
| `label`, `call` | model name, original call | `mc_roc()` |
| `checks` | the list from `validate_classification_data()` | `mc_roc()` |
| `missing_info` | `n_affected`, `pct_affected`, `n_removed`, `n_imputed`, `method` | `handle_missing()` |
| `boot` | `original_auc`, `standard_error`, `lower_ci`, `upper_ci`, `conf_level`, `n_boot`, `n_successful`, `n_failed`, `bootstrap_values` | `mc_auc_ci()` |
| `pROC_version` | character | `mc_roc()` |

## 3. Shared helpers (internal, in `R/mc_roc_helpers.R`)

| Helper | Use it for |
|---|---|
| `mc_refit(x)` | Rebuild after changing `x$truth` / `x$probs`. It reruns the checks and the AUC, keeps `label` and `missing_info`, and clears `boot` |
| `mc_data(x)` | The complete rows: `list(truth, probs)`, with columns aligned |
| `mc_boot_index(truth)` | One stratified resample: row indices drawn within each class |
| `mc_boot_auc(x, n_boot)` | The bootstrap loop. Pass one object for a CI, or `list(x1, x2)` for a paired comparison. Returns an `n_boot x models` matrix; a failed replicate is `NA` |
| `.mc_auc_fast(truth, probs)` | Same AUC as pROC, computed by ranks; tests check it matches to 1e-12, ties included |
| `mc_curves(x)` | Flat list of curves named `"a|b"`, ready for `pROC::ggroc()` |

**Speed** (1000 replicates, n = 300, 4 classes):

| | Current loop | `mc_boot_auc()` |
|---|---|---|
| CI | 3.8 s | 0.4 s |
| paired comparison | 7.7 s | 0.7 s |

At 500 simulated datasets x 1000 replicates, that is about 3 minutes per
scenario instead of about 30.

---

## 4. What each function needs

### `handle_missing()`

- **It now works on real `mc_roc` objects**, because the field names match.
  `test-mc_roc.R` covers this ("mc_refit() rebuilds after handle_missing()").
- **Add `mc_refit()` on the way out.** Otherwise `x$auc` and `x$pairwise`
  still describe the old data. Change both `return(x)` lines (after "remove"
  and at the end) to:
  ```r
  return(mc_refit(x))
  ```
  The no-missing-values branch can stay `return(x)`.
- **Test fixtures will warn.** I tried this change: all tests still pass, but
  the 3–5 row fixtures now raise the validator's "fewer than 10
  observations" warning. Either build fixtures with 10 or more rows per
  class, or wrap the call in `suppressWarnings()` where that warning isn't
  under test.
- **The percentage is already reported** (`pct_affected`). `print(fit)` and
  `summary(fit)` show it.

### `validate_classification_data()` (the plan calls it `check_mc_data()`)

- **`mc_roc()` calls it directly** (`.mc_check()` in `R/mc_roc.R`). If you
  rename it, change that one call.
- **The summary table at the top isn't there yet.** If you add a data frame
  to the returned list as `$table` (e.g. columns `check`, `status`,
  `detail`), `summary(fit)` prints it automatically.
- **The rows-sum-to-1 check is very strict.** At a tolerance of 1e-6 it
  rejects rounded probabilities (0.33 / 0.33 / 0.33) and models that output
  scores. Hand & Till only needs the ranks within each column, so consider
  making this a warning or loosening the tolerance. Your call.

### `mc_auc_ci()`

- **`R/mc_auc_ci.R` is a script, not a package function.** Its demo and
  `try()` tests run every time the package loads, roughly 4,000 bootstrap
  fits during install, `load_all()` and `test()`. To fix it:
  1. Move the demo to `development/mc_auc_ci/`.
  2. Turn the checks into `tests/testthat/test-mc_auc_ci.R`.
  3. Remove `library(pROC)`.
  4. Add roxygen and `@export`.
  5. Write `stats::sd` and `stats::quantile` (this fixes the check NOTE).
- **Take the object and return it**, so `fit |> mc_auc_ci()` works and
  `print()` shows the CI:
  ```r
  mc_auc_ci <- function(x, n_boot = 1000, conf_level = 0.95, seed = NULL) {
    if (!inherits(x, "mc_roc")) stop("x must be an object of class 'mc_roc'.")
    if (x$n_used < x$n) stop("Missing values detected. Run handle_missing() first.")
    # ... keep your n_boot / conf_level checks ...
    if (!is.null(seed)) set.seed(seed)
    vals <- mc_boot_auc(x, n_boot)[, 1]
    ok <- vals[!is.na(vals)]
    if (length(ok) < 2) stop("Too few successful bootstrap replicates ...")
    alpha <- 1 - conf_level
    ci <- stats::quantile(ok, c(alpha / 2, 1 - alpha / 2), names = FALSE)
    x$boot <- list(original_auc = x$auc, standard_error = stats::sd(ok),
                   lower_ci = ci[1], upper_ci = ci[2], conf_level = conf_level,
                   n_boot = n_boot, n_successful = length(ok),
                   n_failed = n_boot - length(ok), bootstrap_values = ok)
    x
  }
  ```
- **Bug in the current resampling.** `sample(class_index, size =
  length(class_index), replace = TRUE)` misbehaves when a class has exactly
  one row. If that row is number 5, `sample(5, 1)` draws from rows 1–5.
  `mc_boot_index()` uses `sample.int()` to avoid this, and a test covers it.
- **Coverage study.** `sim_mc(n, k, signal, weights)` in
  `tests/testthat/helper-sim.R` already varies sample size, number of
  classes and imbalance (`weights`). For the "true" AUC, use `hand_till()`
  on one very large simulated sample with the same settings.

### `mc_auc_compare()`

- **Take two objects:** `mc_auc_compare(x1, x2, n_boot, conf_level, seed)`.
  The paired loop becomes:
  ```r
  b <- mc_boot_auc(list(x1, x2), n_boot)   # same rows for both models
  diffs <- b[, 1] - b[, 2]
  ```
  `mc_boot_auc()` already refuses models scored on different observations
  or classes. Use `x1$label` and `x2$label` in `print.mc_compare()` instead
  of "Model 1" / "Model 2".
- **The same `sample()` single-row bug is present here.**
- **Calibration.** The current check is 2 x 60 runs, but the plan asks for
  several hundred. At about 0.7 s per comparison, 500 runs takes roughly
  6 minutes.
- **Argument names.** `boot.n` / `conf.level` here vs `n_boot` / `conf_level`
  in `mc_auc_ci()`. Pick one style for the package.

### `plot()`

- **`pROC::ggroc(fit$roc)` errors**, because there is no method for
  multiclass objects. Use `pROC::ggroc(mc_curves(fit))`; the legend then
  reads `"setosa|versicolor"` and so on.
- **The bootstrap histogram** comes from `fit$boot$bootstrap_values`.
- **Add `ggplot2` to `DESCRIPTION`.**

### `profile_outliers()` / `profile_auc_influence()`

- **No roxygen or `@export` yet**, so users can't call them.
- **The leave-one-out loop calls pROC once per flagged row.**
  `.mc_auc_fast()` gives the same number, faster.

---

## 5. Package housekeeping (done in this change)

- **Resolved the merge-conflict markers** in `DESCRIPTION` and `NAMESPACE`.
  `main` could not build.
- **Added `Imports: pROC, stats, utils` and `Suggests: nnet`.**
  `mc_auc_compare()`'s example uses nnet.
- **Regenerated `NAMESPACE` and added `man/mc_roc.Rd`.**

Still to do:

- **Fill in the placeholders** in `DESCRIPTION`: `Title`, `Description` and
  `Authors@R`.
- **Run `devtools::document()` after uploading files through GitHub's web
  page.** Uploading skips it, so `NAMESPACE` and `man/` go stale.
- **Add `README.md` and a vignette**, both listed in `PACKAGE_PLAN.md`.
