#' Handle missing values in multiclass ROC data
#'
#' Detects observations affected by missing values in an `mc_roc` object.
#' Missing observations can either be removed or missing prediction values
#' can be filled using a selected method.
#'
#' Missing true class labels are not imputed because this would require
#' guessing the observed class. These observations are removed.
#'
#' The function also reports the number and percentage of observations
#' affected by missing values.
#'
#' @param x An object of class `mc_roc`.
#' @param method Method used to handle missing values. One of
#'   `"remove"`, `"mean"`, or `"median"`.
#'
#' @return An updated `mc_roc` object containing the cleaned data and
#'   information about the missing observations.
#'
#' @export
handle_missing <- function(
    x,
    method = c("remove", "mean", "median")
) {

  method <- match.arg(method)

  # Check input object

  if (!inherits(x, "mc_roc")) {
    stop("x must be an object of class 'mc_roc'.")
  }

  # Check required components

  if (is.null(x$truth) || is.null(x$probs)) {
    stop("x must contain 'truth' and 'probs' components.")
  }

  # Check probability structure

  if (!is.data.frame(x$probs) && !is.matrix(x$probs)) {
    stop("x$probs must be a data frame or matrix.")
  }

  if (length(x$truth) != nrow(x$probs)) {
    stop(
      "x$truth and x$probs must contain the same number of observations."
    )
  }

  # Probability columns must be numeric for imputation

  numeric_cols <- vapply(
    as.data.frame(x$probs),
    is.numeric,
    logical(1)
  )

  if (!all(numeric_cols)) {
    stop("All probability columns must be numeric.")
  }

  # Detect observations affected by missing values

  truth_missing <- is.na(x$truth)

  probs_missing <- apply(
    is.na(x$probs),
    1,
    any
  )

  affected <- truth_missing | probs_missing

  n_affected <- sum(affected)

  pct_affected <- if (length(affected) > 0) {
    100 * n_affected / length(affected)
  } else {
    0
  }

  # Return unchanged object if there are no missing values

  if (n_affected == 0) {

    x$missing_info <- list(
      n_affected = 0L,
      pct_affected = 0,
      n_removed = 0L,
      n_imputed = 0L,
      method = method
    )

    return(x)
  }

  # Remove all incomplete observations

  if (method == "remove") {

    warning(
      paste0(
        n_affected,
        " observation(s) containing missing values were removed (",
        round(pct_affected, 2),
        "% of the data)."
      )
    )

    x$truth <- x$truth[!affected]

    x$probs <- x$probs[
      !affected,
      ,
      drop = FALSE
    ]

    x$missing_info <- list(
      n_affected = n_affected,
      pct_affected = pct_affected,
      n_removed = n_affected,
      n_imputed = 0L,
      method = method
    )

    return(x)
  }

  # Mean or median imputation

  # Missing true labels cannot be safely imputed.
  # Remove those observations first.

  n_truth_removed <- sum(truth_missing)

  if (n_truth_removed > 0) {

    warning(
      paste0(
        n_truth_removed,
        " observation(s) with missing true class labels were removed."
      )
    )

    keep <- !truth_missing

    x$truth <- x$truth[keep]

    x$probs <- x$probs[
      keep,
      ,
      drop = FALSE
    ]
  }

  # Count rows that still contain missing probabilities

  probs_missing_after <- apply(
    is.na(x$probs),
    1,
    any
  )

  n_imputed <- sum(probs_missing_after)

  # Impute each probability column separately

  if (n_imputed > 0) {

    for (j in seq_len(ncol(x$probs))) {

      missing_j <- is.na(x$probs[, j])

      if (any(missing_j)) {

        observed_values <- x$probs[!missing_j, j]

        if (length(observed_values) == 0) {
          stop(
            paste0(
              "Cannot impute probability column ",
              j,
              " because all values are missing."
            )
          )
        }

        fill_value <- if (method == "mean") {
          mean(observed_values)
        } else {
          median(observed_values)
        }

        x$probs[missing_j, j] <- fill_value
      }
    }

    # Renormalise affected probability rows so that probabilities sum to 1

    rows_to_normalise <- which(probs_missing_after)

    row_totals <- rowSums(
      x$probs[
        rows_to_normalise,
        ,
        drop = FALSE
      ]
    )

    if (any(!is.finite(row_totals)) || any(row_totals <= 0)) {
      stop(
        "Imputed probabilities could not be normalised."
      )
    }

    x$probs[
      rows_to_normalise,
    ] <-
      x$probs[
        rows_to_normalise,
        ,
        drop = FALSE
      ] / row_totals

    warning(
      paste0(
        n_imputed,
        " observation(s) had missing probability values filled using ",
        method,
        " imputation."
      )
    )
  }

  # Store missing-data information

  x$missing_info <- list(
    n_affected = n_affected,
    pct_affected = pct_affected,
    n_removed = n_truth_removed,
    n_imputed = n_imputed,
    method = method
  )

  return(x)
}
