#' Validate multiclass classification data
#'
#' Checks whether true class labels and predicted class probabilities
#' are suitable for multiclass analysis.
#'
#' @param truth A vector or factor containing the true class labels.
#' @param probs A data frame or matrix containing predicted probabilities
#'   for each class.
#' @param min_class_n Minimum number of observations expected in each class.
#' @param imbalance_threshold Minimum class proportion before an imbalance
#'   warning is generated.
#'
#' @return A list containing validation status, errors, warnings,
#'   class counts, and class proportions.
#'
#' @export
validate_classification_data <- function(
    truth,
    probs,
    min_class_n = 10,
    imbalance_threshold = 0.05
) {

  errors <- character()
  warnings <- character()

  # Basic input validation
  if (!is.atomic(truth) || !is.null(dim(truth))) {
    errors <- c(
      errors,
      "truth must be a vector or factor."
    )
  }

  if (!is.data.frame(probs) && !is.matrix(probs)) {
    errors <- c(
      errors,
      "probs must be a data frame or matrix."
    )
  }

  if (is.data.frame(probs) || is.matrix(probs)) {

    if (length(truth) != nrow(probs)) {
      errors <- c(
        errors,
        "truth and probs must contain the same number of observations."
      )
    }

    if (is.data.frame(probs)) {

      numeric_cols <- vapply(
        probs,
        is.numeric,
        logical(1)
      )

      if (!all(numeric_cols)) {
        errors <- c(
          errors,
          "All probability columns must be numeric."
        )
      }
    }

    if (is.matrix(probs) && !is.numeric(probs)) {
      errors <- c(
        errors,
        "All probability values must be numeric."
      )
    }
  }

  # Multiclass validation
  classes <- unique(as.character(truth))

  if (length(classes) < 3) {
    errors <- c(
      errors,
      "At least 3 classes are required for multiclass analysis."
    )
  }

  if (is.data.frame(probs) || is.matrix(probs)) {

    if (is.null(colnames(probs))) {

      errors <- c(
        errors,
        "Probability columns must have class names."
      )

    } else if (
      ncol(probs) != length(classes) ||
      !setequal(colnames(probs), classes)
    ) {

      errors <- c(
        errors,
        "Probability column names must match the class labels."
      )
    }
  }

  # Probability validation
  if (
    (is.data.frame(probs) || is.matrix(probs)) &&
    all(vapply(as.data.frame(probs), is.numeric, logical(1)))
  ) {

    if (any(!is.finite(as.matrix(probs)))) {
      errors <- c(
        errors,
        "Probability values cannot contain NA, NaN, or Inf."
      )
    }

    if (any(probs < 0 | probs > 1, na.rm = TRUE)) {
      errors <- c(
        errors,
        "Probability values must be between 0 and 1."
      )
    }

    row_totals <- rowSums(probs, na.rm = TRUE)

    if (any(abs(row_totals - 1) > 1e-6)) {
      errors <- c(
        errors,
        "Probabilities for each observation must sum to 1."
      )
    }
  }

  # Reliability warnings
  class_counts <- table(truth)
  class_proportions <- prop.table(class_counts)

  if (any(class_counts < min_class_n)) {
    warnings <- c(
      warnings,
      paste0(
        "At least one class has fewer than ",
        min_class_n,
        " observations."
      )
    )
  }

  if (any(class_proportions < imbalance_threshold)) {
    warnings <- c(
      warnings,
      paste0(
        "At least one class represents less than ",
        imbalance_threshold * 100,
        "% of the data."
      )
    )
  }

  # Overall status
  if (length(errors) > 0) {
    status <- "INVALID"
  } else if (length(warnings) > 0) {
    status <- "VALID_WITH_WARNINGS"
  } else {
    status <- "VALID"
  }

  # Return result
  list(
    valid = length(errors) == 0,
    status = status,
    errors = errors,
    warnings = warnings,
    class_counts = class_counts,
    class_proportions = class_proportions
  )
}
