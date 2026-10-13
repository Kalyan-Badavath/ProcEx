#' Validate multiclass classification data
#'
#' Checks whether true class labels and predicted class probabilities
#' are suitable for multiclass classification analysis.
#'
#' @param truth A vector or factor containing the true class labels.
#' @param probs A data frame or matrix containing predicted probabilities
#'   for each class.
#' @param min_class_n Minimum number of observations expected in each class.
#'   Classes below this value generate a warning.
#' @param imbalance_ratio Maximum allowed ratio between the largest and
#'   smallest class. Ratios above this value generate a warning.
#'
#' @return A list containing the validation status, errors, warnings,
#'   number of observations, number of classes, class counts, and
#'   class proportions.
#'
#' @export
validate_classification_data <- function(
    truth,
    probs,
    min_class_n = 10,
    imbalance_ratio = 10
) {

  errors <- character()
  warnings <- character()

  # Validate function settings

  if (
    length(min_class_n) != 1 ||
    !is.numeric(min_class_n) ||
    !is.finite(min_class_n) ||
    min_class_n < 1
  ) {
    errors <- c(
      errors,
      "min_class_n must be a single positive numeric value."
    )
  }

  if (
    length(imbalance_ratio) != 1 ||
    !is.numeric(imbalance_ratio) ||
    !is.finite(imbalance_ratio) ||
    imbalance_ratio < 1
  ) {
    errors <- c(
      errors,
      "imbalance_ratio must be a single numeric value greater than or equal to 1."
    )
  }

  # Validate truth

  truth_valid_structure <-
    (is.atomic(truth) || is.factor(truth)) &&
    is.null(dim(truth))

  if (!truth_valid_structure) {

    errors <- c(
      errors,
      "truth must be a vector or factor."
    )

    classes <- character()
    class_counts <- integer()
    class_proportions <- numeric()

  } else {

    if (length(truth) == 0) {
      errors <- c(
        errors,
        "truth cannot be empty."
      )
    }

    if (anyNA(truth)) {
      errors <- c(
        errors,
        "truth cannot contain missing values."
      )
    }

    valid_truth <- truth[!is.na(truth)]

    # Only classes actually observed in the data
    classes <- unique(as.character(valid_truth))

    if (length(classes) < 3) {
      errors <- c(
        errors,
        "At least 3 classes are required for multiclass analysis."
      )
    }

    # Count only observed classes
    class_counts <- table(as.character(valid_truth))

    if (length(class_counts) > 0) {
      class_proportions <- prop.table(class_counts)
    } else {
      class_proportions <- numeric()
    }
  }

  # Validate probability data structure

  probs_valid_structure <-
    is.data.frame(probs) || is.matrix(probs)

  if (!probs_valid_structure) {

    errors <- c(
      errors,
      "probs must be a data frame or matrix."
    )

  } else {

    if (nrow(probs) == 0) {
      errors <- c(
        errors,
        "probs cannot be empty."
      )
    }

    if (
      truth_valid_structure &&
      length(truth) != nrow(probs)
    ) {
      errors <- c(
        errors,
        "truth and probs must contain the same number of observations."
      )
    }

    # Check probability column names
    if (is.null(colnames(probs))) {

      errors <- c(
        errors,
        "Probability columns must have class names."
      )

    } else {

      if (anyDuplicated(colnames(probs)) > 0) {
        errors <- c(
          errors,
          "Probability column names must be unique."
        )
      }

      if (
        truth_valid_structure &&
        length(classes) > 0 &&
        (
          ncol(probs) != length(classes) ||
          !setequal(colnames(probs), classes)
        )
      ) {
        errors <- c(
          errors,
          "Probability column names must match the class labels."
        )
      }
    }

    # Check that all probability columns are numeric
    numeric_cols <- vapply(
      as.data.frame(probs),
      is.numeric,
      logical(1)
    )

    if (!all(numeric_cols)) {

      errors <- c(
        errors,
        "All probability columns must be numeric."
      )

    } else {

      probs_matrix <- as.matrix(probs)

      # Missing / infinite values
      if (any(!is.finite(probs_matrix))) {

        errors <- c(
          errors,
          "Probability values cannot contain NA, NaN, or Inf."
        )

      } else {

        # Probability range
        if (any(probs_matrix < 0 | probs_matrix > 1)) {
          errors <- c(
            errors,
            "Probability values must be between 0 and 1."
          )
        }

        # Probability rows should sum to 1
        row_totals <- rowSums(probs_matrix)

        if (any(abs(row_totals - 1) > 1e-6)) {
          errors <- c(
            errors,
            "Probabilities for each observation must sum to 1."
          )
        }
      }
    }
  }

  # Reliability warnings

  # Small class warning
  if (
    length(class_counts) > 0 &&
    is.numeric(min_class_n) &&
    length(min_class_n) == 1 &&
    is.finite(min_class_n) &&
    min_class_n >= 1
  ) {

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
  }

  # Class imbalance warning
  if (
    length(class_counts) > 0 &&
    is.numeric(imbalance_ratio) &&
    length(imbalance_ratio) == 1 &&
    is.finite(imbalance_ratio) &&
    imbalance_ratio >= 1
  ) {

    observed_ratio <-
      max(as.numeric(class_counts)) /
      min(as.numeric(class_counts))

    if (observed_ratio > imbalance_ratio) {
      warnings <- c(
        warnings,
        paste0(
          "Class imbalance ratio is ",
          round(observed_ratio, 2),
          ", which exceeds the threshold of ",
          imbalance_ratio,
          "."
        )
      )
    }
  }

  # Overall status

  errors <- unique(errors)
  warnings <- unique(warnings)

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
    n_obs = if (truth_valid_structure) length(truth) else NA_integer_,
    n_classes = length(classes),
    class_counts = class_counts,
    class_proportions = class_proportions
  )
}
