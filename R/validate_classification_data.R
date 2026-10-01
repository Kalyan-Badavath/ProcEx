validate_classification_data <- function(truth, probs) {

  errors <- character()

  # -------------------------
  # Checkpoint 2
  # Basic input validation
  # -------------------------

  # Check truth input
  if (!is.atomic(truth) || !is.null(dim(truth))) {
    errors <- c(
      errors,
      "truth must be a vector or factor."
    )
  }

  # Check probability input
  if (!is.data.frame(probs) && !is.matrix(probs)) {
    errors <- c(
      errors,
      "probs must be a data frame or matrix."
    )
  }

  # Continue only if probs has correct structure
  if (is.data.frame(probs) || is.matrix(probs)) {

    # Check number of observations
    if (length(truth) != nrow(probs)) {
      errors <- c(
        errors,
        "truth and probs must contain the same number of observations."
      )
    }

    # Check numeric probability columns
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

    # Check numeric matrix
    if (is.matrix(probs) && !is.numeric(probs)) {
      errors <- c(
        errors,
        "All probability values must be numeric."
      )
    }
  }

  # -------------------------
  # Checkpoint 3
  # Multiclass validation
  # -------------------------

  classes <- unique(as.character(truth))

  # Must contain at least 3 classes
  if (length(classes) < 3) {
    errors <- c(
      errors,
      "At least 3 classes are required for multiclass analysis."
    )
  }

  # Probability columns must match classes
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

  # -------------------------
  # Return result
  # -------------------------

  list(
    valid = length(errors) == 0,
    errors = errors
  )
}






truth_two <- c("A", "B", "A")

probs_two <- data.frame(
  A = c(0.8, 0.2, 0.7),
  B = c(0.2, 0.8, 0.3)
)

validate_classification_data(truth_two, probs_two)




truth_wrong <- c("A", "B", "C")

probs_wrong <- data.frame(
  A = c(0.8, 0.1, 0.2),
  B = c(0.1, 0.7, 0.2),
  D = c(0.1, 0.2, 0.6)
)

validate_classification_data(truth_wrong, probs_wrong)
