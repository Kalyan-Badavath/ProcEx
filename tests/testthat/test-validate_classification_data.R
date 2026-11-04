# Helper function for creating valid probability matrices
make_test_probs <- function(truth) {

  classes <- sort(unique(as.character(truth)))
  n <- length(truth)
  k <- length(classes)

  # Special case for one observed class
  if (k == 1) {
    probs <- matrix(1, nrow = n, ncol = 1)
    colnames(probs) <- classes
    return(probs)
  }

  probs <- matrix(
    0.2 / (k - 1),
    nrow = n,
    ncol = k,
    dimnames = list(NULL, classes)
  )

  for (i in seq_len(n)) {
    probs[i, as.character(truth[i])] <- 0.8
  }

  probs
}


# Standard valid test data
base_truth <- factor(rep(c("A", "B", "C"), each = 10))
base_probs <- make_test_probs(base_truth)


test_that("valid multiclass data returns VALID", {

  result <- validate_classification_data(
    base_truth,
    base_probs
  )

  expect_identical(result$status, "VALID")
  expect_length(result$errors, 0)
  expect_length(result$concerns, 0)

  expect_identical(result$n_obs, 30L)
  expect_identical(result$n_classes, 3L)
})


test_that("return object has the new API", {

  result <- validate_classification_data(
    base_truth,
    base_probs
  )

  expect_named(
    result,
    c(
      "status",
      "errors",
      "concerns",
      "n_obs",
      "n_classes",
      "class_counts",
      "class_proportions"
    ),
    ignore.order = FALSE
  )

  # Old outputs should no longer exist
  expect_false("valid" %in% names(result))
  expect_false("warnings" %in% names(result))
})


test_that("class counts and proportions are correct", {

  result <- validate_classification_data(
    base_truth,
    base_probs
  )

  expect_equal(
    as.numeric(result$class_counts),
    c(10, 10, 10)
  )

  expect_equal(
    names(result$class_counts),
    c("A", "B", "C")
  )

  expect_equal(
    as.numeric(result$class_proportions),
    rep(1 / 3, 3)
  )
})


test_that("character truth is accepted", {

  result <- validate_classification_data(
    as.character(base_truth),
    base_probs
  )

  expect_identical(result$status, "VALID")
})


test_that("probability data frame is accepted", {

  result <- validate_classification_data(
    base_truth,
    as.data.frame(base_probs)
  )

  expect_identical(result$status, "VALID")
})


test_that("custom concern thresholds are accepted", {

  result <- validate_classification_data(
    base_truth,
    base_probs,
    min_class_n = 5,
    imbalance_ratio = 2
  )

  expect_identical(result$status, "VALID")
})


# concern tests

test_that("small classes produce a concern, not an error", {

  result <- validate_classification_data(
    base_truth,
    base_probs,
    min_class_n = 11
  )

  expect_identical(
    result$status,
    "VALID_WITH_CONCERNS"
  )

  expect_length(result$errors, 0)

  expect_true(
    any(
      grepl(
        "fewer than 11",
        result$concerns
      )
    )
  )
})


test_that("class imbalance produces a concern, not an error", {

  truth <- factor(
    c(
      rep("A", 20),
      rep("B", 10),
      rep("C", 2)
    )
  )

  probs <- make_test_probs(truth)

  result <- validate_classification_data(
    truth,
    probs,
    min_class_n = 1,
    imbalance_ratio = 5
  )

  expect_identical(
    result$status,
    "VALID_WITH_CONCERNS"
  )

  expect_length(result$errors, 0)

  expect_true(
    any(
      grepl(
        "Class imbalance ratio is 10",
        result$concerns
      )
    )
  )
})


test_that("imbalance exactly at threshold does not create concern", {

  truth <- factor(
    c(
      rep("A", 20),
      rep("B", 10),
      rep("C", 2)
    )
  )

  probs <- make_test_probs(truth)

  result <- validate_classification_data(
    truth,
    probs,
    min_class_n = 1,
    imbalance_ratio = 10
  )

  expect_identical(
    result$status,
    "VALID"
  )

  expect_length(
    result$concerns,
    0
  )
})


# two class test
test_that("two classes return NOT_MULTICLASS rather than INVALID", {

  truth <- factor(
    rep(c("A", "B"), each = 10)
  )

  probs <- make_test_probs(truth)

  result <- validate_classification_data(
    truth,
    probs
  )

  expect_identical(
    result$status,
    "NOT_MULTICLASS"
  )

  # Binary classification itself is not malformed
  expect_length(result$errors, 0)

  expect_true(
    any(
      grepl(
        "Only 2 classes were detected",
        result$concerns
      )
    )
  )

  expect_identical(
    result$n_classes,
    2L
  )
})


test_that("one observed class is invalid", {

  truth <- factor(
    rep("A", 10)
  )

  probs <- make_test_probs(truth)

  result <- validate_classification_data(
    truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "At least 2 observed classes",
        result$errors
      )
    )
  )
})



# truth tests

test_that("empty truth is invalid", {

  probs <- matrix(
    numeric(),
    nrow = 0,
    ncol = 0
  )

  result <- validate_classification_data(
    character(),
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "truth cannot be empty",
        result$errors
      )
    )
  )
})


test_that("truth with missing values is invalid", {

  truth <- c(
    "A",
    "B",
    "C",
    NA
  )

  probs <- matrix(
    c(
      0.8, 0.1, 0.1,
      0.1, 0.8, 0.1,
      0.1, 0.1, 0.8,
      0.3, 0.3, 0.4
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(
      NULL,
      c("A", "B", "C")
    )
  )

  result <- validate_classification_data(
    truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "truth cannot contain missing values",
        result$errors
      )
    )
  )
})


test_that("truth must be a vector or factor", {

  truth <- data.frame(
    class = base_truth
  )

  result <- validate_classification_data(
    truth,
    base_probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "truth must be a vector or factor",
        result$errors
      )
    )
  )
})


# --------------------------------------------------
# PROBABILITY STRUCTURE TESTS
# --------------------------------------------------

test_that("probs must be a data frame or matrix", {

  result <- validate_classification_data(
    base_truth,
    as.numeric(base_probs)
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "probs must be a data frame or matrix",
        result$errors
      )
    )
  )
})


test_that("truth and probs must have same number of observations", {

  result <- validate_classification_data(
    base_truth,
    base_probs[-1, ]
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "same number of observations",
        result$errors
      )
    )
  )
})


test_that("probability columns require names", {

  probs <- base_probs

  colnames(probs) <- NULL

  result <- validate_classification_data(
    base_truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "must have class names",
        result$errors
      )
    )
  )
})


test_that("probability column names must be unique", {

  probs <- base_probs

  colnames(probs) <- c(
    "A",
    "A",
    "C"
  )

  result <- validate_classification_data(
    base_truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "must be unique",
        result$errors
      )
    )
  )
})


test_that("probability column names must match observed classes", {

  probs <- base_probs

  colnames(probs) <- c(
    "A",
    "B",
    "D"
  )

  result <- validate_classification_data(
    base_truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "must match the class labels",
        result$errors
      )
    )
  )
})


test_that("all probability columns must be numeric", {

  probs <- as.data.frame(
    base_probs
  )

  probs$B <- as.character(
    probs$B
  )

  result <- validate_classification_data(
    base_truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "must be numeric",
        result$errors
      )
    )
  )
})



# invalida probability values


test_that("NA probability values are invalid", {

  probs <- base_probs

  probs[1, 1] <- NA_real_

  result <- validate_classification_data(
    base_truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "NA, NaN, or Inf",
        result$errors
      )
    )
  )
})


test_that("NaN probability values are invalid", {

  probs <- base_probs

  probs[1, 1] <- NaN

  result <- validate_classification_data(
    base_truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "NA, NaN, or Inf",
        result$errors
      )
    )
  )
})


test_that("infinite probability values are invalid", {

  probs <- base_probs

  probs[1, 1] <- Inf

  result <- validate_classification_data(
    base_truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "NA, NaN, or Inf",
        result$errors
      )
    )
  )
})


test_that("probabilities outside zero to one are invalid", {

  probs <- base_probs

  probs[1, 1] <- 1.1
  probs[1, 2] <- -0.1

  result <- validate_classification_data(
    base_truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "between 0 and 1",
        result$errors
      )
    )
  )
})


test_that("probability rows must sum to one", {

  probs <- base_probs

  probs[1, 1] <- 0.7

  result <- validate_classification_data(
    base_truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    any(
      grepl(
        "must sum to 1",
        result$errors
      )
    )
  )
})


test_that("small floating point differences are accepted", {

  probs <- base_probs

  probs[1, 1] <-
    probs[1, 1] + 5e-7

  result <- validate_classification_data(
    base_truth,
    probs
  )

  expect_identical(
    result$status,
    "VALID"
  )
})

#setting validation

test_that("invalid min_class_n values are rejected", {

  bad_values <- list(
    0,
    -1,
    NA_real_,
    Inf,
    c(1, 2),
    "10"
  )

  for (value in bad_values) {

    result <- validate_classification_data(
      base_truth,
      base_probs,
      min_class_n = value
    )

    expect_identical(
      result$status,
      "INVALID"
    )

    expect_true(
      any(
        grepl(
          "min_class_n",
          result$errors
        )
      )
    )
  }
})


test_that("invalid imbalance_ratio values are rejected", {

  bad_values <- list(
    0,
    -1,
    NA_real_,
    Inf,
    c(1, 2),
    "10"
  )

  for (value in bad_values) {

    result <- validate_classification_data(
      base_truth,
      base_probs,
      imbalance_ratio = value
    )

    expect_identical(
      result$status,
      "INVALID"
    )

    expect_true(
      any(
        grepl(
          "imbalance_ratio",
          result$errors
        )
      )
    )
  }
})


#status priority

test_that("structural errors take priority over concerns", {

  truth <- factor(
    rep(c("A", "B"), each = 10)
  )

  probs <- make_test_probs(truth)

  # Deliberately invalid probability
  probs[1, 1] <- 2

  result <- validate_classification_data(
    truth,
    probs
  )

  expect_identical(
    result$status,
    "INVALID"
  )

  expect_true(
    length(result$errors) > 0
  )
})


test_that("unused factor levels are ignored", {

  truth <- factor(
    rep(
      c("A", "B", "C"),
      each = 10
    ),
    levels = c(
      "A",
      "B",
      "C",
      "D"
    )
  )

  probs <- make_test_probs(truth)

  result <- validate_classification_data(
    truth,
    probs
  )

  expect_identical(
    result$status,
    "VALID"
  )

  expect_identical(
    result$n_classes,
    3L
  )

  expect_false(
    "D" %in%
      names(result$class_counts)
  )
})
