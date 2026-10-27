# Tests for mc_auc_compare()
make_iris_pair <- function() {
  set.seed(1)
  strong <- nnet::multinom(Species ~ Petal.Length + Petal.Width,
                           data = iris, trace = FALSE)
  weak   <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
                           data = iris, trace = FALSE)
  list(
    response = iris$Species,
    strong   = stats::predict(strong, type = "probs"),
    weak     = stats::predict(weak,   type = "probs")
  )
}


test_that("returns a well-formed mc_compare object", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$response, d$strong, d$weak,
                        boot.n = 100, seed = 42)

  expect_s3_class(res, "mc_compare")
  expect_true(res$lower <= res$upper)
  expect_length(res$replicates, res$boot.n)
  expect_equal(res$boot.n + res$n_failed, 100)
  expect_true(res$auc1 >= 0 && res$auc1 <= 1)
  expect_true(res$auc2 >= 0 && res$auc2 <= 1)
  expect_equal(res$difference, res$auc1 - res$auc2)
})


test_that("comparing a model with itself gives exactly zero", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$response, d$strong, d$strong,
                        boot.n = 100, seed = 1)

  expect_true(all(res$replicates == 0))
  expect_equal(res$difference, 0)
  expect_equal(res$lower, 0)
  expect_equal(res$upper, 0)
})


test_that("the same seed reproduces the same result", {
  d <- make_iris_pair()
  a <- mc_auc_compare(d$response, d$strong, d$weak, boot.n = 100, seed = 99)
  b <- mc_auc_compare(d$response, d$strong, d$weak, boot.n = 100, seed = 99)

  expect_identical(a$replicates, b$replicates)
  expect_identical(a$lower, b$lower)
})


test_that("a genuinely better model is detected", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$response, d$strong, d$weak,
                        boot.n = 200, seed = 42)

  expect_gt(res$difference, 0)
  expect_gt(res$lower, 0)
  expect_lt(res$p.value, 0.05)
})


test_that("conf.level widens the interval", {
  d <- make_iris_pair()
  narrow <- mc_auc_compare(d$response, d$strong, d$weak,
                           boot.n = 200, conf.level = 0.80, seed = 7)
  wide   <- mc_auc_compare(d$response, d$strong, d$weak,
                           boot.n = 200, conf.level = 0.99, seed = 7)

  expect_gt(wide$upper - wide$lower, narrow$upper - narrow$lower)
})


test_that("fewer than three classes is rejected", {
  two <- iris$Species != "virginica"
  resp <- droplevels(iris$Species[two])
  probs <- matrix(runif(sum(two) * 2), ncol = 2,
                  dimnames = list(NULL, levels(resp)))

  expect_error(
    mc_auc_compare(resp, probs, probs),
    "at least 3 classes",
    ignore.case = TRUE
  )
})


test_that("mismatched row counts are rejected", {
  d <- make_iris_pair()

  expect_error(
    mc_auc_compare(d$response, d$strong[1:140, ], d$weak),
    "rows but response has"
  )
  expect_error(
    mc_auc_compare(d$response, d$strong, d$weak[1:140, ]),
    "rows but response has"
  )
})


test_that("a non-matrix predictor is rejected", {
  d <- make_iris_pair()

  expect_error(
    mc_auc_compare(d$response, "not a matrix", d$weak)
  )
})


test_that("print method returns its input invisibly", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$response, d$strong, d$weak,
                        boot.n = 100, seed = 42)

  expect_output(print(res), "Multiclass AUC comparison")
  expect_invisible(print(res))
})
