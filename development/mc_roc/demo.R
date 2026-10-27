# demo.R -- showcase for mc_roc() and the shared helpers.
# Run from the package root (the folder with DESCRIPTION), one section at a
# time with Ctrl+Enter. load_all() also exposes the internal helpers.

# ---- 0. Setup ----------------------------------------------------------------
# install.packages(c("devtools", "pROC", "nnet", "testthat"))
devtools::load_all()
library(nnet)   # used to fit a real 3-class model


# ---- 1. A real model: iris, multinomial logistic regression -----------------
# Only the two sepal measurements, so the model is good but not perfect.
set.seed(2026)
train_rows <- sample(nrow(iris), 100)
train <- iris[train_rows, ]
test  <- iris[-train_rows, ]

model <- multinom(Species ~ Sepal.Length + Sepal.Width, data = train,
                  trace = FALSE)
probs <- predict(model, newdata = test, type = "probs")
head(probs)                       # one column per class, named by class

fit <- mc_roc(test$Species, probs, label = "multinom (sepal only)")
fit                               # short readout, not a wall of data
summary(fit)                      # counts, every pairwise AUC, data checks


# ---- 2. Missing values: reported with a %, then handled ---------------------
probs_na <- probs
probs_na[c(3, 10), "versicolor"] <- NA
fit_na <- mc_roc(test$Species, probs_na, label = "with 2 missing rows")
fit_na                            # "48 of 50 (2 rows, 4.0%, ... left out)"

fit_filled <- mc_refit(handle_missing(fit_na, method = "mean"))
fit_filled                        # all 50 rows used, AUC recomputed


# ---- 3. Bad input gets a clear message --------------------------------------
try(mc_roc(test$Species[-1], probs))           # lengths differ
try(mc_roc(test$Species, unname(probs)))       # no column names
bad <- probs
colnames(bad)[3] <- "virginca"                 # typo in a class name
try(mc_roc(test$Species, bad))
keep <- test$Species != "setosa"
try(mc_roc(droplevels(test$Species[keep]), probs[keep, 2:3]))  # only 2 classes


# ---- 4. What teammates receive ----------------------------------------------
names(fit)          # the agreed object structure
fit$auc             # one number
fit$pairwise        # one row per class pair
fit$checks$status   # from validate_classification_data()
names(mc_curves(fit))   # "setosa|versicolor", ... ready for pROC::ggroc()


# ---- 5. Shared bootstrap engine ---------------------------------------------
# Same Hand & Till AUC as pROC, much faster, for bootstrap loops:
c(pROC = fit$auc, fast = .mc_auc_fast(mc_data(fit)$truth, mc_data(fit)$probs))

set.seed(42)
boot <- mc_boot_auc(fit, n_boot = 1000)       # what mc_auc_ci() needs
quantile(boot[, 1], c(0.025, 0.975))

full <- multinom(Species ~ ., data = train, trace = FALSE)
fit_full <- mc_roc(test$Species, predict(full, test, type = "probs"),
                   label = "multinom (all four)")
set.seed(42)
paired <- mc_boot_auc(list(fit_full, fit), n_boot = 1000)  # mc_auc_compare()
quantile(paired[, 1] - paired[, 2], c(0.025, 0.975))


# ---- 6. Proof it's correct: run the tests -----------------------------------
devtools::test(filter = "mc_roc")
