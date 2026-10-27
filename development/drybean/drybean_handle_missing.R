# drybean_handle_missing.R -- prototype evidence on real data
#
# Workflow (agreed with the team):
#   Dry Bean dataset -> train a multiclass model -> probability predictions
#   -> mc_roc object -> introduce controlled missing probabilities
#   -> handle_missing() -> compare remove / mean / median
#
# Run from the package root (the folder with DESCRIPTION):
#   source("development/drybean/drybean_handle_missing.R")
# Takes about 3-4 minutes; section 4 is the slow part. Outputs go to
# development/drybean/results/.
#
# Data: Koklu, M. & Ozkan, I. A. (2020). Multiclass classification of dry
# beans using computer vision and machine learning techniques. Computers and
# Electronics in Agriculture, 174, 105507. UCI Machine Learning Repository.

# ---- 0. Setup ----------------------------------------------------------------
# install.packages(c("devtools", "pROC", "nnet", "readxl", "ggplot2"))
devtools::load_all(quiet = TRUE)
library(nnet)
library(ggplot2)

out_dir <- "development/drybean/results"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# handle_missing() changes x$truth / x$probs but does not recompute the AUC
# yet, so every call is followed by mc_refit(). If handle_missing() later
# calls mc_refit() itself, this extra call is harmless (same result).
clean <- function(fit, method) {
  suppressWarnings(mc_refit(handle_missing(fit, method = method)))
}


# ---- 1. Data and model ------------------------------------------------------
beans <- as.data.frame(readxl::read_excel(
  "development/drybean/data/Dry_Bean_Dataset.xlsx"))
beans$Class <- factor(beans$Class)
features <- setdiff(names(beans), "Class")

dim(beans)            # 13,611 beans, 16 shape features + class
table(beans$Class)    # 7 classes, from 522 (BOMBAY) to 3,546 (DERMASON)

# Stratified 70/30 split: same class mix in training and test sets.
set.seed(2026)
test_rows <- unlist(lapply(split(seq_len(nrow(beans)), beans$Class),
                           function(i) i[sample.int(length(i), round(0.3 * length(i)))]))
train <- beans[-test_rows, ]
test  <- beans[test_rows, ]

# Standardise with the training means/SDs (features differ by 10^7 in scale).
mu <- colMeans(train[features])
sdv <- apply(train[features], 2, stats::sd)
train[features] <- scale(train[features], mu, sdv)
test[features]  <- scale(test[features], mu, sdv)

model <- multinom(Class ~ ., data = train, maxit = 1000, trace = FALSE)
stopifnot(model$convergence == 0)

probs <- predict(model, newdata = test, type = "probs")
accuracy <- mean(colnames(probs)[max.col(probs)] == test$Class)
round(accuracy, 3)    # ~0.92, in line with the published results


# ---- 2. The mc_roc object on complete data ----------------------------------
fit <- mc_roc(test$Class, probs, label = "multinom, Dry Bean test set")
fit
summary(fit)

auc_full <- fit$auc   # the reference: what every method should recover


# ---- 3. One worked example: 5% of rows lose one probability -----------------
# For each chosen row, one of its 7 probabilities is set to NA (random column).
add_missing <- function(probs, rate, rows_from = c("random", "uncertain")) {
  rows_from <- match.arg(rows_from)
  n <- nrow(probs)
  n_miss <- round(rate * n)
  w <- if (rows_from == "random") {
    rep(1, n)
  } else {
    # Missing values concentrate on rows the model is unsure about:
    # weight = 1 - largest probability (+ small floor so every row can be hit).
    1 - apply(probs, 1, max) + 0.01
  }
  rows <- sample.int(n, n_miss, prob = w)
  cols <- sample.int(ncol(probs), n_miss, replace = TRUE)
  probs[cbind(rows, cols)] <- NA
  probs
}

set.seed(1)
probs_na <- add_missing(probs, rate = 0.05)
fit_na <- mc_roc(test$Class, probs_na, label = "5% missing")   # warns: count + %
fit_na

# handle_missing() reports what it did, with the percentage affected:
fit_remove <- mc_refit(handle_missing(fit_na, method = "remove"))
fit_mean   <- mc_refit(handle_missing(fit_na, method = "mean"))
fit_median <- mc_refit(handle_missing(fit_na, method = "median"))

# The handled objects are complete mc_roc objects that print normally:
fit_mean
fit_mean$missing_info

example <- data.frame(
  version   = c("complete data", "remove", "mean", "median"),
  rows_used = c(fit$n_used, fit_remove$n_used, fit_mean$n_used, fit_median$n_used),
  auc       = c(auc_full, fit_remove$auc, fit_mean$auc, fit_median$auc)
)
example$difference <- example$auc - auc_full
example


# ---- 4. Controlled study: rates x mechanisms x 100 repetitions --------------
rates      <- c(0.01, 0.05, 0.10, 0.20)
mechanisms <- c("random", "uncertain")
methods    <- c("remove", "mean", "median")
n_rep      <- 100

grid <- expand.grid(rep = seq_len(n_rep), rate = rates,
                    mechanism = mechanisms, stringsAsFactors = FALSE)

set.seed(2026)
t_start <- Sys.time()
runs <- lapply(seq_len(nrow(grid)), function(g) {
  r <- grid[g, ]
  p_na <- add_missing(probs, r$rate, r$mechanism)
  f_na <- suppressWarnings(mc_roc(test$Class, p_na))
  do.call(rbind, lapply(methods, function(m) {
    f <- clean(f_na, m)
    data.frame(mechanism = r$mechanism, rate = r$rate, rep = r$rep,
               method = m, n_affected = f$missing_info$n_affected,
               pct_affected = f$missing_info$pct_affected,
               rows_used = f$n_used, auc = f$auc,
               error = f$auc - auc_full,
               # sanity: "remove" must equal the complete-case AUC mc_roc
               # already reported before handle_missing() ran
               matches_mc_roc = if (m == "remove") isTRUE(all.equal(f$auc, f_na$auc)) else NA)
  }))
})
runs <- do.call(rbind, runs)
elapsed <- difftime(Sys.time(), t_start, units = "mins")
round(elapsed, 1)

stopifnot(all(runs$matches_mc_roc, na.rm = TRUE))
write.csv(runs, file.path(out_dir, "drybean_missing_runs.csv"), row.names = FALSE)


# ---- 5. Summary table -------------------------------------------------------
summ <- do.call(rbind, lapply(split(runs, list(runs$mechanism, runs$rate, runs$method)),
  function(d) data.frame(
    mechanism   = d$mechanism[1],
    rate_pct    = 100 * d$rate[1],
    method      = d$method[1],
    rows_used   = round(mean(d$rows_used)),
    mean_error  = mean(d$error),
    sd_error    = stats::sd(d$error),
    rmse        = sqrt(mean(d$error^2)),
    max_abs_err = max(abs(d$error)),
    lo_2.5      = stats::quantile(d$error, 0.025, names = FALSE),
    hi_97.5     = stats::quantile(d$error, 0.975, names = FALSE))))
summ$method <- factor(summ$method, methods)
summ <- summ[order(summ$mechanism, summ$rate_pct, summ$method), ]
rownames(summ) <- NULL
write.csv(summ, file.path(out_dir, "drybean_missing_summary.csv"), row.names = FALSE)

print(transform(summ, mean_error = signif(mean_error, 2), sd_error = signif(sd_error, 2),
                rmse = signif(rmse, 2), max_abs_err = signif(max_abs_err, 2),
                lo_2.5 = signif(lo_2.5, 2), hi_97.5 = signif(hi_97.5, 2)),
      row.names = FALSE)


# ---- 6. Figure --------------------------------------------------------------
# Error = AUC after handling - AUC on the complete data (0 = perfect recovery).
# Line = mean over 100 runs; band = middle 95% of runs.
cols <- c(remove = "#2a78d6", mean = "#eb6834", median = "#1baf7a")
mech_lab <- c(random = "Missing at random rows",
              uncertain = "Missing on rows the model is unsure about")
plot_df <- summ
plot_df$panel <- factor(mech_lab[plot_df$mechanism], mech_lab)
label_df <- plot_df[plot_df$rate_pct == max(rates) * 100, ]

p <- ggplot(plot_df, aes(rate_pct, mean_error, colour = method, fill = method)) +
  geom_hline(yintercept = 0, colour = "#8a8984", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo_2.5, ymax = hi_97.5), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.6, stroke = 1.2, shape = 21, colour = "#fcfcfb") +
  geom_text(data = label_df, aes(label = method), hjust = 0, nudge_x = 0.6,
            size = 3.6, colour = "#52514e", show.legend = FALSE) +
  facet_wrap(~ panel, nrow = 1) +
  scale_colour_manual(values = cols, name = "handle_missing() method") +
  scale_fill_manual(values = cols, name = "handle_missing() method") +
  scale_x_continuous(breaks = rates * 100, labels = paste0(rates * 100, "%"),
                     expand = expansion(mult = c(0.04, 0.16))) +
  labs(x = "Rows with a missing probability",
       y = "AUC error (handled - complete data)",
       title = "How well each method recovers the complete-data AUC",
       subtitle = sprintf(paste0("Dry Bean test set: %s beans, 7 classes, multinomial model. ",
                                 "Complete-data AUC = %.4f. %d runs per point; band = middle 95%%."),
                          format(nrow(test), big.mark = ","), auc_full, n_rep)) +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e4e3df", linewidth = 0.3),
        strip.text = element_text(face = "bold", colour = "#0b0b0b", hjust = 0),
        axis.text = element_text(colour = "#52514e"),
        axis.title = element_text(colour = "#52514e"),
        plot.title = element_text(face = "bold", colour = "#0b0b0b"),
        plot.subtitle = element_text(colour = "#52514e", size = 9.5),
        legend.position = "top", legend.justification = "left",
        legend.title = element_text(colour = "#52514e", size = 9.5),
        legend.text = element_text(colour = "#0b0b0b"))

ggsave(file.path(out_dir, "drybean_missing_error.png"), p,
       width = 10, height = 4.8, dpi = 200, bg = "#fcfcfb")


# ---- 7. Numbers for the write-up --------------------------------------------
pick <- function(mech, rate, m) summ[summ$mechanism == mech & summ$rate_pct == rate &
                                     summ$method == m, ]
cat(sprintf("\nTest set: %d beans, accuracy %.1f%%, complete-data AUC %.4f\n",
            nrow(test), 100 * accuracy, auc_full))
for (mech in mechanisms) for (m in methods) {
  s <- pick(mech, 20, m)
  cat(sprintf("%-9s 20%% %-6s mean error %+.5f  95%% of runs within [%+.5f, %+.5f]\n",
              mech, m, s$mean_error, s$lo_2.5, s$hi_97.5))
}
