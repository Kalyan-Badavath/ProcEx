# Plot methods for mc_roc, mc_auc_ci and mc_auc_compare objects, using base
# graphics.

# Colours used by the plots.
.mc_col <- list(
  model1    = "#2a78d6",   # first curve or model (blue)
  model2    = "#eb6834",   # second curve or model (orange)
  ink       = "#0b0b0b",   # main text
  ink2      = "#52514e",   # secondary text
  muted     = "#8a8984",   # axes and reference lines
  grid      = "#e3e2de",   # grid lines
  band      = "#cde2fb",   # interval band
  neutral   = "#f0efec"    # AUC of 0.5 in the heatmap
)

# Diverging ramp for AUC: red below 0.5 (worse than chance), grey at 0.5,
# blue toward 1. Equal steps on each arm.
.mc_auc_colours <- function(n = 101) {
  half <- (n - 1) / 2
  low  <- grDevices::colorRampPalette(c("#9e2424", "#e34948", "#f4b8b0",
                                        .mc_col$neutral))(half + 1)
  high <- grDevices::colorRampPalette(c(.mc_col$neutral, "#9ec5f4",
                                        "#2a78d6", "#104281"))(half + 1)
  c(low, high[-1])
}

# Dark fills get white text, light fills get primary ink.
.mc_text_on <- function(fill) {
  rgb <- grDevices::col2rgb(fill) / 255
  lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
  ifelse(lum < 0.45, "#ffffff", .mc_col$ink)
}

# Margin (in text lines) needed for the longest label, before a plot exists.
.mc_label_lines <- function(labels, cex = 0.85) {
  max(nchar(as.character(labels)), 1) * 0.55 * cex + 1.2
}

.mc_check_choice <- function(type, choices, what = "type") {
  if (!is.character(type) || length(type) < 1 || !type[1] %in% choices) {
    stop(what, " must be one of: ",
         paste0('"', choices, '"', collapse = ", "), ".", call. = FALSE)
  }
  type[1]
}

#' Plot a multiclass ROC object
#'
#' Draws the pairwise ROC curves behind the Hand & Till (2001) multiclass
#' AUC, or a heatmap of the pairwise AUCs. pROC has no plot method for
#' multiclass objects; this fills that gap.
#'
#' @details
#' The Hand & Till AUC averages one AUC per pair of classes. Each pair has
#' two ROC curves: one ranks the rows of the two classes by the predicted
#' probability of the first class, the other by the probability of the
#' second. The pair's AUC is the mean of the two, and the multiclass AUC is
#' the mean over all pairs, so a single weak pair can hide behind a high
#' overall value.
#'
#' * `type = "pairs"` (default) draws one panel per class pair with both
#'   curves and their AUCs. With more than 25 pairs (more than 7 classes)
#'   only the 25 pairs with the lowest AUC are drawn, with a message; choose
#'   others with `pairs`.
#' * `type = "heatmap"` shows every pair's AUC in a class-by-class grid.
#'   Use it when there are many classes. With `scale = "auto"` (default) and
#'   every pair at or above 0.5, cells run from light blue (the lowest pair
#'   AUC) to dark blue (1), so the weakest pairs stand out even when all AUCs
#'   are high. If any pair is below 0.5, or with `scale = "full"`, the scale
#'   is fixed at 0 to 1: red below 0.5 (worse than chance), grey at 0.5,
#'   blue toward 1. The colour key shows the range used.
#'
#' Only the rows used for the AUC (complete rows) are plotted.
#'
#' @param x An `mc_roc` object from [mc_roc()].
#' @param type `"pairs"` for ROC curves per class pair, or `"heatmap"` for a
#'   grid of pairwise AUCs.
#' @param pairs For `type = "pairs"`: which pairs to draw, as row numbers of
#'   `x$pairwise` or pair names such as `"setosa vs virginica"`. `NULL`
#'   (default) draws all pairs, up to 25.
#' @param scale For `type = "heatmap"`: `"auto"` (default) or `"full"`; see
#'   Details.
#' @param main Optional overall title.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @references Hand, D. J. & Till, R. J. (2001). A simple generalisation of
#'   the area under the ROC curve for multiple class classification problems.
#'   Machine Learning, 45, 171-186.
#'
#' @examples
#' set.seed(1)
#' truth  <- factor(sample(c("a", "b", "c"), 150, replace = TRUE))
#' scores <- matrix(stats::rnorm(450), 150, 3) +
#'   1.5 * outer(as.integer(truth), 1:3, "==")
#' probs  <- exp(scores) / rowSums(exp(scores))
#' colnames(probs) <- levels(truth)
#' fit <- mc_roc(truth, probs, label = "toy model")
#'
#' plot(fit)
#' plot(fit, type = "heatmap")
#' plot(fit, pairs = "a vs c")
#'
#' @seealso [mc_roc()], [plot.mc_auc_compare()]
#' @export
plot.mc_roc <- function(x, type = c("pairs", "heatmap"), pairs = NULL,
                        scale = c("auto", "full"), main = NULL, ...) {
  .mc_stop_if_not_fit(x)
  type  <- .mc_check_choice(type, c("pairs", "heatmap"))
  scale <- .mc_check_choice(scale, c("auto", "full"), "scale")

  if (is.null(main)) {
    main <- sprintf("%sMulticlass AUC %.3f (Hand & Till)",
                    if (is.null(x$label)) "" else paste0(x$label, ": "),
                    x$auc)
  }

  if (type == "heatmap") {
    if (!is.null(pairs)) {
      warning("pairs is ignored when type = \"heatmap\".", call. = FALSE)
    }
    .mc_plot_heatmap(x, main, scale)
  } else {
    .mc_plot_pairs(x, .mc_select_pairs(x$pairwise, pairs), main)
  }
  invisible(x)
}

# Resolve `pairs` to row numbers of the pairwise table.
.mc_select_pairs <- function(pw, pairs, max_panels = 25L) {
  n <- nrow(pw)
  if (is.null(pairs)) {
    if (n <= max_panels) return(seq_len(n))
    message("Showing the ", max_panels, " lowest-AUC of ", n,
            " class pairs. Use pairs = to choose others, or ",
            "type = \"heatmap\" to see every pair.")
    return(sort(order(pw$auc)[seq_len(max_panels)]))
  }
  if (is.numeric(pairs)) {
    if (anyNA(pairs) || any(pairs != round(pairs)) ||
        any(pairs < 1) || any(pairs > n)) {
      stop("pairs must be row numbers between 1 and ", n, ".", call. = FALSE)
    }
    return(unique(as.integer(pairs)))
  }
  if (is.character(pairs)) {
    idx <- match(pairs, pw$pair)
    if (anyNA(idx)) {
      stop("Unknown pair(s): ", paste(pairs[is.na(idx)], collapse = ", "),
           ". Available: ", paste(pw$pair, collapse = ", "), ".",
           call. = FALSE)
    }
    return(unique(idx))
  }
  stop("pairs must be NULL, row numbers or pair names.", call. = FALSE)
}

.mc_plot_pairs <- function(x, idx, main) {
  pw <- x$pairwise
  n_panels <- length(idx)
  layout_rc <- rev(grDevices::n2mfrow(n_panels))   # wide rather than tall

  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mfrow = layout_rc, pty = "s", mar = c(2.6, 2.6, 2.6, 0.8),
                oma = c(2, 2, 2.6, 0), mgp = c(1.6, 0.45, 0), tcl = -0.25,
                cex.axis = 0.8, col.axis = .mc_col$ink2, las = 1)
  key_cex <- if (n_panels > 9) 0.65 else 0.8

  for (i in idx) {
    curves <- x$roc$rocs[[i]]
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i",
                          yaxs = "i")
    graphics::abline(h = c(0.25, 0.5, 0.75), v = c(0.25, 0.5, 0.75),
                     col = .mc_col$grid, lwd = 1)
    graphics::abline(0, 1, col = .mc_col$muted, lty = 2, lwd = 1)

    cols <- c(.mc_col$model1, .mc_col$model2)
    for (k in 1:2) {
      graphics::lines(1 - curves[[k]]$specificities,
                      curves[[k]]$sensitivities,
                      col = cols[k], lwd = 2)
    }

    graphics::axis(1, at = c(0, 0.5, 1), labels = c("0", "0.5", "1"),
                   col = .mc_col$muted, col.ticks = .mc_col$muted)
    graphics::axis(2, at = c(0, 0.5, 1), labels = c("0", "0.5", "1"),
                   col = .mc_col$muted, col.ticks = .mc_col$muted)
    graphics::title(main = pw$pair[i], cex.main = 0.95, font.main = 2,
                    col.main = .mc_col$ink, line = 1.3)
    graphics::mtext(sprintf("pair AUC %.3f", pw$auc[i]), side = 3,
                    line = 0.3, cex = 0.8 * graphics::par("cex"),
                    col = .mc_col$ink2)

    graphics::legend("bottomright", bty = "n", cex = key_cex, lwd = 2,
                     col = cols, text.col = .mc_col$ink, inset = 0.02,
                     legend = c(sprintf("by P(%s)  %.3f", pw$class1[i],
                                        pw$auc_1v2[i]),
                                sprintf("by P(%s)  %.3f", pw$class2[i],
                                        pw$auc_2v1[i])))
  }

  graphics::mtext("False positive rate (1 - specificity)", side = 1,
                  outer = TRUE, line = 0.6, cex = 0.85, col = .mc_col$ink2)
  graphics::mtext("True positive rate (sensitivity)", side = 2,
                  outer = TRUE, line = 0.6, cex = 0.85, col = .mc_col$ink2,
                  las = 0)
  graphics::mtext(main, side = 3, outer = TRUE, line = 0.8, cex = 1.05,
                  font = 2, col = .mc_col$ink)
}

.mc_plot_heatmap <- function(x, main, scale = "auto") {
  pw <- x$pairwise
  lv <- x$levels
  k  <- length(lv)
  m  <- matrix(NA_real_, k, k, dimnames = list(lv, lv))
  i1 <- match(pw$class1, lv)
  i2 <- match(pw$class2, lv)
  m[cbind(i1, i2)] <- pw$auc
  m[cbind(i2, i1)] <- pw$auc

  # Colour scale: sequential blue over the observed range when every pair
  # is at least 0.5, otherwise the fixed diverging 0-1 scale.
  full <- scale == "full" || min(pw$auc) < 0.5
  if (full) {
    lim  <- c(0, 1)
    ramp <- .mc_auc_colours(101)
    key_at  <- c(0, 0.5, 1)
    key_lab <- c("0", "0.5\nchance", "1")
  } else {
    lim  <- c(min(min(pw$auc), 0.99), 1)
    ramp <- grDevices::colorRampPalette(c("#cde2fb", "#86b6ef", "#3987e5",
                                          "#1c5cab", "#0d366b"))(101)
    key_at  <- c(0, 1)
    key_lab <- formatC(lim, format = "f", digits = 3)
  }
  pos  <- (m - lim[1]) / diff(lim)
  fill <- ramp[pmin(pmax(round(pos * 100), 0), 100) + 1]
  digits <- if (k <= 8) 3 else 2
  cell_cex <- max(0.45, min(0.9, 6 / k))

  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)

  graphics::par(oma = c(0, 0, 3.2, 0))
  graphics::layout(matrix(1:2, 1), widths = c(5, 1))
  side_lines <- .mc_label_lines(lv, 0.85)
  graphics::par(mar = c(side_lines, side_lines, 0.6, 0.5), las = 1,
                col.axis = .mc_col$ink2)

  graphics::plot.new()
  graphics::plot.window(xlim = c(0.5, k + 0.5), ylim = c(k + 0.5, 0.5),
                        xaxs = "i", yaxs = "i", asp = 1)
  for (r in seq_len(k)) {
    for (cc in seq_len(k)) {
      if (r == cc) next
      f <- fill[(cc - 1) * k + r]
      graphics::rect(cc - 0.5, r - 0.5, cc + 0.5, r + 0.5, col = f,
                     border = "#ffffff", lwd = 2)
      graphics::text(cc, r, formatC(m[r, cc], format = "f", digits = digits),
                     col = .mc_text_on(f), cex = cell_cex)
    }
  }
  graphics::axis(1, at = seq_len(k), labels = lv, las = 2, lwd = 0,
                 cex.axis = 0.85, line = -0.6)
  graphics::axis(2, at = seq_len(k), labels = lv, las = 1, lwd = 0,
                 cex.axis = 0.85, line = -0.6)
  graphics::mtext(main, side = 3, outer = TRUE, line = 1.6, cex = 1.05,
                  font = 2, col = .mc_col$ink)
  graphics::mtext("Pairwise AUC: mean of the two curves for each class pair",
                  side = 3, outer = TRUE, line = 0.4, cex = 0.8,
                  col = .mc_col$ink2)

  # Colour key
  graphics::par(mar = c(side_lines, 0.5, 0.6, 3.2))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), yaxs = "i")
  y <- seq(0, 1, length.out = 102)
  graphics::rect(0.15, y[-102], 0.55, y[-1], col = ramp, border = ramp)
  graphics::axis(4, at = key_at, pos = 0.6, lwd = 0, labels = key_lab,
                 cex.axis = 0.75, col.axis = .mc_col$ink2)
}

#' Plot a comparison of two multiclass models
#'
#' Shows the result of [mc_auc_compare()] either as the bootstrap
#' distribution of the AUC difference or as each class pair's AUC under both
#' models.
#'
#' @details
#' * `type = "bootstrap"` (default) is a histogram of the bootstrap
#'   differences (model 1 minus model 2) with the confidence interval shaded,
#'   the observed difference marked, and zero (no difference) as a dashed
#'   line. If the shaded interval does not cover zero, the models differ at
#'   that confidence level.
#' * `type = "pairs"` draws each class pair's AUC under both models, joined
#'   by a line and sorted by the difference, so you can see which pairs of
#'   classes one model separates better. Where the two dots sit on top of
#'   each other the models agree. The axis zooms to the data, and the
#'   dashed chance line (0.5) is shown when it falls in view. These per-pair
#'   values come from the observed data only; they have no intervals.
#'
#' @param x An `mc_auc_compare` object.
#' @param type `"bootstrap"` or `"pairs"`.
#' @param labels Names for the two models, in order.
#' @param main Optional title.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' \donttest{
#' fit1 <- nnet::multinom(Species ~ Petal.Length + Petal.Width,
#'                        data = iris, trace = FALSE)
#' fit2 <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
#'                        data = iris, trace = FALSE)
#' res <- mc_auc_compare(iris$Species,
#'                       predict(fit1, type = "probs"),
#'                       predict(fit2, type = "probs"),
#'                       n_boot = 200, seed = 42)
#' plot(res)
#' plot(res, type = "pairs", labels = c("Petals", "Sepals"))
#' }
#'
#' @seealso [mc_auc_compare()], [plot.mc_roc()]
#' @export
plot.mc_auc_compare <- function(x, type = c("bootstrap", "pairs"),
                                labels = c("Model 1", "Model 2"),
                                main = NULL, ...) {
  if (!inherits(x, "mc_auc_compare")) {
    stop("x must be an object of class 'mc_auc_compare'.", call. = FALSE)
  }
  type <- .mc_check_choice(type, c("bootstrap", "pairs"))
  if (!is.character(labels) || length(labels) != 2 || anyNA(labels)) {
    stop("labels must be a character vector of length 2.", call. = FALSE)
  }

  if (type == "bootstrap") {
    .mc_plot_boot_diff(x, labels, main)
  } else {
    if (is.null(x$pairwise)) {
      stop("This result has no per-pair AUCs. They need probability ",
           "columns named after the classes.", call. = FALSE)
    }
    .mc_plot_pair_compare(x, labels, main)
  }
  invisible(x)
}

.mc_plot_boot_diff <- function(x, labels, main) {
  v <- x$bootstrap_values
  if (is.null(main)) {
    p_txt <- if (x$p_value < 1 / x$n_successful) {
      sprintf("p < %.3f", 1 / x$n_successful)
    } else {
      sprintf("p = %.3f", x$p_value)
    }
    main <- sprintf("%s minus %s: %.4f (%.0f%% CI %.4f to %.4f, %s)",
                    labels[1], labels[2], x$difference,
                    x$conf_level * 100, x$lower_ci, x$upper_ci, p_txt)
  }

  rng <- range(c(v, 0, x$lower_ci, x$upper_ci, x$difference))
  pad <- if (diff(rng) > 0) diff(rng) * 0.08 else 0.01
  xlim <- rng + c(-pad, pad)
  breaks <- if (diff(range(v)) > 0) {
    seq(min(v), max(v), length.out = min(40, max(10, ceiling(sqrt(length(v))))))
  } else {
    c(v[1] - pad / 4, v[1] + pad / 4)
  }
  h <- graphics::hist(v, breaks = breaks, plot = FALSE)

  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mar = c(4.2, 4.2, 3.2, 1), las = 1, mgp = c(2.6, 0.6, 0),
                tcl = -0.25, col.axis = .mc_col$ink2, cex.axis = 0.85)

  ymax <- max(h$counts) * 1.18
  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = c(0, ymax), yaxs = "i")
  graphics::abline(h = pretty(c(0, ymax)), col = .mc_col$grid)
  graphics::rect(x$lower_ci, 0, x$upper_ci, ymax, col = .mc_col$band,
                 border = NA)
  graphics::rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], h$counts,
                 col = .mc_col$model1, border = "#ffffff", lwd = 1)
  graphics::abline(v = 0, col = .mc_col$muted, lty = 2, lwd = 1.5)
  graphics::abline(v = x$difference, col = .mc_col$ink, lwd = 2)

  graphics::axis(1, col = .mc_col$muted, col.ticks = .mc_col$muted)
  graphics::axis(2, col = NA, col.ticks = NA)
  graphics::title(xlab = sprintf("Bootstrap AUC difference (%s minus %s)",
                                 labels[1], labels[2]),
                  ylab = "Bootstrap replicates", col.lab = .mc_col$ink2)
  graphics::title(main = main, cex.main = 0.95, col.main = .mc_col$ink,
                  line = 1.2)
  graphics::legend("topright", bty = "n", cex = 0.8, inset = c(0.01, 0),
                   legend = c("Observed difference",
                              sprintf("%.0f%% interval", x$conf_level * 100),
                              "No difference (0)"),
                   col = c(.mc_col$ink, .mc_col$band, .mc_col$muted),
                   lty = c(1, NA, 2), lwd = c(2, NA, 1.5),
                   pch = c(NA, 15, NA), pt.cex = 2,
                   text.col = .mc_col$ink)
}

.mc_plot_pair_compare <- function(x, labels, main) {
  pw  <- x$pairwise
  pw  <- pw[order(pw$difference), , drop = FALSE]  # largest gain at the top
  n   <- nrow(pw)
  if (is.null(main)) main <- "AUC by class pair"

  lo   <- min(pw$auc1, pw$auc2)
  pad  <- max(0.005, 0.08 * (1 - lo))
  xlim <- c(max(0, lo - pad), 1 + pad / 4)
  show_chance <- xlim[1] <= 0.5

  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mar = c(4.2, .mc_label_lines(pw$pair, 0.8), 4.2, 1),
                las = 1, mgp = c(2.6, 0.6, 0), tcl = -0.25,
                col.axis = .mc_col$ink2, cex.axis = 0.85)

  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = c(0.5, n + 0.5), yaxs = "i")
  graphics::abline(v = pretty(xlim), col = .mc_col$grid)
  graphics::abline(h = seq_len(n), col = .mc_col$grid, lty = 3)
  if (show_chance) graphics::abline(v = 0.5, col = .mc_col$muted, lty = 2)
  y <- seq_len(n)
  graphics::segments(pw$auc1, y, pw$auc2, y, col = .mc_col$muted, lwd = 2)
  pt_cex <- if (n > 30) 1 else 1.4
  graphics::points(pw$auc2, y, pch = 21, bg = .mc_col$model2, col = "#ffffff",
                   cex = pt_cex, lwd = 1.5)
  graphics::points(pw$auc1, y, pch = 21, bg = .mc_col$model1, col = "#ffffff",
                   cex = pt_cex, lwd = 1.5)

  graphics::axis(1, col = .mc_col$muted, col.ticks = .mc_col$muted)
  graphics::axis(2, at = y, labels = pw$pair, lwd = 0,
                 cex.axis = if (n > 30) 0.6 else 0.8)
  graphics::title(xlab = if (show_chance) {
    "Pair AUC (dashed line = chance, 0.5)"
  } else {
    "Pair AUC (axis zoomed to the data)"
  }, col.lab = .mc_col$ink2)
  graphics::title(main = main, cex.main = 1, col.main = .mc_col$ink,
                  line = 2.6)
  # Legend just above the plotting area, so it never covers a row.
  usr <- graphics::par("usr")
  top_in <- graphics::grconvertY(usr[4], from = "user", to = "inches")
  graphics::legend(x = mean(usr[1:2]),
                   y = graphics::grconvertY(top_in + 0.03, from = "inches",
                                            to = "user"),
                   xjust = 0.5, yjust = 0, horiz = TRUE, bty = "n",
                   cex = 0.8, xpd = NA, pch = 21, pt.cex = 1.3,
                   pt.bg = c(.mc_col$model1, .mc_col$model2),
                   col = "#ffffff", text.col = .mc_col$ink,
                   legend = sprintf("%s (overall %.3f)", labels,
                                    c(x$auc1, x$auc2)))
}

#' Plot a bootstrap confidence interval for the AUC
#'
#' Shows the result of [mc_auc_ci()] as a histogram of the bootstrap AUCs,
#' with the confidence interval shaded and the AUC of the original data
#' marked.
#'
#' @details
#' Each bar counts the bootstrap samples whose AUC fell in that range. The
#' shaded band is the percentile confidence interval reported by
#' [mc_auc_ci()], and the solid line is the AUC of the original data. A wide
#' band means the AUC would change a lot with a different sample of rows.
#'
#' @param x An `mc_auc_ci` object from [mc_auc_ci()].
#' @param main Optional title.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' set.seed(1)
#' truth  <- factor(sample(c("a", "b", "c"), 150, replace = TRUE))
#' scores <- matrix(stats::rnorm(450), 150, 3) +
#'   1.5 * outer(as.integer(truth), 1:3, "==")
#' probs  <- exp(scores) / rowSums(exp(scores))
#' colnames(probs) <- levels(truth)
#'
#' ci <- mc_auc_ci(truth, probs, n_boot = 200, seed = 1)
#' plot(ci)
#'
#' @seealso [mc_auc_ci()]
#' @export
plot.mc_auc_ci <- function(x, main = NULL, ...) {
  if (!inherits(x, "mc_auc_ci")) {
    stop("x must be an object of class 'mc_auc_ci'.", call. = FALSE)
  }
  v <- x$bootstrap_values
  if (length(v) < 2) {
    stop("x has fewer than two bootstrap values to plot.", call. = FALSE)
  }
  if (is.null(main)) {
    main <- sprintf("AUC %.3f (%.0f%% CI %.3f to %.3f)", x$original_auc,
                    x$conf_level * 100, x$lower_ci, x$upper_ci)
  }

  rng <- range(c(v, x$original_auc, x$lower_ci, x$upper_ci))
  pad <- if (diff(rng) > 0) diff(rng) * 0.08 else 0.01
  xlim <- rng + c(-pad, pad)
  breaks <- if (diff(range(v)) > 0) {
    seq(min(v), max(v), length.out = min(40, max(10, ceiling(sqrt(length(v))))))
  } else {
    c(v[1] - pad / 4, v[1] + pad / 4)
  }
  h <- graphics::hist(v, breaks = breaks, plot = FALSE)

  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mar = c(4.2, 4.2, 3.2, 1), las = 1, mgp = c(2.6, 0.6, 0),
                tcl = -0.25, col.axis = .mc_col$ink2, cex.axis = 0.85)

  ymax <- max(h$counts) * 1.18
  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = c(0, ymax), yaxs = "i")
  graphics::abline(h = pretty(c(0, ymax)), col = .mc_col$grid)
  graphics::rect(x$lower_ci, 0, x$upper_ci, ymax, col = .mc_col$band,
                 border = NA)
  graphics::rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], h$counts,
                 col = .mc_col$model1, border = "#ffffff", lwd = 1)
  graphics::abline(v = x$original_auc, col = .mc_col$ink, lwd = 2)

  graphics::axis(1, col = .mc_col$muted, col.ticks = .mc_col$muted)
  graphics::axis(2, col = NA, col.ticks = NA)
  graphics::title(xlab = "Bootstrap AUC", ylab = "Bootstrap replicates",
                  col.lab = .mc_col$ink2)
  graphics::title(main = main, cex.main = 0.95, col.main = .mc_col$ink,
                  line = 1.2)
  graphics::legend("topleft", bty = "n", cex = 0.8, inset = c(0.01, 0),
                   legend = c("AUC of the original data",
                              sprintf("%.0f%% interval", x$conf_level * 100)),
                   col = c(.mc_col$ink, .mc_col$band), lty = c(1, NA),
                   lwd = c(2, NA), pch = c(NA, 15), pt.cex = 2,
                   text.col = .mc_col$ink)
  invisible(x)
}
