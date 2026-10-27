# Shared test helpers. testthat loads every helper-*.R file before the tests,
# so mc_auc_ci() and mc_auc_compare() tests can use these too.

# Simulated K-class data with softmax probabilities.
#   signal   how separable the classes are (0 = no information)
#   weights  class proportions, e.g. c(0.7, 0.2, 0.1) for imbalance
sim_mc <- function(n = 300, k = 3, signal = 1.5, seed = 1,
                   weights = rep(1, k)) {
  set.seed(seed)
  y <- factor(sample(letters[seq_len(k)], n, replace = TRUE, prob = weights),
              levels = letters[seq_len(k)])
  z <- matrix(stats::rnorm(n * k), n, k)
  idx <- cbind(seq_len(n), as.integer(y))
  z[idx] <- z[idx] + signal
  p <- exp(z) / rowSums(exp(z))
  colnames(p) <- levels(y)
  list(y = y, p = p)
}

# Independent Hand & Till (2001) implementation, written from the paper.
# A(i|j) = P(a class-i row scores higher on P(class i) than a class-j row).
ht_A <- function(y, p, i, j) {
  si <- p[y == i, i]; sj <- p[y == j, i]
  mean(outer(si, sj, ">") + 0.5 * outer(si, sj, "=="))
}
hand_till <- function(y, p) {
  pr <- utils::combn(levels(y), 2)
  mean(apply(pr, 2, function(q) (ht_A(y, p, q[1], q[2]) + ht_A(y, p, q[2], q[1])) / 2))
}
