# ------------------------------------------------------------------------------
# --- Additional Distribution Functions ---
# ------------------------------------------------------------------------------

# --- Shifted Beta ---

# min = a, max = b
dshift_beta <- function(x, alpha, beta, a = 0, b = 1) {
  1 / (b - a) * dbeta((x - a) / (b - a), shape1 = alpha, shape2 = beta)
}

pshift_beta <- function(q, alpha, beta, a = 0, b = 1) {
  pbeta((q - a) / (b - a), shape1 = alpha, shape2 = beta)
}

qshift_beta <- function(p, alpha, beta, a = 0, b = 1) {
  (b - a) * qbeta(p, shape1 = alpha, shape2 = beta) + a
}

rshift_beta <- function(n, alpha, beta, a = 0, b = 1) {
  sim <- rbeta(n, alpha, beta)
  a + (b - a) * sim
}
