# demo_ranking_to_probs.R
# Demonstrates how .ranking_to_probs() converts user rankings to a CPT.
#
# The function uses a logistic model with geometric-decay log-odds effects:
#   Rank 1 positive parent: +2.00 log-odds
#   Rank 2 positive parent: +1.40 log-odds  (70% of previous)
#   Rank 3 positive parent: +0.98 log-odds
#   ...mirrored for negative parents (same magnitudes, negative sign)
#   "No effect" parents:     0.00 log-odds

# ---------------------------------------------------------------------------
# Copy of the internal function (not exported)
# ---------------------------------------------------------------------------
ranking_to_probs <- function(base_prob, pos_parents, neg_parents, all_parents) {
  logit    <- function(p) log(p / (1 - p))
  logistic <- function(x) 1 / (1 + exp(-x))

  base_prob  <- pmax(0.01, pmin(0.99, base_prob))
  base_logit <- logit(base_prob)

  .effects <- function(n) if (n == 0L) numeric(0) else 2.0 * 0.7^(seq_len(n) - 1L)

  all_effects <- stats::setNames(rep(0, length(all_parents)), all_parents)
  if (length(pos_parents) > 0L)
    all_effects[pos_parents] <-  .effects(length(pos_parents))
  if (length(neg_parents) > 0L)
    all_effects[neg_parents] <- -.effects(length(neg_parents))

  if (length(all_parents) == 0L) return(base_prob)

  combos <- expand.grid(
    lapply(all_parents, function(.) c(0L, 1L)),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  names(combos) <- all_parents

  probs <- apply(combos, 1L, function(row)
    logistic(base_logit + sum(all_effects * as.numeric(row)))
  )
  cbind(combos, prob = round(probs, 3))
}

# ---------------------------------------------------------------------------
# Helper to print a readable CPT
# ---------------------------------------------------------------------------
show_cpt <- function(title, ...) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
  print(ranking_to_probs(...), row.names = FALSE)
}

# ---------------------------------------------------------------------------
# Example 1: Single positive parent
#   X positively affects Y; base P(Y=1 | X=0) = 0.2
# ---------------------------------------------------------------------------
show_cpt(
  "Example 1: One positive parent (X -> Y), base = 0.20",
  base_prob   = 0.20,
  pos_parents = "X",
  neg_parents = character(0),
  all_parents = "X"
)
# Expected: P(Y=1|X=0) ≈ 0.20, P(Y=1|X=1) ≈ 0.72  (rank-1 +2.0 log-odds)

# ---------------------------------------------------------------------------
# Example 2: One negative parent
#   Z negatively affects Y; base P(Y=1 | Z=0) = 0.8
# ---------------------------------------------------------------------------
show_cpt(
  "Example 2: One negative parent (Z -> Y), base = 0.80",
  base_prob   = 0.80,
  pos_parents = character(0),
  neg_parents = "Z",
  all_parents = "Z"
)
# Expected: P(Y=1|Z=0) ≈ 0.80, P(Y=1|Z=1) ≈ 0.28

# ---------------------------------------------------------------------------
# Example 3: Two positive parents ranked by strength
#   X (rank 1) and W (rank 2) both positively affect Y; base = 0.3
# ---------------------------------------------------------------------------
show_cpt(
  "Example 3: Two positive parents (X rank 1, W rank 2), base = 0.30",
  base_prob   = 0.30,
  pos_parents = c("X", "W"),   # X is stronger
  neg_parents = character(0),
  all_parents = c("X", "W")
)
# X effect: +2.00 log-odds; W effect: +1.40 log-odds
# All four combos: (X=0,W=0), (X=1,W=0), (X=0,W=1), (X=1,W=1)

# ---------------------------------------------------------------------------
# Example 4: Mixed — one positive, one negative, one no-effect parent
#   X positive (rank 1), Z negative (rank 1), W no effect; base = 0.5
# ---------------------------------------------------------------------------
show_cpt(
  "Example 4: Mixed parents (X pos, Z neg, W none), base = 0.50",
  base_prob   = 0.50,
  pos_parents = "X",
  neg_parents = "Z",
  all_parents = c("X", "Z", "W")
)
# W contributes 0 log-odds regardless of its value

# ---------------------------------------------------------------------------
# Effect size illustration: rank vs. resulting probability shift from 0.5
# ---------------------------------------------------------------------------
cat("\n\nEffect of rank on P(Y=1 | single parent = 1), base = 0.50\n")
cat(strrep("-", 50), "\n")
logistic <- function(x) 1 / (1 + exp(-x))
for (r in 1:5) {
  lo <- 2.0 * 0.7^(r - 1L)          # log-odds increment for rank r
  p  <- logistic(lo)                 # base_logit = logit(0.5) = 0, so P = logistic(lo)
  cat(sprintf("  Rank %d: log-odds effect = %+.3f  ->  P(Y=1 | parent=1) = %.3f\n",
              r, lo, p))
}
