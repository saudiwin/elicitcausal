# demo_ranking_to_probs.R
# Demonstrates how .ranking_to_probs() converts user rankings to a CPT.
#
# The function uses a logistic model with geometric-decay log-odds effects.
# The top-ranked parent in each bucket shifts P(node=1) away from base_prob
# by at most `max_effect` (in probability units); lower ranks receive 70% of
# the log-odds magnitude of the rank above them.
#   Rank 1: log-odds effect = logit(base_prob +/- max_effect) - logit(base_prob)
#   Rank 2: 70% of rank 1's log-odds magnitude
#   Rank 3: 70% of rank 2's log-odds magnitude
#   ...mirrored (opposite sign) for negative parents
#   "No effect" parents:     0.00 log-odds

# ---------------------------------------------------------------------------
# Copy of the internal function (not exported)
# ---------------------------------------------------------------------------
ranking_to_probs <- function(base_prob, pos_parents, neg_parents, all_parents,
                              max_effect = 0.4) {
  logit    <- function(p) log(p / (1 - p))
  logistic <- function(x) 1 / (1 + exp(-x))

  base_prob  <- pmax(0.01, pmin(0.99, base_prob))
  base_logit <- logit(base_prob)
  max_effect <- pmax(0.01, pmin(0.49, max_effect))

  top_pos <- logit(pmin(0.99, base_prob + max_effect)) - base_logit
  top_neg <- base_logit - logit(pmax(0.01, base_prob - max_effect))

  .effects <- function(n, top) if (n == 0L) numeric(0) else top * 0.7^(seq_len(n) - 1L)

  all_effects <- stats::setNames(rep(0, length(all_parents)), all_parents)
  if (length(pos_parents) > 0L)
    all_effects[pos_parents] <-  .effects(length(pos_parents), top_pos)
  if (length(neg_parents) > 0L)
    all_effects[neg_parents] <- -.effects(length(neg_parents), top_neg)

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
# Expected: P(Y=1|X=0) ≈ 0.20, P(Y=1|X=1) ≈ 0.60  (rank-1 shift = +max_effect = 0.40)

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
# Expected: P(Y=1|Z=0) ≈ 0.80, P(Y=1|Z=1) ≈ 0.40  (rank-1 shift = -max_effect = 0.40)

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
# X (rank 1) gets the full top-rank log-odds effect; W (rank 2) gets 70% of it
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
# Effect size illustration: rank vs. resulting probability shift from 0.5,
# shown for two choices of max_effect
# ---------------------------------------------------------------------------
logistic <- function(x) 1 / (1 + exp(-x))
logit    <- function(p) log(p / (1 - p))
for (max_effect in c(0.4, 0.2)) {
  cat(sprintf("\n\nEffect of rank on P(Y=1 | single parent = 1), base = 0.50, max_effect = %.2f\n",
              max_effect))
  cat(strrep("-", 65), "\n")
  top <- logit(0.5 + max_effect) - logit(0.5)   # base_logit = logit(0.5) = 0
  for (r in 1:5) {
    lo <- top * 0.7^(r - 1L)          # log-odds increment for rank r
    p  <- logistic(lo)
    cat(sprintf("  Rank %d: log-odds effect = %+.3f  ->  P(Y=1 | parent=1) = %.3f\n",
                r, lo, p))
  }
}
