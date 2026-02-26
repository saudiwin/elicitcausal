# causalqueries_recovery.R
#
# Demonstrates that the elicited CPT probabilities are faithfully recovered
# by query_model() after converting to a CausalQueries model.
#
# Run with: source("scripts/causalqueries_recovery.R")

library(dagitty)
library(elicitcausal)
library(CausalQueries)

# Helper: run a single query_model call and return the scalar mean probability.
qm <- function(cq, query, given = NULL) {
  q <- query_model(cq, queries = query, given = given, using = "parameters")
  q$mean[[1L]]
}

# ---------------------------------------------------------------------------
# 1. Simple X -> Y
# ---------------------------------------------------------------------------
cat("=== X -> Y ===\n")

py0 <- 0.20; py1 <- 0.75
dag    <- dagitty("dag { X -> Y }")
result <- elicit_dag_priors(
  dag, verbose = FALSE,
  .responses = list(X = list(0.5), Y = list(py0, py1))
)
cq <- to_causalqueries(result)

cat(sprintf("  P(Y=1 | X=0):      %.4f  (elicited: %.4f)\n",
            qm(cq, "Y == 1", "X == 0"), py0))
cat(sprintf("  P(Y=1 | X=1):      %.4f  (elicited: %.4f)\n",
            qm(cq, "Y == 1", "X == 1"), py1))
cat(sprintf("  P(Y[X=0]=1):       %.4f  (elicited: %.4f)\n",
            qm(cq, "Y[X=0] == 1"), py0))
cat(sprintf("  P(Y[X=1]=1):       %.4f  (elicited: %.4f)\n",
            qm(cq, "Y[X=1] == 1"), py1))
cat(sprintf("  ATE = P(Y[X=1]) - P(Y[X=0]): %.4f  (expected: %.4f)\n",
            qm(cq, "Y[X=1] - Y[X=0]"), py1 - py0))

# ---------------------------------------------------------------------------
# 2. Collider  X -> Z <- Y  (all four CPT cells)
# ---------------------------------------------------------------------------
cat("\n=== Collider: X -> Z <- Y ===\n")

p00 <- 0.10; p10 <- 0.40; p01 <- 0.30; p11 <- 0.90
dag    <- dagitty("dag { X -> Z; Y -> Z }")
result <- elicit_dag_priors(
  dag, verbose = FALSE,
  .responses = list(
    X = list(0.5),
    Y = list(0.5),
    Z = list(p00, p10, p01, p11)
  )
)
cq <- to_causalqueries(result)

expected <- list(
  c(x = 0, y = 0, p = p00),
  c(x = 1, y = 0, p = p10),
  c(x = 0, y = 1, p = p01),
  c(x = 1, y = 1, p = p11)
)
for (e in expected) {
  given <- sprintf("X == %d & Y == %d", e["x"], e["y"])
  cat(sprintf("  P(Z=1 | X=%d, Y=%d): %.4f  (elicited: %.4f)\n",
              e["x"], e["y"], qm(cq, "Z == 1", given), e["p"]))
}

# ---------------------------------------------------------------------------
# 3. Chain  X -> Y -> Z
# ---------------------------------------------------------------------------
cat("\n=== Chain: X -> Y -> Z ===\n")

py0 <- 0.10; py1 <- 0.90
pz0 <- 0.20; pz1 <- 0.80
dag    <- dagitty("dag { X -> Y -> Z }")
result <- elicit_dag_priors(
  dag, verbose = FALSE,
  .responses = list(
    X = list(0.5),
    Y = list(py0, py1),
    Z = list(pz0, pz1)
  )
)
cq <- to_causalqueries(result)

cat(sprintf("  P(Y=1 | X=0): %.4f  (elicited: %.4f)\n", qm(cq, "Y == 1", "X == 0"), py0))
cat(sprintf("  P(Y=1 | X=1): %.4f  (elicited: %.4f)\n", qm(cq, "Y == 1", "X == 1"), py1))
cat(sprintf("  P(Z=1 | Y=0): %.4f  (elicited: %.4f)\n", qm(cq, "Z == 1", "Y == 0"), pz0))
cat(sprintf("  P(Z=1 | Y=1): %.4f  (elicited: %.4f)\n", qm(cq, "Z == 1", "Y == 1"), pz1))

# Total effect of X on Z (integrating over Y)
ate_xz <- qm(cq, "Z[X=1] - Z[X=0]")
cat(sprintf("  ATE of X on Z:        %.4f\n", ate_xz))

ate_xy <- qm(cq, "Y[X=1] - Y[X=0]")
cat(sprintf("  ATE of X on Y:        %.4f\n", ate_xy))

ate_yz <- qm(cq, "Z[Y=1] - Z[Y=0]")
cat(sprintf("  ATE of Y on Z:        %.4f\n", ate_yz))

# remove floating point issues

round(ate_xy,2) * round(ate_yz,2) == round(ate_xz,2)

# ---------------------------------------------------------------------------
# 4. Fork  Z -> X ; Z -> Y  (common cause)
# ---------------------------------------------------------------------------
cat("\n=== Fork: Z -> X ; Z -> Y ===\n")

px0 <- 0.30; px1 <- 0.70
py0 <- 0.10; py1 <- 0.60
dag    <- dagitty("dag { Z -> X; Z -> Y }")
result <- elicit_dag_priors(
  dag, verbose = FALSE,
  .responses = list(
    Z = list(0.5),
    X = list(px0, px1),
    Y = list(py0, py1)
  )
)
cq <- to_causalqueries(result)

cat(sprintf("  P(X=1 | Z=0): %.4f  (elicited: %.4f)\n", qm(cq, "X == 1", "Z == 0"), px0))
cat(sprintf("  P(X=1 | Z=1): %.4f  (elicited: %.4f)\n", qm(cq, "X == 1", "Z == 1"), px1))
cat(sprintf("  P(Y=1 | Z=0): %.4f  (elicited: %.4f)\n", qm(cq, "Y == 1", "Z == 0"), py0))
cat(sprintf("  P(Y=1 | Z=1): %.4f  (elicited: %.4f)\n", qm(cq, "Y == 1", "Z == 1"), py1))
