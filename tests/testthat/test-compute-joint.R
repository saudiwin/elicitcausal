library(dagitty)

# Helper to build CPT objects directly
make_root_cpt <- function(node, prob) {
  cpt <- data.frame(prob = prob)
  attr(cpt, "node")    <- node
  attr(cpt, "parents") <- character(0)
  class(cpt)           <- c("cpt", "data.frame")
  cpt
}

make_cpt <- function(node, parent_names, probs) {
  combos <- expand.grid(lapply(parent_names, function(.) c(0L, 1L)),
                        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  names(combos) <- parent_names
  combos$prob   <- probs
  attr(combos, "node")    <- node
  attr(combos, "parents") <- parent_names
  class(combos)           <- c("cpt", "data.frame")
  combos
}

test_that("joint distribution sums to 1", {
  cpts <- list(
    X = make_root_cpt("X", 0.5),
    Y = make_cpt("Y", "X", c(0.2, 0.8))
  )
  joint <- elicitcausal:::compute_joint_distribution(c("X", "Y"), cpts)
  expect_equal(sum(joint$prob), 1, tolerance = 1e-10)
  expect_equal(nrow(joint), 4L)
})

test_that("joint distribution recovers independent probabilities", {
  # X ~ Bernoulli(0.3), Y ~ Bernoulli(0.6), independent (no parents for Y
  # -- but Y has no edges, so we treat it as a root with P(Y=1) = 0.6)
  cpts <- list(
    X = make_root_cpt("X", 0.3),
    Y = make_root_cpt("Y", 0.6)
  )
  joint <- elicitcausal:::compute_joint_distribution(c("X", "Y"), cpts)

  p_x1_y1 <- joint$prob[joint$X == 1L & joint$Y == 1L]
  expect_equal(p_x1_y1, 0.3 * 0.6, tolerance = 1e-10)

  p_x0_y0 <- joint$prob[joint$X == 0L & joint$Y == 0L]
  expect_equal(p_x0_y0, 0.7 * 0.4, tolerance = 1e-10)
})

test_that("joint distribution handles deterministic relationships", {
  # Y = X with certainty
  cpts <- list(
    X = make_root_cpt("X", 0.5),
    Y = make_cpt("Y", "X", c(0.0, 1.0))
  )
  joint <- elicitcausal:::compute_joint_distribution(c("X", "Y"), cpts)

  # P(X=0, Y=1) should be 0
  expect_equal(joint$prob[joint$X == 0L & joint$Y == 1L], 0, tolerance = 1e-8)
  # P(X=1, Y=1) should be 0.5
  expect_equal(joint$prob[joint$X == 1L & joint$Y == 1L], 0.5, tolerance = 1e-8)
})
