test_that("elicit_node_cpt handles root node", {
  cpt <- elicitcausal:::elicit_node_cpt("X", character(0),
                                         .responses = list(0.4))
  expect_s3_class(cpt, "cpt")
  expect_equal(cpt$prob, 0.4)
  expect_equal(attr(cpt, "node"), "X")
  expect_equal(attr(cpt, "parents"), character(0))
})

test_that("elicit_node_cpt handles single-parent node", {
  cpt <- elicitcausal:::elicit_node_cpt("Y", "X",
                                         .responses = list(0.2, 0.8))
  expect_s3_class(cpt, "cpt")
  expect_equal(nrow(cpt), 2L)
  expect_equal(cpt$prob[cpt$X == 0L], 0.2)
  expect_equal(cpt$prob[cpt$X == 1L], 0.8)
  expect_equal(attr(cpt, "parents"), "X")
})

test_that("elicit_node_cpt handles two-parent node (collider)", {
  # Parents in expand.grid order: X cycles fastest (0,1,0,1), Y next (0,0,1,1)
  cpt <- elicitcausal:::elicit_node_cpt("Z", c("X", "Y"),
                                         .responses = list(0.1, 0.4, 0.4, 0.9))
  expect_equal(nrow(cpt), 4L)
  expect_equal(cpt$prob[cpt$X == 0L & cpt$Y == 0L], 0.1)
  expect_equal(cpt$prob[cpt$X == 1L & cpt$Y == 1L], 0.9)
})

# ---- Likert normalisation ---------------------------------------------------

test_that(".normalize_if_likert is a no-op in probability mode", {
  probs <- c(0.3, 0.7, 0.5)
  expect_equal(elicitcausal:::.normalize_if_likert(probs, "probability"), probs)
})

test_that(".normalize_if_likert normalises in likert mode", {
  probs <- c(0.7, 0.7)   # raw Likert anchors that don't sum to 1
  out   <- elicitcausal:::.normalize_if_likert(probs, "likert")
  expect_equal(sum(out), 1, tolerance = 1e-10)
  expect_equal(out, c(0.5, 0.5), tolerance = 1e-10)
})

test_that(".normalize_if_likert preserves relative weights", {
  probs <- c(0.15, 0.50, 0.85)
  out   <- elicitcausal:::.normalize_if_likert(probs, "likert")
  expect_equal(sum(out), 1, tolerance = 1e-10)
  # Ratios should be preserved
  expect_equal(out[1] / out[2], 0.15 / 0.50, tolerance = 1e-10)
  expect_equal(out[2] / out[3], 0.50 / 0.85, tolerance = 1e-10)
})

test_that(".normalize_if_likert is a no-op for single-combo root nodes", {
  # Length-1 vector: nothing to normalise regardless of mode
  expect_equal(elicitcausal:::.normalize_if_likert(0.7, "likert"), 0.7)
})

test_that("elicit_node_cpt normalises Likert probs and emits a message", {
  # Simulate likert responses (numeric anchors from LIKERT_PROBS)
  expect_message(
    cpt <- elicitcausal:::elicit_node_cpt(
      "Y", "X", mode = "likert",
      .responses = list(0.7, 0.7)   # both "Likely"; raw sum = 1.4
    ),
    regexp = "rescaled to sum to 1"
  )
  expect_equal(sum(cpt$prob), 1, tolerance = 1e-10)
  expect_equal(cpt$prob[cpt$X == 0L], 0.5, tolerance = 1e-10)
  expect_equal(cpt$prob[cpt$X == 1L], 0.5, tolerance = 1e-10)
})

test_that("elicit_node_cpt emits no message when Likert probs already sum to 1", {
  # Two symmetric values that happen to sum to 1 — no message expected
  expect_no_message(
    elicitcausal:::elicit_node_cpt(
      "Y", "X", mode = "likert",
      .responses = list(0.5, 0.5)   # raw sum = 1.0
    )
  )
})

test_that("elicit_node_cpt does NOT normalise Likert probs for root nodes", {
  # Root nodes have a single probability; normalisation would clamp it to 1
  expect_no_message(
    cpt <- elicitcausal:::elicit_node_cpt(
      "X", character(0), mode = "likert",
      .responses = list(0.7)
    )
  )
  expect_equal(cpt$prob, 0.7)
})
