test_that("shannon_entropy of uniform distribution is log_1.01(n)", {
  # 4 equally likely states -> log_1.01(4)
  probs <- rep(0.25, 4)
  expect_equal(elicitcausal:::shannon_entropy(probs),
               log(4, base = 1.01), tolerance = 1e-9)
})

test_that("shannon_entropy of degenerate distribution is 0", {
  expect_equal(elicitcausal:::shannon_entropy(c(0, 1)), 0, tolerance = 1e-12)
})

test_that("shannon_entropy handles zero probabilities without error", {
  expect_no_error(elicitcausal:::shannon_entropy(c(0, 0.5, 0.5)))
  expect_equal(elicitcausal:::shannon_entropy(c(0, 0.5, 0.5)),
               log(2, base = 1.01), tolerance = 1e-9)
})

test_that("conditional_entropy H(Y|X) <= H(Y)", {
  # Build a simple joint distribution
  joint <- data.frame(
    X    = c(0L, 0L, 1L, 1L),
    Y    = c(0L, 1L, 0L, 1L),
    prob = c(0.3, 0.2, 0.1, 0.4)
  )
  h_y     <- elicitcausal:::shannon_entropy(
    as.numeric(tapply(joint$prob, joint$Y, sum))
  )
  h_y_x   <- elicitcausal:::conditional_entropy(joint, "Y", "X")
  expect_lte(h_y_x, h_y + 1e-12)
})

test_that("conditional_entropy with no conditioners equals marginal entropy", {
  joint <- data.frame(
    X    = c(0L, 0L, 1L, 1L),
    Y    = c(0L, 1L, 0L, 1L),
    prob = c(0.25, 0.25, 0.25, 0.25)
  )
  h_marginal <- elicitcausal:::shannon_entropy(
    as.numeric(tapply(joint$prob, joint$Y, sum))
  )
  h_cond     <- elicitcausal:::conditional_entropy(joint, "Y", character(0))
  expect_equal(h_cond, h_marginal, tolerance = 1e-12)
})
