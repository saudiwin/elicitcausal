test_that("parse_prob_input handles probability mode", {
  expect_equal(elicitcausal:::parse_prob_input("0.5",  "probability"), 0.5)
  expect_equal(elicitcausal:::parse_prob_input("0",    "probability"), 0)
  expect_equal(elicitcausal:::parse_prob_input("1",    "probability"), 1)
  expect_true(is.na(elicitcausal:::parse_prob_input("1.5",  "probability")))
  expect_true(is.na(elicitcausal:::parse_prob_input("abc",  "probability")))
})

test_that("parse_prob_input handles likert mode keys", {
  expect_equal(elicitcausal:::parse_prob_input("1", "likert"), 0.05)
  expect_equal(elicitcausal:::parse_prob_input("4", "likert"), 0.50)
  expect_equal(elicitcausal:::parse_prob_input("7", "likert"), 0.95)
})

test_that("parse_prob_input accepts decimals in likert mode", {
  expect_equal(elicitcausal:::parse_prob_input("0.3", "likert"), 0.3)
})

test_that("parse_prob_input rejects out-of-range Likert keys", {
  # "8" is not a valid Likert key and not a valid decimal in [0,1]
  expect_true(is.na(elicitcausal:::parse_prob_input("8", "likert")))
  # "0" is accepted as a decimal probability (p = 0) not as a Likert key
  expect_equal(elicitcausal:::parse_prob_input("0", "likert"), 0)
})

test_that("format_query formats root node correctly", {
  q <- elicitcausal:::format_query("X", setNames(integer(0), character(0)))
  expect_equal(q, "P(X = 1)")
})

test_that("format_query formats conditional correctly", {
  q <- elicitcausal:::format_query("Y", c(X = 1L))
  expect_equal(q, "P(Y = 1 | X = 1)")

  q2 <- elicitcausal:::format_query("Z", c(X = 0L, Y = 1L))
  expect_equal(q2, "P(Z = 1 | X = 0, Y = 1)")
})

test_that("prompt_for_prob uses pre-specified .response", {
  p <- elicitcausal:::prompt_for_prob("Y", c(X = 1L), "probability",
                                       .response = "0.8")
  expect_equal(p, 0.8)
})

test_that("prompt_for_prob maps Likert key via .response", {
  p <- elicitcausal:::prompt_for_prob("Y", c(X = 0L), "likert",
                                       .response = "6")
  expect_equal(p, 0.85)
})
