library(dagitty)

# ---- Convenience wrapper --------------------------------------------------
run_chain <- function(responses = NULL, target = NULL) {
  dag <- dagitty("dag { X -> Y -> Z }")
  elicit_dag_priors(
    dag, verbose = FALSE, target = target,
    .responses = responses %||% list(
      X = list(0.5),
      Y = list(0.2, 0.8),
      Z = list(0.1, 0.9)
    )
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------

test_that("elicit_dag_priors returns the correct class", {
  result <- run_chain()
  expect_s3_class(result, "elicit_dag_result")
})

test_that("elicit_dag_priors produces one CPT per node", {
  result <- run_chain()
  expect_equal(length(result$cpts), 3L)
  expect_named(result$cpts, c("X", "Y", "Z"))
})

test_that("joint distribution has 2^n rows and sums to 1", {
  result <- run_chain()
  expect_equal(nrow(result$joint), 8L)
  expect_equal(sum(result$joint$prob), 1, tolerance = 1e-10)
})

test_that("entropy is non-negative and bounded by log2(2^n)", {
  result <- run_chain()
  expect_gte(result$entropy, 0)
  expect_lte(result$entropy, log2(8) + 1e-10)
})

test_that("target_marginal is returned when target is specified", {
  result <- run_chain(target = "Z")
  expect_false(is.null(result$target_marginal))
  expect_equal(nrow(result$target_marginal), 2L)
  expect_equal(sum(result$target_marginal$prob), 1, tolerance = 1e-10)
})

test_that("target_marginal is NULL when no target", {
  result <- run_chain()
  expect_null(result$target_marginal)
})

test_that("get_marginal recovers correct probabilities for root node", {
  result <- run_chain()
  marg   <- get_marginal(result, "X")
  expect_equal(marg$prob[marg$value == 1L], 0.5, tolerance = 1e-8)
  expect_equal(marg$prob[marg$value == 0L], 0.5, tolerance = 1e-8)
})

test_that("get_conditional P(Y=1|X=1) ~ 0.8", {
  result <- run_chain()
  p      <- get_conditional(result, query = c(Y = 1L), evidence = c(X = 1L))
  expect_equal(p, 0.8, tolerance = 1e-8)
})

test_that("get_conditional P(Y=1|X=0) ~ 0.2", {
  result <- run_chain()
  p      <- get_conditional(result, query = c(Y = 1L), evidence = c(X = 0L))
  expect_equal(p, 0.2, tolerance = 1e-8)
})

test_that("get_mutual_info is non-negative", {
  result <- run_chain()
  mi     <- get_mutual_info(result, "X", "Z")
  expect_gte(mi, 0)
})

test_that("get_mutual_info between independent variables is ~0", {
  # Make X and Z independent by setting Y as a deterministic 0 (blocking)
  dag  <- dagitty("dag { X -> Y -> Z }")
  resp <- list(
    X = list(0.5),
    Y = list(0, 0),  # Y is always 0 regardless of X
    Z = list(0.5, 0.5)
  )
  result <- elicit_dag_priors(dag, verbose = FALSE, .responses = resp)
  mi     <- get_mutual_info(result, "X", "Z")
  expect_equal(mi, 0, tolerance = 1e-8)
})

test_that("collider DAG: joint sums to 1 with two parents", {
  dag <- dagitty("dag { X -> Z; Y -> Z }")
  result <- elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(
      X = list(0.5),
      Y = list(0.5),
      Z = list(0.1, 0.4, 0.4, 0.9)
    )
  )
  expect_equal(sum(result$joint$prob), 1, tolerance = 1e-10)
})

test_that("elicit_dag_priors errors on unknown target", {
  dag <- dagitty("dag { X -> Y }")
  expect_error(
    elicit_dag_priors(dag, target = "W", verbose = FALSE,
                      .responses = list(X = list(0.5), Y = list(0.2, 0.8))),
    "W"
  )
})

test_that("elicit_dag_priors errors on unknown .responses key", {
  dag <- dagitty("dag { X -> Y }")
  expect_error(
    elicit_dag_priors(dag, verbose = FALSE,
                      .responses = list(X = list(0.5), W = list(0.3))),
    "W"
  )
})

test_that("likert mode maps keys correctly via .responses", {
  dag <- dagitty("dag { X -> Y }")
  result <- elicit_dag_priors(
    dag, mode = "likert", verbose = FALSE,
    .responses = list(
      X = list("4"),    # -> 0.50
      Y = list("2", "6")  # -> 0.15, 0.85
    )
  )
  expect_equal(result$cpts$X$prob, 0.50)
  expect_equal(result$cpts$Y$prob[result$cpts$Y$X == 0L], 0.15)
  expect_equal(result$cpts$Y$prob[result$cpts$Y$X == 1L], 0.85)
})
