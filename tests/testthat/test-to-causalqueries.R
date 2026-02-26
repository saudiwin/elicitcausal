library(dagitty)

skip_if_not_installed("CausalQueries")

# ---- shared setup -----------------------------------------------------------

make_chain_result <- function() {
  dag <- dagitty("dag { X -> Y -> Z }")
  elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(
      X = list(0.4),
      Y = list(0.1, 0.9),
      Z = list(0.2, 0.8)
    )
  )
}

make_xy_result <- function(px = 0.3, py0 = 0.1, py1 = 0.75) {
  dag <- dagitty("dag { X -> Y }")
  elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(X = list(px), Y = list(py0, py1))
  )
}

# ---- basic structure --------------------------------------------------------

test_that("to_causalqueries returns a causal_model object", {
  res <- make_xy_result()
  cq  <- to_causalqueries(res)
  expect_s3_class(cq, "causal_model")
})

test_that("parameters_df has correct number of rows", {
  # X -> Y: 2 (X) + 4 (Y) = 6 parameters
  cq <- to_causalqueries(make_xy_result())
  expect_equal(nrow(cq$parameters_df), 6L)
})

test_that("all parameter values lie in [0, 1]", {
  cq <- to_causalqueries(make_chain_result())
  vals <- cq$parameters_df$param_value
  expect_true(all(vals >= 0))
  expect_true(all(vals <= 1))
})

test_that("parameter values sum to 1 within each param_set", {
  cq <- to_causalqueries(make_chain_result())
  sums <- tapply(cq$parameters_df$param_value,
                 cq$parameters_df$param_set, sum)
  expect_equal(as.numeric(sums), rep(1, length(sums)), tolerance = 1e-10)
})

# ---- root node probabilities ------------------------------------------------

test_that("root node P(X=1) is correctly set", {
  cq  <- to_causalqueries(make_xy_result(px = 0.3))
  pdf <- cq$parameters_df
  expect_equal(pdf$param_value[pdf$param_names == "X.0"], 0.7, tolerance = 1e-10)
  expect_equal(pdf$param_value[pdf$param_names == "X.1"], 0.3, tolerance = 1e-10)
})

# ---- CPT recovery under independence assumption ----------------------------
# For X -> Y with P(Y=1|X=0)=py0 and P(Y=1|X=1)=py1,
# independence gives:
#   P(Y.00) = (1-py0)*(1-py1)
#   P(Y.10) = py0*(1-py1)       <- Y=1|X=0, Y=0|X=1
#   P(Y.01) = (1-py0)*py1       <- Y=0|X=0, Y=1|X=1
#   P(Y.11) = py0*py1

test_that("nodal type probs satisfy the independence formula for X -> Y", {
  py0 <- 0.2
  py1 <- 0.7
  cq  <- to_causalqueries(make_xy_result(py0 = py0, py1 = py1))
  pdf <- cq$parameters_df

  get_p <- function(name) pdf$param_value[pdf$param_names == name]

  expect_equal(get_p("Y.00"), (1 - py0) * (1 - py1), tolerance = 1e-10)
  expect_equal(get_p("Y.10"), py0 * (1 - py1),        tolerance = 1e-10)
  expect_equal(get_p("Y.01"), (1 - py0) * py1,         tolerance = 1e-10)
  expect_equal(get_p("Y.11"), py0 * py1,               tolerance = 1e-10)
})

test_that("nodal type probs recover correct CPT marginals", {
  # P(Y=1 | X=0) = P(Y.10) + P(Y.11) must equal the elicited py0
  py0 <- 0.2
  py1 <- 0.7
  cq  <- to_causalqueries(make_xy_result(py0 = py0, py1 = py1))
  pdf <- cq$parameters_df

  get_p <- function(name) pdf$param_value[pdf$param_names == name]

  recovered_py0 <- get_p("Y.10") + get_p("Y.11")
  recovered_py1 <- get_p("Y.01") + get_p("Y.11")

  expect_equal(recovered_py0, py0, tolerance = 1e-10)
  expect_equal(recovered_py1, py1, tolerance = 1e-10)
})

# ---- collider / two-parent node --------------------------------------------

test_that("collider with two parents: param_set Z has 16 rows summing to 1", {
  dag <- dagitty("dag { X -> Z; Y -> Z }")
  res <- elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(
      X = list(0.5),
      Y = list(0.5),
      Z = list(0.05, 0.30, 0.40, 0.90)
    )
  )
  cq   <- to_causalqueries(res)
  z_rows <- cq$parameters_df[cq$parameters_df$node == "Z", ]
  expect_equal(nrow(z_rows), 16L)
  expect_equal(sum(z_rows$param_value), 1, tolerance = 1e-10)
})

test_that("collider CPT marginals are recovered", {
  # P(Z=1 | X=0, Y=0) = sum of nodal types where bit 1 = 1 (X=0,Y=0 position)
  p00 <- 0.05; p10 <- 0.30; p01 <- 0.40; p11 <- 0.90
  dag <- dagitty("dag { X -> Z; Y -> Z }")
  res <- elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(
      X = list(0.5), Y = list(0.5),
      Z = list(p00, p10, p01, p11)
    )
  )
  cq  <- to_causalqueries(res)
  pdf <- cq$parameters_df

  # Position 1 of the nodal type string = Z | X=0, Y=0
  # Sum all Z parameters whose nodal_type starts with "1"
  z_rows <- pdf[pdf$node == "Z", ]
  recover_00 <- sum(z_rows$param_value[startsWith(z_rows$nodal_type, "1")])
  expect_equal(recover_00, p00, tolerance = 1e-8)

  # Position 2 = Z | X=1, Y=0: second character == "1"
  recover_10 <- sum(z_rows$param_value[substr(z_rows$nodal_type, 2, 2) == "1"])
  expect_equal(recover_10, p10, tolerance = 1e-8)
})

# ---- chain: three-node -------------------------------------------------------

test_that("chain result converts without error and has correct node count", {
  cq <- to_causalqueries(make_chain_result())
  expect_equal(length(unique(cq$parameters_df$node)), 3L)
})

# ---- query_model: end-to-end CPT recovery -----------------------------------
#
# These tests verify that the *model-level* conditional probabilities returned
# by query_model(using = "parameters") match the originally elicited CPT values.
# They serve as integration tests for the full pipeline:
#   elicit_dag_priors -> to_causalqueries -> query_model.

# Helper: run a single query_model call and return the scalar mean.
.qm <- function(cq, query, given = NULL) {
  q <- CausalQueries::query_model(
    cq, queries = query, given = given, using = "parameters"
  )
  q$mean[[1L]]
}

test_that("query_model: P(Y=1|X=0) and P(Y=1|X=1) match elicited CPT (X -> Y)", {
  py0 <- 0.2; py1 <- 0.75
  dag <- dagitty("dag { X -> Y }")
  res <- elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(X = list(0.5), Y = list(py0, py1))
  )
  cq <- to_causalqueries(res)

  expect_equal(.qm(cq, "Y == 1", given = "X == 0"), py0, tolerance = 1e-8)
  expect_equal(.qm(cq, "Y == 1", given = "X == 1"), py1, tolerance = 1e-8)
})

test_that("query_model: interventional P(Y[X=0]=1) and P(Y[X=1]=1) match CPT", {
  py0 <- 0.15; py1 <- 0.80
  dag <- dagitty("dag { X -> Y }")
  res <- elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(X = list(0.5), Y = list(py0, py1))
  )
  cq <- to_causalqueries(res)

  expect_equal(.qm(cq, "Y[X=0] == 1"), py0, tolerance = 1e-8)
  expect_equal(.qm(cq, "Y[X=1] == 1"), py1, tolerance = 1e-8)
})

test_that("query_model: ATE equals py1 - py0 for X -> Y", {
  py0 <- 0.15; py1 <- 0.80
  dag <- dagitty("dag { X -> Y }")
  res <- elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(X = list(0.5), Y = list(py0, py1))
  )
  cq <- to_causalqueries(res)

  # P(Y=1 | do(X=1)) - P(Y=1 | do(X=0))
  ate <- .qm(cq, "Y[X=1] == 1") - .qm(cq, "Y[X=0] == 1")
  expect_equal(ate, py1 - py0, tolerance = 1e-8)
})

test_that("query_model: all four CPT cells recovered for collider X -> Z <- Y", {
  p00 <- 0.10; p10 <- 0.40; p01 <- 0.30; p11 <- 0.90
  dag <- dagitty("dag { X -> Z; Y -> Z }")
  res <- elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(
      X = list(0.5),
      Y = list(0.5),
      Z = list(p00, p10, p01, p11)
    )
  )
  cq <- to_causalqueries(res)

  expect_equal(.qm(cq, "Z == 1", "X == 0 & Y == 0"), p00, tolerance = 1e-8)
  expect_equal(.qm(cq, "Z == 1", "X == 1 & Y == 0"), p10, tolerance = 1e-8)
  expect_equal(.qm(cq, "Z == 1", "X == 0 & Y == 1"), p01, tolerance = 1e-8)
  expect_equal(.qm(cq, "Z == 1", "X == 1 & Y == 1"), p11, tolerance = 1e-8)
})

test_that("query_model: chain X -> Y -> Z — each conditional matches CPT", {
  py0 <- 0.10; py1 <- 0.90
  pz0 <- 0.20; pz1 <- 0.80
  dag <- dagitty("dag { X -> Y -> Z }")
  res <- elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(
      X = list(0.5),
      Y = list(py0, py1),
      Z = list(pz0, pz1)
    )
  )
  cq <- to_causalqueries(res)

  # CPT for Y | X
  expect_equal(.qm(cq, "Y == 1", "X == 0"), py0, tolerance = 1e-8)
  expect_equal(.qm(cq, "Y == 1", "X == 1"), py1, tolerance = 1e-8)

  # CPT for Z | Y
  expect_equal(.qm(cq, "Z == 1", "Y == 0"), pz0, tolerance = 1e-8)
  expect_equal(.qm(cq, "Z == 1", "Y == 1"), pz1, tolerance = 1e-8)
})

test_that("query_model: fork X <- Z -> Y — CPTs for both children match", {
  # Z is the common cause; X and Y are independent given Z
  px0 <- 0.30; px1 <- 0.70   # P(X=1 | Z=0), P(X=1 | Z=1)
  py0 <- 0.10; py1 <- 0.60   # P(Y=1 | Z=0), P(Y=1 | Z=1)
  dag <- dagitty("dag { Z -> X; Z -> Y }")
  res <- elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(
      Z = list(0.5),
      X = list(px0, px1),
      Y = list(py0, py1)
    )
  )
  cq <- to_causalqueries(res)

  expect_equal(.qm(cq, "X == 1", "Z == 0"), px0, tolerance = 1e-8)
  expect_equal(.qm(cq, "X == 1", "Z == 1"), px1, tolerance = 1e-8)
  expect_equal(.qm(cq, "Y == 1", "Z == 0"), py0, tolerance = 1e-8)
  expect_equal(.qm(cq, "Y == 1", "Z == 1"), py1, tolerance = 1e-8)
})

# ---- error handling ---------------------------------------------------------

test_that("to_causalqueries errors on non-result input", {
  expect_error(to_causalqueries(list()), "elicit_dag_result")
})

test_that("to_causalqueries errors on bidirected edges", {
  dag <- dagitty("dag { X <-> Y }")
  res <- elicit_dag_priors(
    dag, verbose = FALSE,
    .responses = list(X = list(0.5), Y = list(0.5))
  )
  expect_error(to_causalqueries(res), "bidirected")
})
