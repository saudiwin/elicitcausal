library(dagitty)

test_that("parse_dag extracts nodes and parents correctly", {
  dag <- dagitty("dag { X -> Y }")
  info <- elicitcausal:::parse_dag(dag)

  expect_setequal(info$nodes, c("X", "Y"))
  expect_equal(info$parents[["X"]], character(0))
  expect_equal(info$parents[["Y"]], "X")
})

test_that("parse_dag produces valid topological order", {
  dag  <- dagitty("dag { X -> Y -> Z }")
  info <- elicitcausal:::parse_dag(dag)
  ord  <- info$order

  expect_equal(length(ord), 3L)
  # X must precede Y, Y must precede Z
  expect_lt(which(ord == "X"), which(ord == "Y"))
  expect_lt(which(ord == "Y"), which(ord == "Z"))
})

test_that("parse_dag handles fork and collider structures", {
  # Fork: Z -> X, Z -> Y
  dag_fork <- dagitty("dag { Z -> X; Z -> Y }")
  info     <- elicitcausal:::parse_dag(dag_fork)
  expect_true(which(info$order == "Z") < which(info$order == "X"))
  expect_true(which(info$order == "Z") < which(info$order == "Y"))

  # Collider: X -> Z <- Y
  dag_coll <- dagitty("dag { X -> Z; Y -> Z }")
  info2    <- elicitcausal:::parse_dag(dag_coll)
  expect_true(which(info2$order == "Z") > which(info2$order == "X"))
  expect_true(which(info2$order == "Z") > which(info2$order == "Y"))
})

test_that("parse_dag rejects non-dagitty input", {
  expect_error(elicitcausal:::parse_dag(list()), "dagitty")
})
