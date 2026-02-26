library(dagitty)

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

make_xy <- function(px = 0.3, py0 = 0.2, py1 = 0.75) {
  elicit_dag_priors(
    dagitty("dag { X -> Y }"), verbose = FALSE,
    .responses = list(X = list(px), Y = list(py0, py1))
  )
}

make_chain <- function() {
  elicit_dag_priors(
    dagitty("dag { X -> Y -> Z }"), verbose = FALSE,
    .responses = list(X = list(0.4), Y = list(0.1, 0.9), Z = list(0.2, 0.8))
  )
}

make_collider <- function() {
  elicit_dag_priors(
    dagitty("dag { X -> Z; Y -> Z }"), verbose = FALSE,
    .responses = list(
      X = list(0.5), Y = list(0.5),
      Z = list(0.05, 0.30, 0.40, 0.90)
    )
  )
}

# ---------------------------------------------------------------------------
# 1. export_priors_csv -> import_elicit_result (.csv) round-trip
# ---------------------------------------------------------------------------

test_that("CSV round-trip: X -> Y — CPT probs preserved", {
  res <- make_xy()
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  export_priors_csv(res, tmp)
  imp <- import_elicit_result(tmp)

  expect_s3_class(imp, "elicit_dag_result")
  expect_equal(imp$cpts$X$prob, res$cpts$X$prob, tolerance = 1e-9)
  expect_equal(imp$cpts$Y$prob, res$cpts$Y$prob, tolerance = 1e-9)
})

test_that("CSV round-trip: metadata fields preserved", {
  res <- elicit_dag_priors(
    dagitty("dag { X -> Y }"), verbose = FALSE,
    target = "Y",
    labels = list(X = c("control", "treated"), Y = c("no", "yes")),
    .responses = list(X = list(0.4), Y = list(0.1, 0.8))
  )
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  export_priors_csv(res, tmp)
  imp <- import_elicit_result(tmp)

  expect_equal(imp$mode,   "probability")
  expect_equal(imp$target, "Y")
  expect_equal(imp$labels$X, c("control", "treated"))
  expect_equal(imp$labels$Y, c("no", "yes"))
  expect_false(is.null(imp$target_marginal))
})

test_that("CSV round-trip: chain X -> Y -> Z", {
  res <- make_chain()
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  export_priors_csv(res, tmp)
  imp <- import_elicit_result(tmp)

  expect_equal(imp$cpts$X$prob, res$cpts$X$prob, tolerance = 1e-9)
  expect_equal(imp$cpts$Y$prob, res$cpts$Y$prob, tolerance = 1e-9)
  expect_equal(imp$cpts$Z$prob, res$cpts$Z$prob, tolerance = 1e-9)
})

test_that("CSV round-trip: collider X -> Z <- Y (two parents)", {
  res <- make_collider()
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  export_priors_csv(res, tmp)
  imp <- import_elicit_result(tmp)

  expect_equal(imp$cpts$Z$prob, res$cpts$Z$prob, tolerance = 1e-9)
})

test_that("CSV round-trip: joint entropy is preserved", {
  res <- make_xy()
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  export_priors_csv(res, tmp)
  imp <- import_elicit_result(tmp)

  expect_equal(imp$entropy, res$entropy, tolerance = 1e-9)
})

test_that("CSV file has 5 metadata lines before the header", {
  res <- make_xy()
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  export_priors_csv(res, tmp)
  meta <- readLines(tmp, n = 5L)
  # Row 1 must start with elicitcausal_export (possibly quoted)
  expect_true(grepl("elicitcausal_export", meta[1L]))
  # Row 2 must contain dag
  expect_true(grepl("dag", meta[2L], fixed = TRUE))
  # 6th line is the CPT header
  header_line <- readLines(tmp, n = 6L)[6L]
  expect_true(grepl("node", header_line, fixed = TRUE))
  expect_true(grepl("prob", header_line, fixed = TRUE))
})

test_that("import_elicit_result errors on unsupported extension", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("dummy", tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_error(import_elicit_result(tmp), "Unsupported file extension")
})

test_that("import_elicit_result errors on missing file", {
  expect_error(import_elicit_result("/nonexistent/path/file.csv"), "not found")
})

# ---------------------------------------------------------------------------
# 2. export_priors_qmd (qmd_bundle) -> import_elicit_result (.qmd) round-trip
# ---------------------------------------------------------------------------

test_that("QMD bundle round-trip: CPT probs preserved after unzip", {
  res     <- make_xy()
  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempfile("qmd_rt_")
  dir.create(tmp_dir)
  on.exit({ unlink(tmp_zip); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

  export_priors_qmd(res, tmp_zip, output_format = "qmd_bundle")
  expect_true(file.exists(tmp_zip))

  utils::unzip(tmp_zip, exdir = tmp_dir)
  qmd_file <- file.path(tmp_dir, "preregistration.qmd")
  csv_file <- file.path(tmp_dir, "priors.csv")
  expect_true(file.exists(qmd_file))
  expect_true(file.exists(csv_file))

  imp <- import_elicit_result(qmd_file)
  expect_s3_class(imp, "elicit_dag_result")
  expect_equal(imp$cpts$X$prob, res$cpts$X$prob, tolerance = 1e-9)
  expect_equal(imp$cpts$Y$prob, res$cpts$Y$prob, tolerance = 1e-9)
})

test_that("QMD bundle: falls back to cpt_inline when no companion CSV", {
  res     <- make_xy()
  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempfile("qmd_inline_")
  dir.create(tmp_dir)
  on.exit({ unlink(tmp_zip); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

  export_priors_qmd(res, tmp_zip, output_format = "qmd_bundle")
  utils::unzip(tmp_zip, exdir = tmp_dir)

  qmd_file <- file.path(tmp_dir, "preregistration.qmd")
  csv_file <- file.path(tmp_dir, "priors.csv")

  # Remove the companion CSV to force YAML cpt_inline path
  unlink(csv_file)
  expect_false(file.exists(csv_file))

  imp <- import_elicit_result(qmd_file)
  expect_s3_class(imp, "elicit_dag_result")
  expect_equal(imp$cpts$X$prob, res$cpts$X$prob, tolerance = 1e-9)
  expect_equal(imp$cpts$Y$prob, res$cpts$Y$prob, tolerance = 1e-9)
})

test_that("QMD file contains elicitcausal YAML block", {
  res     <- make_xy()
  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempfile("qmd_yaml_")
  dir.create(tmp_dir)
  on.exit({ unlink(tmp_zip); unlink(tmp_dir, recursive = TRUE) }, add = TRUE)

  export_priors_qmd(res, tmp_zip, output_format = "qmd_bundle")
  utils::unzip(tmp_zip, exdir = tmp_dir)
  qmd_lines <- readLines(file.path(tmp_dir, "preregistration.qmd"))

  expect_true(any(grepl("^elicitcausal:", qmd_lines)))
  expect_true(any(grepl("cpt_inline:", qmd_lines, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# 3. RDS round-trip (elicit_dag_result saved and re-loaded)
# ---------------------------------------------------------------------------

test_that("RDS round-trip: elicit_dag_result returned as-is", {
  res <- make_xy()
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  saveRDS(res, tmp)
  imp <- import_elicit_result(tmp)

  expect_s3_class(imp, "elicit_dag_result")
  expect_equal(imp$cpts$X$prob, res$cpts$X$prob, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# 4. to_causalqueries -> from_causalqueries round-trip
# ---------------------------------------------------------------------------

test_that("CQ round-trip: X -> Y — CPT probs preserved", {
  skip_if_not_installed("CausalQueries")
  res  <- make_xy(px = 0.4, py0 = 0.15, py1 = 0.80)
  cq   <- to_causalqueries(res)
  back <- from_causalqueries(cq)

  expect_s3_class(back, "elicit_dag_result")
  expect_equal(back$cpts$X$prob, res$cpts$X$prob, tolerance = 1e-8)
  expect_equal(back$cpts$Y$prob, res$cpts$Y$prob, tolerance = 1e-8)
})

test_that("CQ round-trip: collider X -> Z <- Y", {
  skip_if_not_installed("CausalQueries")
  res  <- make_collider()
  cq   <- to_causalqueries(res)
  back <- from_causalqueries(cq)

  expect_equal(back$cpts$Z$prob, res$cpts$Z$prob, tolerance = 1e-8)
})

test_that("CQ round-trip: chain X -> Y -> Z", {
  skip_if_not_installed("CausalQueries")
  res  <- make_chain()
  cq   <- to_causalqueries(res)
  back <- from_causalqueries(cq)

  expect_equal(back$cpts$Y$prob, res$cpts$Y$prob, tolerance = 1e-8)
  expect_equal(back$cpts$Z$prob, res$cpts$Z$prob, tolerance = 1e-8)
})

test_that("CQ RDS round-trip: causal_model RDS imported via import_elicit_result", {
  skip_if_not_installed("CausalQueries")
  res <- make_xy()
  cq  <- to_causalqueries(res)
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  saveRDS(cq, tmp)
  imp <- import_elicit_result(tmp)

  expect_s3_class(imp, "elicit_dag_result")
  expect_equal(imp$cpts$X$prob, res$cpts$X$prob, tolerance = 1e-8)
  expect_equal(imp$cpts$Y$prob, res$cpts$Y$prob, tolerance = 1e-8)
})

test_that("from_causalqueries errors on non-causal_model input", {
  skip_if_not_installed("CausalQueries")
  expect_error(from_causalqueries(list()), "causal_model")
})

# ---------------------------------------------------------------------------
# 5. to_theorytools -> from_theorytools round-trip
# ---------------------------------------------------------------------------

test_that("TT round-trip: X -> Y — CPT probs preserved", {
  res  <- make_xy(px = 0.4, py0 = 0.20, py1 = 0.70)
  tt   <- to_theorytools(res)
  back <- from_theorytools(tt)

  expect_s3_class(back, "elicit_dag_result")
  expect_equal(back$cpts$X$prob, res$cpts$X$prob, tolerance = 1e-9)
  expect_equal(back$cpts$Y$prob, res$cpts$Y$prob, tolerance = 1e-6)
})

test_that("TT round-trip: collider X -> Z <- Y", {
  res  <- make_collider()
  tt   <- to_theorytools(res)
  back <- from_theorytools(tt)

  expect_equal(back$cpts$Z$prob, res$cpts$Z$prob, tolerance = 1e-6)
})

test_that("TT round-trip: chain X -> Y -> Z", {
  res  <- make_chain()
  tt   <- to_theorytools(res)
  back <- from_theorytools(tt)

  expect_equal(back$cpts$Y$prob, res$cpts$Y$prob, tolerance = 1e-6)
  expect_equal(back$cpts$Z$prob, res$cpts$Z$prob, tolerance = 1e-6)
})

test_that("TT RDS round-trip: theorytools dagitty RDS imported via import_elicit_result", {
  res <- make_xy()
  tt  <- to_theorytools(res)
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  saveRDS(tt, tmp)
  imp <- import_elicit_result(tmp)

  expect_s3_class(imp, "elicit_dag_result")
  expect_equal(imp$cpts$X$prob, res$cpts$X$prob, tolerance = 1e-9)
  expect_equal(imp$cpts$Y$prob, res$cpts$Y$prob, tolerance = 1e-6)
})

test_that("from_theorytools errors on plain dagitty without distribution attributes", {
  plain <- dagitty("dag { X -> Y }")
  expect_error(from_theorytools(plain), "distribution")
})

test_that("from_theorytools errors on non-dagitty input", {
  expect_error(from_theorytools(list()), "dagitty")
})

# ---------------------------------------------------------------------------
# 6. Helper: .encode_labels / .decode_labels round-trip
# ---------------------------------------------------------------------------

test_that(".encode_labels / .decode_labels are inverse for typical labels", {
  labels <- list(X = c("control", "treated"), Y = c("no", "yes"))
  enc    <- elicitcausal:::.encode_labels(labels)
  dec    <- elicitcausal:::.decode_labels(enc)
  expect_equal(dec$X, labels$X)
  expect_equal(dec$Y, labels$Y)
})

test_that(".encode_labels returns empty string for NULL input", {
  expect_equal(elicitcausal:::.encode_labels(NULL), "")
})

test_that(".decode_labels returns NULL for empty string", {
  expect_null(elicitcausal:::.decode_labels(""))
})
