# Helpers -------------------------------------------------------------------

.make_result <- function(dag_str, responses) {
  dag <- dagitty::dagitty(dag_str)
  elicit_dag_priors(dag, verbose = FALSE, .responses = responses)
}

# ---- .cpt_multilinear -------------------------------------------------------

test_that(".cpt_multilinear gives intercept = P(Y=1|X=0) for single parent", {
  probs <- c(0.2, 0.8)   # P(Y=1|X=0)=0.2, P(Y=1|X=1)=0.8
  alpha <- elicitcausal:::.cpt_multilinear(probs, "X")
  expect_equal(alpha[["(Intercept)"]], 0.2, tolerance = 1e-9)
  expect_equal(alpha[["X"]], 0.6, tolerance = 1e-9)  # 0.8 - 0.2
})

test_that(".cpt_multilinear recovers all four CPT entries for two parents", {
  p00 <- 0.1; p10 <- 0.4; p01 <- 0.3; p11 <- 0.9
  probs <- c(p00, p10, p01, p11)
  alpha <- elicitcausal:::.cpt_multilinear(probs, c("X", "Y"))

  poly <- function(x, y) {
    alpha[["(Intercept)"]] +
      alpha[["X"]] * x +
      alpha[["Y"]] * y +
      alpha[["X:Y"]] * x * y
  }

  expect_equal(poly(0, 0), p00, tolerance = 1e-9)
  expect_equal(poly(1, 0), p10, tolerance = 1e-9)
  expect_equal(poly(0, 1), p01, tolerance = 1e-9)
  expect_equal(poly(1, 1), p11, tolerance = 1e-9)
})

test_that(".cpt_multilinear handles independent parents (zero interaction)", {
  # P(Z=1|X,Y) = 0.5 + 0.2*X + 0.1*Y  -- no interaction term
  p00 <- 0.5; p10 <- 0.7; p01 <- 0.6; p11 <- 0.8
  probs <- c(p00, p10, p01, p11)
  alpha <- elicitcausal:::.cpt_multilinear(probs, c("X", "Y"))
  expect_equal(alpha[["X:Y"]], 0, tolerance = 1e-9)
})

# ---- .format_form_terms -----------------------------------------------------

test_that(".format_form_terms formats constant + single term", {
  s <- elicitcausal:::.format_form_terms(c(0.2, 0.6), c("", "X"))
  expect_equal(s, "0.2000 + 0.6000*X")
})

test_that(".format_form_terms handles negative coefficient", {
  s <- elicitcausal:::.format_form_terms(c(0.5, -0.3), c("", "X"))
  expect_equal(s, "0.5000 - 0.3000*X")
})

test_that(".format_form_terms simplifies coefficient of +1 and -1", {
  s_pos <- elicitcausal:::.format_form_terms(c(1), c("X"))
  expect_equal(s_pos, "X")
  s_neg <- elicitcausal:::.format_form_terms(c(-1), c("X"))
  expect_equal(s_neg, "-X")
})

# ---- .alpha_to_edge_forms ---------------------------------------------------

test_that(".alpha_to_edge_forms: single parent gets intercept + main effect", {
  alpha <- c("(Intercept)" = 0.2, "X" = 0.6)
  forms <- elicitcausal:::.alpha_to_edge_forms(alpha, "X")
  expect_equal(forms[["X"]], "0.2000 + 0.6000*X")
})

test_that(".alpha_to_edge_forms: two parents — interaction on first parent", {
  # p00=0.1, p10=0.4, p01=0.3, p11=0.9 -> interaction = 0.9-0.4-0.3+0.1 = 0.3
  probs <- c(0.1, 0.4, 0.3, 0.9)
  alpha <- elicitcausal:::.cpt_multilinear(probs, c("X", "Y"))
  forms <- elicitcausal:::.alpha_to_edge_forms(alpha, c("X", "Y"))

  # X:Y interaction should be on the X edge (first parent)
  expect_true(grepl("X:Y", forms[["X"]]))
  # Y edge should have only the Y main effect
  expect_false(grepl("X:Y", forms[["Y"]]))
})

test_that(".alpha_to_edge_forms: combining edge forms reproduces full polynomial", {
  p00 <- 0.1; p10 <- 0.4; p01 <- 0.3; p11 <- 0.9
  probs <- c(p00, p10, p01, p11)
  alpha <- elicitcausal:::.cpt_multilinear(probs, c("X", "Y"))
  forms <- elicitcausal:::.alpha_to_edge_forms(alpha, c("X", "Y"))

  # Evaluate each form and sum them for all four binary combinations
  # ":" is R's sequence operator when eval'd directly; replace with "*" for
  # numeric multiplication (matching theorytools formula-context semantics)
  form_as_r <- function(s) gsub(":", "*", s, fixed = TRUE)
  eval_form <- function(form_str, X, Y) {
    eval(parse(text = form_as_r(form_str)), list(X = X, Y = Y))
  }
  combined <- function(x, y) {
    eval_form(forms[["X"]], x, y) + eval_form(forms[["Y"]], x, y)
  }

  expect_equal(combined(0, 0), p00, tolerance = 1e-9)
  expect_equal(combined(1, 0), p10, tolerance = 1e-9)
  expect_equal(combined(0, 1), p01, tolerance = 1e-9)
  expect_equal(combined(1, 1), p11, tolerance = 1e-9)
})

# ---- to_theorytools integration ---------------------------------------------

test_that("to_theorytools returns a dagitty object", {
  result <- .make_result(
    "dag { X -> Y }",
    list(X = list(0.5), Y = list(0.2, 0.8))
  )
  addag <- to_theorytools(result)
  expect_s3_class(addag, "dagitty")
})

test_that("to_theorytools: root node has rbinom distribution with correct prob", {
  result <- .make_result(
    "dag { X -> Y }",
    list(X = list(0.35), Y = list(0.2, 0.8))
  )
  addag  <- to_theorytools(result)
  dag_str <- as.character(addag)
  # n is intentionally omitted so theorytools::simulate_data can inject it
  expect_true(grepl('X \\[distribution="rbinom\\(size = 1, prob = 0\\.3500\\)"\\]',
                     dag_str))
})

test_that("to_theorytools: non-root node has rnorm() distribution", {
  result <- .make_result(
    "dag { X -> Y }",
    list(X = list(0.5), Y = list(0.2, 0.8))
  )
  addag   <- to_theorytools(result)
  dag_str <- as.character(addag)
  expect_true(grepl('Y \\[distribution="rnorm\\(\\)"\\]', dag_str))
})

test_that("to_theorytools: edge form encodes conditional mean (single parent)", {
  p0 <- 0.2; p1 <- 0.8
  result <- .make_result(
    "dag { X -> Y }",
    list(X = list(0.5), Y = list(p0, p1))
  )
  dag_str <- as.character(to_theorytools(result))

  # Extract X->Y form and verify it evaluates to the correct conditional means
  m <- regmatches(dag_str,
                  regexpr('X -> Y \\[form="([^"]+)"\\]', dag_str))
  form_str <- sub('X -> Y \\[form="([^"]+)"\\]', "\\1", m)
  form_r   <- gsub(":", "*", form_str, fixed = TRUE)
  expect_equal(eval(parse(text = form_r), list(X = 0)), p0, tolerance = 1e-9)
  expect_equal(eval(parse(text = form_r), list(X = 1)), p1, tolerance = 1e-9)
})

test_that("to_theorytools: simulate_data script is valid R (single parent)", {
  skip_if_not_installed("theorytools")
  result <- .make_result(
    "dag { X -> Y }",
    list(X = list(0.5), Y = list(0.2, 0.8))
  )
  addag  <- to_theorytools(result)
  script <- theorytools::simulate_data(addag, n = 10, run = FALSE)
  # Must parse without error
  expect_no_error(parse(text = paste(script, collapse = "\n")))
})

test_that("to_theorytools: simulate_data: root nodes binary, non-root means correct (single parent)", {
  skip_if_not_installed("theorytools")
  p0 <- 0.2; p1 <- 0.8
  result <- .make_result(
    "dag { X -> Y }",
    list(X = list(0.5), Y = list(p0, p1))
  )
  addag <- to_theorytools(result)
  set.seed(42)
  df <- theorytools::simulate_data(addag, n = 500)
  # Root node X is binary (rbinom)
  expect_true(all(df$X %in% 0:1))
  # Non-root Y is continuous (LPM); conditional means match CPT
  expect_equal(mean(df$Y[df$X == 0]), p0, tolerance = 0.15)
  expect_equal(mean(df$Y[df$X == 1]), p1, tolerance = 0.15)
})

test_that("to_theorytools: simulate_data: collider conditional mean recovers CPT", {
  skip_if_not_installed("theorytools")
  p00 <- 0.1; p10 <- 0.4; p01 <- 0.3; p11 <- 0.9
  result <- .make_result(
    "dag { X -> Z; Y -> Z }",
    list(X = list(0.5), Y = list(0.5), Z = list(p00, p10, p01, p11))
  )
  addag <- to_theorytools(result)
  set.seed(42)
  df <- theorytools::simulate_data(addag, n = 1000)
  # Root nodes X and Y are binary
  expect_true(all(df$X %in% 0:1))
  expect_true(all(df$Y %in% 0:1))
  # Non-root Z is continuous (LPM); conditional mean at X=1,Y=1 ≈ p11
  z11 <- df$Z[df$X == 1 & df$Y == 1]
  expect_gt(length(z11), 10L)
  expect_equal(mean(z11), p11, tolerance = 0.2)
})

test_that("to_theorytools: two-parent edge forms sum recovers CPT", {
  p00 <- 0.1; p10 <- 0.4; p01 <- 0.3; p11 <- 0.9
  result <- .make_result(
    "dag { X -> Z; Y -> Z }",
    list(X = list(0.5), Y = list(0.5), Z = list(p00, p10, p01, p11))
  )
  addag   <- to_theorytools(result)
  dag_str <- as.character(addag)

  # Extract X->Z and Y->Z edge forms and verify their sum equals the full CPT
  extract_form <- function(from, to) {
    m <- regmatches(dag_str,
                    regexpr(sprintf('%s -> %s \\[form="([^"]+)"\\]', from, to),
                            dag_str))
    form_str <- sub(sprintf('%s -> %s \\[form="([^"]+)"\\]', from, to),
                    "\\1", m)
    gsub(":", "*", form_str, fixed = TRUE)  # theorytools semantics: : -> *
  }

  form_xz <- extract_form("X", "Z")
  form_yz <- extract_form("Y", "Z")

  # Sum of both edge form contributions should reproduce the conditional mean
  combined <- function(x, y) {
    eval(parse(text = form_xz), list(X = x, Y = y)) +
    eval(parse(text = form_yz), list(X = x, Y = y))
  }
  expect_equal(combined(0, 0), p00, tolerance = 1e-9)
  expect_equal(combined(1, 0), p10, tolerance = 1e-9)
  expect_equal(combined(0, 1), p01, tolerance = 1e-9)
  expect_equal(combined(1, 1), p11, tolerance = 1e-9)
})

test_that("to_theorytools: chain X -> Y -> Z is handled correctly", {
  result <- .make_result(
    "dag { X -> Y -> Z }",
    list(X = list(0.5), Y = list(0.3, 0.7), Z = list(0.1, 0.9))
  )
  addag   <- to_theorytools(result)
  expect_s3_class(addag, "dagitty")
  dag_str <- as.character(addag)
  # Non-root nodes have rnorm() distribution; edge forms encode conditional mean
  expect_true(grepl('Y \\[distribution="rnorm\\(\\)"\\]', dag_str))
  expect_true(grepl('Z \\[distribution="rnorm\\(\\)"\\]', dag_str))
  expect_true(grepl('X -> Y \\[form=', dag_str))
  expect_true(grepl('Y -> Z \\[form=', dag_str))
})

test_that("to_theorytools: simulate_data works for chain (theorytools)", {
  skip_if_not_installed("theorytools")
  p_Y0 <- 0.3; p_Y1 <- 0.7; p_Z0 <- 0.1; p_Z1 <- 0.9
  result <- .make_result(
    "dag { X -> Y -> Z }",
    list(X = list(0.5), Y = list(p_Y0, p_Y1), Z = list(p_Z0, p_Z1))
  )
  addag <- to_theorytools(result)
  set.seed(42)
  df <- theorytools::simulate_data(addag, n = 500)
  # Root node X is binary
  expect_true(all(df$X %in% 0:1))
  # Non-root Y and Z are continuous (LPM); check conditional means
  expect_equal(mean(df$Y[df$X == 0]), p_Y0, tolerance = 0.15)
  expect_equal(mean(df$Y[df$X == 1]), p_Y1, tolerance = 0.15)
})

test_that("to_theorytools: error on non-elicit_dag_result input", {
  expect_error(to_theorytools(list(a = 1)), "elicit_dag_result")
})
