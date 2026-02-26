#' Convert elicited CPTs to a theorytools augmented DAG
#'
#' Takes an \code{elicit_dag_result} object and produces a \code{dagitty}
#' object annotated with \code{distribution} (node attribute) and \code{form}
#' (edge attribute) metadata as expected by the \pkg{theorytools} package.
#'
#' @section Distribution attribute:
#' Every node receives a \code{distribution} annotation:
#' \itemize{
#'   \item \strong{Root nodes} (no parents): \code{"rbinom(size = 1, prob = p)"}
#'     where \code{p} is the elicited prior probability.  The sample size
#'     \code{n} is intentionally omitted because
#'     \code{theorytools::simulate_data} injects it automatically.
#'   \item \strong{Non-root nodes}: \code{"rnorm()"}.  The conditional mean is
#'     encoded in the edge \code{form} attributes (see below); theorytools
#'     generates \code{node <- <form_sum> + rnorm(n = n)}, producing a linear
#'     probability model (LPM) with the correct conditional expectation for
#'     every parent combination.
#' }
#'
#' @section Form attribute:
#' Every edge into a non-root node carries a \code{form} attribute encoding
#' that edge's contribution to the conditional mean.  The multilinear
#' probability polynomial \eqn{P(\text{node}=1 \mid \text{pa})} is distributed
#' across edges using the earliest-parent rule: the intercept and any
#' interaction monomial are attributed to the edge of the first (lowest-index)
#' parent involved.  Summing all edge form contributions reproduces the full
#' conditional mean exactly.  Coefficients follow standard R arithmetic; the
#' \code{:} separator is used for interaction terms as expected by theorytools
#' (which converts \code{:} to \code{*} before evaluation).
#'
#' For statistical estimation the multilinear polynomial implies the model
#' formula \code{Y ~ X1 * X2 * \ldots * Xk} fit with
#' \code{glm(\ldots, family = binomial)} or OLS.
#'
#' @section Using with theorytools:
#' The returned object is a standard \code{dagitty} object and is compatible
#' with all \pkg{dagitty} functions.  \pkg{theorytools} functions that consume
#' augmented DAGs — such as \code{\link[theorytools]{simulate_data}} and
#' \code{\link[theorytools]{derive_formula}} — will read the \code{distribution}
#' and \code{form} attributes automatically.
#'
#' To simulate data:
#' \preformatted{
#' addag <- to_theorytools(result)
#' theorytools::simulate_data(addag, n = 500)
#' }
#'
#' To derive formulae for statistical estimation:
#' \preformatted{
#' theorytools::derive_formula(addag, exposure = "X", outcome = "Y")
#' }
#'
#' For binary outcomes the appropriate estimation model is logistic
#' regression (or, for exact representation, binomial regression with an
#' identity link):
#' \preformatted{
#' glm(Y ~ X1 * X2, data = df, family = binomial)
#' }
#'
#' @param result An \code{elicit_dag_result} object from
#'   \code{\link{elicit_dag_priors}}.
#' @param ... Currently unused; reserved for future arguments.
#'
#' @return A \code{dagitty} object of class \code{c("dagitty")} annotated
#'   with \code{distribution} node attributes and \code{form} edge attributes.
#'
#' @seealso \code{\link{elicit_dag_priors}}, \code{\link{to_causalqueries}}
#'
#' @examples
#' \dontrun{
#' library(dagitty)
#'
#' dag <- dagitty("dag { X -> Y -> Z }")
#' result <- elicit_dag_priors(
#'   dag,
#'   .responses = list(
#'     X = list(0.5),
#'     Y = list(0.2, 0.8),
#'     Z = list(0.1, 0.9)
#'   )
#' )
#'
#' addag <- to_theorytools(result)
#' cat(as.character(addag))   # inspect the annotated DAG string
#'
#' # Simulate data (requires theorytools)
#' # theorytools::simulate_data(addag, n = 300)
#' }
#'
#' @export
to_theorytools <- function(result, ...) {
  .check_result(result)

  nodes        <- result$dag_info$order
  parents_list <- result$dag_info$parents
  cpts         <- result$cpts

  # ---- Per-node distribution strings and per-edge form strings ---------------
  node_dists <- stats::setNames(character(length(nodes)), nodes)
  edge_forms <- list()   # key: "parent::child", value: form string

  for (node in nodes) {
    cpt  <- cpts[[node]]
    pars <- attr(cpt, "parents")

    if (length(pars) == 0L) {
      # Root node: full Bernoulli specification with the elicited prior.
      # Note: theorytools' simulate_data injects n itself, so it must NOT
      # appear in the distribution string (matching the theorytools convention
      # e.g. "rbinom(size = 2, prob = .5)").
      p <- cpt$prob[[1L]]
      node_dists[node] <- sprintf("rbinom(size = 1, prob = %.4f)", p)

    } else {
      # Non-root: encode the conditional expectation through per-edge form
      # attributes and use rnorm() as the residual distribution.
      #
      # theorytools::simulate_data generates:
      #   node <- <form_sum> + rnorm(n = n)
      # where <form_sum> is the sum of all incoming edge form contributions.
      # By distributing the multilinear polynomial across edges (intercept and
      # interaction monomials on the earliest-involved parent edge) the sum
      # of all form contributions equals the full conditional mean E[node|pa].
      # This produces a linear probability model (LPM) — continuous-valued but
      # with the correct conditional expectation for each parent combination.
      alpha <- .cpt_multilinear(cpt$prob, pars)
      forms <- .alpha_to_edge_forms(alpha, pars)
      node_dists[node] <- "rnorm()"
      for (par in pars) {
        edge_forms[[paste0(par, "::", node)]] <- forms[[par]]
      }
    }
  }

  # ---- Build the annotated dagitty string ------------------------------------
  dag_str <- .build_addag_string(nodes, node_dists, parents_list, edge_forms)
  dagitty::dagitty(dag_str)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Compute multilinear polynomial coefficients from a CPT
#'
#' For a binary child node with \code{k} binary parents the unique
#' multilinear polynomial that exactly matches every CPT entry is determined
#' by the Möbius transform.  The \eqn{2^k} coefficients satisfy:
#' \deqn{P(Y=1 \mid x_1,\ldots,x_k)
#'   = \sum_{S \subseteq \{1,\ldots,k\}} \alpha_S \prod_{i \in S} x_i.}
#'
#' The coefficients are found by solving the \eqn{2^k \times 2^k} linear
#' system \eqn{X_{\text{design}} \alpha = p}, where the design matrix is the
#' model matrix for \code{~ p1 * p2 * \ldots * pk} evaluated at all binary
#' parent combinations in \code{\link[base]{expand.grid}} order.
#'
#' @param probs Numeric vector of length \eqn{2^k}.  CPT probabilities in
#'   the same order as \code{\link[base]{expand.grid}(lapply(pars, \(.) c(0L,1L)))},
#'   i.e. the first parent cycles fastest.
#' @param parent_names Character vector of parent names (length \eqn{k \ge 1}).
#' @return Named numeric vector of \eqn{2^k} coefficients.  Names follow
#'   \code{model.matrix} conventions: \code{"(Intercept)"} for \eqn{\alpha_\emptyset},
#'   parent name for main effects, \code{"p1:p2"} for two-way interactions, etc.
#' @keywords internal
.cpt_multilinear <- function(probs, parent_names) {
  k <- length(parent_names)

  combos <- expand.grid(
    lapply(parent_names, function(.) c(0L, 1L)),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  names(combos) <- parent_names

  formula_rhs <- if (k == 1L) parent_names
                 else paste(parent_names, collapse = " * ")
  X_design <- stats::model.matrix(
    stats::as.formula(paste0("~ ", formula_rhs)),
    data = as.data.frame(combos)
  )

  alpha        <- solve(X_design, probs)
  names(alpha) <- colnames(X_design)
  alpha
}


#' Distribute multilinear coefficients across edges
#'
#' Each monomial \eqn{\alpha_S \prod_{i \in S} x_i} is attributed to the edge
#' of \eqn{\min(S)} (the parent with the smallest position in
#' \code{parent_names}).  The intercept (\eqn{\alpha_\emptyset}) goes to the
#' first parent's edge.
#'
#' @param alpha Named numeric vector as returned by \code{.cpt_multilinear}.
#' @param parent_names Character vector of parent names in the order they
#'   appear in the CPT (same order as in \code{alpha}).
#' @return Named list with one element per parent; each element is a character
#'   string representing that edge's contribution to the conditional probability.
#' @keywords internal
.alpha_to_edge_forms <- function(alpha, parent_names) {
  k <- length(parent_names)

  # Accumulate (coef, term_string) pairs per parent
  acc_coefs <- vector("list", k)
  acc_terms <- vector("list", k)
  for (i in seq_len(k)) { acc_coefs[[i]] <- numeric(0); acc_terms[[i]] <- character(0) }

  for (term_name in names(alpha)) {
    coef <- alpha[[term_name]]
    if (abs(coef) < 1e-9) next   # negligible — skip

    if (term_name == "(Intercept)") {
      # Constant term: assign to the first parent's edge
      acc_coefs[[1L]] <- c(acc_coefs[[1L]], coef)
      acc_terms[[1L]] <- c(acc_terms[[1L]], "")
    } else {
      parts <- strsplit(term_name, ":")[[1L]]
      # Assign to the earliest parent involved in this monomial
      idx <- min(match(parts, parent_names), na.rm = TRUE)
      acc_coefs[[idx]] <- c(acc_coefs[[idx]], coef)
      acc_terms[[idx]] <- c(acc_terms[[idx]], paste(parts, collapse = ":"))
    }
  }

  # Format each parent's accumulated terms into a single form string
  forms <- vector("list", k)
  names(forms) <- parent_names
  for (i in seq_len(k)) {
    forms[[i]] <- if (length(acc_coefs[[i]]) == 0L) "0"
                  else .format_form_terms(acc_coefs[[i]], acc_terms[[i]])
  }
  forms
}


#' Build the complete multilinear probability expression (R arithmetic syntax)
#'
#' Produces the full polynomial \eqn{f(x_1,\ldots,x_k)} as a single R
#' expression suitable for embedding in a \code{distribution} attribute, e.g.
#' \code{"0.1000 + 0.3000*X + 0.2000*Y + 0.3000*X*Y"}.  Interaction terms
#' use \code{*} (standard R multiplication) rather than \code{:}, so the
#' expression can be directly evaluated inside a call to \code{rbinom()}.
#'
#' @param alpha Named numeric vector as returned by \code{.cpt_multilinear}.
#' @param parent_names Character vector of parent names.
#' @return A single character string.
#' @keywords internal
.full_prob_expr <- function(alpha, parent_names) {
  coefs <- numeric(0)
  terms <- character(0)
  for (term_name in names(alpha)) {
    coef <- alpha[[term_name]]
    if (abs(coef) < 1e-9) next
    coefs <- c(coefs, coef)
    if (term_name == "(Intercept)") {
      terms <- c(terms, "")
    } else {
      # Use * for multiplication so the string is evaluable R code
      parts <- strsplit(term_name, ":")[[1L]]
      terms <- c(terms, paste(parts, collapse = "*"))
    }
  }
  if (length(coefs) == 0L) return("0")
  .format_form_terms(coefs, terms)
}


#' Format a vector of (coefficient, term) pairs as a form expression string
#'
#' Produces a human-readable expression like \code{"0.2000 + 0.6000*X"} or
#' \code{"0.1000 + 0.3000*X - 0.1000*X:Y"}.  Terms with coefficient
#' \eqn{\approx \pm 1} are simplified (e.g. \code{"X"} instead of
#' \code{"1.0000*X"}).
#'
#' @param coefs Numeric vector of coefficients.
#' @param terms Character vector of term names (same length as \code{coefs}).
#'   An empty string \code{""} denotes the constant/intercept term.
#' @return A single character string.
#' @keywords internal
.format_form_terms <- function(coefs, terms) {
  parts <- character(length(coefs))
  for (i in seq_along(coefs)) {
    c_val <- coefs[i]
    t_nm  <- terms[i]
    if (nchar(t_nm) == 0L) {
      parts[i] <- sprintf("%.4f", c_val)        # pure constant
    } else if (abs(c_val - 1) < 1e-8) {
      parts[i] <- t_nm                           # coefficient is +1
    } else if (abs(c_val + 1) < 1e-8) {
      parts[i] <- paste0("-", t_nm)             # coefficient is -1
    } else {
      parts[i] <- sprintf("%.4f*%s", c_val, t_nm)
    }
  }

  # Combine with "+" / "-" separators, handling leading negatives cleanly
  result <- parts[1L]
  for (i in seq_along(parts)[-1L]) {
    p <- parts[i]
    if (startsWith(p, "-")) {
      result <- paste0(result, " - ", substring(p, 2L))
    } else {
      result <- paste0(result, " + ", p)
    }
  }
  result
}


#' Build an augmented dagitty string
#'
#' Constructs the \code{dag { ... }} text that dagitty can parse, with
#' \code{distribution} attributes on nodes and \code{form} attributes on edges.
#'
#' @param nodes Character vector of node names in topological order.
#' @param node_dists Named character vector of distribution strings.
#' @param parents_list Named list mapping each node to its parent names.
#' @param edge_forms Named list; key is \code{"parent::child"}, value is the
#'   form string for that edge.
#' @return A single character string (valid dagitty model code).
#' @keywords internal
.build_addag_string <- function(nodes, node_dists, parents_list, edge_forms) {
  lines <- character(0)

  # Node declarations with distribution attribute
  for (node in nodes) {
    lines <- c(lines,
               sprintf('  %s [distribution="%s"]', node, node_dists[node]))
  }

  # Edge declarations with form attribute
  for (node in nodes) {
    pars <- parents_list[[node]]
    for (par in pars) {
      key  <- paste0(par, "::", node)
      form <- edge_forms[[key]]
      if (!is.null(form)) {
        lines <- c(lines, sprintf('  %s -> %s [form="%s"]', par, node, form))
      } else {
        lines <- c(lines, sprintf('  %s -> %s', par, node))
      }
    }
  }

  paste0("dag {\n", paste(lines, collapse = "\n"), "\n}")
}
