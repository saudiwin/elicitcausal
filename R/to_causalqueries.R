#' Convert elicited CPTs to a CausalQueries model
#'
#' Takes an \code{elicit_dag_result} object and produces a
#' \code{CausalQueries} model whose nodal-type probabilities are derived from
#' the elicited conditional probability tables.
#'
#' @section Independence assumption:
#' A CPT specifies \eqn{P(\text{node}=1 \mid \text{parents} = \mathbf{c})}
#' for every parent combination \eqn{\mathbf{c}}, but a CausalQueries model
#' requires a probability for every \emph{nodal type} — a complete response
#' function mapping all possible parent value combinations to an outcome. For
#' a node with \eqn{k} binary parents there are \eqn{2^{2^k}} such functions.
#'
#' The conversion adopts the \strong{independence of potential outcomes}
#' assumption: the potential outcomes \eqn{Y(\mathbf{c})} for different parent
#' combinations are treated as independent Bernoulli variables. Under this
#' assumption, the probability of nodal type \eqn{\theta} is
#' \deqn{P(\theta) = \prod_{\mathbf{c} \in \{0,1\}^k}
#'   P(Y = \theta(\mathbf{c}) \mid \text{parents} = \mathbf{c}).}
#' This is the maximum-entropy distribution over response functions that is
#' consistent with the elicited CPT.
#'
#' @section DAG compatibility:
#' The function converts the \pkg{dagitty} DAG to the string format expected
#' by \code{\link[CausalQueries]{make_model}}. Only directed (\code{->}) edges
#' are supported; bidirected (\code{<->}) edges used for latent confounders
#' in dagitty have no direct CausalQueries equivalent and will raise an error.
#'
#' @param result An \code{elicit_dag_result} object from
#'   \code{\link{elicit_dag_priors}}.
#' @param ... Additional arguments passed to
#'   \code{\link[CausalQueries]{make_model}} (e.g.
#'   \code{add_causal_types = FALSE} to skip causal type enumeration for large
#'   models).
#'
#' @return A \code{causal_model} object (as returned by
#'   \code{\link[CausalQueries]{make_model}}) with \code{param_value} in
#'   \code{model$parameters_df} set according to the elicited CPTs and the
#'   independence assumption.
#'
#' @seealso \code{\link{elicit_dag_priors}},
#'   \code{\link[CausalQueries]{make_model}},
#'   \code{\link[CausalQueries]{set_parameters}}
#'
#' @examples
#' \dontrun{
#' library(dagitty)
#' library(CausalQueries)
#'
#' dag <- dagitty("dag { X -> Y -> Z }")
#' result <- elicit_dag_priors(
#'   dag,
#'   verbose    = FALSE,
#'   .responses = list(
#'     X = list(0.5),
#'     Y = list(0.1, 0.9),
#'     Z = list(0.2, 0.8)
#'   )
#' )
#'
#' cq <- to_causalqueries(result)
#' cq$parameters_df
#'
#' # Query with CausalQueries
#' query_model(cq, query = "Y[X=1] > Y[X=0]", using = "parameters")
#' }
#'
#' @export
to_causalqueries <- function(result, ...) {
  .check_result(result)

  if (!requireNamespace("CausalQueries", quietly = TRUE)) {
    stop(
      "Package 'CausalQueries' is required for this function.\n",
      "Install it with: install.packages('CausalQueries')"
    )
  }

  # ------------------------------------------------------------------
  # Build the CausalQueries DAG string from the dagitty object
  # ------------------------------------------------------------------
  cq_dag_str <- .dagitty_to_cq_string(result$dag)

  # ------------------------------------------------------------------
  # Create the base CausalQueries model (default flat parameters)
  # ------------------------------------------------------------------
  cq_model <- CausalQueries::make_model(cq_dag_str, ...)

  # ------------------------------------------------------------------
  # Compute nodal-type probabilities from the CPTs
  # ------------------------------------------------------------------
  pars   <- cq_model$parameters_df
  values <- vapply(
    seq_len(nrow(pars)),
    function(i) .nodal_type_prob(pars[i, ], result$cpts, cq_model$parents),
    numeric(1L)
  )

  # ------------------------------------------------------------------
  # Write the computed probabilities back into the model
  # ------------------------------------------------------------------
  cq_model <- CausalQueries::set_parameters(
    cq_model,
    param_names = pars$param_names,
    parameters  = values
  )

  cq_model
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Convert a dagitty DAG to a CausalQueries model string
#'
#' @param dag A dagitty object
#' @return Character string, e.g. "X -> Y; Z -> Y"
#' @keywords internal
.dagitty_to_cq_string <- function(dag) {
  edge_df  <- as.data.frame(dagitty::edges(dag))

  # Reject bidirected edges — CausalQueries has no equivalent concept
  bidir <- edge_df[edge_df$e == "<->", , drop = FALSE]
  if (nrow(bidir) > 0L) {
    pairs <- paste0(bidir$v, " <-> ", bidir$w)
    stop(
      "The dagitty DAG contains bidirected edge(s) which have no direct ",
      "CausalQueries equivalent:\n  ",
      paste(pairs, collapse = "\n  "),
      "\nRemove or re-encode these edges before converting."
    )
  }

  directed <- edge_df[edge_df$e == "->", , drop = FALSE]

  if (nrow(directed) == 0L) {
    # Isolated nodes — CausalQueries treats them as independent Bernoulli roots
    return(paste(names(dag), collapse = " "))
  }

  paste(paste0(directed$v, " -> ", directed$w), collapse = "; ")
}


#' Compute the probability of one nodal type under the independence assumption
#'
#' @param par_row One row of \code{cq_model$parameters_df}
#' @param cpts Named list of CPT data frames from the elicited result
#' @param cq_parents_df Data frame as stored in \code{cq_model$parents}, with
#'   columns \code{node}, \code{root}, \code{parents}, \code{parent_nodes}
#' @return Numeric probability in [0, 1]
#' @keywords internal
.nodal_type_prob <- function(par_row, cpts, cq_parents_df) {
  node      <- par_row$node
  nt_string <- par_row$nodal_type   # e.g. "0", "1", "01", "1010", "00001111"
  cpt       <- cpts[[node]]
  cpt_parents <- attr(cpt, "parents")

  # ---- Root node -----------------------------------------------------------
  if (length(cpt_parents) == 0L) {
    val <- as.integer(nt_string)   # "0" or "1"
    return(if (val == 1L) cpt$prob[1L] else 1 - cpt$prob[1L])
  }

  # ---- Non-root node -------------------------------------------------------
  # cq_model$parents has a 'parent_nodes' column with comma-separated parent
  # names in the order CausalQueries uses for nodal type encoding.
  # The j-th character of the nodal type string (1-indexed, j in 1..2^k)
  # encodes node's value when parents take the values at position (j-1) in
  # expand.grid(p1=0:1, p2=0:1, ...) — first listed parent cycles fastest.
  pn_str     <- cq_parents_df$parent_nodes[cq_parents_df$node == node]
  cq_parents <- trimws(strsplit(pn_str, ",")[[1L]])

  nt_chars <- strsplit(nt_string, "")[[1L]]
  n_combos <- length(nt_chars)     # = 2^k

  log_prob <- sum(vapply(seq_len(n_combos), function(j) {
    # Decode parent values at position j-1 (0-indexed), CausalQueries ordering
    combo <- .combo_at_position(j - 1L, cq_parents)

    # Look up P(node = 1 | combo) from the CPT (matching by value, not position)
    p1    <- .cpt_lookup(cpt, combo)

    y_val <- as.integer(nt_chars[j])
    p     <- if (y_val == 1L) p1 else 1 - p1
    log(max(p, .Machine$double.eps))
  }, numeric(1L)))

  exp(log_prob)
}


#' Decode the parent value combination at position j (0-indexed)
#'
#' In CausalQueries, the j-th parent combination in the nodal type string
#' is the j-th row of \code{expand.grid(p1=0:1, p2=0:1, ...)}, where the
#' first parent cycles fastest.  Position j encodes:
#'   parent_i = floor(j / 2^(i-1)) mod 2
#'
#' @param j Integer. 0-indexed position
#' @param parent_order Character vector. Parents in CausalQueries order
#' @return Named integer vector of parent values (0 or 1)
#' @keywords internal
.combo_at_position <- function(j, parent_order) {
  vals <- vapply(seq_along(parent_order), function(i) {
    as.integer(floor(j / 2L^(i - 1L)) %% 2L)
  }, integer(1L))
  names(vals) <- parent_order
  vals
}


#' Look up P(node = 1 | parent combo) from a CPT data frame
#'
#' Finds the CPT row where every parent column matches the supplied values
#' and returns the corresponding probability.
#'
#' @param cpt A CPT data frame (class \code{"cpt"})
#' @param parent_combo Named integer vector of parent values
#' @return Numeric probability
#' @keywords internal
.cpt_lookup <- function(cpt, parent_combo) {
  parents <- attr(cpt, "parents")

  mask <- apply(cpt[, parents, drop = FALSE], 1L, function(row) {
    all(as.integer(row) == as.integer(parent_combo[parents]))
  })

  cpt$prob[which(mask)]
}
