#' Compute the marginal distribution of a node
#'
#' Marginalises the full joint distribution over all other nodes.
#'
#' @param result An \code{elicit_dag_result} object
#' @param node Character. Node name
#' @return A data frame with columns \code{value} (0 or 1) and \code{prob}
#' @export
#'
#' @examples
#' \dontrun{
#' result <- elicit_dag_priors(dag, .responses = list(...))
#' get_marginal(result, "Y")
#' }
get_marginal <- function(result, node) {
  .check_result(result)
  if (!node %in% result$dag_info$nodes) {
    stop(sprintf("Node '%s' not found in DAG.", node))
  }

  marginal <- tapply(result$joint$prob, result$joint[[node]], sum)

  data.frame(
    value = as.integer(names(marginal)),
    prob  = as.numeric(marginal),
    stringsAsFactors = FALSE
  )
}


#' Compute a conditional probability P(query | evidence)
#'
#' Filters the joint distribution to rows consistent with the evidence and
#' sums over rows also consistent with the query variables.
#'
#' @param result An \code{elicit_dag_result} object
#' @param query Named integer vector. Query variables and their values (0/1)
#' @param evidence Named integer vector. Evidence variables and their values
#'   (0/1). Default: empty (no conditioning).
#' @return Numeric probability in [0, 1], or \code{NA} if the evidence has
#'   zero probability
#' @export
#'
#' @examples
#' \dontrun{
#' # P(Y = 1 | X = 1)
#' get_conditional(result, query = c(Y = 1L), evidence = c(X = 1L))
#' }
get_conditional <- function(result, query, evidence = integer(0L)) {
  .check_result(result)

  nodes   <- result$dag_info$nodes
  all_vars <- c(names(query), names(evidence))
  bad <- all_vars[!all_vars %in% nodes]
  if (length(bad) > 0L) {
    stop(sprintf("Unknown node(s): %s", paste(bad, collapse = ", ")))
  }

  joint <- result$joint

  # Rows matching the evidence
  ev_mask <- if (length(evidence) > 0L) {
    apply(joint[, names(evidence), drop = FALSE], 1L, function(row) {
      all(as.integer(row) == as.integer(evidence))
    })
  } else {
    rep(TRUE, nrow(joint))
  }

  # Rows matching both query AND evidence
  qe_mask <- apply(joint[, names(query), drop = FALSE], 1L, function(row) {
    all(as.integer(row) == as.integer(query))
  }) & ev_mask

  p_ev <- sum(joint$prob[ev_mask])
  if (p_ev == 0) {
    warning("Evidence has zero probability under the elicited model.")
    return(NA_real_)
  }

  sum(joint$prob[qe_mask]) / p_ev
}


#' Compute mutual information between two nodes
#'
#' \deqn{I(X; Y) = H(Y) - H(Y \mid X)}
#'
#' @param result An \code{elicit_dag_result} object
#' @param x_node Character. First node name
#' @param y_node Character. Second node name
#' @param base Numeric. Logarithm base (default 2 = bits)
#' @return Numeric mutual information in the chosen unit
#' @export
#'
#' @examples
#' \dontrun{
#' get_mutual_info(result, "X", "Y")
#' }
get_mutual_info <- function(result, x_node, y_node, base = 2) {
  .check_result(result)
  nodes <- result$dag_info$nodes
  for (n in c(x_node, y_node)) {
    if (!n %in% nodes) stop(sprintf("Node '%s' not found in DAG.", n))
  }

  joint <- result$joint

  # H(Y)
  py  <- tapply(joint$prob, joint[[y_node]], sum)
  h_y <- shannon_entropy(as.numeric(py), base)

  # H(Y | X)
  h_y_x <- conditional_entropy(joint, y_node, x_node, base)

  # Clamp to zero: floating-point imprecision can produce tiny negatives
  max(0, h_y - h_y_x)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.check_result <- function(x) {
  if (!inherits(x, "elicit_dag_result")) {
    stop("Input must be an 'elicit_dag_result' object from elicit_dag_priors().")
  }
}
