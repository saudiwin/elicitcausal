#' Compute the full joint probability distribution from a set of CPTs
#'
#' Uses the Bayesian network chain rule:
#' \deqn{P(x_1, \ldots, x_n) = \prod_{i=1}^{n} P(x_i \mid \text{parents}(x_i))}
#'
#' All nodes are assumed binary (0/1). The resulting distribution has
#' \eqn{2^n} rows.
#'
#' @param nodes Character vector. Node names in **topological order**
#'   (parents before children).
#' @param cpts Named list of CPT data frames as returned by
#'   \code{\link{elicit_node_cpt}}.
#' @return A data frame with one column per node (integer 0/1 values) plus a
#'   numeric \code{prob} column. Row probabilities sum to 1.
#' @keywords internal
compute_joint_distribution <- function(nodes, cpts) {
  # Enumerate every combination of node values
  all_combos <- expand.grid(
    lapply(nodes, function(.) c(0L, 1L)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  names(all_combos) <- nodes

  # Compute log-probability for each state for numerical stability
  log_probs <- apply(all_combos, 1L, function(row) {
    state        <- as.integer(row)
    names(state) <- nodes
    compute_log_prob(state, nodes, cpts)
  })

  # Shift by max before exponentiating (avoids underflow)
  log_probs <- log_probs - max(log_probs)
  probs     <- exp(log_probs)
  probs     <- probs / sum(probs)   # renormalise (handles floating-point drift)

  all_combos$prob <- probs
  all_combos
}


#' Compute the log-probability of one complete state assignment
#'
#' @param state Named integer vector. Full assignment of all node values (0/1)
#' @param nodes Character vector. Node names in topological order
#' @param cpts Named list of CPTs
#' @return Numeric log-probability (may be \code{-Inf} for impossible states)
#' @keywords internal
compute_log_prob <- function(state, nodes, cpts) {
  log_p <- 0

  for (node in nodes) {
    cpt      <- cpts[[node]]
    parents  <- attr(cpt, "parents")
    node_val <- state[node]

    if (length(parents) == 0L) {
      p1 <- cpt$prob[1L]
    } else {
      parent_vals <- state[parents]

      # Find the CPT row that matches all parent values
      match_mask <- apply(
        cpt[, parents, drop = FALSE], 1L,
        function(r) all(as.integer(r) == as.integer(parent_vals))
      )
      p1 <- cpt$prob[which(match_mask)]
    }

    p     <- if (node_val == 1L) p1 else (1 - p1)
    log_p <- log_p + log(max(p, .Machine$double.eps))
  }

  log_p
}
