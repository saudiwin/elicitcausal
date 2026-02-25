#' Shannon entropy
#'
#' Computes \eqn{H = -\sum_i p_i \log_b p_i}, ignoring zero-probability
#' entries (treating \eqn{0 \log 0 = 0}).
#'
#' @param probs Numeric vector of probabilities (need not be pre-normalised to
#'   sum to 1, but should be non-negative)
#' @param base Numeric. Logarithm base. Default 2 gives entropy in **bits**;
#'   use \code{exp(1)} for nats.
#' @return Numeric scalar. Entropy in the chosen unit.
#' @keywords internal
shannon_entropy <- function(probs, base = 2) {
  probs <- probs[probs > 0]
  -sum(probs * log(probs, base = base))
}


#' Marginal Shannon entropy for every node
#'
#' @param joint Data frame. Joint distribution as returned by
#'   \code{\link{compute_joint_distribution}}
#' @param nodes Character vector. Node names
#' @param base Numeric. Logarithm base
#' @return Named numeric vector of marginal entropies, one per node
#' @keywords internal
node_marginal_entropies <- function(joint, nodes, base = 2) {
  vapply(nodes, function(node) {
    marginal <- tapply(joint$prob, joint[[node]], sum)
    shannon_entropy(as.numeric(marginal), base)
  }, numeric(1L))
}


#' Conditional entropy \eqn{H(Y \mid \mathbf{X})}
#'
#' \deqn{H(Y \mid \mathbf{X}) = \sum_{\mathbf{x}} P(\mathbf{X} = \mathbf{x})
#'   H(Y \mid \mathbf{X} = \mathbf{x})}
#'
#' @param joint Data frame. Joint distribution
#' @param y_node Character. Outcome node name
#' @param x_nodes Character vector. Conditioning node names (may be length-0,
#'   in which case the marginal entropy is returned)
#' @param base Numeric. Logarithm base
#' @return Numeric conditional entropy
#' @keywords internal
conditional_entropy <- function(joint, y_node, x_nodes, base = 2) {
  if (length(x_nodes) == 0L) {
    marginal <- tapply(joint$prob, joint[[y_node]], sum)
    return(shannon_entropy(as.numeric(marginal), base))
  }

  x_combos <- unique(joint[, x_nodes, drop = FALSE])
  h_cond   <- 0

  for (i in seq_len(nrow(x_combos))) {
    x_vals <- x_combos[i, , drop = FALSE]

    mask <- apply(joint[, x_nodes, drop = FALSE], 1L, function(row) {
      all(as.integer(row) == as.integer(x_vals))
    })

    p_x <- sum(joint$prob[mask])
    if (p_x > 0) {
      # Marginalize over all variables except y_node to get P(Y | X = x_vals)
      p_y_x  <- tapply(joint$prob[mask], joint[[y_node]][mask], sum) / p_x
      h_cond <- h_cond + p_x * shannon_entropy(as.numeric(p_y_x), base)
    }
  }

  h_cond
}
