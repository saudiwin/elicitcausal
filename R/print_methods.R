#' Print an elicit_dag_result object
#'
#' Displays the elicited conditional probability tables, a summary of the
#' joint distribution, Shannon entropy, and (if a target was specified) the
#' target node's marginal distribution.
#'
#' @param x An \code{elicit_dag_result} object
#' @param digits Integer. Decimal places for displayed probabilities
#' @param top_states Integer. Number of highest-probability joint states to
#'   display (set to 0 to suppress)
#' @param ... Ignored
#' @export
print.elicit_dag_result <- function(x, digits = 4L, top_states = 5L, ...) {
  nodes    <- x$dag_info$order
  n_nodes  <- length(nodes)
  n_states <- 2L^n_nodes

  # ------------------------------------------------------------------
  # Conditional probability tables
  # ------------------------------------------------------------------
  cat("=== Conditional Probability Tables ===\n")
  for (node in nodes) {
    cat("\n")
    print(x$cpts[[node]], digits = digits)
  }

  # ------------------------------------------------------------------
  # Joint distribution summary
  # ------------------------------------------------------------------
  max_entropy <- log2(n_states)
  pct_max     <- if (max_entropy > 0) 100 * x$entropy / max_entropy else 0

  cat(sprintf(
    "\n=== Joint Distribution (%d nodes, %d states) ===\n",
    n_nodes, n_states
  ))
  cat(sprintf(
    "Shannon entropy : %.4f bits  (max %.4f bits, %.1f%% of maximum)\n",
    x$entropy, max_entropy, pct_max
  ))

  # Marginal entropy per node
  cat("\nMarginal entropy per node (bits):\n")
  for (node in nodes) {
    cat(sprintf("  H(%-12s) = %.4f\n", node, x$node_entropies[node]))
  }

  # Top joint states
  if (top_states > 0L) {
    cat(sprintf("\nTop %d highest-probability states:\n",
                min(top_states, n_states)))
    joint_sorted <- x$joint[order(-x$joint$prob), ]
    df_show      <- head(joint_sorted, top_states)
    df_show$prob <- round(df_show$prob, digits)
    print(df_show, row.names = FALSE)
  }

  # ------------------------------------------------------------------
  # Target node
  # ------------------------------------------------------------------
  if (!is.null(x$target_marginal)) {
    tm  <- x$target_marginal
    h_t <- shannon_entropy(tm$prob)
    cat(sprintf(
      "\nMarginal distribution of target node '%s':\n", x$target
    ))
    for (i in seq_len(nrow(tm))) {
      cat(sprintf(
        "  P(%s = %d) = %.*f\n", x$target, tm$value[i], digits, tm$prob[i]
      ))
    }
    cat(sprintf("  H(%s) = %.4f bits\n", x$target, h_t))
  }

  invisible(x)
}


#' Summary method for elicit_dag_result
#'
#' Delegates to \code{\link{print.elicit_dag_result}}.
#'
#' @param object An \code{elicit_dag_result} object
#' @param ... Passed to \code{print.elicit_dag_result}
#' @export
summary.elicit_dag_result <- function(object, ...) {
  print(object, ...)
}
