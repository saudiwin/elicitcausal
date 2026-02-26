#' Reconstruct an elicit_dag_result from a CausalQueries model
#'
#' Converts a \code{causal_model} object (as produced by
#' \code{\link[CausalQueries]{make_model}} or by
#' \code{\link{to_causalqueries}}) back into an \code{elicit_dag_result} by
#' recovering the conditional probability tables from the nodal-type
#' parameter values.
#'
#' @section CPT recovery:
#' For each node and parent combination \eqn{\mathbf{c}}, the recovered
#' \eqn{P(\text{node}=1 \mid \text{parents}=\mathbf{c})} is the sum of
#' \code{param_value} over all nodal types whose \eqn{j\text{-th}} character
#' equals \code{"1"}, where \eqn{j} is the 1-indexed position corresponding
#' to \eqn{\mathbf{c}} in the CausalQueries \code{expand.grid} ordering
#' (first listed parent cycles fastest).
#'
#' @param cq_model A \code{causal_model} object.
#' @return An \code{elicit_dag_result} object.
#' @seealso \code{\link{to_causalqueries}}, \code{\link{import_elicit_result}}
#' @export
from_causalqueries <- function(cq_model) {
  if (!requireNamespace("CausalQueries", quietly = TRUE))
    stop(
      "Package 'CausalQueries' is required.\n",
      "Install with: install.packages('CausalQueries')"
    )

  if (!inherits(cq_model, "causal_model"))
    stop("'cq_model' must be a causal_model object from CausalQueries.")

  parents_df <- cq_model$parents   # node, root, parents, parent_nodes
  param_df   <- cq_model$parameters_df

  # -- Reconstruct DAG string -------------------------------------------------
  edges <- character(0L)
  for (i in seq_len(nrow(parents_df))) {
    nd     <- parents_df$node[i]
    pn_str <- parents_df$parent_nodes[i]
    if (!is.na(pn_str) && nzchar(trimws(pn_str))) {
      pns <- trimws(strsplit(pn_str, ",", fixed = TRUE)[[1L]])
      for (pn in pns)
        edges <- c(edges, paste0(pn, " -> ", nd))
    }
  }
  dag_str <- if (length(edges) == 0L) {
    paste0("dag { ", paste(parents_df$node, collapse = " "), " }")
  } else {
    paste0("dag { ", paste(edges, collapse = "; "), " }")
  }

  dag_obj  <- dagitty::dagitty(dag_str)
  dag_info <- parse_dag(dag_obj)
  nodes    <- dag_info$order

  # -- Build CPTs from nodal-type parameters ----------------------------------
  cpts <- vector("list", length(nodes))
  names(cpts) <- nodes

  for (nd in nodes) {
    row_idx <- which(parents_df$node == nd)[1L]
    pn_str  <- parents_df$parent_nodes[row_idx]
    cq_pars <- if (is.na(pn_str) || !nzchar(trimws(pn_str))) {
      character(0L)
    } else {
      trimws(strsplit(pn_str, ",", fixed = TRUE)[[1L]])
    }

    node_params <- param_df[param_df$node == nd, ]

    if (length(cq_pars) == 0L) {
      # Root node: nodal_type "1" holds P(node = 1)
      p1  <- node_params$param_value[node_params$nodal_type == "1"]
      cpt <- data.frame(prob = p1)
    } else {
      n_combos <- 2L^length(cq_pars)
      combos   <- expand.grid(
        lapply(cq_pars, function(.) c(0L, 1L)),
        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
      )
      names(combos) <- cq_pars

      # Row i (1-indexed) of combos corresponds to position i in the nodal
      # type string (CQ ordering: first parent cycles fastest).
      probs <- vapply(seq_len(n_combos), function(i) {
        sum(node_params$param_value[
          substr(node_params$nodal_type, i, i) == "1"
        ])
      }, numeric(1L))

      cpt       <- combos
      cpt$prob  <- probs
    }

    attr(cpt, "node")    <- nd
    attr(cpt, "parents") <- cq_pars
    class(cpt)           <- c("cpt", "data.frame")
    cpts[[nd]]           <- cpt
  }

  # -- Joint distribution and entropy ----------------------------------------
  joint          <- compute_joint_distribution(nodes, cpts)
  entropy_val    <- shannon_entropy(joint$prob)
  node_entropies <- node_marginal_entropies(joint, nodes)

  structure(
    list(
      cpts            = cpts,
      joint           = joint,
      entropy         = entropy_val,
      node_entropies  = node_entropies,
      target          = NULL,
      target_marginal = NULL,
      dag             = dag_obj,
      dag_info        = dag_info,
      mode            = "probability",
      labels          = NULL
    ),
    class = "elicit_dag_result"
  )
}
