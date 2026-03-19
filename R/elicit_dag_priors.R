#' Elicit conditional probability tables from a causal DAG
#'
#' Accepts a causal graph in \pkg{dagitty} format and interactively elicits
#' the probability of each binary node given every possible combination of its
#' parents' values. The elicited tables are combined via the chain rule of
#' probability to produce the full joint distribution over all nodes, and
#' Shannon entropy is computed.
#'
#' @section Input modes:
#' \describe{
#'   \item{\code{"probability"}}{Enter an exact decimal value in [0, 1] for
#'     each conditional probability.}
#'   \item{\code{"likert"}}{Choose from a 7-point verbal scale (adapted from
#'     the Sherman Kent probability scale): 1 = "almost certainly not" (~5%)
#'     through 7 = "almost certain" (~95%). Decimal overrides are also accepted
#'     in this mode.}
#' }
#'
#' @section Non-interactive use:
#' The \code{.responses} argument lets you bypass \code{readline()} entirely,
#' which is useful for scripting, reproducible analyses, and unit tests. Provide
#' a named list (one element per node, in any order) where each element is a
#' list of responses ordered by the corresponding CPT row order
#' (\code{expand.grid} with parent values cycling from 0 to 1). Root nodes
#' need a single-element list. Responses may be numeric values or character
#' strings parseable as probabilities (or Likert keys in \code{"likert"} mode).
#'
#' @param dag A \code{dagitty} object. The causal DAG. All nodes are treated
#'   as binary (0 = absent/false, 1 = present/true).
#' @param mode Character. Input mode: \code{"probability"} (default) or
#'   \code{"likert"}.
#' @param target Character or \code{NULL}. Optional name of a focal node for
#'   which the marginal distribution is reported separately in the output.
#' @param verbose Logical. If \code{TRUE} (default), print progress messages
#'   and a result summary.
#' @param .responses Named list of pre-specified responses for non-interactive
#'   use. See the "Non-interactive use" section for the expected structure.
#' @param labels Optional named list of value labels.  Each element must be a
#'   length-2 character vector \code{c(label_for_0, label_for_1)} giving
#'   human-readable names for the two values of that node (e.g.
#'   \code{list(X = c("untreated", "treated"), Y = c("no", "yes"))}).  When
#'   supplied, each console prompt is accompanied by a plain-English sentence
#'   such as \emph{"How likely is Y to be 'yes' when X is 'treated'?"}.
#'   Labels are stored in the returned result object and forwarded to the
#'   Shiny app when using \code{\link{launch_app}}.  Nodes without an entry
#'   in \code{labels} display numeric values (0 / 1) in the sentence.
#'
#' @return An object of class \code{"elicit_dag_result"}, invisibly. It is a
#'   list with components:
#'   \describe{
#'     \item{\code{cpts}}{Named list of CPT data frames (class \code{"cpt"}),
#'       one per node in topological order. Each CPT has one integer column per
#'       parent (0/1 values) and a \code{prob} column for
#'       \eqn{P(\text{node}=1|\ldots)}.}
#'     \item{\code{joint}}{Data frame with \eqn{2^n} rows (one per joint
#'       state), one integer column per node, and a \code{prob} column for the
#'       joint probability of that state.}
#'     \item{\code{entropy}}{Shannon entropy of the joint distribution in bits.}
#'     \item{\code{node_entropies}}{Named numeric vector of marginal Shannon
#'       entropy for each node.}
#'     \item{\code{target}}{The supplied \code{target} node name, or
#'       \code{NULL}.}
#'     \item{\code{target_marginal}}{Data frame with columns \code{value} and
#'       \code{prob} for the target node's marginal distribution, or
#'       \code{NULL} if no target was specified.}
#'     \item{\code{dag}}{The original \code{dagitty} object.}
#'     \item{\code{dag_info}}{Parsed DAG metadata: \code{nodes},
#'       \code{parents}, \code{order}.}
#'     \item{\code{mode}}{The input mode used.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(dagitty)
#'
#' # Simple chain: X -> Y -> Z
#' dag <- dagitty("dag { X -> Y -> Z }")
#'
#' # Interactive (probability mode)
#' result <- elicit_dag_priors(dag, target = "Z")
#'
#' # Scripted (non-interactive) with pre-specified probabilities
#' result <- elicit_dag_priors(
#'   dag,
#'   target = "Z",
#'   .responses = list(
#'     X = list(0.5),          # P(X = 1) = 0.5
#'     Y = list(0.1, 0.9),     # P(Y=1|X=0) = 0.1, P(Y=1|X=1) = 0.9
#'     Z = list(0.2, 0.8)      # P(Z=1|Y=0) = 0.2, P(Z=1|Y=1) = 0.8
#'   )
#' )
#'
#' # Inspect results
#' result$joint
#' result$entropy
#' get_marginal(result, "Z")
#' get_conditional(result, query = c(Z = 1L), evidence = c(X = 1L))
#' get_mutual_info(result, "X", "Z")
#'
#' # Fork: X <- Z -> Y
#' dag2 <- dagitty("dag { Z -> X; Z -> Y }")
#' result2 <- elicit_dag_priors(dag2, .responses = list(
#'   Z = list(0.4),
#'   X = list(0.2, 0.8),
#'   Y = list(0.3, 0.7)
#' ))
#'
#' # Collider: X -> Z <- Y
#' dag3 <- dagitty("dag { X -> Z; Y -> Z }")
#' result3 <- elicit_dag_priors(dag3, target = "Z", .responses = list(
#'   X = list(0.5),
#'   Y = list(0.5),
#'   Z = list(0.1, 0.4, 0.4, 0.9)   # P(Z=1 | X=0,Y=0), X=0,Y=1, X=1,Y=0, X=1,Y=1
#' ))
#' }
#'
#' @seealso \code{\link{get_marginal}}, \code{\link{get_conditional}},
#'   \code{\link{get_mutual_info}}
#' @export
elicit_dag_priors <- function(dag,
                               mode       = c("likert", "probability"),
                               target     = NULL,
                               verbose    = TRUE,
                               .responses = NULL,
                               labels     = NULL) {
  mode <- match.arg(mode)

  # ------------------------------------------------------------------
  # Parse the DAG
  # ------------------------------------------------------------------
  dag_info       <- parse_dag(dag)
  nodes_ordered  <- dag_info$order
  parents        <- dag_info$parents

  # Validate target
  if (!is.null(target) && !target %in% dag_info$nodes) {
    stop(sprintf(
      "Target node '%s' not found. Available nodes: %s",
      target, paste(dag_info$nodes, collapse = ", ")
    ))
  }

  # Validate .responses keys
  if (!is.null(.responses)) {
    bad_keys <- setdiff(names(.responses), dag_info$nodes)
    if (length(bad_keys) > 0L) {
      stop(sprintf(
        "'.responses' contains unknown node name(s): %s",
        paste(bad_keys, collapse = ", ")
      ))
    }
  }

  # Validate labels
  if (!is.null(labels)) {
    if (!is.list(labels)) stop("'labels' must be a named list.")
    bad_keys <- setdiff(names(labels), dag_info$nodes)
    if (length(bad_keys) > 0L) {
      stop(sprintf(
        "'labels' contains unknown node name(s): %s",
        paste(bad_keys, collapse = ", ")
      ))
    }
    for (nm in names(labels)) {
      if (!is.character(labels[[nm]]) || length(labels[[nm]]) != 2L) {
        stop(sprintf(
          "labels[['%s']] must be a character vector of length 2: c(label_for_0, label_for_1)",
          nm
        ))
      }
    }
  }

  # ------------------------------------------------------------------
  # Header
  # ------------------------------------------------------------------
  if (verbose) {
    cat("=== Causal DAG Probability Elicitation ===\n")
    cat(sprintf(
      "Nodes (%d, topological order): %s\n",
      length(nodes_ordered), paste(nodes_ordered, collapse = " \u2192 ")
    ))
    if (mode == "probability") {
      cat("Mode: probability  (enter decimals in [0, 1])\n")
    } else {
      cat("Mode: likert  (1 = almost certainly not \u2026 7 = almost certain)\n")
    }
    cat("\n")
  }

  # ------------------------------------------------------------------
  # Elicit CPTs
  # ------------------------------------------------------------------
  cpts <- vector("list", length(nodes_ordered))
  names(cpts) <- nodes_ordered

  for (node in nodes_ordered) {
    node_resps <- if (!is.null(.responses)) .responses[[node]] else NULL
    cpts[[node]] <- elicit_node_cpt(
      node         = node,
      parent_names = parents[[node]],
      mode         = mode,
      labels       = labels,
      .responses   = node_resps
    )
  }

  # ------------------------------------------------------------------
  # Compute joint distribution
  # ------------------------------------------------------------------
  if (verbose) cat("\nComputing joint distribution...\n")
  joint <- compute_joint_distribution(nodes_ordered, cpts)

  # ------------------------------------------------------------------
  # Entropy
  # ------------------------------------------------------------------
  entropy_val    <- shannon_entropy(joint$prob)
  node_entropies <- node_marginal_entropies(joint, nodes_ordered)

  # ------------------------------------------------------------------
  # Target marginal
  # ------------------------------------------------------------------
  target_marginal <- NULL
  if (!is.null(target)) {
    marg <- tapply(joint$prob, joint[[target]], sum)
    target_marginal <- data.frame(
      value = as.integer(names(marg)),
      prob  = as.numeric(marg),
      stringsAsFactors = FALSE
    )
  }

  # ------------------------------------------------------------------
  # Assemble result
  # ------------------------------------------------------------------
  result <- structure(
    list(
      cpts            = cpts,
      joint           = joint,
      entropy         = entropy_val,
      node_entropies  = node_entropies,
      target          = target,
      target_marginal = target_marginal,
      dag             = dag,
      dag_info        = dag_info,
      mode            = mode,
      labels          = labels
    ),
    class = "elicit_dag_result"
  )

  if (verbose) {
    cat("\n")
    print(result)
  }

  invisible(result)
}
