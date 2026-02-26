#' Reconstruct an elicit_dag_result from a theorytools-annotated dagitty object
#'
#' Converts an annotated \code{dagitty} object produced by
#' \code{\link{to_theorytools}} back into an \code{elicit_dag_result} by
#' evaluating the multilinear probability polynomial encoded in the edge
#' \code{form} attributes at every binary parent combination.
#'
#' @section Root nodes:
#' Root nodes must have a \code{distribution} attribute of the form
#' \code{"rbinom(size = 1, prob = <p>)"}.  The prior probability is extracted
#' from the \code{prob = } value.
#'
#' @section Non-root nodes:
#' Each incoming edge carries a \code{form} attribute encoding one portion of
#' the multilinear conditional mean.  For each parent combination the sum of
#' all incoming edge form contributions (with \code{:} replaced by \code{*}
#' for evaluation) is clamped to \eqn{[0, 1]} to recover
#' \eqn{P(\text{node}=1 \mid \text{parents})}.
#'
#' @param tt_dag A \code{dagitty} object with \code{distribution} node
#'   attributes and \code{form} edge attributes, as produced by
#'   \code{\link{to_theorytools}}.
#' @return An \code{elicit_dag_result} object.
#' @seealso \code{\link{to_theorytools}}, \code{\link{import_elicit_result}}
#' @export
from_theorytools <- function(tt_dag) {
  if (!inherits(tt_dag, "dagitty"))
    stop("'tt_dag' must be a dagitty object.")

  dag_str  <- as.character(tt_dag)
  dag_info <- parse_dag(tt_dag)
  nodes    <- dag_info$order
  parents_list <- dag_info$parents

  # -- Parse node and edge attributes from the DAG string -------------------
  lines      <- strsplit(dag_str, "\n", fixed = TRUE)[[1L]]
  node_dists <- list()   # nd -> distribution string
  edge_forms <- list()   # "parent::child" -> form string

  for (line in lines) {
    # Node with distribution attribute
    m_nd <- regexec(
      '^\\s*(\\w+)\\s*\\[.*?distribution="([^"]+)".*?\\]',
      line, perl = TRUE
    )[[1L]]
    if (m_nd[1L] != -1L) {
      nd_name  <- substr(line, m_nd[2L], m_nd[2L] + attr(m_nd, "match.length")[2L] - 1L)
      dist_str <- substr(line, m_nd[3L], m_nd[3L] + attr(m_nd, "match.length")[3L] - 1L)
      node_dists[[nd_name]] <- dist_str
      next
    }

    # Edge with form attribute
    m_ed <- regexec(
      '^\\s*(\\w+)\\s*->\\s*(\\w+)\\s*\\[.*?form="([^"]+)".*?\\]',
      line, perl = TRUE
    )[[1L]]
    if (m_ed[1L] != -1L) {
      par_name  <- substr(line, m_ed[2L], m_ed[2L] + attr(m_ed, "match.length")[2L] - 1L)
      child_name <- substr(line, m_ed[3L], m_ed[3L] + attr(m_ed, "match.length")[3L] - 1L)
      form_str  <- substr(line, m_ed[4L], m_ed[4L] + attr(m_ed, "match.length")[4L] - 1L)
      edge_forms[[paste0(par_name, "::", child_name)]] <- form_str
    }
  }

  # -- Build CPTs ------------------------------------------------------------
  cpts <- vector("list", length(nodes))
  names(cpts) <- nodes

  for (nd in nodes) {
    pars <- parents_list[[nd]]
    dist <- node_dists[[nd]]

    if (length(pars) == 0L) {
      # Root node: extract prob from "rbinom(size = 1, prob = <p>)"
      if (is.null(dist))
        stop(sprintf("Node '%s' has no distribution attribute.", nd))
      m_prob <- regmatches(dist, regexpr("prob = [0-9.eE+\\-]+", dist))
      if (length(m_prob) == 0L || !nzchar(m_prob))
        stop(sprintf("Cannot extract prob from distribution '%s' of node '%s'.", dist, nd))
      p <- as.numeric(sub("prob = ", "", m_prob, fixed = TRUE))
      cpt <- data.frame(prob = p)

    } else {
      # Non-root: evaluate sum of edge forms for each parent combination
      combos <- expand.grid(
        lapply(pars, function(.) c(0L, 1L)),
        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
      )
      names(combos) <- pars

      probs <- vapply(seq_len(nrow(combos)), function(i) {
        env <- as.list(lapply(combos[i, , drop = FALSE], as.numeric))

        total <- 0
        for (par in pars) {
          key  <- paste0(par, "::", nd)
          form <- edge_forms[[key]]
          if (!is.null(form)) {
            eval_form <- gsub(":", "*", form, fixed = TRUE)
            total <- total + eval(parse(text = eval_form), envir = env)
          }
        }
        # Clamp to [0, 1] (floating-point safety)
        max(0, min(1, total))
      }, numeric(1L))

      cpt      <- combos
      cpt$prob <- probs
    }

    attr(cpt, "node")    <- nd
    attr(cpt, "parents") <- pars
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
      dag             = tt_dag,
      dag_info        = dag_info,
      mode            = "probability",
      labels          = NULL
    ),
    class = "elicit_dag_result"
  )
}
