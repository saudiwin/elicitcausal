#' Parse a dagitty DAG and extract node/parent structure
#'
#' @param dag A dagitty object
#' @return A list with components:
#'   \describe{
#'     \item{nodes}{Character vector of all node names}
#'     \item{parents}{Named list mapping each node to its parent node names}
#'     \item{order}{Character vector of nodes in topological order}
#'   }
#' @keywords internal
parse_dag <- function(dag) {
  if (!inherits(dag, "dagitty")) {
    stop(
      "Input must be a dagitty object. ",
      "Create one with dagitty::dagitty(), e.g.:\n",
      "  dag <- dagitty::dagitty(\"dag { X -> Y }\")"
    )
  }

  node_names <- as.character(names(dag))

  parents_list <- lapply(node_names, function(n) {
    as.character(dagitty::parents(dag, n))
  })
  names(parents_list) <- node_names

  ord <- topo_sort(node_names, parents_list)

  list(
    nodes  = node_names,
    parents = parents_list,
    order  = ord
  )
}


#' Topological sort using Kahn's algorithm
#'
#' Produces a linear ordering of nodes such that every parent comes before
#' its children. Stops with an error if the graph contains a cycle.
#'
#' @param nodes Character vector of node names
#' @param parents Named list mapping each node to its parent names
#' @return Character vector of nodes in topological order
#' @keywords internal
topo_sort <- function(nodes, parents) {
  in_degree <- vapply(nodes, function(n) length(parents[[n]]), integer(1))
  names(in_degree) <- nodes

  queue  <- nodes[in_degree == 0L]
  result <- character(0)

  while (length(queue) > 0L) {
    n     <- queue[1L]
    queue <- queue[-1L]
    result <- c(result, n)

    # Find all nodes that have n as a direct parent
    children <- nodes[vapply(
      nodes,
      function(m) n %in% parents[[m]],
      logical(1)
    )]

    for (child in children) {
      in_degree[child] <- in_degree[child] - 1L
      if (in_degree[child] == 0L) {
        queue <- c(queue, child)
      }
    }
  }

  if (length(result) != length(nodes)) {
    stop("The supplied graph contains a cycle and is not a valid DAG.")
  }

  result
}
