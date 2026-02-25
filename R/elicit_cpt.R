#' Elicit a conditional probability table for a single node
#'
#' Iterates over all combinations of parent values (in binary counting order:
#' 00, 01, 10, 11, …) and prompts the user for \eqn{P(\text{node}=1 |
#' \text{parents})}. For root nodes (no parents) a single prior probability
#' is collected.
#'
#' @param node Character. Name of the node
#' @param parent_names Character vector. Names of parent nodes (may be
#'   length-0 for root nodes)
#' @param mode Character. "probability" or "likert"
#' @param .responses Optional list of pre-specified responses, one per parent
#'   combination row (in the same order as \code{expand.grid} with 0/1
#'   values). Root nodes expect a single-element list.
#' @return A data frame of class \code{c("cpt", "data.frame")} with one
#'   column per parent (integer 0/1), a \code{prob} column
#'   (\eqn{P(\text{node}=1|\ldots)}), and attributes \code{"node"} and
#'   \code{"parents"}.
#' @keywords internal
elicit_node_cpt <- function(node, parent_names,
                             mode       = "probability",
                             .responses = NULL) {
  n_parents <- length(parent_names)

  if (n_parents == 0L) {
    # ---- Root node: single prior probability --------------------------------
    cat(sprintf(
      "\n--- Node: %s  (no parents; provide prior probability) ---\n", node
    ))

    resp <- if (!is.null(.responses)) .responses[[1L]] else NULL
    prob <- prompt_for_prob(
      node, setNames(integer(0L), character(0L)), mode, .response = resp
    )

    cpt <- data.frame(prob = prob, stringsAsFactors = FALSE)

  } else {
    # ---- Non-root node: one row per parent combination ----------------------
    combos <- expand.grid(
      lapply(parent_names, function(.) c(0L, 1L)),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    names(combos) <- parent_names
    n_combos <- nrow(combos)

    cat(sprintf(
      "\n--- Node: %s | Parents: %s (%d combination%s) ---\n",
      node,
      paste(parent_names, collapse = ", "),
      n_combos,
      if (n_combos == 1L) "" else "s"
    ))

    if (n_parents >= 4L) {
      cat(sprintf(
        "Note: %d parents require %d probability values.\n",
        n_parents, n_combos
      ))
    }

    probs <- numeric(n_combos)
    for (i in seq_len(n_combos)) {
      parent_vals        <- as.integer(combos[i, ])
      names(parent_vals) <- parent_names

      resp     <- if (!is.null(.responses)) .responses[[i]] else NULL
      probs[i] <- prompt_for_prob(node, parent_vals, mode, .response = resp)
    }

    # In Likert mode the raw values are discrete anchors that don't naturally
    # sum to 1 across parent combinations; normalise so they do.
    raw_sum <- sum(probs)
    probs   <- .normalize_if_likert(probs, mode)
    if (mode == "likert" && abs(raw_sum - 1) > 1e-6) {
      message(sprintf(
        "  Note: Likert probabilities for node '%s' summed to %.4f and have been rescaled to sum to 1.",
        node, raw_sum
      ))
    }

    cpt       <- combos
    cpt$prob  <- probs
  }

  attr(cpt, "node")    <- node
  attr(cpt, "parents") <- parent_names
  class(cpt)           <- c("cpt", "data.frame")
  cpt
}


# ---------------------------------------------------------------------------
# Internal helper: Likert normalisation
# ---------------------------------------------------------------------------

#' Normalise a probability vector so its elements sum to 1, Likert mode only
#'
#' In Likert mode the user selects from discrete verbal anchors whose numeric
#' equivalents (e.g. 0.50, 0.70, 0.85) do not naturally sum to 1 across
#' parent combinations.  This function rescales them proportionally so the
#' resulting CPT row probabilities form a valid distribution.  In probability
#' mode the simplex constraint is already enforced by the sliders, so the
#' vector is returned unchanged.
#'
#' @param probs Numeric vector of elicited probabilities (one per parent combo).
#' @param mode Character. \code{"likert"} triggers normalisation; anything
#'   else returns \code{probs} unchanged.
#' @return Numeric vector summing to 1 (Likert mode) or the original vector.
#' @keywords internal
.normalize_if_likert <- function(probs, mode) {
  if (mode != "likert" || length(probs) <= 1L) return(probs)
  s <- sum(probs)
  if (s > 1e-9) probs / s else rep(1 / length(probs), length(probs))
}


# ---------------------------------------------------------------------------
# S3 print method for cpt objects
# ---------------------------------------------------------------------------

#' Print a conditional probability table
#'
#' @param x A \code{cpt} object
#' @param digits Integer. Decimal places for probabilities
#' @param ... Ignored
#' @export
print.cpt <- function(x, digits = 4L, ...) {
  node    <- attr(x, "node")
  parents <- attr(x, "parents")

  if (length(parents) == 0L) {
    cat(sprintf("P(%s = 1) = %.*f\n", node, digits, x$prob))
  } else {
    cat(sprintf(
      "P(%s = 1 | %s):\n", node, paste(parents, collapse = ", ")
    ))
    df       <- x
    class(df) <- "data.frame"
    df$prob  <- round(df$prob, digits)
    print(df, row.names = FALSE)
  }

  invisible(x)
}
