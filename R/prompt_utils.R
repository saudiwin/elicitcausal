# ---------------------------------------------------------------------------
# Likert-scale probability mapping
# Verbal labels adapted from the Sherman Kent probability scale used in
# intelligence analysis (Kent 1964).
# ---------------------------------------------------------------------------

#' Named numeric vector mapping 7-point Likert keys to probabilities
#' @keywords internal
LIKERT_PROBS <- c(
  "1" = 0.05,   # Almost certainly not
  "2" = 0.15,   # Very unlikely
  "3" = 0.30,   # Unlikely
  "4" = 0.50,   # About as likely as not
  "5" = 0.70,   # Likely
  "6" = 0.85,   # Very likely
  "7" = 0.95    # Almost certain
)

#' Display lines for the Likert scale
#' @keywords internal
LIKERT_LABELS <- c(
  "  1 = Almost certainly not  (~5%)",
  "  2 = Very unlikely         (~15%)",
  "  3 = Unlikely              (~30%)",
  "  4 = About as likely       (~50%)",
  "  5 = Likely                (~70%)",
  "  6 = Very likely           (~85%)",
  "  7 = Almost certain        (~95%)"
)


# ---------------------------------------------------------------------------
# Query formatting
# ---------------------------------------------------------------------------

#' Format a conditional probability query as a readable string
#'
#' @param node Character. Name of the outcome node
#' @param parent_values Named integer vector of parent node values (may be
#'   length-0 for root nodes)
#' @return Character string of the form "P(node = 1 | ...)"
#' @keywords internal
format_query <- function(node, parent_values) {
  if (length(parent_values) == 0L) {
    sprintf("P(%s = 1)", node)
  } else {
    cond_parts <- paste(names(parent_values), "=", parent_values)
    sprintf("P(%s = 1 | %s)", node, paste(cond_parts, collapse = ", "))
  }
}


#' Default value labels used when the caller supplies no \code{labels}
#' @keywords internal
.DEFAULT_LABELS <- c("Low", "High")   # index 1 = value 0, index 2 = value 1


#' Resolve the label for one node value
#'
#' Returns the user-supplied label if available, otherwise falls back to
#' \code{"Low"} (value 0) or \code{"High"} (value 1).
#' @keywords internal
.node_val_label <- function(node, val, labels) {
  val_idx <- as.integer(val) + 1L          # 0 -> 1, 1 -> 2
  if (!is.null(labels) && !is.null(labels[[node]]))
    labels[[node]][val_idx]
  else
    .DEFAULT_LABELS[val_idx]
}


#' Build a natural-language label sentence for a probability prompt
#'
#' Converts a formal probability query into a plain-English sentence using
#' user-supplied value labels, falling back to \code{"Low"} / \code{"High"}
#' for any node without an explicit label entry.
#'
#' \itemize{
#'   \item \strong{Root nodes} (no parents): \emph{"In general, how likely is X to be
#'     \sQuote{High}?"}
#'   \item \strong{Endogenous nodes} (one or more parents): \emph{"Suppose
#'     that Z is \sQuote{High} and X is \sQuote{Low}. In that case, how
#'     likely is it that Y will be \sQuote{High}?"}
#' }
#'
#' @param node Character. Name of the outcome node.
#' @param parent_values Named integer vector of parent values (length 0 for
#'   root nodes).
#' @param labels Named list; each element is a length-2 character vector
#'   \code{c(label_for_0, label_for_1)} for that node.  Nodes without an
#'   entry use the default \code{"Low"} / \code{"High"} labels.
#' @return A character string (never \code{NULL}).
#' @keywords internal
format_label_sentence <- function(node, parent_values, labels) {
  node_lbl <- .node_val_label(node, 1L, labels)   # label for outcome = 1

  if (length(parent_values) == 0L) {
    # Root / exogenous node
    sprintf("In general, how likely is %s to be \"%s\"?", node, node_lbl)
  } else {
    # Endogenous node: "Suppose that … . In that case, how likely is it that Y will be '…'?"
    parent_parts <- vapply(names(parent_values), function(p) {
      lbl <- .node_val_label(p, parent_values[[p]], labels)
      sprintf("%s is \"%s\"", p, lbl)
    }, character(1L))
    sprintf("Suppose that %s. In that case, how likely is it that %s will be \"%s\"?",
            paste(parent_parts, collapse = " and "), node, node_lbl)
  }
}


# ---------------------------------------------------------------------------
# Input parsing
# ---------------------------------------------------------------------------

#' Parse a user-supplied string into a probability
#'
#' In "likert" mode accepts "1"–"7" (mapped via \code{LIKERT_PROBS}) OR any
#' decimal in [0, 1]. In "probability" mode only decimals in [0, 1] are
#' accepted.
#'
#' @param input Character. Raw user input (already trimmed)
#' @param mode Character. "probability" or "likert"
#' @return Numeric probability in [0, 1], or \code{NA_real_} if invalid
#' @keywords internal
parse_prob_input <- function(input, mode) {
  input <- trimws(as.character(input))

  # Likert key takes priority in likert mode
  if (mode == "likert" && input %in% names(LIKERT_PROBS)) {
    return(unname(LIKERT_PROBS[input]))
  }

  val <- suppressWarnings(as.numeric(input))
  if (!is.na(val) && val >= 0 && val <= 1) {
    return(val)
  }

  NA_real_
}


# ---------------------------------------------------------------------------
# Interactive prompting
# ---------------------------------------------------------------------------

#' Prompt the user for a single conditional probability
#'
#' If \code{.response} is supplied the function bypasses \code{readline()} and
#' uses that value directly (useful for scripted / testing workflows).
#' Requires an interactive session when \code{.response} is \code{NULL}.
#'
#' @param node Character. Outcome node name
#' @param parent_values Named integer vector of parent values (length 0 for
#'   root nodes)
#' @param mode Character. "probability" or "likert"
#' @param labels Optional named list of value labels; each element is a
#'   length-2 character vector \code{c(label_for_0, label_for_1)}.  When
#'   supplied, a plain-English sentence is printed beneath the formal query.
#' @param .response Optional pre-specified response (character or numeric).
#'   When provided, interactive input is skipped.
#' @return Numeric probability in [0, 1]
#' @keywords internal
prompt_for_prob <- function(node, parent_values,
                            mode      = "probability",
                            labels    = NULL,
                            .response = NULL) {
  query_str  <- format_query(node, parent_values)
  label_sent <- format_label_sentence(node, parent_values, labels)

  # ---- Non-interactive path -----------------------------------------------
  if (!is.null(.response)) {
    prob <- parse_prob_input(as.character(.response), mode)
    if (is.na(prob)) {
      stop(sprintf(
        "Pre-specified response '%s' is not a valid probability for query: %s",
        .response, query_str
      ))
    }
    return(prob)
  }

  # ---- Interactive path ---------------------------------------------------
  if (!interactive()) {
    stop(
      "elicit_dag_priors() requires an interactive R session.\n",
      "Supply pre-specified responses via the '.responses' argument."
    )
  }

  if (mode == "likert") {
    cat("\n", query_str, "\n", sep = "")
    if (!is.null(label_sent)) cat("  ", label_sent, "\n", sep = "")
    cat("Rate the probability on a 1-7 scale",
        "(or enter a decimal 0\u20131 directly):\n")
    cat(paste(LIKERT_LABELS, collapse = "\n"), "\n")

    repeat {
      raw <- trimws(readline(prompt = "  Your rating: "))
      prob <- parse_prob_input(raw, mode)
      if (!is.na(prob)) {
        if (raw %in% names(LIKERT_PROBS)) {
          cat(sprintf("  -> Mapped to probability: %.2f\n", prob))
        }
        return(prob)
      }
      cat("  Please enter a number from 1 to 7, or a decimal in [0, 1].\n")
    }

  } else {
    # probability mode
    if (!is.null(label_sent)) {
      repeat {
        cat(sprintf("\n%s = ?\n  %s\n", query_str, label_sent))
        raw  <- trimws(readline(prompt = "  Enter probability: "))
        prob <- parse_prob_input(raw, mode)
        if (!is.na(prob)) return(prob)
        cat("  Please enter a decimal between 0 and 1 (e.g., 0.75).\n")
      }
    } else {
      repeat {
        raw  <- trimws(readline(prompt = sprintf("\n%s = ", query_str)))
        prob <- parse_prob_input(raw, mode)
        if (!is.na(prob)) return(prob)
        cat("  Please enter a decimal between 0 and 1 (e.g., 0.75).\n")
      }
    }
  }
}
