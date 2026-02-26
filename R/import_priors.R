#' Import elicited priors from a previously exported file
#'
#' Dispatches on file extension:
#' \describe{
#'   \item{\code{.rds}}{Calls \code{readRDS()} and performs class-based
#'     dispatch: \code{elicit_dag_result} is returned as-is,
#'     \code{causal_model} is converted via \code{\link{from_causalqueries}},
#'     and a \pkg{theorytools}-annotated \code{dagitty} object (one that
#'     contains \code{distribution=} attributes) is converted via
#'     \code{\link{from_theorytools}}.}
#'   \item{\code{.csv}}{Reads the 5-line metadata header and the CPT table
#'     and reconstructs an \code{elicit_dag_result}.}
#'   \item{\code{.qmd} / \code{.md}}{Looks for a companion \code{priors.csv}
#'     in the same directory; if found calls \code{.from_csv()}.  Otherwise
#'     parses the YAML \code{elicitcausal:} front-matter block (which embeds
#'     the CPT data as \code{cpt_inline}) and reconstructs from that.}
#' }
#'
#' @param file Path to the file to import.
#' @return An \code{elicit_dag_result} object.
#' @seealso \code{\link{export_priors_csv}}, \code{\link{export_priors_qmd}}
#' @export
import_elicit_result <- function(file) {
  if (!file.exists(file))
    stop(sprintf("File not found: %s", file))

  ext <- tolower(tools::file_ext(file))

  switch(ext,
    rds = {
      obj <- readRDS(file)
      .from_rds(obj)
    },
    csv = .from_csv(file),
    qmd = ,
    md  = .from_qmd(file),
    stop(sprintf(
      "Unsupported file extension '.%s'.  Accepted: .rds, .csv, .qmd, .md",
      ext
    ))
  )
}


# ---------------------------------------------------------------------------
# Internal dispatchers / importers
# ---------------------------------------------------------------------------

#' @keywords internal
.from_rds <- function(obj) {
  if (inherits(obj, "elicit_dag_result")) return(obj)

  if (inherits(obj, "causal_model")) {
    if (!requireNamespace("CausalQueries", quietly = TRUE))
      stop(
        "Package 'CausalQueries' is required to import a causal_model RDS.\n",
        "Install with: install.packages('CausalQueries')"
      )
    return(from_causalqueries(obj))
  }

  if (inherits(obj, "dagitty")) {
    dag_str <- as.character(obj)
    if (grepl("distribution=", dag_str, fixed = TRUE))
      return(from_theorytools(obj))
    stop(
      "The RDS contains a plain dagitty object without distribution attributes.\n",
      "Expected an elicit_dag_result, causal_model, or theorytools-annotated dagitty."
    )
  }

  stop(
    "Unrecognised RDS content (class: ",
    paste(class(obj), collapse = ", "),
    ").\nExpected an elicit_dag_result, causal_model, or theorytools dagitty."
  )
}


#' Read a structured CSV exported by \code{export_priors_csv}
#' @keywords internal
.from_csv <- function(file) {
  # -- 5-line metadata header ------------------------------------------------
  meta_lines <- readLines(file, n = 5L)
  meta <- tryCatch(
    read.csv(text = paste(meta_lines, collapse = "\n"),
             header = FALSE, stringsAsFactors = FALSE),
    error = function(e)
      stop("Could not parse CSV metadata header: ", conditionMessage(e))
  )
  meta_map        <- stats::setNames(as.character(meta[[2L]]),
                                     as.character(meta[[1L]]))
  dag_str         <- unname(meta_map["dag"])
  mode_val        <- unname(meta_map["mode"])
  target_val      <- unname(meta_map["target"])
  labels_str      <- unname(meta_map["labels"])

  # -- CPT data ---------------------------------------------------------------
  cpt_df <- tryCatch(
    read.csv(file, skip = 5L, stringsAsFactors = FALSE),
    error = function(e)
      stop("Could not parse CPT data section: ", conditionMessage(e))
  )

  # -- Reconstruct dagitty + parse -------------------------------------------
  dag_obj  <- tryCatch(
    dagitty::dagitty(dag_str),
    error = function(e)
      stop("Could not reconstruct DAG from stored string: ", conditionMessage(e))
  )
  dag_info <- parse_dag(dag_obj)
  nodes    <- dag_info$order

  # -- Reconstruct CPTs -------------------------------------------------------
  cpts <- vector("list", length(nodes))
  names(cpts) <- nodes

  for (nd in nodes) {
    pars    <- dag_info$parents[[nd]]
    nd_rows <- cpt_df[cpt_df$node == nd, , drop = FALSE]

    if (length(pars) == 0L) {
      cpt <- data.frame(prob = nd_rows$prob)
    } else {
      cpt <- nd_rows[, pars, drop = FALSE]
      for (p in pars) cpt[[p]] <- as.integer(cpt[[p]])
      cpt$prob <- nd_rows$prob
      rownames(cpt) <- NULL
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

  # -- Target marginal --------------------------------------------------------
  target <- if (!is.null(target_val) && nzchar(target_val)) target_val else NULL
  target_marginal <- NULL
  if (!is.null(target)) {
    marg <- tapply(joint$prob, joint[[target]], sum)
    target_marginal <- data.frame(
      value = as.integer(names(marg)),
      prob  = as.numeric(marg),
      stringsAsFactors = FALSE
    )
  }

  # -- Labels -----------------------------------------------------------------
  labels <- .decode_labels(labels_str)

  # -- Mode validation --------------------------------------------------------
  if (!mode_val %in% c("probability", "likert")) mode_val <- "probability"

  structure(
    list(
      cpts            = cpts,
      joint           = joint,
      entropy         = entropy_val,
      node_entropies  = node_entropies,
      target          = target,
      target_marginal = target_marginal,
      dag             = dag_obj,
      dag_info        = dag_info,
      mode            = mode_val,
      labels          = labels
    ),
    class = "elicit_dag_result"
  )
}


#' Import from a Quarto / Markdown file
#'
#' First looks for a companion \code{priors.csv} in the same directory.
#' Falls back to parsing the YAML \code{elicitcausal:} front-matter block,
#' which must contain a \code{cpt_inline} field (written by
#' \code{\link{export_priors_qmd}}).
#' @keywords internal
.from_qmd <- function(file) {
  # -- Fast path: companion priors.csv ----------------------------------------
  companion <- file.path(dirname(file), "priors.csv")
  if (file.exists(companion))
    return(.from_csv(companion))

  # -- Slow path: parse YAML front matter ------------------------------------
  lines <- readLines(file, warn = FALSE)
  ec    <- .parse_qmd_yaml(lines)

  dag_val    <- ec[["dag"]]    %||% stop("'dag' field missing from elicitcausal YAML block.")
  mode_val   <- ec[["mode"]]   %||% "probability"
  target_val <- ec[["target"]] %||% ""
  labels_val <- ec[["labels"]] %||% ""
  cpt_inline <- ec[["cpt_inline"]] %||%
    stop("'cpt_inline' field missing from elicitcausal YAML block.  ",
         "Place a companion priors.csv in the same directory or export ",
         "again with export_priors_qmd().")

  # Restore actual newlines from literal \n
  cpt_restored <- gsub("\\n", "\n", cpt_inline, fixed = TRUE)

  # Build a complete 5-line-header CSV in a temp file
  version_str <- as.character(utils::packageVersion("elicitcausal"))
  meta_df <- data.frame(
    key   = c("elicitcausal_export", "dag", "mode", "target", "labels"),
    value = c(version_str, dag_val, mode_val, target_val, labels_val),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write.table(meta_df, tmp, sep = ",", quote = TRUE,
              row.names = FALSE, col.names = FALSE, qmethod = "double")
  cat(cpt_restored, "\n", file = tmp, append = TRUE, sep = "")

  .from_csv(tmp)
}


#' Parse the \code{elicitcausal:} YAML block from QMD/MD file lines
#'
#' Returns a named list of key-value pairs found directly under the
#' \code{elicitcausal:} key in the YAML front matter.  Handles plain and
#' double-quoted scalar values; internal escaped quotes (\code{\\\"}) are
#' unescaped.
#' @keywords internal
.parse_qmd_yaml <- function(lines) {
  trimmed  <- trimws(lines)
  dash_idx <- which(trimmed == "---")
  if (length(dash_idx) < 2L)
    stop("YAML front matter not found in file (need two '---' delimiters).")

  yaml_lines <- lines[(dash_idx[1L] + 1L):(dash_idx[2L] - 1L)]
  ec_idx     <- grep("^elicitcausal\\s*:", yaml_lines)
  if (length(ec_idx) == 0L)
    stop("No 'elicitcausal:' block found in the YAML front matter.")

  result <- list()
  for (i in seq(ec_idx[1L] + 1L, length(yaml_lines))) {
    line <- yaml_lines[i]
    # Must start with exactly two spaces followed by a non-space (sub-key)
    if (!grepl("^  [^ ]", line)) break

    # Split on first ":" to get key and raw value
    colon_pos <- regexpr(":", line, fixed = TRUE)[1L]
    key       <- trimws(substr(line, 1L, colon_pos - 1L))
    val       <- trimws(substr(line, colon_pos + 1L, nchar(line)))

    # Strip surrounding double quotes and unescape internal \"
    if (grepl('^".*"$', val)) {
      val <- substr(val, 2L, nchar(val) - 1L)
      val <- gsub('\\"', '"', val, fixed = TRUE)
    }
    # Strip surrounding single quotes (no escape handling needed)
    if (grepl("^'.*'$", val)) {
      val <- substr(val, 2L, nchar(val) - 1L)
    }

    result[[key]] <- val
  }
  result
}


# Re-export .encode_labels / .decode_labels so import_priors.R can use them
# (they are defined in export_priors.R which is loaded first alphabetically,
#  so no re-definition needed here — included as a reminder comment only).
