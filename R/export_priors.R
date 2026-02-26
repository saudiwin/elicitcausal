# ---------------------------------------------------------------------------
# QMD template embedded constant
# ---------------------------------------------------------------------------

.QMD_TEMPLATE <- "---
title: \"Causal Priors Preregistration\"
date: \"DATE_PLACEHOLDER\"
format:
  html:
    self-contained: true
  gfm: default
  docx: default
execute:
  warning: false
  echo: false
elicitcausal:
  dag: \"DAG_ESCAPED_PLACEHOLDER\"
  mode: \"MODE_PLACEHOLDER\"
  target: \"TARGET_PLACEHOLDER\"
  labels: \"LABELS_PLACEHOLDER\"
  cpt_inline: \"CPT_INLINE_PLACEHOLDER\"
---

# Causal Priors Preregistration

*Generated: DATE_PLACEHOLDER*

## Causal DAG Specification

```
DAG_PLACEHOLDER
```

## Prior Conditional Probability Tables

```{r echo=FALSE, message=FALSE}
if (file.exists(\"priors.csv\")) {
  cpt <- read.csv(\"priors.csv\", skip = 5)
  knitr::kable(cpt, digits = 4, caption = \"Elicited conditional probability tables\")
}
```

## Entropy Summary

ENTROPY_PLACEHOLDER
"


# ---------------------------------------------------------------------------
# Exported functions
# ---------------------------------------------------------------------------

#' Export elicited priors to a structured CSV file
#'
#' Writes a self-contained two-section CSV that can be re-imported with
#' \code{\link{import_elicit_result}}.
#'
#' @section File format:
#' \enumerate{
#'   \item Lines 1\enc{–}{-}5: two-column metadata (key, value) with keys
#'     \code{elicitcausal_export}, \code{dag}, \code{mode}, \code{target},
#'     \code{labels}.  Labels are encoded as
#'     \code{"X=label0|label1,Y=label0|label1"}.
#'   \item Line 6: column headers (\code{node}, one column per unique parent
#'     across all CPTs, \code{prob}).
#'   \item Lines 7+: one row per CPT entry; parent columns for nodes that are
#'     not parents of the current node contain \code{NA}.
#' }
#'
#' @param result An \code{elicit_dag_result} object.
#' @param file Path to the output \code{.csv} file.
#' @return Invisibly returns \code{file}.
#' @seealso \code{\link{import_elicit_result}}, \code{\link{export_priors_qmd}}
#' @export
export_priors_csv <- function(result, file) {
  .check_result(result)

  dag_str    <- .compact_dag_str(result$dag)
  mode_val   <- result$mode
  target_val <- if (!is.null(result$target)) result$target else ""
  labels_str <- .encode_labels(result$labels)
  version    <- as.character(utils::packageVersion("elicitcausal"))

  # -- 5-row metadata (key, value) -------------------------------------------
  meta_df <- data.frame(
    key   = c("elicitcausal_export", "dag", "mode", "target", "labels"),
    value = c(version, dag_str, mode_val, target_val, labels_str),
    stringsAsFactors = FALSE
  )
  write.table(meta_df, file = file, sep = ",", quote = TRUE,
              row.names = FALSE, col.names = FALSE, qmethod = "double")

  # -- CPT rows ---------------------------------------------------------------
  all_parents <- unique(unlist(lapply(result$cpts,
                                      function(cpt) attr(cpt, "parents"))))
  nodes <- result$dag_info$order

  rows <- lapply(nodes, function(nd) {
    cpt  <- result$cpts[[nd]]
    pars <- attr(cpt, "parents")
    n_r  <- nrow(cpt)

    out <- data.frame(node = rep(nd, n_r), stringsAsFactors = FALSE)
    for (p in all_parents) {
      out[[p]] <- if (p %in% pars) as.integer(cpt[[p]]) else NA_integer_
    }
    out$prob <- cpt$prob
    out
  })

  cpt_df <- do.call(rbind, rows)
  suppressWarnings(
    write.table(cpt_df, file = file, sep = ",", quote = FALSE,
                row.names = FALSE, col.names = TRUE, append = TRUE, na = "NA")
  )

  invisible(file)
}


#' Export elicited priors to a Quarto preregistration document
#'
#' Writes a \code{priors.csv} (via \code{\link{export_priors_csv}}) and a
#' \code{preregistration.qmd} Quarto document to a temporary directory, then
#' either bundles them as a \code{.zip} or renders the document to the
#' requested format.
#'
#' @param result An \code{elicit_dag_result} object.
#' @param path Output file path.  Extension should match \code{output_format}:
#'   \code{.zip} for \code{"qmd_bundle"}, \code{.html}, \code{.md}, or
#'   \code{.docx} for the rendered formats.
#' @param output_format Character.  One of:
#'   \describe{
#'     \item{\code{"qmd_bundle"}}{Zip the \code{.qmd} and \code{priors.csv}
#'       together.  No Quarto installation required.}
#'     \item{\code{"html"}, \code{"gfm"}, \code{"docx"}}{Render via
#'       \code{quarto::quarto_render()}.  Falls back to \code{"qmd_bundle"} if
#'       the \pkg{quarto} package is not installed.}
#'   }
#' @return Invisibly returns \code{path}.
#' @seealso \code{\link{export_priors_csv}}, \code{\link{import_elicit_result}}
#' @export
export_priors_qmd <- function(result, path,
                               output_format = c("qmd_bundle", "html",
                                                 "gfm", "docx")) {
  .check_result(result)
  output_format <- match.arg(output_format)

  tmpdir <- tempfile("elicit_qmd_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  # Write companion CSV
  csv_path <- file.path(tmpdir, "priors.csv")
  export_priors_csv(result, csv_path)

  # Build the cpt_inline string (CSV header + data, no quoting)
  all_parents <- unique(unlist(lapply(result$cpts,
                                      function(cpt) attr(cpt, "parents"))))
  nodes <- result$dag_info$order
  rows <- lapply(nodes, function(nd) {
    cpt  <- result$cpts[[nd]]
    pars <- attr(cpt, "parents")
    n_r  <- nrow(cpt)
    out  <- data.frame(node = rep(nd, n_r), stringsAsFactors = FALSE)
    for (p in all_parents) {
      out[[p]] <- if (p %in% pars) as.integer(cpt[[p]]) else NA_integer_
    }
    out$prob <- cpt$prob
    out
  })
  cpt_df <- do.call(rbind, rows)

  tc <- textConnection("csv_text_lines", "w")
  suppressWarnings(
    write.table(cpt_df, tc, sep = ",", quote = FALSE,
                row.names = FALSE, col.names = TRUE, na = "NA")
  )
  close(tc)
  cpt_inline_raw     <- paste(csv_text_lines, collapse = "\n")
  cpt_inline_escaped <- gsub("\n", "\\n", cpt_inline_raw, fixed = TRUE)

  # Escape internal " for YAML double-quoted string
  dag_str    <- .compact_dag_str(result$dag)
  dag_escaped <- gsub('"', '\\"', dag_str, fixed = TRUE)
  labels_str  <- .encode_labels(result$labels)
  target_val  <- if (!is.null(result$target)) result$target else ""
  date_str    <- format(Sys.Date(), "%Y-%m-%d")

  n_nodes     <- length(nodes)
  max_entropy <- n_nodes * log(2, base = 1.01)
  ent_str <- sprintf(
    "The joint Shannon entropy of the elicited prior model is **%.2f%%** (out of a maximum of %.2f%% for %d binary nodes).",
    result$entropy, max_entropy, n_nodes
  )

  qmd_content <- .QMD_TEMPLATE
  qmd_content <- gsub("DATE_PLACEHOLDER",          date_str,          qmd_content, fixed = TRUE)
  qmd_content <- gsub("DAG_ESCAPED_PLACEHOLDER",   dag_escaped,       qmd_content, fixed = TRUE)
  qmd_content <- gsub("DAG_PLACEHOLDER",            dag_str,           qmd_content, fixed = TRUE)
  qmd_content <- gsub("MODE_PLACEHOLDER",           result$mode,       qmd_content, fixed = TRUE)
  qmd_content <- gsub("TARGET_PLACEHOLDER",         target_val,        qmd_content, fixed = TRUE)
  qmd_content <- gsub("LABELS_PLACEHOLDER",         labels_str,        qmd_content, fixed = TRUE)
  qmd_content <- gsub("CPT_INLINE_PLACEHOLDER",     cpt_inline_escaped, qmd_content, fixed = TRUE)
  qmd_content <- gsub("ENTROPY_PLACEHOLDER",        ent_str,           qmd_content, fixed = TRUE)

  qmd_path <- file.path(tmpdir, "preregistration.qmd")
  writeLines(qmd_content, qmd_path)

  # Deliver output
  abs_path <- normalizePath(path, mustWork = FALSE)

  if (output_format == "qmd_bundle") {
    owd <- setwd(tmpdir)
    on.exit(setwd(owd), add = TRUE)
    utils::zip(zipfile = abs_path,
               files   = c("priors.csv", "preregistration.qmd"))
  } else {
    # Rendered formats
    if (!requireNamespace("quarto", quietly = TRUE)) {
      message("Package 'quarto' is not installed; falling back to qmd_bundle.")
      owd <- setwd(tmpdir)
      on.exit(setwd(owd), add = TRUE)
      utils::zip(zipfile = abs_path,
                 files   = c("priors.csv", "preregistration.qmd"))
    } else {
      out_ext  <- switch(output_format, html = ".html", gfm = ".md", docx = ".docx")
      out_file <- file.path(tmpdir,
                            paste0("preregistration", out_ext))
      quarto::quarto_render(
        input         = qmd_path,
        output_format = output_format,
        quiet         = TRUE
      )
      if (!file.exists(out_file)) {
        # quarto may use a slightly different filename; find it
        candidates <- list.files(tmpdir, pattern = paste0("\\", out_ext, "$"),
                                 full.names = TRUE)
        if (length(candidates) == 0L)
          stop("quarto_render did not produce the expected output file.")
        out_file <- candidates[[1L]]
      }
      file.copy(out_file, abs_path, overwrite = TRUE)
    }
  }

  invisible(path)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Compact a dagitty object to a single-line string
#' @keywords internal
.compact_dag_str <- function(dag) {
  s <- as.character(dag)
  s <- gsub("\n", " ", s, fixed = TRUE)
  trimws(gsub("\\s+", " ", s))
}


#' Encode a labels list as a pipe-encoded string
#'
#' Returns a string like \code{"X=no|yes,Y=absent|present"} or \code{""} if
#' \code{labels} is \code{NULL} or empty.
#' @keywords internal
.encode_labels <- function(labels) {
  if (is.null(labels) || length(labels) == 0L) return("")
  paste(vapply(names(labels), function(nm) {
    paste0(nm, "=", paste(labels[[nm]], collapse = "|"))
  }, character(1L)), collapse = ",")
}


#' Decode a pipe-encoded labels string to a named list
#'
#' Parses \code{"X=no|yes,Y=absent|present"} back to
#' \code{list(X = c("no", "yes"), Y = c("absent", "present"))}.
#' Returns \code{NULL} for an empty string.
#' @keywords internal
.decode_labels <- function(labels_str) {
  if (is.null(labels_str) || !nzchar(trimws(labels_str))) return(NULL)
  parts <- strsplit(labels_str, ",", fixed = TRUE)[[1L]]
  result <- vector("list", length(parts))
  nms    <- character(length(parts))
  for (i in seq_along(parts)) {
    p <- parts[i]
    # Split on first "=" only
    eq_pos  <- regexpr("=", p, fixed = TRUE)[1L]
    nms[i]  <- trimws(substr(p, 1L, eq_pos - 1L))
    rest    <- substr(p, eq_pos + 1L, nchar(p))
    result[[i]] <- strsplit(rest, "|", fixed = TRUE)[[1L]]
  }
  names(result) <- nms
  result
}
