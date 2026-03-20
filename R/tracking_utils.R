# ===========================================================================
# Usage tracking (optional — only active when RDS_TOKEN env var is set)
# ===========================================================================

#' Read a required connection environment variable, returning NULL if unset.
#' @keywords internal
.rds_env <- function(var) {
  val <- Sys.getenv(var, unset = "")
  if (nchar(val) == 0L) NULL else val
}

#' Check whether PostgreSQL usage tracking is fully configured.
#'
#' Returns \code{TRUE} only when all five connection environment variables are
#' non-empty: \code{RDS_TOKEN}, \code{RDS_HOST}, \code{RDS_PORT},
#' \code{RDS_DBNAME}, and \code{RDS_USER}.
#'
#' @return Logical.
#' @keywords internal
.tracking_enabled <- function() {
  all(vapply(
    c("RDS_TOKEN", "RDS_HOST", "RDS_PORT", "RDS_DBNAME", "RDS_USER"),
    function(v) nchar(Sys.getenv(v, unset = "")) > 0L,
    logical(1L)
  ))
}

#' Log a download action to the elicit_tracking PostgreSQL table.
#'
#' All connection details come from environment variables; tracking is silently
#' skipped if any variable is unset.
#'
#' \describe{
#'   \item{\code{RDS_TOKEN}}{Database password or IAM authentication token.}
#'   \item{\code{RDS_HOST}}{Hostname or IP address of the PostgreSQL server.}
#'   \item{\code{RDS_PORT}}{TCP port (typically \code{5432}).}
#'   \item{\code{RDS_DBNAME}}{Database name.}
#'   \item{\code{RDS_USER}}{Database username.}
#' }
#'
#' Expected table DDL:
#' \preformatted{
#' CREATE TABLE elicit_tracking (
#'   id          SERIAL      PRIMARY KEY,
#'   timestamp   TIMESTAMPTZ NOT NULL DEFAULT NOW(),
#'   action      TEXT,
#'   industry    TEXT,
#'   user_status TEXT,
#'   purpose     TEXT,
#'   region      TEXT,
#'   tab         TEXT
#' );
#' }
#'
#' @param action      Character. Download action identifier
#'   (e.g. \code{"download_result"}, \code{"download_doc"}).
#' @param industry    Character. Respondent's sector.
#' @param user_status Character. Respondent's role/status.
#' @param purpose     Character. Intended use of the download.
#' @param region      Character. Respondent's continent/region.
#' @param tab         Character. Elicitation tab: \code{"pre"},
#'   \code{"post"}, or \code{"learning"}.
#' @return Invisibly \code{NULL}.  Any database error is downgraded to a
#'   warning so a failed log never blocks the download.
#' @keywords internal
.log_tracking_event <- function(action, industry, user_status, purpose, region,
                                 tab = NA_character_) {
  host   <- .rds_env("RDS_HOST")
  port   <- .rds_env("RDS_PORT")
  dbname <- .rds_env("RDS_DBNAME")
  user   <- .rds_env("RDS_USER")
  token  <- .rds_env("RDS_TOKEN")

  if (any(vapply(list(host, port, dbname, user, token), is.null, logical(1L))))
    return(invisible(NULL))

  if (!requireNamespace("DBI",       quietly = TRUE) ||
      !requireNamespace("RPostgres", quietly = TRUE)) {
    warning(
      "DBI and RPostgres are required for usage tracking. ",
      "Install with: install.packages(c('DBI', 'RPostgres'))",
      call. = FALSE
    )
    return(invisible(NULL))
  }

  tryCatch({
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host     = host,
      port     = as.integer(port),
      dbname   = dbname,
      user     = user,
      password = token
    )
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO elicit_tracking",
        "(timestamp, action, industry, user_status, purpose, region, tab)",
        "VALUES ($1, $2, $3, $4, $5, $6, $7)"
      ),
      list(Sys.time(), action, industry, user_status, purpose, region, tab)
    )
  }, error = function(e) {
    warning(
      "elicitcausal tracking: could not log event – ",
      conditionMessage(e),
      call. = FALSE
    )
  })
  invisible(NULL)
}

#' Build the tracking survey modal dialog.
#'
#' @param ns Namespace function from the current Shiny module session.
#' @return A \code{modalDialog} tag object.
#' @keywords internal
.tracking_modal <- function(ns) {
  shiny::modalDialog(
    title = "A quick question before you download",
    shiny::p(
      style = "color:#555; font-size:0.9em;",
      "Your responses help us understand who uses this tool. ",
      "All responses are collected anonymously."
    ),
    shiny::selectInput(
      ns("tracking_industry"),
      "Your sector:",
      choices = c(
        "Select one..."                    = "",
        "Academia / Higher Education"      = "academia",
        "Research Institute / Think Tank"  = "research",
        "Government"                       = "government",
        "Non-profit / NGO"                 = "nonprofit",
        "Industry / Private Sector"        = "industry",
        "Other"                            = "other"
      )
    ),
    shiny::selectInput(
      ns("tracking_status"),
      "Your role:",
      choices = c(
        "Select one..."              = "",
        "Faculty / Researcher"       = "faculty",
        "Post-doctoral Researcher"   = "postdoc",
        "PhD Student"                = "phd_student",
        "Master's Student"           = "masters_student",
        "Undergraduate Student"      = "undergrad",
        "Employee / Analyst"         = "employee",
        "Other"                      = "other"
      )
    ),
    shiny::selectInput(
      ns("tracking_purpose"),
      "What is this for?",
      choices = c(
        "Select one..."                          = "",
        "Experimental pre-registration"          = "experimental_prereg",
        "Observational pre-registration"         = "observational_prereg",
        "Use in other environments / packages"   = "other_env_packages",
        "Other"                                  = "other"
      )
    ),
    shiny::selectInput(
      ns("tracking_region"),
      "Your region:",
      choices = c(
        "Select one..."    = "",
        "Africa"           = "africa",
        "Asia"             = "asia",
        "Europe"           = "europe",
        "North America"    = "north_america",
        "Oceania"          = "oceania",
        "South America"    = "south_america"
      )
    ),
    footer = shiny::tagList(
      shiny::actionButton(
        ns("tracking_skip"),    "Skip",
        class = "btn-default"
      ),
      shiny::actionButton(
        ns("tracking_confirm"), "Confirm & Download",
        class = "btn-primary"
      )
    ),
    easyClose = FALSE
  )
}
