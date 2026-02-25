# Maximum parent combinations shown in a single modal (2^4 = 16 for 4 parents)
.MAX_COMBOS <- 16L

# Likert choices: display label -> probability string
.LIKERT_CHOICES <- c(
  "1 \u2014 Almost certainly not (~5%)"    = "0.05",
  "2 \u2014 Very unlikely (~15%)"          = "0.15",
  "3 \u2014 Unlikely (~30%)"               = "0.30",
  "4 \u2014 About as likely as not (~50%)" = "0.50",
  "5 \u2014 Likely (~70%)"                 = "0.70",
  "6 \u2014 Very likely (~85%)"            = "0.85",
  "7 \u2014 Almost certain (~95%)"         = "0.95"
)


#' Launch the elicitcausal Shiny application
#'
#' Opens a four-tab browser interface:
#' \enumerate{
#'   \item \strong{Instructions} — how to use the app.
#'   \item \strong{Causal Graph Pre-Study} — elicit beliefs before the study.
#'   \item \strong{Causal Graph Post-Study} — elicit beliefs after the study.
#'   \item \strong{Causal Learning from Graphs} — compare entropy between the
#'     two elicited graphs and optionally close the app and return both results
#'     to R.
#' }
#'
#' @section Input modes:
#' \describe{
#'   \item{\code{"probability"}}{Sliders in \eqn{[0,1]} coupled to sum to 1.}
#'   \item{\code{"likert"}}{7-point verbal drop-down; values are normalised to
#'     sum to 1 across parent combinations.}
#' }
#'
#' @param dag A \code{dagitty} object used as the initial DAG for both tabs,
#'   or \code{NULL} to start with an empty text box.
#' @param mode Character. Initial elicitation mode: \code{"probability"}
#'   (default) or \code{"likert"}. Can be changed inside the app.
#' @param hide_close Logical. If \code{TRUE}, the "Close \u2014 Return to R"
#'   button in the Causal Learning tab is hidden. Default \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#'
#' @return A named list \code{list(pre = ..., post = ...)} where each element
#'   is an \code{elicit_dag_result} object or \code{NULL} if that tab was not
#'   completed before closing.
#'
#' @examples
#' \dontrun{
#' library(dagitty)
#' dag <- dagitty("dag { X -> Y -> Z }")
#' results <- launch_app(dag)
#' results$pre   # pre-study elicit_dag_result
#' results$post  # post-study elicit_dag_result
#' }
#'
#' @export
launch_app <- function(dag = NULL, mode = c("probability", "likert"),
                       hide_close = FALSE, ...) {
  mode <- match.arg(mode)

  for (pkg in c("shiny", "ggdag", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        "Package '%s' is required for launch_app().\nInstall with: install.packages('%s')",
        pkg, pkg
      ))
    }
  }

  if (is.null(dag)) {
    dag_str  <- ""
    dag_info <- NULL
  } else {
    dag_str  <- as.character(dag)
    dag_info <- parse_dag(dag)
  }

  app <- shiny::shinyApp(
    ui     = .app_ui(dag_str, mode),
    server = .app_server(dag, dag_info, mode, hide_close)
  )

  result <- shiny::runApp(app, ...)
  invisible(result)
}


# ===========================================================================
# Top-level UI
# ===========================================================================

.app_ui <- function(dag_str, mode) {
  shiny::navbarPage(
    title = "elicitcausal",
    id    = "main_tabs",

    # Global CSS injected once at the page level
    header = shiny::tags$head(shiny::tags$style(shiny::HTML("
      body  { background-color: #f8f9fa; }
      .navbar { background-color: #2c3e50 !important; }
      .navbar .navbar-brand, .navbar .navbar-nav > li > a { color: #ecf0f1 !important; }
      .navbar .navbar-nav > .active > a,
      .navbar .navbar-nav > li > a:hover { background-color: #34495e !important; color: #fff !important; }
      .well { background: #ffffff; border: 1px solid #dee2e6; box-shadow: none; }
      .node-header { font-weight: 600; color: #2c3e50; margin: 10px 0 4px; font-size: 0.95em; }
      .cpt-section  { margin-bottom: 16px; }
      .prob-row     { border-left: 3px solid #3498db; padding: 4px 0 4px 10px; margin-bottom: 12px; }
      .comp-bar     { height: 16px; border-radius: 3px; overflow: hidden; display: flex;
                      margin: -4px 0 6px; }
      .bar-p0 { background: #e74c3c; display: flex; align-items: center;
                justify-content: center; color: white; font-size: 0.7em; min-width: 0; }
      .bar-p1 { background: #2ecc71; display: flex; align-items: center;
                justify-content: center; color: white; font-size: 0.7em; min-width: 0; }
      .comp-text    { font-size: 0.8em; color: #7f8c8d; margin-top: -2px; margin-bottom: 6px; }
      .progress-tag { background: #ecf0f1; padding: 2px 9px; border-radius: 10px;
                      font-size: 0.82em; color: #555; display: inline-block; }
      .entropy-note { font-size: 0.85em; color: #666; margin-top: 10px;
                      padding: 8px 12px; background: #f0f4f8; border-radius: 4px; }
      .legend-dot   { font-size: 1.2em; line-height: 1; vertical-align: middle; }
      .dag-textarea textarea {
        font-family: 'Courier New', Courier, monospace; font-size: 0.82em; resize: vertical; }
      .dag-hint { font-size: 0.78em; color: #7f8c8d; margin-top: 4px; line-height: 1.4; }
      .dag-hint a { color: #2980b9; }
      .dl-section { margin-top: 12px; padding-top: 10px; border-top: 1px solid #dee2e6; }
      .dl-section .btn { margin: 3px 4px 3px 0; }
      .instr-section { max-width: 820px; margin: 0 auto; }
      .instr-section h3 { color: #2c3e50; border-bottom: 2px solid #3498db;
                           padding-bottom: 4px; margin-top: 28px; }
      .instr-section .step { background: #fff; border: 1px solid #dee2e6;
                              border-radius: 6px; padding: 12px 16px; margin-bottom: 10px; }
      .instr-section .step .num { font-size: 1.4em; font-weight: 700;
                                   color: #3498db; margin-right: 8px; }
      .learning-status { display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 16px; }
      .stat-complete { background:#d5f5e3; color:#1e8449; }
      .stat-pending  { background:#fdebd0; color:#d35400; }
    "))),

    # ------------------------------------------------------------------
    shiny::tabPanel(
      "Instructions",
      value = "tab_instructions",
      .instructions_ui()
    ),

    # ------------------------------------------------------------------
    shiny::tabPanel(
      "Causal Graph Pre-Study",
      value = "tab_pre",
      .elicit_ui("pre", dag_str, mode)
    ),

    # ------------------------------------------------------------------
    shiny::tabPanel(
      "Causal Graph Post-Study",
      value = "tab_post",
      .elicit_ui("post", dag_str, mode)
    ),

    # ------------------------------------------------------------------
    shiny::tabPanel(
      "Causal Learning from Graphs",
      value = "tab_learning",
      .learning_ui()
    )
  )
}


# ---------------------------------------------------------------------------
# Tab 1: Instructions (static content)
# ---------------------------------------------------------------------------

.instructions_ui <- function() {
  shiny::div(
    class = "instr-section",
    style = "padding: 20px 10px;",

    shiny::h2("How to Use elicitcausal", style = "margin-top: 0; color: #2c3e50;"),
    shiny::HTML(
    "This app helps you construct a causal graph along with priors for the relationships between variables
    in the causal graph. Constructing a causal graph before doing a study can help you understand how much 
    causal learning you can gain from a variety of interventions. You can also compare the pre-study graph to a post-study graph
    to gain very precise measures of causal learning after doing a study. 
    For more information about this method, see 
    <a href='https://doi.org/10.31235/osf.io/a492b' target='_blank' rel='noopener noreferrer'>
    the paper here</a>."
  ),

    shiny::h3("Workflow"),
    shiny::div(class = "step",
      shiny::span(class = "num", "1"),
      shiny::strong("Build a causal graph."),
      " Go to the ",
      shiny::strong("Causal Graph Pre-Study"), " tab. You'll need to paste a causal graph
       specification to get started. You can either get one from the visual graph builder at ", shiny::tags$a("dagitty.net", href="https://dagitty.net",
                                            target="_blank"),
       " or type one directly using dagitty notation, e.g. ", shiny::code('dag { X -> Y -> Z }'),
       ". All variables in the graph are treated as binary
       (0 / 1) variables to simplify the workflow."),
    shiny::div(class = "step",
      shiny::span(class = "num", "2"),
      shiny::strong("Choose an elicitation mode."),
      " Select ", shiny::em("Probability sliders"), " to drag values between
       0% (never happens) and 100% (always happens), or ", shiny::em("Likert scale"), " to choose a verbal
       description if you don't want to bother with putting in precise numbers. Sliders are coupled so that all values for a given node
       sum to 1. Likert values are normalised to sum to 1 automatically."),
    shiny::div(class = "step",
      shiny::span(class = "num", "3"),
      shiny::strong("Input variable relationships."),
      " Click ", shiny::strong("Elicit Probabilities"), " and work through
       the modal dialogs, one per node, to add your prior beliefs about how variables are related to each other. Use Previous / Next to revise answers
       before clicking Done."),
    shiny::div(class = "step",
      shiny::span(class = "num", "4"),
      shiny::strong("Download (optional)."),
      " After completing elicitation, use the download buttons to save the
       result as an RDS file that can be used in R. You can also export as a CausalQueries model RDS file, and/or save the
       graph as a JPEG."),
    shiny::div(class = "step",
      shiny::span(class = "num", "5"),
      shiny::strong("Repeat for Post-Study."),
      " Go to the ", shiny::strong("Causal Graph Post-Study"), " tab and
       repeat the process after completing the study. You should update the graph with any new information that changes your beliefs about how variables are related to each other."),
    shiny::div(class = "step",
      shiny::span(class = "num", "6"),
      shiny::strong("Compare."),
      " Open the ", shiny::strong("Causal Learning from Graphs"), " tab to
       see the amount of causal learning that occurred from pre- to post-study. We use the entropy metric to provide a quantified summary of learning. An entropy reduction means we became more certain of the graph, while an increase means we became less certain."),

    shiny::h3("Causal Graph/DAG specification format"),
    shiny::p("Nodes are separated by spaces or newlines. Directed edges use ",
             shiny::code("->"), ". Examples:"),
    shiny::tags$pre(style = "background:#f4f4f4; padding:10px; border-radius:4px;",
      "dag { X -> Y }\n",
      "dag { X -> Y; Z -> Y }    # collider\n",
      "dag { X -> Y -> Z; X -> Z }  # fork + direct path"
    ),

    shiny::h3("Entropy interpretation"),
    shiny::tags$ul(
      shiny::tags$li(shiny::strong("H = 0 bits"), " \u2014 complete certainty (probability 0 or 1 for all states)."),
      shiny::tags$li(shiny::strong("H = n bits"), " \u2014 maximum uncertainty over 2\u207F states (all equally likely)."),
      shiny::tags$li(shiny::strong("\u0394H < 0"), " (post \u2212 pre) \u2014 beliefs became more precise after the study."),
      shiny::tags$li(shiny::strong("\u0394H > 0"), " \u2014 beliefs became more uncertain; the study introduced new complexity.")
    )
  )
}


# ---------------------------------------------------------------------------
# Elicitation module: UI
# ---------------------------------------------------------------------------

#' UI for one elicitation panel (used by both Pre- and Post-Study tabs)
#' @param id Module namespace id (\code{"pre"} or \code{"post"}).
#' @param dag_str Initial DAG specification string.
#' @param mode Initial elicitation mode.
#' @keywords internal
.elicit_ui <- function(id, dag_str, mode) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    style = "padding: 14px 0;",

    # ---- Left: DAG plot + text editor ------------------------------------
    shiny::column(5,
      shiny::wellPanel(
        shiny::h4("Causal Graph", style = "margin-top:0"),
        shiny::plotOutput(ns("dag_plot"), height = "300px"),
        shiny::div(
          style = "margin-top: 6px; font-size: 0.8em; color: #555;",
          shiny::span(class = "legend-dot", style = "color:#27ae60;", "\u25cf"),
          " Elicited  ",
          shiny::span(class = "legend-dot", style = "color:#e67e22;", "\u25cf"),
          " Active  ",
          shiny::span(class = "legend-dot", style = "color:#bdc3c7;", "\u25cf"),
          " Pending"
        ),
        shiny::hr(style = "margin: 10px 0;"),
        shiny::tags$label(
          `for` = ns("dag_text"),
          style = "font-weight:600; font-size:0.9em; margin-bottom:3px; display:block;",
          "DAG specification"
        ),
        shiny::div(
          class = "dag-textarea",
          shiny::textAreaInput(ns("dag_text"), label = NULL, value = dag_str,
                               rows = 3L, width = "100%",
                               placeholder = "dag { X -> Y -> Z }")
        ),
        shiny::uiOutput(ns("dag_text_status")),
        shiny::div(
          class = "dag-hint",
          "Build your DAG visually at ",
          shiny::tags$a("dagitty.net", href = "https://dagitty.net", target = "_blank"),
          ", then copy the model code (Model \u2192 Export \u2192 dagitty code)",
          " and paste it above. The graph updates automatically."
        )
      )
    ),

    # ---- Right: controls + results --------------------------------------
    shiny::column(7,
      shiny::wellPanel(
        style = "margin-bottom: 12px;",
        shiny::div(
          style = "margin-bottom: 8px;",
          shiny::radioButtons(ns("mode_select"),
                              label    = shiny::strong("Elicitation mode:"),
                              choices  = c("Probability sliders" = "probability",
                                           "Likert scale"        = "likert"),
                              selected = mode, inline = TRUE)
        ),
        shiny::hr(style = "margin: 8px 0;"),
        shiny::div(
          style = "display:flex; align-items:center; flex-wrap:wrap; gap:10px;",
          shiny::actionButton(ns("btn_elicit"), "Elicit Probabilities",
                              class = "btn-primary btn-lg",
                              icon  = shiny::icon("sliders-h")),
          shiny::uiOutput(ns("elicit_status"))
        ),
        shiny::uiOutput(ns("download_buttons"))
      ),
      shiny::uiOutput(ns("cpt_display"))
    )
  )
}


# ---------------------------------------------------------------------------
# Tab 4: Learning UI (mostly server-rendered)
# ---------------------------------------------------------------------------

.learning_ui <- function() {
  shiny::div(
    style = "padding: 20px 10px; max-width: 960px; margin: 0 auto;",
    shiny::uiOutput("learning_display"),
    shiny::plotOutput("learning_plot", height = "320px"),
    shiny::uiOutput("btn_close_global_ui")
  )
}


# ===========================================================================
# Top-level server
# ===========================================================================

.app_server <- function(dag, dag_info, mode, hide_close = FALSE) {
  function(input, output, session) {

    # Instantiate the two elicitation modules; each returns reactive(rv$result)
    pre_r  <- .elicit_server("pre",  dag, dag_info, mode)
    post_r <- .elicit_server("post", dag, dag_info, mode)

    # ------------------------------------------------------------------
    # Tab 4: Causal Learning summary
    # ------------------------------------------------------------------
    output$learning_display <- shiny::renderUI({
      pre  <- pre_r()
      post <- post_r()
      pre_done  <- !is.null(pre)
      post_done <- !is.null(post)

      status <- shiny::div(
        class = "learning-status",
        shiny::span(class = paste("progress-tag", if (pre_done) "stat-complete" else "stat-pending"),
                    shiny::icon(if (pre_done) "check-circle" else "clock"),
                    if (pre_done) " Pre-Study: complete" else " Pre-Study: not yet completed"),
        shiny::span(class = paste("progress-tag", if (post_done) "stat-complete" else "stat-pending"),
                    shiny::icon(if (post_done) "check-circle" else "clock"),
                    if (post_done) " Post-Study: complete" else " Post-Study: not yet completed")
      )

      if (!pre_done || !post_done) {
        return(shiny::tagList(
          shiny::h3("Causal Learning from Graphs", style = "margin-top:0;"),
          status,
          shiny::div(style = "color:#999; margin-top:10px; font-size:1em;",
                     shiny::icon("arrow-left"),
                     " Complete both the Pre-Study and Post-Study tabs to see the entropy comparison.")
        ))
      }

      # --- Both complete: build comparison ---
      pre_H  <- pre$entropy
      post_H <- post$entropy
      dH     <- post_H - pre_H
      pct    <- if (pre_H > 1e-9) 100 * dH / pre_H else NA_real_
      pct_str <- if (is.na(pct)) "\u2014" else sprintf("%+.1f%%", pct)

      # Node-level table
      pre_nodes  <- names(pre$node_entropies)
      post_nodes <- names(post$node_entropies)
      common     <- intersect(pre_nodes, post_nodes)
      only_pre   <- setdiff(pre_nodes,  post_nodes)
      only_post  <- setdiff(post_nodes, pre_nodes)

      rows <- lapply(common, function(nd) {
        dh  <- post$node_entropies[nd] - pre$node_entropies[nd]
        col <- if (dh < -1e-6) "#27ae60" else if (dh > 1e-6) "#e74c3c" else "#555"
        lbl <- if (dh < -1e-6) "\u2193 less uncertain" else
               if (dh > 1e-6) "\u2191 more uncertain" else "\u2015 no change"
        shiny::tags$tr(
          shiny::tags$td(shiny::strong(nd)),
          shiny::tags$td(sprintf("%.4f", pre$node_entropies[nd])),
          shiny::tags$td(sprintf("%.4f", post$node_entropies[nd])),
          shiny::tags$td(style = sprintf("color:%s; font-weight:600;", col),
                         sprintf("%+.4f", dh)),
          shiny::tags$td(style = sprintf("color:%s;", col), lbl)
        )
      })

      node_table <- shiny::tags$table(
        class = "table table-striped table-bordered table-condensed",
        style = "font-size:0.9em;",
        shiny::tags$thead(shiny::tags$tr(
          lapply(c("Node", "Pre entropy (bits)", "Post entropy (bits)",
                   "\u0394 entropy", "Direction"), shiny::tags$th)
        )),
        shiny::tags$tbody(rows)
      )

      dag_note <- if (length(only_pre) > 0L || length(only_post) > 0L) {
        shiny::div(style = "color:#e67e22; font-size:0.85em; margin-top:6px;",
          shiny::icon("exclamation-triangle"),
          sprintf(" Note: the two DAGs have different nodes. Nodes only in Pre: %s; only in Post: %s.",
                  if (length(only_pre)  > 0L) paste(only_pre,  collapse=", ") else "\u2014",
                  if (length(only_post) > 0L) paste(only_post, collapse=", ") else "\u2014"))
      }

      shiny::tagList(
        shiny::h3("Causal Learning from Graphs", style = "margin-top:0;"),
        status,
        shiny::div(
          class = "entropy-note",
          style = "font-size:0.95em; margin-bottom:14px;",
          shiny::strong("Overall joint entropy: "),
          sprintf("Pre = %.4f bits, Post = %.4f bits, \u0394H = %+.4f bits (%s)",
                  pre_H, post_H, dH, pct_str),
          shiny::br(),
          if (dH < -1e-6)
            shiny::span(style="color:#27ae60;",
                        shiny::icon("arrow-down"), " Beliefs became more precise after the study.")
          else if (dH > 1e-6)
            shiny::span(style="color:#e74c3c;",
                        shiny::icon("arrow-up"), " Beliefs became more uncertain after the study.")
          else
            shiny::span(style="color:#555;", "\u2015 No overall change in uncertainty.")
        ),
        shiny::h4("Node-level entropy comparison"),
        node_table,
        dag_note
      )
    })

    # Bar chart of per-node entropy changes (only when both complete)
    output$learning_plot <- shiny::renderPlot({
      pre  <- pre_r()
      post <- post_r()
      if (is.null(pre) || is.null(post)) return(NULL)

      common <- intersect(names(pre$node_entropies), names(post$node_entropies))
      if (length(common) == 0L) return(NULL)

      df <- data.frame(
        node      = common,
        delta     = as.numeric(post$node_entropies[common] - pre$node_entropies[common]),
        stringsAsFactors = FALSE
      )
      df$direction <- ifelse(df$delta < -1e-6, "More precise",
                      ifelse(df$delta >  1e-6, "More uncertain", "No change"))
      df$node <- stats::reorder(df$node, df$delta)

      ggplot2::ggplot(df, ggplot2::aes(x = delta, y = node, fill = direction)) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#555") +
        ggplot2::scale_fill_manual(
          values = c("More precise"   = "#27ae60",
                     "More uncertain" = "#e74c3c",
                     "No change"      = "#bdc3c7"),
          drop = FALSE
        ) +
        ggplot2::labs(
          x = "\u0394 entropy (bits, post \u2212 pre)",
          y = NULL,
          title = "Change in node-level entropy",
          fill  = NULL
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          legend.position   = "bottom",
          plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
          panel.grid.major.y = ggplot2::element_blank()
        )
    }, bg = "white")

    # ------------------------------------------------------------------
    # Global "Close & Return to R" button in tab 4
    # ------------------------------------------------------------------
    output$btn_close_global_ui <- shiny::renderUI({
      if (hide_close) return(NULL)
      pre  <- pre_r()
      post <- post_r()
      if (is.null(pre) && is.null(post)) return(NULL)
      shiny::div(
        style = "margin-top: 20px; padding-top: 14px; border-top: 1px solid #dee2e6;",
        shiny::actionButton(
          "btn_close_global", "Close \u2014 Return to R",
          class = "btn-warning btn-lg",
          icon  = shiny::icon("sign-out-alt")
        ),
        shiny::span(style = "font-size:0.82em; color:#888; margin-left:12px;",
                    "Returns list(pre = ..., post = ...) to the R session.")
      )
    })

    shiny::observeEvent(input$btn_close_global, {
      shiny::stopApp(returnValue = list(pre = pre_r(), post = post_r()))
    })
  }
}


# ===========================================================================
# Elicitation module server
# ===========================================================================

#' Server for one elicitation panel
#'
#' @param id Module namespace id (\code{"pre"} or \code{"post"}).
#' @param dag Initial \code{dagitty} object (or \code{NULL}).
#' @param dag_info Parsed DAG info list (or \code{NULL}).
#' @param mode Initial elicitation mode string.
#' @return A \code{reactive} that yields \code{rv$result} (an
#'   \code{elicit_dag_result} or \code{NULL}).
#' @keywords internal
.elicit_server <- function(id, dag, dag_info, mode) {
  shiny::moduleServer(id, function(input, output, session) {

    # ------------------------------------------------------------------
    # Reactive mode
    # ------------------------------------------------------------------
    mode_r <- shiny::reactive(input$mode_select %||% mode)

    # ------------------------------------------------------------------
    # Reactive DAG state
    # ------------------------------------------------------------------
    dag_rv      <- shiny::reactiveVal(dag)
    dag_info_rv <- shiny::reactiveVal(dag_info)

    nodes_r   <- shiny::reactive({
      di <- dag_info_rv(); if (is.null(di)) character(0) else di$order
    })
    parents_r <- shiny::reactive({
      di <- dag_info_rv(); if (is.null(di)) list() else di$parents
    })
    n_nodes_r <- shiny::reactive(length(nodes_r()))

    rv <- shiny::reactiveValues(
      current_idx     = 1L,
      pending         = NULL,
      cpts            = NULL,
      result          = NULL,
      modal_probs     = NULL,
      n_active_combos = 0L,
      dag_error       = NULL
    )

    # ------------------------------------------------------------------
    # DAG text observer
    # ------------------------------------------------------------------
    dag_text_d <- shiny::debounce(shiny::reactive(input$dag_text), 600)

    shiny::observeEvent(dag_text_d(), {
      txt <- trimws(dag_text_d())
      if (nchar(txt) == 0L) {
        dag_rv(NULL); dag_info_rv(NULL)
        rv$dag_error <- NULL
        rv$pending <- NULL; rv$cpts <- NULL; rv$result <- NULL
        return()
      }
      tryCatch({
        new_dag      <- dagitty::dagitty(txt)
        new_dag_info <- parse_dag(new_dag)
        dag_rv(new_dag); dag_info_rv(new_dag_info)
        rv$dag_error   <- NULL
        rv$pending     <- NULL; rv$cpts <- NULL; rv$result <- NULL
        rv$current_idx <- 1L
      }, error = function(e) rv$dag_error <- conditionMessage(e))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    output$dag_text_status <- shiny::renderUI({
      if (!is.null(rv$dag_error))
        shiny::div(style="font-size:0.78em;color:#c0392b;margin-top:3px;",
                   shiny::icon("times-circle"), " ", rv$dag_error)
      else {
        n <- n_nodes_r(); if (n == 0L) return(NULL)
        shiny::div(style="font-size:0.78em;color:#27ae60;margin-top:3px;",
                   shiny::icon("check-circle"),
                   sprintf(" %d node%s detected", n, if (n==1L) "" else "s"))
      }
    })

    # ------------------------------------------------------------------
    # Initialise pending when DAG changes
    # ------------------------------------------------------------------
    shiny::observe({
      nodes <- nodes_r(); parents <- parents_r()
      if (length(nodes) == 0L || !is.null(rv$pending)) return()
      rv$pending <- stats::setNames(
        lapply(seq_along(nodes), function(i) {
          nc <- max(1L, 2L^length(parents[[nodes[i]]]))
          if (nc > 1L) as.list(rep(1/nc, nc)) else list(0.5)
        }),
        nodes
      )
    })

    # ------------------------------------------------------------------
    # Pre-register complement text + bar outputs
    # ------------------------------------------------------------------
    for (ci in seq_len(.MAX_COMBOS)) {
      local({
        cii <- ci; pid <- paste0("prob_", cii)

        output[[paste0("comp_", cii)]] <- shiny::renderText({
          val <- input[[pid]]; if (is.null(val)) return("\u2014")
          sprintf("P = 0: %.3f  |  P = 1: %.3f", 1-as.numeric(val), as.numeric(val))
        })

        output[[paste0("bar_", cii)]] <- shiny::renderUI({
          val <- input[[pid]]; if (is.null(val)) return(NULL)
          p1 <- as.numeric(val); p0 <- 1-p1
          lbl0 <- if (p0 > 0.12) sprintf("%.0f%%", p0*100) else ""
          lbl1 <- if (p1 > 0.12) sprintf("%.0f%%", p1*100) else ""
          shiny::div(class="comp-bar",
                     shiny::div(class="bar-p0",style=sprintf("width:%.2f%%",p0*100),lbl0),
                     shiny::div(class="bar-p1",style=sprintf("width:%.2f%%",p1*100),lbl1))
        })
      })
    }

    # ------------------------------------------------------------------
    # Coupled slider observers (simplex constraint, probability mode)
    # ------------------------------------------------------------------
    for (ci in seq_len(.MAX_COMBOS)) {
      local({
        cii <- ci; pid <- paste0("prob_", cii)

        shiny::observeEvent(input[[pid]], {
          if (shiny::isolate(mode_r()) != "probability") return()
          n <- shiny::isolate(rv$n_active_combos)
          if (n <= 1L || cii > n) return()
          new_val <- as.numeric(input[[pid]])
          probs   <- shiny::isolate(rv$modal_probs)
          if (is.null(probs) || length(probs) < n) return()
          if (abs(new_val - probs[cii]) < 0.006) return()
          others    <- setdiff(seq_len(n), cii)
          old_sum   <- sum(probs[others])
          remaining <- max(0, 1 - new_val)
          if (old_sum > 1e-9) probs[others] <- probs[others]*(remaining/old_sum)
          else                probs[others] <- remaining/length(others)
          probs[cii] <- new_val
          probs      <- pmin(pmax(probs, 0), 1)
          rv$modal_probs <- probs
          for (j in others)
            shiny::updateSliderInput(session, paste0("prob_", j), value = probs[j])
        }, ignoreInit = TRUE)
      })
    }

    # ------------------------------------------------------------------
    # DAG plot
    # ------------------------------------------------------------------
    output$dag_plot <- shiny::renderPlot({
      d <- dag_rv(); di <- dag_info_rv()
      if (is.null(d) || is.null(di)) {
        return(ggplot2::ggplot() +
          ggplot2::annotate("text", x=.5, y=.5,
                            label="Paste a DAG specification\nto see the graph",
                            colour="#aaaaaa", size=5, hjust=.5) +
          ggplot2::theme_void() +
          ggplot2::theme(plot.background=ggplot2::element_rect(fill="white",colour=NA)))
      }
      nodes <- shiny::isolate(nodes_r()); n <- length(nodes)
      elicited <- if (!is.null(rv$cpts)) names(rv$cpts) else character(0)
      active   <- if (is.null(rv$result) && !is.null(rv$pending) &&
                      rv$current_idx >= 1L && rv$current_idx <= n)
                    nodes[rv$current_idx] else character(0)
      .draw_dag(d, di, elicited = elicited, active = active)
    }, bg = "white")

    # ------------------------------------------------------------------
    # Status badge
    # ------------------------------------------------------------------
    output$elicit_status <- shiny::renderUI({
      n_nodes <- n_nodes_r()
      if (!is.null(rv$result))
        shiny::span(class="progress-tag", style="background:#d5f5e3;color:#1e8449;",
                    shiny::icon("check-circle"), " Complete")
      else if (!is.null(rv$pending) && n_nodes > 0L)
        shiny::span(class="progress-tag",
                    shiny::icon("circle-notch fa-spin"),
                    sprintf(" Node %d / %d", rv$current_idx, n_nodes))
      else
        shiny::span(class="progress-tag",
                    shiny::icon("info-circle"), " Not started")
    })

    # ------------------------------------------------------------------
    # Download handlers
    # ------------------------------------------------------------------
    output$download_result <- shiny::downloadHandler(
      filename = function() paste0("elicit_dag_result_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".rds"),
      content  = function(file) saveRDS(rv$result, file)
    )
    output$download_cq <- shiny::downloadHandler(
      filename = function() paste0("causalqueries_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".rds"),
      content  = function(file) saveRDS(to_causalqueries(rv$result), file)
    )
    output$download_plot <- shiny::downloadHandler(
      filename = function() paste0("dag_plot_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".jpg"),
      content  = function(file) {
        d <- shiny::isolate(dag_rv()); di <- shiny::isolate(dag_info_rv())
        elicited <- if (!is.null(rv$cpts)) names(rv$cpts) else character(0)
        p <- .draw_dag(d, di, elicited = elicited)
        grDevices::jpeg(file, width=1200, height=900, res=150, quality=95)
        print(p)
        grDevices::dev.off()
      }
    )

    output$download_buttons <- shiny::renderUI({
      shiny::req(rv$result)
      has_cq <- requireNamespace("CausalQueries", quietly = TRUE)
      shiny::div(
        class = "dl-section",
        shiny::strong(style="font-size:0.9em;", "Download results:"),
        shiny::br(),
        shiny::downloadButton(session$ns("download_result"), "Result (.rds)",
                              class="btn-default btn-sm", icon=shiny::icon("download")),
        if (has_cq)
          shiny::downloadButton(session$ns("download_cq"), "CausalQueries (.rds)",
                                class="btn-default btn-sm", icon=shiny::icon("download")),
        shiny::downloadButton(session$ns("download_plot"), "Graph (.jpg)",
                              class="btn-default btn-sm", icon=shiny::icon("image"))
      )
    })

    # ------------------------------------------------------------------
    # CPT display (inline tables, no pre-registration needed)
    # ------------------------------------------------------------------
    output$cpt_display <- shiny::renderUI({
      if (is.null(rv$cpts))
        return(shiny::div(style="color:#999;margin-top:16px;",
                          shiny::icon("arrow-left"),
                          ' Click "Elicit Probabilities" to begin.'))

      nodes <- nodes_r(); parents <- parents_r(); n_nodes <- n_nodes_r()
      shiny::tagList(
        shiny::h4("Conditional Probability Tables", style="margin-top:4px"),
        lapply(nodes, function(node) {
          pars <- parents[[node]]; cpt <- rv$cpts[[node]]
          df   <- cpt; class(df) <- "data.frame"; df$prob <- round(df$prob, 4L)
          shiny::div(class="cpt-section",
            shiny::div(class="node-header",
              if (length(pars)==0L) sprintf("P(%s = 1)", node)
              else sprintf("P(%s = 1 \u2502 %s)", node, paste(pars,collapse=", "))
            ),
            shiny::tags$table(
              class="table table-striped table-bordered table-condensed",
              style="font-size:0.85em;margin-bottom:4px;",
              shiny::tags$thead(shiny::tags$tr(lapply(names(df), shiny::tags$th))),
              shiny::tags$tbody(lapply(seq_len(nrow(df)), function(i)
                shiny::tags$tr(lapply(df[i,,drop=FALSE], function(v)
                  shiny::tags$td(as.character(v))))))
            )
          )
        }),
        shiny::div(class="entropy-note",
          sprintf("\U0001F4CA Shannon entropy: %.4f bits  (%.1f%% of maximum %d bits)",
                  rv$result$entropy, 100*rv$result$entropy/log2(2^n_nodes), n_nodes))
      )
    })

    # ------------------------------------------------------------------
    # Open elicitation chain
    # ------------------------------------------------------------------
    shiny::observeEvent(input$btn_elicit, {
      nodes   <- shiny::isolate(nodes_r())
      parents <- shiny::isolate(parents_r())
      n_nodes <- shiny::isolate(n_nodes_r())
      m       <- shiny::isolate(mode_r())
      if (n_nodes == 0L) {
        shiny::showNotification("Please enter a valid DAG before eliciting probabilities.",
                                type="warning", duration=4)
        return()
      }
      rv$current_idx <- 1L; rv$cpts <- NULL; rv$result <- NULL
      .init_modal_state(rv, nodes, parents, 1L)
      .show_node_modal(session, nodes, parents, m, rv$pending, 1L, n_nodes)
    })

    shiny::observeEvent(input$modal_next, {
      nodes   <- shiny::isolate(nodes_r())
      parents <- shiny::isolate(parents_r())
      n_nodes <- shiny::isolate(n_nodes_r())
      m       <- shiny::isolate(mode_r())
      .save_modal_inputs(rv, nodes, parents, input, rv$current_idx, m)
      nxt <- rv$current_idx + 1L; rv$current_idx <- nxt
      .init_modal_state(rv, nodes, parents, nxt)
      .show_node_modal(session, nodes, parents, m, rv$pending, nxt, n_nodes)
    })

    shiny::observeEvent(input$modal_prev, {
      nodes   <- shiny::isolate(nodes_r())
      parents <- shiny::isolate(parents_r())
      n_nodes <- shiny::isolate(n_nodes_r())
      m       <- shiny::isolate(mode_r())
      .save_modal_inputs(rv, nodes, parents, input, rv$current_idx, m)
      prv <- rv$current_idx - 1L; rv$current_idx <- prv
      .init_modal_state(rv, nodes, parents, prv)
      .show_node_modal(session, nodes, parents, m, rv$pending, prv, n_nodes)
    })

    shiny::observeEvent(input$modal_done, {
      nodes   <- shiny::isolate(nodes_r())
      parents <- shiny::isolate(parents_r())
      m       <- shiny::isolate(mode_r())
      .save_modal_inputs(rv, nodes, parents, input, rv$current_idx, m)
      shiny::removeModal()

      cur_dag      <- shiny::isolate(dag_rv())
      cur_dag_info <- shiny::isolate(dag_info_rv())

      normalized_nodes <- Filter(function(node) {
        pars  <- parents[[node]]
        probs <- as.numeric(unlist(rv$pending[[node]]))
        m == "likert" && length(pars) > 0L &&
          length(probs) > 1L && abs(sum(probs) - 1) > 1e-6
      }, nodes)

      cpts <- lapply(nodes, function(node) {
        pars  <- parents[[node]]
        probs <- as.numeric(unlist(rv$pending[[node]]))
        if (length(pars) == 0L) {
          cpt <- data.frame(prob = probs[1L])
        } else {
          probs      <- .normalize_if_likert(probs, m)
          cpt        <- expand.grid(lapply(pars, function(.) c(0L,1L)),
                                    KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
          names(cpt) <- pars; cpt$prob <- probs
        }
        attr(cpt,"node") <- node; attr(cpt,"parents") <- pars
        class(cpt) <- c("cpt","data.frame"); cpt
      })
      names(cpts) <- nodes; rv$cpts <- cpts

      if (length(normalized_nodes) > 0L) {
        nn  <- normalized_nodes
        msg <- sprintf("Likert probabilities for node%s %s were rescaled to sum to 1.",
                       if (length(nn)==1L) "" else "s", paste(nn, collapse=", "))
        shiny::showNotification(
          ui       = shiny::tagList(shiny::strong("Normalisation applied"), shiny::br(), msg),
          type     = "message", duration = 8)
      }

      joint          <- compute_joint_distribution(nodes, cpts)
      entropy_val    <- shannon_entropy(joint$prob)
      node_entropies <- node_marginal_entropies(joint, nodes)

      rv$result <- structure(
        list(cpts=cpts, joint=joint, entropy=entropy_val,
             node_entropies=node_entropies, target=NULL, target_marginal=NULL,
             dag=cur_dag, dag_info=cur_dag_info, mode=m),
        class = "elicit_dag_result"
      )
    })

    shiny::observeEvent(input$modal_cancel, {
      shiny::removeModal(); rv$current_idx <- 1L
    })

    # Return the result as a reactive for the parent server
    shiny::reactive(rv$result)
  })
}


# ===========================================================================
# Modal builder
# ===========================================================================

#' Build and display a probability elicitation modal for one node
#'
#' All input/output IDs are namespaced via \code{session$ns()} so that
#' two independent module instances do not share IDs.
#' @keywords internal
.show_node_modal <- function(session, nodes, parents, mode,
                              pending, idx, n_total) {
  ns   <- session$ns
  node <- nodes[idx]
  pars <- parents[[node]]
  n_combos <- if (length(pars) == 0L) 1L else 2L^length(pars)

  combos <- if (length(pars) > 0L)
    stats::setNames(
      expand.grid(lapply(pars, function(.) c(0L,1L)),
                  KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE),
      pars
    ) else data.frame()

  saved <- pending[[node]]

  input_rows <- lapply(seq_len(n_combos), function(i) {
    label <- if (length(pars) == 0L) {
      sprintf("P(%s = 1)", node)
    } else {
      parts <- paste(pars, as.integer(combos[i, pars, drop=TRUE]), sep=" = ")
      sprintf("P(%s = 1 \u2502 %s)", node, paste(parts, collapse=", "))
    }
    val <- if (!is.null(saved[[i]])) as.numeric(saved[[i]]) else 0.5
    shiny::div(class="prob-row",
      if (mode == "probability") {
        shiny::tagList(
          shiny::sliderInput(ns(paste0("prob_", i)), label,
                             min=0, max=1, step=0.01, value=val, width="100%"),
          shiny::uiOutput(ns(paste0("bar_", i))),
          shiny::div(class="comp-text",
                     shiny::textOutput(ns(paste0("comp_", i)), inline=TRUE))
        )
      } else {
        nearest_key <- names(LIKERT_PROBS)[which.min(abs(LIKERT_PROBS - val))]
        shiny::selectInput(ns(paste0("prob_", i)), label,
                           choices  = .LIKERT_CHOICES,
                           selected = as.character(LIKERT_PROBS[nearest_key]),
                           width    = "100%")
      }
    )
  })

  title_str <- if (length(pars) == 0L)
    sprintf("Node: %s  \u2014  prior probability", node)
  else
    sprintf("Node: %s  \u2502  Parents: %s", node, paste(pars, collapse=", "))

  hint <- if (mode == "probability")
    "Drag each slider to set the probability that the node equals 1. Sliders are coupled and sum to 1."
  else
    "Choose the verbal probability that best describes each conditional probability."

  shiny::showModal(shiny::modalDialog(
    title = shiny::div(
      style = "display:flex;justify-content:space-between;align-items:center;",
      shiny::strong(title_str),
      shiny::span(class="progress-tag", sprintf("%d of %d", idx, n_total))
    ),
    shiny::p(class="text-muted", style="margin-bottom:10px;", hint),
    shiny::hr(style="margin:8px 0 14px;"),
    do.call(shiny::tagList, input_rows),
    footer = shiny::tagList(
      shiny::actionButton(ns("modal_cancel"), "Cancel",
                          class="btn-default pull-left", icon=shiny::icon("times")),
      if (idx > 1L)
        shiny::actionButton(ns("modal_prev"), "\u2190 Previous", class="btn-default"),
      if (idx < n_total)
        shiny::actionButton(ns("modal_next"), "Next \u2192", class="btn-primary")
      else
        shiny::actionButton(ns("modal_done"), "\u2713 Done", class="btn-success")
    ),
    size="m", easyClose=FALSE
  ))
}


#' @keywords internal
.save_modal_inputs <- function(rv, nodes, parents, input, idx, mode) {
  node     <- nodes[idx]
  n_combos <- max(1L, 2L^length(parents[[node]]))
  if (mode == "probability" && n_combos > 1L &&
      !is.null(rv$modal_probs) && length(rv$modal_probs) >= n_combos) {
    rv$pending[[node]] <- as.list(rv$modal_probs[seq_len(n_combos)])
  } else {
    rv$pending[[node]] <- lapply(seq_len(n_combos), function(i) {
      raw <- input[[paste0("prob_", i)]]
      if (is.null(raw)) 1/n_combos else as.numeric(raw)
    })
  }
}


#' @keywords internal
.init_modal_state <- function(rv, nodes, parents, idx) {
  node     <- nodes[idx]
  n_combos <- max(1L, 2L^length(parents[[node]]))
  probs    <- as.numeric(unlist(rv$pending[[node]]))
  if (n_combos > 1L) {
    s <- sum(probs)
    probs <- if (s > 1e-9) probs/s else rep(1/n_combos, n_combos)
  }
  rv$modal_probs     <- probs[seq_len(n_combos)]
  rv$n_active_combos <- n_combos
}


# ===========================================================================
# DAG visualisation
# ===========================================================================

#' @keywords internal
.draw_dag <- function(dag, dag_info, elicited=character(0), active=character(0)) {
  nodes  <- dag_info$nodes
  status <- ifelse(nodes %in% active,   "active",
            ifelse(nodes %in% elicited, "elicited", "pending"))
  status_df <- data.frame(name=nodes, status=status, stringsAsFactors=FALSE)
  tidy      <- ggdag::tidy_dagitty(dag)
  tidy$data <- merge(tidy$data, status_df, by="name", all.x=TRUE)
  tidy$data$status[is.na(tidy$data$status)] <- "pending"

  ggplot2::ggplot(tidy$data, ggplot2::aes(x=x, y=y, xend=xend, yend=yend)) +
    ggdag::geom_dag_edges(edge_colour="#95a5a6") +
    ggdag::geom_dag_point(ggplot2::aes(colour=status), size=20) +
    ggdag::geom_dag_text(ggplot2::aes(label=name), colour="white", size=3.8, fontface="bold") +
    ggplot2::scale_colour_manual(
      values = c(elicited="#27ae60", active="#e67e22", pending="#bdc3c7"),
      guide  = "none") +
    ggdag::theme_dag() +
    ggplot2::theme(plot.background=ggplot2::element_rect(fill="white", colour=NA))
}


# ===========================================================================
# Utilities
# ===========================================================================

`%||%` <- function(x, y) if (!is.null(x)) x else y
