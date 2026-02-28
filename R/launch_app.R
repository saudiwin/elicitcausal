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
      .elicit_ui("post", dag_str, mode, locked_dag = TRUE)
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
    shiny::p(
    "This app helps you construct a causal graph along with priors for the relationships between variables
    in the causal graph. It is designed for non-experts by using a question-and-answer format to 
    populate the causal graph with the researcher's knowledge about the research question. Constructing a causal graph before doing a study can help you understand how much 
    causal learning you can gain from a variety of interventions--especially if you preregister the pre-study causal graph. You can then compare the pre-study graph to a post-study graph
    to gain very precise measures of causal learning after doing a study (using entropy as a metric). Of course, the
    causal graphs are useful for many other kinds of analyses, such as diagnosing issues in research design and meta-analysis."), 
    shiny::HTML("To facilitate inserting the pre-study graph into a preregistration, this app provides options for downloading Latex/Markdown/Word version of the causal graph as a 
    table with associated priors on relationships between nodes. Alternatively, you can download an R object from this package or other packages
    that help you analyze causal graphs--<a href='https://integrated-inferences.github.io/CausalQueries/' target='_blank' rel='noopener noreferrer'>CausalQueries</a> and <a href='https://cjvanlissa.github.io/theorytools/' target='_blank' rel='noopener noreferrer'>theorytools</a>.
    The theorytools package has specific support for <a href='https://cjvanlissa.github.io/theorytools/articles/fair-theory.html' target='_blank' rel='noopener noreferrer'>creating files suitable for preregistration using FAIR principles</a>."),
    shiny::HTML("<br><br>After performing the study, you can re-upload the pre-study causal graph--either from the Latex/Markdown/Word file or as a elicitcausal/CausalQueries/theorytools R object--
      and then update the graph with what you learned from the study. The app will calculate the post-study learning using entropy metrics as described in the paper below."),
    shiny::HTML("
    <br><br>This app was developed by <a href='https://www.robertkubinec.com' target='_blank' rel='noopener noreferrer'>Robert Kubinec</a> at the University of South Carolina. For more information about this method, see 
    <a href='https://doi.org/10.31235/osf.io/a492b' target='_blank' rel='noopener noreferrer'>
    the related paper 'Holistic Causal Learning with Causal Graphs: A Credible Method for Study Design and Preregistration in the Social Sciences'</a>."
  ),
  shiny::HTML("
    <br><br>To report a bug, please file an issue at the <a href='https://github.com/saudiwin/elicitcausal' target='_blank' rel='noopener noreferrer'>package Github site</a>."
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
       result as a Word/Latex/Markdown file or as an elicitcausal RDS file that can be further analyzed in R. You can also export as a CausalQueries and/or theorytools model RDS file."),
    shiny::div(class = "step",
      shiny::span(class = "num", "5"),
      shiny::strong("Repeat for Post-Study."),
      " Go to the ", shiny::strong("Causal Graph Post-Study"), " tab and
       repeat the process after completing the study. You can either upload the file you saved to re-load the pre-study graph or input the pre-study graph probabilities manually (such as if you are interactively checking different research designs). You should update the graph with any new information that changes your beliefs about how variables are related to each other."),
    shiny::div(class = "step",
      shiny::span(class = "num", "6"),
      shiny::strong("Compare."),
      " Open the ", shiny::strong("Causal Learning from Graphs"), " tab to
       see the amount of causal learning that occurred from pre- to post-study. We use the entropy metric to provide a quantified summary of learning. An entropy reduction means we became more certain of the graph (type I causal learning), while an increase means we became less certain (type II causal learning)."),

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
      shiny::tags$li(shiny::strong("H = 0%"), " \u2014 complete certainty (probability 0 or 1 for all states)."),
      shiny::tags$li(shiny::strong("H = n \u00d7 69.66%"), " \u2014 maximum uncertainty over 2\u207F states (all equally likely); equals n \u00d7 log\u2081.\u2080\u2081(2) for n binary nodes."),
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
.elicit_ui <- function(id, dag_str, mode, locked_dag = FALSE) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    style = "padding: 14px 0;",

    # ---- Left: DAG plot + text editor ------------------------------------
    shiny::column(5,
      shiny::wellPanel(
        shiny::h4("Causal Graph", style = "margin-top:0"),
        shiny::plotOutput(ns("dag_plot"), height = "300px"),
        shiny::div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-top:6px;",
          shiny::div(
            style = "font-size: 0.8em; color: #555;",
            shiny::span(class = "legend-dot", style = "color:#27ae60;", "\u25cf"),
            " Elicited  ",
            shiny::span(class = "legend-dot", style = "color:#e67e22;", "\u25cf"),
            " Active  ",
            shiny::span(class = "legend-dot", style = "color:#bdc3c7;", "\u25cf"),
            " Pending"
          ),
          shiny::selectInput(
            ns("dag_layout"), label = NULL, width = "130px",
            selected = "circle",
            choices = c(
              "Circle"      = "circle",
              "Linear"      = "linear",
              "Matrix"      = "matrix",
              "Treemap"     = "treemap",
              "Circle pack" = "circlepack",
              "Partition"   = "partition",
              "Cactus tree" = "cactustree",
              "Eigen"       = "eigen",
              "Fabric"      = "fabric",
              "Stress"      = "stress",
              "Unrooted"    = "unrooted",
              "H-tree"      = "htree"
            )
          )
        ),
        shiny::hr(style = "margin: 10px 0;"),
        if (locked_dag) {
          shiny::tagList(
            # Upload card (post tab only)
            shiny::div(
              style = "background:#f0f4f8; border:1px solid #c8d6e5; border-radius:5px; padding:10px 12px; margin-bottom:10px;",
              shiny::div(
                style = "font-size:0.88em; font-weight:600; color:#2c3e50; margin-bottom:4px;",
                shiny::icon("upload"), " Upload pre-study file (optional)"
              ),
              shiny::div(
                style = "font-size:0.79em; color:#7f8c8d; margin-bottom:6px; line-height:1.4;",
                "Accepts: .rds (elicitcausal / CausalQueries / theorytools), .csv, .qmd"
              ),
              shiny::fileInput(ns("upload_prior"), label = NULL,
                               accept = c(".rds", ".csv", ".qmd", ".md"),
                               buttonLabel = "Browse...",
                               placeholder = "No file selected",
                               width = "100%"),
              shiny::uiOutput(ns("upload_status"))
            ),
            # Lock notice
            shiny::div(
              style = "font-size:0.82em; color:#7f8c8d; margin-bottom:6px;",
              shiny::icon("lock"), " ",
              "The causal graph is fixed to the Pre-Study graph. ",
              "Only probabilities can be changed here."
            ),
            shiny::uiOutput(ns("dag_locked_display"))
          )
        } else {
          shiny::tagList(
            shiny::tags$label(
              `for` = ns("dag_text"),
              style = "font-weight:600; font-size:0.9em; margin-bottom:3px; display:block;",
              "Causal graph specification in dagitty format"
            ),
            shiny::div(
              class = "dag-hint",
              "Write your causal graph in dagitty format in the text box below. If you need help, try building your causal graph visually at ",
              shiny::tags$a("dagitty.net", href = "https://dagitty.net", target = "_blank"),
              ", then copy the model code (Model \u2192 Export \u2192 dagitty code)",
              " and paste it below. The graph updates automatically."
            ),
            shiny::div(
              class = "dag-textarea",
              shiny::textAreaInput(ns("dag_text"), label = NULL, value = dag_str,
                                   rows = 3L, width = "100%",
                                   placeholder = "dag { X -> Y -> Z }")
            ),
            shiny::uiOutput(ns("dag_text_status")),
            shiny::uiOutput("btn_close_pre_ui")
          )
        }
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
        shiny::uiOutput(ns("label_table")),
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
    shiny::uiOutput("learning_download_ui"),
    shiny::plotOutput("learning_plot", height = "320px"),
    shiny::uiOutput("btn_close_global_ui")
  )
}


# ===========================================================================
# Top-level server
# ===========================================================================

.app_server <- function(dag, dag_info, mode, hide_close = FALSE) {
  function(input, output, session) {

    # Shared reactive state: the post tab writes an uploaded pre-study result
    # here so the pre tab can read it and update itself for entropy comparison.
    rv_shared <- shiny::reactiveValues(imported_pre = NULL)

    # Instantiate the two elicitation modules; each returns reactive(rv$result).
    # The post module starts with no DAG and syncs from pre_r when pre completes.
    pre_r  <- .elicit_server("pre",  dag, dag_info, mode,
                              uploaded_override_rv = rv_shared)
    post_r <- .elicit_server("post", NULL, NULL, mode, pre_result_r = pre_r,
                              uploaded_override_rv = rv_shared)

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
        ltype <- if (abs(dh) < 1) "No Learning" else
                 if (dh < 0)      "Type I"       else "Type II"
        shiny::tags$tr(
          shiny::tags$td(shiny::strong(nd)),
          shiny::tags$td(sprintf("%.4f", pre$node_entropies[nd])),
          shiny::tags$td(sprintf("%.4f", post$node_entropies[nd])),
          shiny::tags$td(style = sprintf("color:%s; font-weight:600;", col),
                         sprintf("%+.4f", dh)),
          shiny::tags$td(style = sprintf("color:%s;", col), lbl),
          shiny::tags$td(shiny::strong(ltype))
        )
      })

      node_table <- shiny::tags$table(
        class = "table table-striped table-bordered table-condensed",
        style = "font-size:0.9em;",
        shiny::tags$thead(shiny::tags$tr(
          lapply(c("Node", "Pre entropy (%)", "Post entropy (%)",
                   "\u0394 entropy (%)", "Direction", "Learning Type"),
                 shiny::tags$th)
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
          sprintf("Pre = %.2f%%, Post = %.2f%%, \u0394H = %+.2f%% (%s)",
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
          x = "\u0394 entropy (%, post \u2212 pre)",
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
    # Learning tab: download entropy comparison table
    # ------------------------------------------------------------------
    output$learning_download_ui <- shiny::renderUI({
      pre  <- pre_r()
      post <- post_r()
      if (is.null(pre) || is.null(post)) return(NULL)
      shiny::div(
        class = "dl-section",
        style = "margin-top:14px; padding-top:10px; border-top:1px solid #dee2e6;",
        shiny::strong(style = "font-size:0.9em;", "Download entropy comparison table:"),
        shiny::br(),
        shiny::div(
          style = "display:flex; align-items:center; gap:8px; margin-top:6px; flex-wrap:wrap;",
          shiny::selectInput("learning_dl_format", label = NULL, width = "180px",
            choices = c(
              "R data frame (.rds)" = "rds",
              "CSV (.csv)"          = "csv",
              "Markdown (.md)"      = "md",
              "LaTeX (.tex)"        = "tex"
            )
          ),
          shiny::downloadButton("download_learning", "Download Table",
                                class = "btn-default btn-sm",
                                icon  = shiny::icon("table"))
        )
      )
    })

    output$download_learning <- shiny::downloadHandler(
      filename = function() {
        fmt <- shiny::isolate(input$learning_dl_format) %||% "rds"
        ext <- switch(fmt, rds = ".rds", csv = ".csv", md = ".md", tex = ".tex")
        paste0("entropy_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ext)
      },
      content = function(file) {
        df  <- .build_learning_df(pre_r(), post_r())
        fmt <- shiny::isolate(input$learning_dl_format) %||% "rds"
        switch(fmt,
          rds = saveRDS(df, file),
          csv = utils::write.csv(df, file, row.names = FALSE),
          md  = writeLines(knitr::kable(df, format = "markdown", digits = 2), file),
          tex = writeLines(knitr::kable(df, format = "latex",    digits = 2), file)
        )
      }
    )

    # ------------------------------------------------------------------
    # "Close & Return to R" button — shared renderer, two placements
    # ------------------------------------------------------------------
    .close_btn_ui <- function() {
      if (hide_close) return(NULL)
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
    }

    # Pre-study tab: below DAG text box
    output$btn_close_pre_ui <- shiny::renderUI({
      pre_r(); post_r()   # take dependency so button appears once either tab completes
      .close_btn_ui()
    })

    # Learning tab
    output$btn_close_global_ui <- shiny::renderUI({
      pre_r(); post_r()
      .close_btn_ui()
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
.elicit_server <- function(id, dag, dag_info, mode, pre_result_r = NULL,
                            uploaded_override_rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns   # needed for dynamically generated IDs in renderUI

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
      dag_error       = NULL,
      uploaded_pre    = NULL,
      prefill_labels  = NULL   # list(node_labels = ..., value_labels = ...)
    )

    # ------------------------------------------------------------------
    # Upload observer (post tab only — input$upload_prior is NULL in pre)
    # ------------------------------------------------------------------
    shiny::observeEvent(input$upload_prior, {
      req_file <- input$upload_prior
      if (is.null(req_file)) return()
      tryCatch({
        imported        <- import_elicit_result(req_file$datapath)
        rv$uploaded_pre <- imported
        rv$prefill_labels <- list(node_labels  = imported$node_labels,
                                  value_labels = imported$labels)
        # Propagate to the pre-study tab so entropy comparison works
        if (!is.null(uploaded_override_rv))
          uploaded_override_rv$imported_pre <- imported
      }, error = function(e) {
        shiny::showNotification(
          paste("Import failed:", conditionMessage(e)),
          type = "error", duration = 8
        )
      })
    })

    # Upload status badge
    output$upload_status <- shiny::renderUI({
      if (is.null(rv$uploaded_pre)) return(NULL)
      n <- length(rv$uploaded_pre$dag_info$nodes)
      shiny::div(
        style = "font-size:0.78em; color:#1e8449; margin-top:-8px;",
        shiny::icon("check-circle"),
        sprintf(" Pre-study priors loaded (%d node%s)", n, if(n==1L) "" else "s")
      )
    })

    # Effective pre result: uploaded file takes priority over live Pre tab
    effective_pre_r <- shiny::reactive({
      if (!is.null(rv$uploaded_pre)) return(rv$uploaded_pre)
      if (!is.null(pre_result_r))    return(pre_result_r())
      NULL
    })

    # ------------------------------------------------------------------
    # Label table: render inputs + collect current values
    # ------------------------------------------------------------------
    output$label_table <- shiny::renderUI({

      nodes <- nodes_r()
      if (length(nodes) == 0L) return(NULL)
      shiny::tagList(
        shiny::div(
          style = "margin-bottom: 6px;",
          shiny::strong("Value labels", style = "font-size:0.9em;"),
          shiny::span(
            " \u2014 optional: name the 0 and 1 values of each node",
            style = "font-size:0.8em; color:#777;"
          )
        ),
        shiny::tags$table(
          class = "table table-condensed",
          style = "font-size:0.85em; margin-bottom:6px;",
          shiny::tags$thead(shiny::tags$tr(
            shiny::tags$th("Node", style = "width:12%;"),
            shiny::tags$th("Display name", style = "width:30%;"),
            shiny::tags$th("Label for 0", style = "width:29%;"),
            shiny::tags$th("Label for 1", style = "width:29%;")
          )),
          shiny::tags$tbody(
            lapply(nodes, function(nd) {
              pfl      <- rv$prefill_labels
              pfl_name <- pfl$node_labels[[nd]]   %||% ""
              pfl_lbl  <- pfl$value_labels[[nd]]
              val0 <- if (!is.null(pfl_lbl) && nzchar(pfl_lbl[1L])) pfl_lbl[1L] else "Low"
              val1 <- if (!is.null(pfl_lbl) && nzchar(pfl_lbl[2L])) pfl_lbl[2L] else "High"

              shiny::tags$tr(
                shiny::tags$td(shiny::strong(nd)),
                shiny::tags$td(shiny::textInput(
                  shiny::NS(id)(paste0("label_", nd, "_name")), label = NULL,
                  value = pfl_name, placeholder = "e.g. Treatment",
                  width = "100%"
                )),
                shiny::tags$td(shiny::textInput(
                  shiny::NS(id)(paste0("label_", nd, "_0")), label = NULL,
                  value = val0, placeholder = "e.g. absent / no / 0",
                  width = "100%"
                )),
                shiny::tags$td(shiny::textInput(
                  shiny::NS(id)(paste0("label_", nd, "_1")), label = NULL,
                  value = val1, placeholder = "e.g. present / yes / 1",
                  width = "100%"
                ))
              )
            })
          )
        ),
        shiny::hr(style = "margin: 8px 0;")
      )
    })

    # Collect current label values from the text inputs
    labels_r <- shiny::reactive({
      nodes <- nodes_r()
      if (length(nodes) == 0L) return(NULL)
      result <- vector("list", length(nodes))
      names(result) <- nodes
      for (nd in nodes) {
        lbl0 <- trimws(input[[paste0("label_", nd, "_0")]] %||% "")
        lbl1 <- trimws(input[[paste0("label_", nd, "_1")]] %||% "")
        if (nchar(lbl0) > 0L || nchar(lbl1) > 0L) {
          if (nchar(lbl0) == 0L) lbl0 <- "0"
          if (nchar(lbl1) == 0L) lbl1 <- "1"
          result[[nd]] <- c(lbl0, lbl1)
        }
      }
      result <- Filter(Negate(is.null), result)
      if (length(result) == 0L) NULL else result
    })

    # Collect node display-name labels (used in the DAG plot)
    node_display_labels_r <- shiny::reactive({
      nodes <- nodes_r()
      if (length(nodes) == 0L) return(NULL)
      lbl <- vapply(nodes, function(nd) {
        trimws(input[[paste0("label_", nd, "_name")]] %||% "")
      }, character(1L))
      names(lbl) <- nodes
      if (all(!nzchar(lbl))) return(NULL)
      # Fill blanks with the node name so .draw_dag always has a full vector
      lbl[!nzchar(lbl)] <- nodes[!nzchar(lbl)]
      lbl
    })

    # ------------------------------------------------------------------
    # DAG observer: text editor (pre/unlocked) or sync from pre (post/locked)
    # ------------------------------------------------------------------
    if (is.null(pre_result_r)) {
      # Editable DAG: watch the text area
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
          # Only reset elicitation state when the DAG structure actually changes.
          # This prevents an updateTextAreaInput() call (from the upload override)
          # from wiping the freshly loaded CPTs after the debounce delay.
          current_dag <- shiny::isolate(dag_rv())
          dag_changed <- is.null(current_dag) ||
                         !identical(as.character(new_dag), as.character(current_dag))
          dag_rv(new_dag); dag_info_rv(new_dag_info)
          rv$dag_error <- NULL
          if (dag_changed) {
            rv$pending     <- NULL; rv$cpts <- NULL; rv$result <- NULL
            rv$current_idx <- 1L
          }
        }, error = function(e) rv$dag_error <- conditionMessage(e))
      }, ignoreInit = TRUE, ignoreNULL = FALSE)

      output$dag_text_status <- shiny::renderUI({
        if (!is.null(rv$dag_error))
          shiny::div(style = "font-size:0.78em;color:#c0392b;margin-top:3px;",
                     shiny::icon("times-circle"), " ", rv$dag_error)
        else {
          n <- n_nodes_r(); if (n == 0L) return(NULL)
          shiny::div(style = "font-size:0.78em;color:#27ae60;margin-top:3px;",
                     shiny::icon("check-circle"),
                     sprintf(" %d node%s detected", n, if (n == 1L) "" else "s"))
        }
      })

      # When a file is uploaded in the Post tab, populate the Pre tab with those
      # values so the Learning tab can compute a meaningful entropy difference.
      if (!is.null(uploaded_override_rv)) {
        shiny::observeEvent(uploaded_override_rv$imported_pre, {
          imp <- uploaded_override_rv$imported_pre
          if (is.null(imp)) return()
          dag_rv(imp$dag)
          dag_info_rv(imp$dag_info)
          # Update the textarea so the displayed DAG matches; the debounce observer
          # will fire but will see dag_unchanged = TRUE and skip the state reset.
          shiny::updateTextAreaInput(session, "dag_text",
                                     value = as.character(imp$dag))
          nodes_imp <- imp$dag_info$order
          rv$pending <- stats::setNames(
            lapply(nodes_imp, function(nd) as.list(imp$cpts[[nd]]$prob)),
            nodes_imp
          )
          rv$cpts           <- imp$cpts
          rv$result         <- imp
          rv$dag_error      <- NULL
          rv$current_idx    <- 1L
          rv$prefill_labels <- list(node_labels  = imp$node_labels,
                                    value_labels = imp$labels)
          shiny::showNotification(
            paste0("Pre-study file loaded into Pre-Study tab (",
                   length(nodes_imp), " node",
                   if (length(nodes_imp) == 1L) "" else "s", ")."),
            type = "message", duration = 5
          )
        }, ignoreNULL = TRUE)
      }

    } else {
      # Locked DAG: sync structure and probabilities from effective pre result
      shiny::observeEvent(effective_pre_r(), {
        pre <- effective_pre_r()
        if (is.null(pre)) {
          dag_rv(NULL); dag_info_rv(NULL)
          rv$pending <- NULL; rv$cpts <- NULL; rv$result <- NULL
          return()
        }
        dag_rv(pre$dag)
        dag_info_rv(pre$dag_info)
        # Seed probabilities from the pre-study CPTs
        nodes_pre <- pre$dag_info$order
        rv$pending <- stats::setNames(
          lapply(nodes_pre, function(nd) as.list(pre$cpts[[nd]]$prob)),
          nodes_pre
        )
        rv$cpts           <- NULL
        rv$result         <- NULL
        rv$prefill_labels <- list(node_labels  = pre$node_labels,
                                  value_labels = pre$labels)
      }, ignoreNULL = FALSE)

      # Show the current locked DAG string (or a placeholder)
      output$dag_locked_display <- shiny::renderUI({
        d <- dag_rv()
        if (is.null(d)) {
          shiny::p(
            style = "font-size:0.85em; color:#999; font-style:italic; margin:4px 0;",
            "Complete the Pre-Study tab to set the graph."
          )
        } else {
          shiny::pre(
            style = "font-size:0.75em; background:#f8f8f8; border:1px solid #ddd;
                     padding:8px; border-radius:4px; white-space:pre-wrap; margin:0;",
            as.character(d)
          )
        }
      })
    }

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
      .draw_dag(d, di, elicited = elicited, active = active,
                layout = input$dag_layout %||% "circle",
                node_labels = node_display_labels_r())
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
        p <- .draw_dag(d, di, elicited = elicited,
                       layout = shiny::isolate(input$dag_layout) %||% "circle",
                       node_labels = shiny::isolate(node_display_labels_r()))
        grDevices::jpeg(file, width=1200, height=900, res=150, quality=95)
        print(p)
        grDevices::dev.off()
      }
    )

    output$download_tt <- shiny::downloadHandler(
      filename = function() paste0("theorytools_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".rds"),
      content  = function(file) saveRDS(to_theorytools(rv$result), file)
    )
    output$download_doc <- shiny::downloadHandler(
      filename = function() {
        fmt <- shiny::isolate(input$doc_format) %||% "qmd_bundle"
        ext <- switch(fmt, html = ".html", gfm = ".md", docx = ".docx", ".zip")
        paste0("preregistration_", format(Sys.time(),"%Y%m%d_%H%M%S"), ext)
      },
      content = function(file) {
        fmt <- shiny::isolate(input$doc_format) %||% "qmd_bundle"
        export_priors_qmd(rv$result, file, output_format = fmt)
      }
    )

    output$download_buttons <- shiny::renderUI({
      shiny::req(rv$result)
      has_cq     <- requireNamespace("CausalQueries", quietly = TRUE)
      has_tt     <- requireNamespace("theorytools",   quietly = TRUE)
      has_quarto <- requireNamespace("quarto",        quietly = TRUE)

      doc_choices <- c("Quarto + CSV (.zip)" = "qmd_bundle")
      if (has_quarto) {
        doc_choices <- c(doc_choices,
                         "HTML"           = "html",
                         "Markdown (.md)" = "gfm",
                         "Word (.docx)"   = "docx")
      }

      shiny::div(
        class = "dl-section",
        shiny::strong(style="font-size:0.9em;", "Download R objects:"),
        shiny::br(),
        shiny::downloadButton(session$ns("download_result"), "Result (.rds)",
                              class="btn-default btn-sm", icon=shiny::icon("download")),
        if (has_cq)
          shiny::downloadButton(session$ns("download_cq"), "CausalQueries (.rds)",
                                class="btn-default btn-sm", icon=shiny::icon("download")),
        if (has_tt)
          shiny::downloadButton(session$ns("download_tt"), "theorytools (.rds)",
                                class="btn-default btn-sm", icon=shiny::icon("download")),
        shiny::downloadButton(session$ns("download_plot"), "Graph (.jpg)",
                              class="btn-default btn-sm", icon=shiny::icon("image")),
        shiny::hr(style="margin:8px 0 4px;"),
        shiny::strong(style="font-size:0.9em;", "Download preregistration document:"),
        shiny::br(),
        shiny::div(
          style="display:flex; align-items:center; gap:8px; margin-top:4px; flex-wrap:wrap;",
          shiny::selectInput(session$ns("doc_format"), label = NULL,
                             choices  = doc_choices,
                             selected = "qmd_bundle",
                             width    = "220px"),
          shiny::downloadButton(session$ns("download_doc"), "Download Document",
                                class="btn-default btn-sm", icon=shiny::icon("file-alt"))
        )
      )
    })

    # ------------------------------------------------------------------
    # CPT display (inline tables, no pre-registration needed)
    # ------------------------------------------------------------------
    output$cpt_display <- shiny::renderUI({
      if (is.null(rv$cpts))
        return(shiny::div(style="color:#999;margin-top:16px;",
                          shiny::icon("arrow-up"),
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
          sprintf("\U0001F4CA Shannon entropy: %.2f%%  (%.1f%% of maximum %.2f%%)",
                  rv$result$entropy,
                  100 * rv$result$entropy / (n_nodes * log(2, base = 1.01)),
                  n_nodes * log(2, base = 1.01)))
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
      .show_node_modal(session, nodes, parents, m, rv$pending, 1L, n_nodes,
                       labels      = shiny::isolate(labels_r()),
                       node_labels = shiny::isolate(node_display_labels_r()))
    })

    shiny::observeEvent(input$modal_next, {
      nodes   <- shiny::isolate(nodes_r())
      parents <- shiny::isolate(parents_r())
      n_nodes <- shiny::isolate(n_nodes_r())
      m       <- shiny::isolate(mode_r())
      .save_modal_inputs(rv, nodes, parents, input, rv$current_idx, m)
      nxt <- rv$current_idx + 1L; rv$current_idx <- nxt
      .init_modal_state(rv, nodes, parents, nxt)
      .show_node_modal(session, nodes, parents, m, rv$pending, nxt, n_nodes,
                       labels      = shiny::isolate(labels_r()),
                       node_labels = shiny::isolate(node_display_labels_r()))
    })

    shiny::observeEvent(input$modal_prev, {
      nodes   <- shiny::isolate(nodes_r())
      parents <- shiny::isolate(parents_r())
      n_nodes <- shiny::isolate(n_nodes_r())
      m       <- shiny::isolate(mode_r())
      .save_modal_inputs(rv, nodes, parents, input, rv$current_idx, m)
      prv <- rv$current_idx - 1L; rv$current_idx <- prv
      .init_modal_state(rv, nodes, parents, prv)
      .show_node_modal(session, nodes, parents, m, rv$pending, prv, n_nodes,
                       labels      = shiny::isolate(labels_r()),
                       node_labels = shiny::isolate(node_display_labels_r()))
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
             dag=cur_dag, dag_info=cur_dag_info, mode=m,
             labels=shiny::isolate(labels_r()),
             node_labels=shiny::isolate(node_display_labels_r())),
        class = "elicit_dag_result"
      )
    })

    shiny::observeEvent(input$modal_cancel, {
      shiny::removeModal(); rv$current_idx <- 1L
    })

    shiny::observeEvent(input$modal_reset, {
      n <- rv$n_active_combos
      if (n <= 0L) return()
      if (shiny::isolate(mode_r()) == "probability") {
        # Root node (n == 1): max entropy for a binary variable is P = 0.5.
        # Non-root (n > 1): sliders are coupled and sum to 1, so equal = 1/n.
        equal_prob     <- if (n == 1L) 0.5 else 1 / n
        rv$modal_probs <- rep(equal_prob, n)
        for (j in seq_len(n))
          shiny::updateSliderInput(session, paste0("prob_", j), value = equal_prob)
      } else {
        # Likert: each combo is independent; max entropy per combo = ~50%
        for (j in seq_len(n))
          shiny::updateSelectInput(session, paste0("prob_", j), selected = "0.50")
      }
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
                              pending, idx, n_total, labels = NULL,
                              node_labels = NULL) {
  ns   <- session$ns
  node <- nodes[idx]
  pars <- parents[[node]]
  n_combos <- if (length(pars) == 0L) 1L else 2L^length(pars)

  # Returns "X (Display Name)" when a display label exists, else just "X"
  .ndl <- function(nd) {
    lbl <- node_labels[[nd]]
    if (!is.null(lbl) && nzchar(trimws(lbl))) sprintf("%s (%s)", nd, lbl) else nd
  }

  combos <- if (length(pars) > 0L)
    stats::setNames(
      expand.grid(lapply(pars, function(.) c(0L,1L)),
                  KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE),
      pars
    ) else data.frame()

  saved <- pending[[node]]

  input_rows <- lapply(seq_len(n_combos), function(i) {
    parent_vals <- if (length(pars) > 0L) {
      v <- as.integer(combos[i, pars, drop = TRUE])
      stats::setNames(v, pars)
    } else integer(0L)

    node_lbl1  <- .node_val_label(node, 1L, labels)
    prob_label <- if (length(pars) == 0L) {
      sprintf("P(%s = %s)", .ndl(node), node_lbl1)
    } else {
      parts <- vapply(pars, function(p)
        sprintf("%s = %s", .ndl(p), .node_val_label(p, parent_vals[[p]], labels)),
        character(1L))
      sprintf("P(%s = %s \u2502 %s)", .ndl(node), node_lbl1, paste(parts, collapse = ", "))
    }

    lbl_sentence <- format_label_sentence(node, parent_vals, labels)

    val <- if (!is.null(saved[[i]])) as.numeric(saved[[i]]) else 0.5
    shiny::div(class = "prob-row",
      # Label sentence sits above the input control
      shiny::p(lbl_sentence,
               style = "font-weight: bold;
                        color: #000;
                        font-size: 1.1em;
                        margin: 0 0 4px 0;"),
      if (mode == "probability") {
        shiny::tagList(
          shiny::sliderInput(ns(paste0("prob_", i)), prob_label,
                             min = 0, max = 1, step = 0.01, value = val,
                             width = "100%"),
          shiny::div(
            style = "display:flex; justify-content:space-between;
                      font-weight: 700;
                      color: black;
                     font-size:0.76em; margin:-10px 0 4px 0;",
            shiny::span("Never Happens"),
            shiny::span("Always Happens")
          ),
          shiny::uiOutput(ns(paste0("bar_", i))),
          shiny::div(class = "comp-text",
                     shiny::textOutput(ns(paste0("comp_", i)), inline = TRUE))
        )
      } else {
        nearest_key <- names(LIKERT_PROBS)[which.min(abs(LIKERT_PROBS - val))]
        shiny::selectInput(ns(paste0("prob_", i)), prob_label,
                           choices  = .LIKERT_CHOICES,
                           selected = as.character(LIKERT_PROBS[nearest_key]),
                           width    = "100%")
      }
    )
  })

  title_str <- if (length(pars) == 0L)
    sprintf("Node: %s  \u2014  prior probability", .ndl(node))
  else
    sprintf("Node: %s  \u2502  Parents: %s", .ndl(node),
            paste(vapply(pars, .ndl, character(1L)), collapse = ", "))

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
    shiny::p(class="text-muted", style="margin-bottom:6px;", hint),
    shiny::actionButton(ns("modal_reset"),
                        "\u21ba Reset to Maximum Uncertainty (Entropy)",
                        class = "btn-default btn-sm",
                        title = "Sets all probabilities to equal values, maximising entropy for this node"),
    shiny::hr(style="margin:10px 0 14px;"),
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
.draw_dag <- function(dag, dag_info, elicited=character(0), active=character(0),
                      layout = "circle", node_labels = NULL) {
  nodes  <- dag_info$nodes
  status <- ifelse(nodes %in% active,   "active",
            ifelse(nodes %in% elicited, "elicited", "pending"))
  status_df <- data.frame(name=nodes, status=status, stringsAsFactors=FALSE)

  # Merge in display labels; fall back to node name when not supplied
  if (!is.null(node_labels)) {
    lbl_df <- data.frame(name = names(node_labels),
                         display_label = unname(as.character(node_labels)),
                         stringsAsFactors = FALSE)
    status_df <- merge(status_df, lbl_df, by = "name", all.x = TRUE)
    status_df$display_label[is.na(status_df$display_label)] <-
      status_df$name[is.na(status_df$display_label)]
  } else {
    status_df$display_label <- status_df$name
  }

  tidy      <- try(ggdag::tidy_dagitty(dag, layout = layout))

  if('try-error' %in% class(tidy)) return(NULL)

  tidy$data <- merge(tidy$data, status_df, by="name", all.x=TRUE)
  tidy$data$status[is.na(tidy$data$status)] <- "pending"
  tidy$data$display_label[is.na(tidy$data$display_label)] <-
    tidy$data$name[is.na(tidy$data$display_label)]

  # Scale text size to fit longer display labels inside the node circle
  max_len  <- max(nchar(tidy$data$display_label), na.rm = TRUE)
  #txt_size <- if (max_len <= 4L) 3.8 else if (max_len <= 8L) 2.8 else 2.2
  txt_size <- 3.8
  p <- ggplot2::ggplot(tidy$data, ggplot2::aes(x=x, y=y, xend=xend, yend=yend)) +
    ggdag::geom_dag_edges(edge_colour="#95a5a6") +
    ggdag::geom_dag_point(ggplot2::aes(colour=status), size=20) +
    ggdag::geom_dag_text(ggplot2::aes(label=name), colour="white",
                         size=txt_size, fontface="bold") +
    ggplot2::scale_colour_manual(
      values = c(elicited="#27ae60", active="#e67e22", pending="#bdc3c7"),
      guide  = "none") +
    ggdag::theme_dag() +
    ggplot2::coord_cartesian(clip="off") +
    ggplot2::theme(plot.background=ggplot2::element_rect(fill="white", colour=NA))

if(!is.null(node_labels)) {

  p <-  p +     ggdag::geom_dag_label_repel(ggplot2::aes(label=display_label), colour="black",
                         size=3.8, fontface="bold", check_overlap = T)

}

  p

}


# ===========================================================================
# Utilities
# ===========================================================================

`%||%` <- function(x, y) if (!is.null(x)) x else y


#' Build the entropy comparison data frame for the Learning tab
#'
#' Returns one row per node (common to pre and post) plus a \code{[Joint]} row.
#' @keywords internal
.build_learning_df <- function(pre, post) {
  common <- intersect(names(pre$node_entropies), names(post$node_entropies))

  .ltype <- function(dh) {
    if (abs(dh) < 1) "No Learning" else if (dh < 0) "Type I" else "Type II"
  }

  node_rows <- do.call(rbind, lapply(common, function(nd) {
    dh  <- as.numeric(post$node_entropies[nd] - pre$node_entropies[nd])
    dir <- if (dh < -1e-6) "less uncertain" else
           if (dh > 1e-6) "more uncertain" else "no change"
    data.frame(
      Node               = nd,
      `Pre entropy (%)`  = round(as.numeric(pre$node_entropies[nd]),  4),
      `Post entropy (%)` = round(as.numeric(post$node_entropies[nd]), 4),
      `Delta (%)`        = round(dh, 4),
      Direction          = dir,
      `Learning Type`    = .ltype(dh),
      stringsAsFactors   = FALSE, check.names = FALSE
    )
  }))

  dh_joint  <- post$entropy - pre$entropy
  dir_joint <- if (dh_joint < -1e-6) "less uncertain" else
               if (dh_joint > 1e-6) "more uncertain" else "no change"
  joint_row <- data.frame(
    Node               = "[Joint]",
    `Pre entropy (%)`  = round(pre$entropy,  4),
    `Post entropy (%)` = round(post$entropy, 4),
    `Delta (%)`        = round(dh_joint, 4),
    Direction          = dir_joint,
    `Learning Type`    = .ltype(dh_joint),
    stringsAsFactors   = FALSE, check.names = FALSE
  )

  rbind(node_rows, joint_row)
}
