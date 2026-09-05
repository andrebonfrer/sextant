#' Report UI Function
#'
#' @description Coverage at a glance, the full provenance report, and
#' the two exports: the annotated scenario (back to Gyro) and the
#' report itself (to the meeting).
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
mod_Report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      uiOutput(ns("box_coverage")),
      uiOutput(ns("box_hs")),
      uiOutput(ns("box_assumed"))
    ),
    bslib::card(
      bslib::card_header("Provenance report"),
      bslib::card_body(
        downloadButton(ns("dl_scenario"), "Annotated scenario (JSON)"),
        downloadButton(ns("dl_report"), "Report (Markdown)"),
        tags$hr(),
        uiOutput(ns("report_md"))
      )
    )
  )
}

#' Report Server Functions
#' @param scenario_r Reactive returning the current scenario (or NULL).
#' @noRd
mod_Report_server <- function(id, scenario_r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cov_r <- reactive({
      scn <- scenario_r()
      req(is.list(scn))
      annotation_coverage(scn)
    })

    output$box_coverage <- renderUI({
      cov <- cov_r()
      bslib::value_box(
        title = "Annotation coverage",
        value = sprintf("%d / %d", cov$n_annotated, cov$n_targets),
        showcase = icon("clipboard-check"),
        theme = if (cov$pct >= 1) "success" else "primary"
      )
    })
    output$box_hs <- renderUI({
      cov <- cov_r()
      n_missing <- length(cov$missing_high_stakes)
      bslib::value_box(
        title = "High-stakes without provenance",
        value = n_missing,
        showcase = icon("triangle-exclamation"),
        theme = if (n_missing == 0) "success" else "warning"
      )
    })
    output$box_assumed <- renderUI({
      cov <- cov_r()
      n <- length(cov$assumed_high_stakes)
      bslib::value_box(
        title = "High-stakes resting on assumption",
        value = n,
        showcase = icon("scale-unbalanced"),
        theme = if (n == 0) "success" else "danger"
      )
    })

    output$report_md <- renderUI({
      scn <- scenario_r()
      if (!is.list(scn)) {
        return(helpText("Load a scenario to see its provenance report."))
      }
      shiny::markdown(provenance_report(scn))
    })

    slug <- function() {
      scn <- scenario_r()
      nm <- trimws(scn$metadata$scenario_name %||% "")
      if (nzchar(nm)) gsub("[^A-Za-z0-9]+", "-", tolower(nm))
      else "scenario"
    }
    output$dl_scenario <- downloadHandler(
      filename = function() paste0("gyro-", slug(), "-annotated-",
                                   Sys.Date(), ".json"),
      content = function(file) write_scenario(scenario_r(), file)
    )
    output$dl_report <- downloadHandler(
      filename = function() paste0("provenance-", slug(), "-",
                                   Sys.Date(), ".md"),
      content = function(file) {
        writeLines(provenance_report(scenario_r()), file)
      }
    )
  })
}
