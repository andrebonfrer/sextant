#' Load UI Function
#'
#' @description Sidebar loader: open a Gyro scenario file, or start from
#' the bundled demo (Gyro's coffee portfolio, annotations stripped).
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
mod_Load_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("scenario_file"), "Open a Gyro scenario",
              accept = ".json", buttonLabel = "Browse\u2026"),
    actionButton(ns("load_demo"), "Load the demo scenario",
                 class = "btn-secondary btn-sm"),
    uiOutput(ns("loaded_note"))
  )
}

#' Load Server Functions
#' @param set_scenario A `reactiveVal` to receive the loaded scenario.
#' @noRd
mod_Load_server <- function(id, set_scenario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    do_load <- function(path, label) {
      scn <- tryCatch(read_scenario(path), error = function(e) {
        showNotification(conditionMessage(e), type = "error",
                         duration = 12)
        NULL
      })
      req(scn)
      set_scenario(scn)
      nm <- scn$metadata$scenario_name %||% ""
      showNotification(
        if (nzchar(nm)) sprintf("%s '%s' loaded.", label, nm)
        else paste(label, "loaded."),
        type = "message", duration = 5
      )
    }

    observeEvent(input$scenario_file, {
      do_load(input$scenario_file$datapath, "Scenario")
    })
    observeEvent(input$load_demo, {
      do_load(app_sys("demo-scenarios", "coffee-portfolio.json"), "Demo")
    })

    output$loaded_note <- renderUI({
      scn <- set_scenario()
      if (is.null(scn)) {
        helpText("Nothing loaded yet. Export a scenario from Gyro's",
                 "Integrations tab, or start with the demo.")
      } else {
        nm <- scn$metadata$scenario_name %||% "(unnamed)"
        p(class = "text-muted small",
          sprintf("Loaded: %s (%s mode, %d buckets)", nm, scn$mode,
                  length(scn$investments$buckets)))
      }
    })
  })
}
