#' The application server-side
#'
#' One piece of state: the current scenario (a Gyro scenario list, or
#' NULL before anything is loaded). Load sets it; Annotate mutates its
#' annotations; Report reads it.
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  scenario_rv <- reactiveVal(NULL)

  mod_Load_server("Load", set_scenario = scenario_rv)
  mod_Annotate_server("Annotate",
                      scenario_r = reactive(scenario_rv()),
                      set_scenario = scenario_rv)
  mod_Report_server("Report", scenario_r = reactive(scenario_rv()))
}
