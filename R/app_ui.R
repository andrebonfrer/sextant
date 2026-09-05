#' The application User-Interface
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = "sextant",
      theme = bslib::bs_theme(
        version = 5,
        primary = "#1c3a52",
        secondary = "#b08d3e",
        "navbar-bg" = "#1c3a52"
      ),
      sidebar = bslib::sidebar(
        width = 300,
        mod_Load_ui("Load"),
        tags$hr(),
        helpText("sextant records where Gyro's parameter judgements",
                 "come from. It computes nothing \u2014 Gyro prices the",
                 "judgements; this documents their provenance.")
      ),
      bslib::nav_panel("Annotate", mod_Annotate_ui("Annotate")),
      bslib::nav_panel("Report", mod_Report_ui("Report"))
    )
  )
}

#' @import shiny
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))
  tags$head(
    golem::favicon(),
    golem::bundle_resources(path = app_sys("app/www"),
                            app_title = "sextant")
  )
}
