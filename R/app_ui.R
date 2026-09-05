#' The application User-Interface
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      h1("sextant"),
      p("Elicitation for Gyro parameter files. The interview UI arrives",
        "in a later prompt; the question spec, transforms and builder",
        "live in the package now.")
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
