#' The application User-Interface
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_fillable(
      theme = bslib::bs_theme(version = 5, primary = "#1c3a52",
                              secondary = "#b08d3e"),
      title = "sextant",
      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          bslib::card_header("sextant \u2014 the interview"),
          bslib::card_body(div(id = "sextant-survey-top",
                               mod_survey_ui("survey")))
        ),
        tagList(
          bslib::card(
            bslib::card_header("File"),
            bslib::card_body(
              fileInput("load_file", "Open a parameter file",
                        accept = ".json", buttonLabel = "Browse\u2026"),
              downloadButton("save_file", "Save parameter file"),
              tags$hr(),
              uiOutput("ai_panel"),
              helpText("Saving validates against the Gyro schema and",
                       "refuses an invalid file. Downloads never",
                       "overwrite anything without your say-so.")
            )
          ),
          mod_implied_ui("implied")
        )
      )
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
