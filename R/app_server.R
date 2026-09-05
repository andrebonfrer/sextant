#' The application server-side
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  spec <- question_spec()

  load_rv <- reactiveVal(NULL)
  observeEvent(input$load_file, {
    got <- tryCatch({
      params <- read_params(input$load_file$datapath)
      file_to_answers(spec, params)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error",
                       duration = 12)
      NULL
    })
    req(got)
    load_rv(got)
  })

  survey <- mod_survey_server("survey", spec = spec,
                              load_r = reactive(load_rv()))

  mod_implied_server("implied", spec = spec,
                     answers_r = survey$answers, jump = survey$jump)

  draft_params <- reactive({
    assemble_draft(spec, survey$answers(), survey$sources(),
                   survey$rationales(),
                   prior_annotations = survey$prior_annotations())
  })

  output$save_file <- downloadHandler(
    filename = function() paste0("sextant-params-", Sys.Date(), ".json"),
    content = function(file) {
      tryCatch(write_params(draft_params(), file), error = function(e) {
        showNotification(conditionMessage(e), type = "error",
                         duration = 12)
        stop(shiny::safeError(e))
      })
    }
  )
}
