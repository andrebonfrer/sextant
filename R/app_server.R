#' The application server-side
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  spec <- question_spec()

  load_rv <- reactiveVal(NULL)
  restore_rv <- reactiveVal(NULL)
  pending_draft <- reactiveVal(NULL)

  # a draft stored by this browser on a previous visit is offered back
  observeEvent(input$sextant_stored_draft, {
    d <- draft_from_json(input$sextant_stored_draft)
    req(!is.null(d), length(d$answers %||% list()) > 0)
    pending_draft(d)
    showModal(modalDialog(
      title = "Resume your draft?",
      sprintf("This browser holds a draft with %d answer(s), saved %s.",
              length(d$answers), d$saved_at %||% "earlier"),
      footer = tagList(
        actionButton("restore_draft_no", "Discard"),
        actionButton("restore_draft_yes", "Restore",
                     class = "btn-primary")
      ), easyClose = TRUE))
  })
  observeEvent(input$restore_draft_yes, {
    restore_rv(pending_draft())
    removeModal()
  })
  observeEvent(input$restore_draft_no, {
    session$sendCustomMessage("sextant-draft-clear", list())
    pending_draft(NULL)
    removeModal()
  })

  observeEvent(input$load_file, {
    path <- input$load_file$datapath
    # a draft restores; anything else must be a valid parameter file
    d <- draft_from_json(paste(readLines(path, warn = FALSE),
                               collapse = "\n"))
    if (!is.null(d)) {
      restore_rv(d)
      return()
    }
    got <- tryCatch({
      params <- read_params(path)
      file_to_answers(spec, params)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error",
                       duration = 12)
      NULL
    })
    req(got)
    load_rv(got)
  })

  ai_rv <- reactiveVal(NULL)

  output$ai_panel <- renderUI({
    if (!ai_available()) {
      return(helpText("AI prefill is off: set the ANTHROPIC_API_KEY",
                      "environment variable to enable it. The survey",
                      "works fully without it."))
    }
    tagList(
      p(class = "small", tags$b("AI prefill (propose\u2013review\u2013commit)")),
      textAreaInput("brief", NULL, rows = 4,
                    placeholder = paste("Describe the company, market,",
                                        "lines and proposals in a few",
                                        "sentences.")),
      actionButton("prefill", "Prefill from brief",
                   class = "btn-secondary btn-sm"),
      helpText("Proposals load flagged into the survey; nothing is",
               "saved until you review and save. Unanswered means the",
               "model could not ground it.")
    )
  })

  observeEvent(input$prefill, {
    brief <- trimws(input$brief %||% "")
    if (!nzchar(brief)) {
      showNotification("Write a brief first.", type = "warning",
                       duration = 6)
      return()
    }
    res <- tryCatch(
      withProgress(prefill_from_brief(brief, spec),
                   message = "Asking the model\u2026"),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error",
                         duration = 12)
        NULL
      })
    req(res)
    ai_rv(res)
  })

  survey <- mod_survey_server("survey", spec = spec,
                              load_r = reactive(load_rv()),
                              ai_r = reactive(ai_rv()),
                              brief_r = reactive(input$brief %||% ""),
                              restore_r = reactive(restore_rv()))

  # the panel breathes with the survey but not with every keystroke
  answers_calm <- shiny::debounce(survey$answers, 600)
  mod_implied_server("implied", spec = spec,
                     answers_r = answers_calm, jump = survey$jump)

  # autosave the draft into the visitor's own browser as they type
  state_calm <- shiny::debounce(survey$state, 2000)
  observe({
    st <- state_calm()
    req(length(st$answers) > 0)
    session$sendCustomMessage("sextant-draft-store", list(
      draft = draft_to_json(do.call(draft_payload, st))
    ))
  })

  output$save_draft <- downloadHandler(
    filename = function() paste0("sextant-draft-", Sys.Date(), ".json"),
    content = function(file) {
      writeLines(draft_to_json(do.call(draft_payload,
                                       survey$state())), file)
    }
  )

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
