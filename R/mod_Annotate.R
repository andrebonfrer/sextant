#' Annotate UI Function
#'
#' @description The working surface: every annotatable judgement in one
#' table (high-stakes rows flagged), and a form to attach source,
#' rationale and location to the selected one.
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
#' @importFrom shiny NS tagList
mod_Annotate_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header("Parameter judgements"),
      bslib::card_body(
        helpText("Select a row to annotate it. High-stakes rows are the",
                 "judgements Gyro's conclusions lean on hardest \u2014",
                 "cover those first."),
        DT::DTOutput(ns("targets"))
      )
    ),
    bslib::card(
      bslib::card_header("Provenance"),
      bslib::card_body(
        uiOutput(ns("selected_info")),
        radioButtons(ns("source"), "Source",
                     choices = c("Stated \u2014 someone with standing said so" = "stated",
                                 "Benchmark \u2014 measured or external reference" = "benchmark",
                                 "Assumed \u2014 a working guess awaiting evidence" = "assumed")),
        textAreaInput(ns("rationale"), "Rationale", rows = 3,
                      placeholder = "Why this value, on whose authority"),
        textInput(ns("location"), "Location (optional)",
                  placeholder = "Market, segment or geography"),
        actionButton(ns("save"), "Save annotation",
                     class = "btn-primary"),
        actionButton(ns("remove"), "Remove", class = "btn-outline-danger")
      )
    )
  )
}

#' Annotate Server Functions
#' @param scenario_r Reactive returning the current scenario (or NULL).
#' @param set_scenario A `reactiveVal` to write the mutated scenario to.
#' @noRd
mod_Annotate_server <- function(id, scenario_r, set_scenario) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    targets_r <- reactive({
      scn <- scenario_r()
      req(is.list(scn))
      annotation_targets(scn)
    })

    display_tbl <- reactive({
      t <- targets_r()
      data.frame(
        Parameter = t$label,
        Value = t$value,
        `High stakes` = ifelse(t$high_stakes, "\u26a0", ""),
        Source = ifelse(is.na(t$source), "\u2014", t$source),
        Location = ifelse(is.na(t$location), "", t$location),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })

    output$targets <- DT::renderDT({
      DT::datatable(
        isolate(display_tbl()),
        rownames = FALSE, selection = "single",
        options = list(pageLength = 25, dom = "ft", scrollY = 420,
                       ordering = FALSE)
      )
    })

    # keep the table current without destroying the user's selection
    observeEvent(display_tbl(), {
      DT::replaceData(DT::dataTableProxy("targets"), display_tbl(),
                      rownames = FALSE, resetPaging = FALSE)
    }, ignoreInit = TRUE)

    selected_path <- reactive({
      i <- input$targets_rows_selected
      req(length(i) == 1, !is.na(i))
      targets_r()$path[i]
    })

    output$selected_info <- renderUI({
      i <- input$targets_rows_selected
      if (length(i) != 1) {
        return(helpText("Select a parameter on the left."))
      }
      row <- targets_r()[i, ]
      tagList(
        p(tags$b(row$label), br(),
          tags$code(row$path), br(),
          span(class = "text-muted", paste("Current value:", row$value)))
      )
    })

    # seed the form from the existing annotation when selection changes
    observeEvent(input$targets_rows_selected, {
      i <- input$targets_rows_selected
      req(length(i) == 1, !is.na(i))
      row <- targets_r()[i, ]
      updateRadioButtons(session, "source",
                         selected = if (is.na(row$source)) "assumed"
                                    else row$source)
      updateTextAreaInput(session, "rationale",
                          value = if (is.na(row$rationale)) ""
                                  else row$rationale)
      updateTextInput(session, "location",
                      value = if (is.na(row$location)) ""
                              else row$location)
    })

    observeEvent(input$save, {
      scn <- scenario_r()
      req(is.list(scn))
      path <- selected_path()
      set_scenario(set_annotation(
        scn, path, input$source,
        rationale = input$rationale,
        location = input$location
      ))
      showNotification(sprintf("Annotated %s.", path),
                       type = "message", duration = 4)
    })

    observeEvent(input$remove, {
      scn <- scenario_r()
      req(is.list(scn))
      path <- selected_path()
      set_scenario(drop_annotation(scn, path))
      showNotification(sprintf("Annotation removed from %s.", path),
                       type = "message", duration = 4)
    })
  })
}
