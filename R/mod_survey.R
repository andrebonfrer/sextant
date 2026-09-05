#' Survey module
#'
#' @description Renders the interview from the question spec: one
#' section per screen with progress, repetition instantiated from the
#' structural answers, depends_on respected, widgets chosen by type,
#' benchmark defaults visible with their source note. Source tracking:
#' an answer equal to the shown benchmark records `benchmark`; anything
#' the user typed records `stated`. A small optional "why?" field per
#' question captures the rationale.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_survey_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("progress")),
    uiOutput(ns("section_ui")),
    div(class = "mt-3",
        actionButton(ns("back"), "Back"),
        actionButton(ns("nxt"), "Next", class = "btn-primary"))
  )
}

#' @param spec A spec from [question_spec()].
#' @param load_r Reactive returning a [file_to_answers()] result to
#'   seed from (or NULL).
#' @return list(answers, sources, rationales, direct, jump).
#' @noRd
mod_survey_server <- function(id, spec, load_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv_ans <- reactiveVal(list())
    rv_src <- reactiveVal(list())
    rv_rat <- reactiveVal(list())
    rv_direct <- reactiveVal(list())
    rv_prior_ann <- reactiveVal(list())
    section_i <- reactiveVal(1L)
    highlight <- reactiveVal(NULL)
    seen <- new.env(parent = emptyenv())   # observer registry

    observeEvent(load_r(), {
      got <- load_r()
      req(is.list(got))
      rv_ans(got$answers)
      rv_direct(got$direct %||% list())
      rv_prior_ann(got$annotations %||% list())
      src <- lapply(got$answers, function(x) "file")
      rv_src(src)
      section_i(1L)
      showNotification("File loaded; edit anywhere and re-save.",
                       type = "message", duration = 5)
    })

    visible_sections <- reactive({
      a <- rv_ans()
      keep <- vapply(spec$sections, function(s) {
        any(vapply(s$questions, function(q) {
          dep_satisfied(q$depends_on, a)
        }, TRUE))
      }, TRUE)
      spec$sections[keep]
    })

    output$progress <- renderUI({
      vs <- visible_sections()
      i <- min(section_i(), length(vs))
      titles <- vapply(vs, `[[`, "", "title")
      div(class = "mb-2",
          tags$b(sprintf("%d of %d: %s", i, length(vs), titles[i])),
          div(class = "progress", style = "height: 6px;",
              div(class = "progress-bar",
                  style = sprintf("width: %.0f%%;",
                                  100 * i / length(vs)))))
    })

    current_insts <- reactive({
      vs <- visible_sections()
      i <- min(section_i(), length(vs))
      sec_id <- vs[[i]]$id
      inst <- instantiate_questions(spec, rv_ans())
      Filter(function(q) identical(q$section, sec_id), inst)
    })

    absorb <- function(iid, q, v) {
      a <- rv_ans(); a[[iid]] <- v; rv_ans(a)
      bm <- q$benchmark$value
      s <- rv_src()
      s[[iid]] <- if (!is.null(bm) &&
                        isTRUE(all.equal(suppressWarnings(as.numeric(v)),
                                         as.numeric(bm)))) {
        "benchmark"
      } else "stated"
      rv_src(s)
    }

    register <- function(iid, q) {
      if (exists(iid, envir = seen)) return(invisible())
      assign(iid, TRUE, envir = seen)
      observeEvent(input[[iid]], {
        absorb(iid, q, input[[iid]])
      }, ignoreInit = TRUE)
      # inputs revealed by an answer in the same section may already
      # hold a value when their observer is born - harvest it, or
      # ignoreInit swallows it forever
      existing <- isolate(input[[iid]])
      if (!is.null(existing) && is.null(isolate(rv_ans())[[iid]])) {
        absorb(iid, q, existing)
      }
      why_id <- paste0(iid, "__why")
      if (!exists(why_id, envir = seen)) {
        assign(why_id, TRUE, envir = seen)
        observeEvent(input[[why_id]], {
          r <- rv_rat(); r[[why_id]] <- input[[why_id]]; rv_rat(r)
        }, ignoreInit = TRUE)
      }
      if (identical(q$type, "anchor_set")) {
        mod_anchor_server(iid, on_change = function(v) {
          a <- rv_ans()
          for (nm in names(v)) {
            a[[paste0(iid, "__", nm)]] <- v[[nm]]
          }
          rv_ans(a)
          s <- rv_src(); s[[iid]] <- "stated"; rv_src(s)
        })
      }
    }

    widget_for <- function(q) {
      iid <- q$instance_id
      cur <- rv_ans()[[iid]] %||% q$benchmark$value
      base <- switch(
        q$type,
        text = textInput(ns(iid), q$wording,
                         value = cur %||% ""),
        choice = selectInput(
          ns(iid), q$wording,
          choices = stats::setNames(
            lapply(q$options, `[[`, "value"),
            vapply(q$options, `[[`, "", "label")),
          selected = cur),
        anchor_set = mod_anchor_ui(ns(iid), q),
        numericInput(ns(iid), q$wording, value = cur,
                     min = q$bounds$min %||% NA,
                     max = q$bounds$max %||% NA)
      )
      notes <- tagList(
        if (!is.null(q$benchmark)) {
          p(class = "text-muted small mb-1",
            sprintf("Default shown: %s \u2014 %s", q$benchmark$value,
                    q$benchmark$note))
        },
        p(class = "text-muted small mb-1", q$help),
        if (!is.null(rv_direct()[[iid]])) {
          p(class = "small",
            sprintf("Currently in the file: %s (not re-askable directly;
answering here replaces it).",
                    paste(round(as.numeric(rv_direct()[[iid]]), 4),
                          collapse = ", ")))
        },
        tags$details(
          tags$summary(class = "text-muted small", "why? (optional)"),
          textInput(ns(paste0(iid, "__why")), NULL,
                    value = rv_rat()[[paste0(iid, "__why")]] %||% "",
                    placeholder = "On whose authority, from what evidence"))
      )
      style <- if (identical(highlight(), q$id) ||
                     identical(highlight(), iid)) {
        "border-left: 4px solid #b08d3e; padding-left: 10px;"
      } else ""
      div(style = style, class = "mb-3", base, notes)
    }

    output$section_ui <- renderUI({
      insts <- current_insts()
      for (q in insts) register(q$instance_id, q)
      if (length(insts) == 0) {
        return(helpText("Nothing to ask here yet - earlier answers",
                        "decide what appears."))
      }
      tagList(lapply(insts, widget_for))
    })

    observeEvent(input$nxt, {
      section_i(min(section_i() + 1L, length(visible_sections())))
      highlight(NULL)
    })
    observeEvent(input$back, {
      section_i(max(section_i() - 1L, 1L))
      highlight(NULL)
    })

    jump <- function(qid) {
      base <- sub("__.*$", "", qid)
      vs <- visible_sections()
      for (i in seq_along(vs)) {
        if (any(vapply(vs[[i]]$questions, function(q) {
          identical(q$id, base)
        }, TRUE))) {
          section_i(i)
          highlight(base)
          return(invisible())
        }
      }
    }

    list(
      answers = reactive(rv_ans()),
      sources = reactive(rv_src()),
      rationales = reactive(rv_rat()),
      direct = reactive(rv_direct()),
      prior_annotations = reactive(rv_prior_ann()),
      jump = jump
    )
  })
}
