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
mod_survey_server <- function(id, spec, load_r = reactive(NULL),
                              ai_r = reactive(NULL),
                              brief_r = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv_ans <- reactiveVal(list())
    rv_src <- reactiveVal(list())
    rv_rat <- reactiveVal(list())
    rv_direct <- reactiveVal(list())
    rv_prior_ann <- reactiveVal(list())
    rv_ai <- reactiveVal(character())      # instance ids proposed by AI
    rv_suggest <- reactiveVal(list())      # pending per-question suggestions
    skip_once <- new.env(parent = emptyenv())
    section_i <- reactiveVal(1L)
    highlight <- reactiveVal(NULL)
    seen <- new.env(parent = emptyenv())   # observer registry
    renders <- new.env(parent = emptyenv()); renders$n <- 0L

    # Only answers that change WHICH questions exist re-render the
    # section: mode, the collection counts, and anything a depends_on
    # references. Ordinary answers (prices, ratings, names) update
    # state without touching the DOM, so typing keeps its cursor.
    rv_render <- reactiveVal(0L)
    bump <- function() rv_render(isolate(rv_render()) + 1L)
    structural_ids <- unique(c(
      "mode",
      vapply(spec$collections, `[[`, "", "count"),
      unlist(lapply(spec$sections, function(s) {
        lapply(s$questions, function(q) q$depends_on$question)
      }))
    ))

    observeEvent(ai_r(), {
      got <- ai_r()
      req(is.list(got), length(got$answers) > 0)
      a <- rv_ans(); s <- rv_src(); r <- rv_rat()
      taken <- character()
      for (id in names(got$answers)) {
        if (!is.null(a[[id]])) next     # never clobber a human answer
        a[[id]] <- got$answers[[id]]
        s[[id]] <- got$sources[[id]] %||% "assumed"
        taken <- c(taken, id)
      }
      for (k in names(got$rationales %||% list())) {
        if (is.null(r[[k]])) r[[k]] <- got$rationales[[k]]
      }
      rv_ans(a); rv_src(s); rv_rat(r)
      rv_ai(union(rv_ai(), taken))
      bump()
      showNotification(
        sprintf("AI proposed %d answer(s); review each before saving.%s",
                length(taken),
                if (length(got$notes)) paste0(" Notes: ",
                  paste(utils::head(got$notes, 3), collapse = " | "))
                else ""),
        type = "message", duration = 10)
    })

    observeEvent(load_r(), {
      got <- load_r()
      req(is.list(got))
      rv_ans(got$answers)
      rv_direct(got$direct %||% list())
      rv_prior_ann(got$annotations %||% list())
      src <- lapply(got$answers, function(x) "file")
      rv_src(src)
      section_i(1L)
      bump()
      showNotification("File loaded; edit anywhere and re-save.",
                       type = "message", duration = 5)
    })

    visible_sections <- reactive({
      rv_render()
      a <- isolate(rv_ans())
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
      rv_render()
      vs <- visible_sections()
      i <- min(section_i(), length(vs))
      sec_id <- vs[[i]]$id
      inst <- instantiate_questions(spec, isolate(rv_ans()))
      Filter(function(q) identical(q$section, sec_id), inst)
    })

    absorb <- function(iid, q, v) {
      if (exists(iid, envir = skip_once)) {
        rm(list = iid, envir = skip_once)
        return(invisible())
      }
      a <- rv_ans(); a[[iid]] <- v; rv_ans(a)
      rv_ai(setdiff(rv_ai(), iid))       # a human edit ends the proposal
      bm <- q$benchmark$value
      s <- rv_src()
      s[[iid]] <- if (!is.null(bm) &&
                        isTRUE(all.equal(suppressWarnings(as.numeric(v)),
                                         as.numeric(bm)))) {
        "benchmark"
      } else "stated"
      rv_src(s)
      if (sub("__.*$", "", iid) %in% structural_ids) bump()
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
      meta_id <- paste0(iid, "__meta")
      if (!exists(meta_id, envir = seen)) {
        assign(meta_id, TRUE, envir = seen)
        output[[meta_id]] <- renderUI({
          ai_flag <- iid %in% rv_ai()
          sugg <- rv_suggest()[[iid]]
          tagList(
            if (ai_flag) {
              div(class = "small mb-1",
                  tags$span(class = "badge text-bg-warning",
                            paste0("AI-proposed \u00b7 ",
                                   rv_src()[[iid]] %||% "assumed")),
                  " ",
                  tags$em(rv_rat()[[paste0(iid, "__why")]] %||% ""))
            },
            if (!is.null(sugg)) {
              div(class = "border rounded p-2 mb-2 small",
                  tags$b("Suggested: "), format(sugg$answer),
                  if (nzchar(sugg$typical_range %||% "")) {
                    span(class = "text-muted",
                         paste0("  (typical: ", sugg$typical_range, ")"))
                  },
                  div(class = "text-muted", sugg$rationale),
                  actionButton(ns(paste0(iid, "__use")), "Use",
                               class = "btn-sm btn-outline-primary mt-1"))
            }
          )
        })
      }
      why_id <- paste0(iid, "__why")
      if (!exists(why_id, envir = seen)) {
        assign(why_id, TRUE, envir = seen)
        observeEvent(input[[why_id]], {
          r <- rv_rat(); r[[why_id]] <- input[[why_id]]; rv_rat(r)
        }, ignoreInit = TRUE)
      }
      sug_id <- paste0(iid, "__suggest")
      if (!exists(sug_id, envir = seen)) {
        assign(sug_id, TRUE, envir = seen)
        observeEvent(input[[sug_id]], {
          brief <- trimws(brief_r() %||% "")
          if (!nzchar(brief)) {
            showNotification("Add a brief in the AI panel first.",
                             type = "warning", duration = 6)
            return()
          }
          res <- tryCatch(ai_suggest_question(brief, q, spec),
                          error = function(e) {
                            showNotification(conditionMessage(e),
                                             type = "error",
                                             duration = 10)
                            NULL
                          })
          if (is.null(res)) {
            showNotification("No grounded suggestion for this one.",
                             type = "message", duration = 6)
            return()
          }
          sg <- rv_suggest(); sg[[iid]] <- res; rv_suggest(sg)
        }, ignoreInit = TRUE)
      }
      use_id <- paste0(iid, "__use")
      if (!exists(use_id, envir = seen)) {
        assign(use_id, TRUE, envir = seen)
        observeEvent(input[[use_id]], {
          res <- rv_suggest()[[iid]]
          req(!is.null(res))
          a <- rv_ans(); a[[iid]] <- res$answer; rv_ans(a)
          s <- rv_src(); s[[iid]] <- res$source; rv_src(s)
          if (nzchar(res$rationale)) {
            r <- rv_rat(); r[[paste0(iid, "__why")]] <- res$rationale
            rv_rat(r)
          }
          rv_ai(union(rv_ai(), iid))
          assign(iid, TRUE, envir = skip_once)   # programmatic update
          if (identical(q$type, "choice")) {
            updateSelectInput(session, iid, selected = res$answer)
          } else if (identical(q$type, "text")) {
            updateTextInput(session, iid, value = res$answer)
          } else {
            updateNumericInput(session, iid, value = res$answer)
          }
          sg <- rv_suggest(); sg[[iid]] <- NULL; rv_suggest(sg)
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
      cur <- isolate(rv_ans())[[iid]] %||% q$benchmark$value
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
        uiOutput(ns(paste0(q$instance_id, "__meta"))),
        if (ai_available()) {
          div(class = "small",
              actionLink(ns(paste0(q$instance_id, "__suggest")),
                         "suggest"))
        },
        if (!is.null(q$benchmark)) {
          p(class = "text-muted small mb-1",
            sprintf("Default shown: %s \u2014 %s", q$benchmark$value,
                    q$benchmark$note))
        },
        p(class = "text-muted small mb-1", q$help),
        if (!is.null(isolate(rv_direct())[[iid]])) {
          p(class = "small",
            sprintf("Currently in the file: %s (not re-askable directly;
answering here replaces it).",
                    paste(round(as.numeric(rv_direct()[[iid]]), 4),
                          collapse = ", ")))
        },
        tags$details(
          tags$summary(class = "text-muted small", "why? (optional)"),
          textInput(ns(paste0(iid, "__why")), NULL,
                    value = isolate(rv_rat())[[paste0(iid, "__why")]] %||% "",
                    placeholder = "On whose authority, from what evidence"))
      )
      style <- if (identical(highlight(), q$id) ||
                     identical(highlight(), iid)) {
        "border-left: 4px solid #b08d3e; padding-left: 10px;"
      } else ""
      div(style = style, class = "mb-3", base, notes)
    }

    output$section_ui <- renderUI({
      renders$n <- renders$n + 1L
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
      bump()
    })
    observeEvent(input$back, {
      section_i(max(section_i() - 1L, 1L))
      highlight(NULL)
      bump()
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
          bump()
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
      jump = jump,
      .render_count = function() renders$n
    )
  })
}
