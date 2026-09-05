#' Implied-quantities panel
#'
#' @description Always visible, always live: quick implications
#' (closed form) of the current answers - lifetime values, the
#' portfolio rough-up, and a payback scratch line - each with a
#' one-click link back to the answer that drives it. If Gyro is
#' installed, a button runs the full engine on the current draft and
#' shows its equity beside the closed forms.
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_implied_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Quick implications (closed form)"),
    bslib::card_body(
      uiOutput(ns("rows")),
      tags$hr(),
      p(class = "small", tags$b("Payback scratchpad")),
      numericInput(ns("cac"), "Cost to win one customer", value = NA,
                   min = 0),
      uiOutput(ns("payback")),
      uiOutput(ns("engine_ui"))
    )
  )
}

#' @param spec The question spec.
#' @param answers_r Reactive returning the current answers.
#' @param jump Function taking a question id; moves the survey there.
#' @noRd
mod_implied_server <- function(id, spec, answers_r, jump) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rows_r <- reactive(implied_summary(spec, answers_r()))

    output$rows <- renderUI({
      rows <- rows_r()
      if (length(rows) == 0) {
        return(helpText("Implications appear as soon as the market and",
                        "money questions are answered."))
      }
      tagList(lapply(seq_along(rows), function(i) {
        r <- rows[[i]]
        div(class = "mb-2",
            div(tags$b(r$value), " ", r$label),
            div(class = "text-muted small", r$detail, " ",
                actionLink(ns(paste0("jump_", i)), "revisit")))
      }))
    })

    observe({
      rows <- rows_r()
      for (i in seq_along(rows)) {
        local({
          ii <- i
          observeEvent(input[[paste0("jump_", ii)]], {
            jump(rows_r()[[ii]]$drivers[1])
          }, ignoreInit = TRUE, once = TRUE)
        })
      }
    })

    margin_r <- reactive({
      a <- answers_r()
      f <- num1(a$frequency %||% 1)
      q <- num1(a$quantity %||% 1)
      if (identical(a$mode, "funnel")) {
        (num1(a$funnel_price) - num1(a$funnel_cost)) * f * q
      } else {
        (num1(a[["line_price__0"]]) - num1(a[["line_cost__0"]])) * f * q
      }
    })

    output$payback <- renderUI({
      a <- answers_r()
      m <- margin_r()
      d <- suppressWarnings(as.numeric(a$discount_rate)) / 100
      if (is.na(input$cac %||% NA) || !is.finite(m) || m <= 0 ||
          !is.finite(d)) {
        return(helpText("Enter a cost to see how many periods one",
                        "customer's margin takes to repay it."))
      }
      t <- payback_periods(m, d, input$cac)
      p(class = "small",
        if (is.infinite(t)) {
          "Never repaid: the discounted margin stream can't cover it."
        } else {
          sprintf("Repaid in %d period(s) at %s margin per period.",
                  t, paste0("$", format(round(m), big.mark = ",")))
        })
    })

    output$engine_ui <- renderUI({
      if (!has_gyro()) return(NULL)
      tagList(
        tags$hr(),
        actionButton(ns("run_engine"), "Run full model (Gyro)",
                     class = "btn-secondary btn-sm"),
        uiOutput(ns("engine_out"))
      )
    })

    engine_res <- reactiveVal(NULL)
    observeEvent(input$run_engine, {
      res <- tryCatch({
        p <- build_params(spec, answers_r())
        scn <- gyro_fn("validate_scenario")(p)
        sp <- gyro_fn("scenario_params")(scn)
        eq <- if (sp$engine == "ma") gyro_fn("ma_equity")(sp$params)
              else gyro_fn("funnel_equity")(sp$params)
        own <- if (sp$engine == "ma") sp$params$owned else TRUE
        list(total = sum(eq$equity[own]), per = eq$equity)
      }, error = function(e) e)
      engine_res(res)
      if (inherits(res, "error")) {
        showNotification(conditionMessage(res), type = "error",
                         duration = 10)
      }
    })

    output$engine_out <- renderUI({
      r <- engine_res()
      req(!is.null(r), !inherits(r, "error"))
      tagList(
        p(class = "small mt-2",
          tags$b(paste0("$", format(round(r$total), big.mark = ","))),
          " customer equity (full engine, owned lines)"),
        p(class = "text-muted small",
          "The engine runs the whole dynamic model; expect it to differ",
          "from the closed forms - that gap is the dynamics.")
      )
    })
  })
}
