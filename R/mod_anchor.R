#' Anchor-set widget
#'
#' @description Captures the four ADBUDG anchors, fits the curve live,
#' and plots it with the manager's own points marked so they can see -
#' and revise - what their judgements imply. Fitted parameters display
#' read-only; the value that will be written is the linear read at
#' planned spend (current minus zero).
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_anchor_ui <- function(id, q) {
  ns <- NS(id)
  labels <- c(zero = "At zero spend", current = "At the planned spend",
              increased = "At double the planned spend",
              saturation = "With unlimited spend")
  tagList(
    p(q$wording), p(class = "text-muted small", q$help),
    fluidRow(lapply(names(labels), function(a) {
      column(3, numericInput(ns(a), labels[[a]], value = NA,
                             min = q$bounds$min %||% NA,
                             max = q$bounds$max %||% NA))
    })),
    plotOutput(ns("curve"), height = 220),
    uiOutput(ns("fit_note"))
  )
}

#' @noRd
mod_anchor_server <- function(id, on_change) {
  moduleServer(id, function(input, output, session) {
    anchors_r <- reactive({
      v <- lapply(c("zero", "current", "increased", "saturation"),
                  function(a) input[[a]])
      names(v) <- c("zero", "current", "increased", "saturation")
      v
    })

    fit_r <- reactive({
      v <- anchors_r()
      if (any(vapply(v, function(x) is.null(x) || is.na(x), TRUE))) {
        return(NULL)
      }
      tryCatch(
        fit_adbudg(v$zero, v$current, v$increased, v$saturation,
                   x_current = 1, x_increased = 2),
        error = function(e) e
      )
    })

    observe(on_change(anchors_r()))

    output$curve <- renderPlot({
      f <- fit_r()
      req(!is.null(f), !inherits(f, "error"))
      x <- seq(0, 4, length.out = 121)
      graphics::plot(x, f$predict(x), type = "l", lwd = 2, col = "#1c3a52",
           xlab = "Spend (planned = 1)", ylab = "Response",
           main = "What your anchors imply")
      v <- anchors_r()
      graphics::points(c(0, 1, 2), c(v$zero, v$current, v$increased),
             pch = 19, col = "#b08d3e", cex = 1.4)
      graphics::abline(h = f$a, lty = 3)
    })

    output$fit_note <- renderUI({
      f <- fit_r()
      if (is.null(f)) {
        return(helpText("Answer all four anchors to see the curve."))
      }
      if (inherits(f, "error")) {
        return(p(class = "text-danger small", conditionMessage(f)))
      }
      p(class = "text-muted small",
        sprintf(paste("Fitted (read-only): floor %.3g, ceiling %.3g,",
                      "shape %.3g, half-way %.3g. Written to the file:",
                      "the change at planned spend, %.3g."),
                f$b, f$a, f$c, f$d,
                anchors_r()$current - anchors_r()$zero))
    })
  })
}
