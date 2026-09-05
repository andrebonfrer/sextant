test_that("the anchor widget fits live, reports, and hands answers up", {
  captured <- NULL
  q <- list(instance_id = "resp__0",
            wording = "Of 100 customers, how many respond?",
            help = "Anchors.", bounds = list(min = 0, max = 100))
  shiny::testServer(mod_anchor_server,
                    args = list(on_change = function(v) captured <<- v), {
    session$setInputs(zero = 10, current = 30, increased = 42,
                      saturation = 60)
    expect_equal(captured$current, 30)
    html <- as.character(output$fit_note$html)
    expect_match(html, "Fitted \\(read-only\\)")
    expect_match(html, "20\\.")  # linear read, in answer units
  })
})

test_that("non-monotone anchors surface the refusal inline", {
  shiny::testServer(mod_anchor_server,
                    args = list(on_change = function(v) NULL), {
    session$setInputs(zero = 10, current = 50, increased = 40,
                      saturation = 60)
    html <- as.character(output$fit_note$html)
    expect_match(html, "rise strictly")
  })
})
