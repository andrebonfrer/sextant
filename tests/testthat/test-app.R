test_that("modules wire together: load demo, annotate, report", {
  shiny::testServer(app_server, {
    session$setInputs(`Load-load_demo` = 1)
    scn <- scenario_rv()
    expect_equal(scn$mode, "ma")
    expect_null(scn$annotations)

    # Annotate the headline judgement through the module
    t <- annotation_targets(scn)
    i <- which(t$path == "engine/price_sensitivity")
    session$setInputs(`Annotate-targets_rows_selected` = i,
                      `Annotate-source` = "benchmark",
                      `Annotate-rationale` = "Conjoint study T1",
                      `Annotate-location` = "AU metro",
                      `Annotate-save` = 1)
    scn2 <- scenario_rv()
    a <- get_annotation(scn2, "engine/price_sensitivity")
    expect_equal(a$source, "benchmark")
    expect_equal(a$location, "AU metro")

    # Report reflects it
    expect_no_error(output$`Report-box_coverage`)
    expect_no_error(output$`Report-report_md`)

    # Remove flows back to naked
    session$setInputs(`Annotate-remove` = 1)
    expect_null(get_annotation(scenario_rv(),
                               "engine/price_sensitivity"))
  })
})

test_that("the ui builds", {
  expect_s3_class(app_ui(NULL), "shiny.tag.list")
})
