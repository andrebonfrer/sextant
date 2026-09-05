test_that("a draft round-trips through JSON with sources, flags and provenance", {
  p <- draft_payload(
    answers = list(mode = "ma", n_products = 2, product_name__0 = "Acme",
                   loyalty = 1.2),
    sources = list(mode = "stated", loyalty = "assumed"),
    rationales = list(loyalty__why = "Sticky category."),
    ai = c("loyalty"),
    prior_annotations = list("engine/loyalty" = list(source = "assumed"))
  )
  d <- draft_from_json(draft_to_json(p))
  expect_true(is_draft(d))
  expect_equal(d$answers$loyalty, 1.2)
  expect_equal(d$sources$loyalty, "assumed")
  expect_equal(d$rationales$loyalty__why, "Sticky category.")
  expect_equal(d$ai, "loyalty")
  expect_equal(d$prior_annotations[["engine/loyalty"]]$source, "assumed")
  expect_null(draft_from_json("not even json"))
  expect_null(draft_from_json('{"some": "other file"}'))
})

test_that("a browser-stored draft is offered and restores the whole session", {
  json <- draft_to_json(do.call(draft_payload,
                                list(answers = ma_answers(),
                                     sources = list(loyalty = "stated"))))
  shiny::testServer(app_server, {
    session$setInputs(sextant_stored_draft = json)
    session$setInputs(restore_draft_yes = 1)
    session$elapse(2500)                       # let the panel's debounce pass
    html <- as.character(output$`implied-rows`$html)
    expect_match(html, "Lifetime value: Line A")
    expect_match(html, "Implied starting shares")
  })
})

test_that("the file loader recognises a draft file and restores instead of validating", {
  tmp <- tempfile(fileext = ".json")
  writeLines(draft_to_json(do.call(draft_payload,
                                   list(answers = funnel_answers()))), tmp)
  shiny::testServer(app_server, {
    session$setInputs(load_file = list(datapath = tmp, name = "d.json"))
    session$elapse(2500)
    html <- as.character(output$`implied-rows`$html)
    expect_match(html, "Lifetime value of a buyer")
  })
})
