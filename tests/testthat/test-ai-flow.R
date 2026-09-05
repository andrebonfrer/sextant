# Propose-review-commit, end to end, with the API mocked at its single
# seam - no live calls anywhere in the suite.

canned_prefill <- paste0('{',
  '"mode": {"answer": "ma", "source": "assumed", "rationale": "Competing offers."},',
  '"n_products": {"answer": 2, "source": "assumed", "rationale": ""},',
  '"n_attributes": {"answer": 1, "source": "assumed", "rationale": ""},',
  '"n_buckets": {"answer": 1, "source": "assumed", "rationale": ""},',
  '"product_name__0": {"answer": "Acme", "source": "assumed", "rationale": ""},',
  '"product_name__1": {"answer": "Rival", "source": "assumed", "rationale": ""},',
  '"attribute_name__0": {"answer": "Quality", "source": "assumed", "rationale": ""},',
  '"bucket_name__0": {"answer": "Ads", "source": "assumed", "rationale": ""},',
  '"discount_rate": {"answer": 10, "source": "benchmark", "rationale": "Common corporate hurdle-rate norm."},',
  '"loyalty": {"answer": 1.2, "source": "assumed", "rationale": "Sticky category."},',
  '"rating__0__0": {"answer": 9, "source": "assumed", "rationale": "out of bounds on purpose"}',
  '}')

test_that("a canned mock response flows end to end into flagged prefilled state", {
  testthat::local_mocked_bindings(
    anthropic_chat = function(...) canned_prefill, .package = "sextant")
  res <- prefill_from_brief("Acme sells widgets against one rival.")
  expect_equal(res$answers$loyalty, 1.2)
  expect_equal(res$sources$discount_rate, "benchmark")
  expect_null(res$answers$rating__0__0)   # dropped, not clamped
  expect_match(paste(res$notes, collapse = " "), "above the maximum")

  spec <- question_spec()
  rv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_survey_server,
                    args = list(spec = spec, ai_r = reactive(rv())), {
    rv(res)
    session$flushReact()
    ret <- session$getReturned()
    expect_equal(ret$answers()$loyalty, 1.2)
    expect_equal(ret$sources()$discount_rate, "benchmark")
    expect_equal(ret$rationales()[["loyalty__why"]], "Sticky category.")

    # walk to the choice-drivers screen: the proposal is flagged there
    for (k in 1:4) { invisible(output$section_ui)
                     session$setInputs(nxt = k) }
    html <- as.character(output$section_ui$html)
    expect_match(html, "AI-proposed")
    expect_match(html, "Sticky category")

    # a human edit ends the proposal: source flips, flag drops
    session$setInputs(loyalty = 1.5)
    expect_equal(session$getReturned()$sources()$loyalty, "stated")
    html2 <- as.character(output$section_ui$html)
    expect_false(grepl("AI-proposed", html2))

    # an incomplete prefill is refused (omission is correct behaviour;
    # a partial file is still not a valid file)
    partial <- assemble_draft(spec, ret$answers(), ret$sources(),
                              ret$rationales())
    expect_error(write_params(partial, tempfile(fileext = ".json")),
                 "required property")

    # the human completes what the model could not ground; the result
    # is a valid file whose provenance tells the truth about who said
    # what
    completion <- list(
      market_potential = 50000, frequency = 1, quantity = 1,
      periods = 5, price_sensitivity = -0.25,
      importance__0 = 1, rating__0__0 = 4, rating__1__0 = 3,
      line_price__0 = 10, line_price__1 = 9,
      line_cost__0 = 6, line_cost__1 = 6,
      bucket_spend__0 = 5000
    )
    ans <- utils::modifyList(ret$answers(), completion)
    src <- utils::modifyList(ret$sources(),
                             stats::setNames(as.list(rep("stated",
                                                length(completion))),
                                             names(completion)))
    draft <- assemble_draft(spec, ans, src, ret$rationales())
    out <- tempfile(fileext = ".json")
    expect_invisible(write_params(draft, out))
    ann <- jsonlite::fromJSON(out, simplifyVector = FALSE)$annotations
    expect_equal(ann[["settings/discount_rate"]]$source, "benchmark")
    expect_equal(ann[["engine/loyalty"]]$source, "stated")
    expect_equal(ann[["settings/market_potential"]]$source, "stated")
  })
})

test_that("prefill never clobbers an answer the human already gave", {
  spec <- question_spec()
  rv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_survey_server,
                    args = list(spec = spec, ai_r = reactive(rv())), {
    invisible(output$section_ui)
    session$setInputs(mode = "ma", n_products = 2, n_attributes = 1,
                      n_buckets = 1)
    session$flushReact()
    rv(list(answers = list(mode = "funnel", loyalty = 2),
            sources = list(mode = "assumed", loyalty = "assumed"),
            rationales = list(), notes = character()))
    session$flushReact()
    ret <- session$getReturned()
    expect_equal(ret$answers()$mode, "ma")      # human answer stands
    expect_equal(ret$answers()$loyalty, 2)      # blank was filled
  })
})

test_that("the per-question suggest control proposes, and Use commits with provenance", {
  testthat::local_mocked_bindings(
    anthropic_chat = function(...) paste0(
      '{"answer": 1.4, "source": "assumed", ',
      '"rationale": "Subscription-like repeat behaviour.", ',
      '"typical_range": "0.8-2"}'),
    .package = "sextant")
  spec <- question_spec()
  shiny::testServer(mod_survey_server,
                    args = list(spec = spec,
                                brief_r = reactive("Acme brief")), {
    invisible(output$section_ui)
    session$setInputs(mode = "ma", n_products = 2, n_attributes = 1,
                      n_buckets = 1)
    session$flushReact()
    for (k in 1:4) { invisible(output$section_ui)
                     session$setInputs(nxt = k) }
    invisible(output$section_ui)
    session$setInputs(loyalty__suggest = 1)
    html <- as.character(output$section_ui$html)
    expect_match(html, "Suggested")
    expect_match(html, "0.8-2")

    session$setInputs(loyalty__use = 1)
    ret <- session$getReturned()
    expect_equal(ret$answers()$loyalty, 1.4)
    expect_equal(ret$sources()$loyalty, "assumed")
    html2 <- as.character(output$section_ui$html)
    expect_match(html2, "AI-proposed")
  })
})

test_that("without a key the panel shows the notice and no prefill control", {
  old <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old), add = TRUE)
  shiny::testServer(app_server, {
    html <- as.character(output$ai_panel$html)
    expect_match(html, "AI prefill is off")
    expect_false(grepl("Prefill from brief", html))
  })
})
