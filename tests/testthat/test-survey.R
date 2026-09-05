# The scripted full walk-through: drive the survey module section by
# section (renderUI must be touched so each section's observers exist
# before its inputs are set), then assemble and validate the file.

test_that("a scripted full walk-through yields a valid, annotated file", {
  spec <- question_spec()
  shiny::testServer(mod_survey_server, args = list(spec = spec), {
    a <- ma_answers()
    sections <- list(
      c("mode", "n_products", "n_attributes", "n_buckets"),
      grep("_name__", names(a), value = TRUE),
      c("market_potential", "frequency", "quantity", "periods",
        "discount_rate"),
      c(grep("line_owned__|line_parent__", names(a), value = TRUE),
        "nest_similarity"),
      c(grep("importance__|^rating__", names(a), value = TRUE),
        "price_sensitivity", "loyalty"),
      grep("line_price__|line_cost__", names(a), value = TRUE),
      c(grep("bucket_spend__|bucket_target__|bucket_uplift_ma__",
             names(a), value = TRUE))
    )
    for (k in seq_along(sections)) {
      invisible(output$section_ui)      # register this section's inputs
      do.call(session$setInputs, a[sections[[k]]])
      session$flushReact()
      invisible(output$section_ui)      # inputs revealed by the batch
      session$flushReact()              # register (and harvest) too
      if ("rating__0__0" %in% sections[[k]]) {
        session$setInputs(`rating__0__0__why` = "Q2 brand tracker")
      }
      session$setInputs(nxt = k)        # value must change to advance
    }
    ret <- session$getReturned()
    ans <- ret$answers()
    expect_equal(ans$mode, "ma")
    expect_equal(ans$rating__1__1, 4)

    src <- ret$sources()
    # discount_rate answered AT its benchmark (10) -> benchmark;
    # periods answered 5 against a benchmark of 4 -> stated
    expect_equal(src$discount_rate, "benchmark")
    expect_equal(src$periods, "stated")
    expect_equal(src$price_sensitivity, "stated")

    draft <- assemble_draft(spec, ans, src, ret$rationales())
    out <- tempfile(fileext = ".json")
    expect_invisible(write_params(draft, out))
    ann <- jsonlite::fromJSON(out, simplifyVector = FALSE)$annotations
    expect_equal(ann[["settings/discount_rate"]]$source, "benchmark")
    expect_equal(ann[["settings/periods"]]$source, "stated")
    expect_equal(ann[["engine/ratings/0/0"]]$rationale,
                 "Q2 brand tracker")
  })
})

test_that("load, edit one answer, save: only that change (plus its provenance)", {
  spec <- question_spec()
  p0 <- build_params(spec, ma_answers())
  p0$annotations <- list("engine/loyalty" = list(
    source = "stated", rationale = "Board workshop, March"))
  f0 <- tempfile(fileext = ".json"); write_params(p0, f0)
  got <- file_to_answers(spec, read_params(f0))

  rv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_survey_server,
                    args = list(spec = spec, load_r = reactive(rv())), {
    rv(got)
    session$flushReact()
    # loaded answers carry source 'file'
    expect_equal(session$getReturned()$sources()$loyalty, "file")

    # walk to the choice-drivers section so its observers exist
    for (k in 1:4) { invisible(output$section_ui)
                     session$setInputs(nxt = k) }
    invisible(output$section_ui)
    session$setInputs(rating__0__1 = 3.5)

    ret <- session$getReturned()
    expect_equal(ret$sources()$rating__0__1, "stated")
    draft <- assemble_draft(spec, ret$answers(), ret$sources(),
                            ret$rationales(),
                            prior_annotations = ret$prior_annotations())
    f1 <- tempfile(fileext = ".json"); write_params(draft, f1)
    p1 <- read_params(f1)

    expect_equal(p1$engine$ratings[1, 2], 3.5)         # the edit
    j <- function(x) as.character(jsonlite::toJSON(
      x, auto_unbox = TRUE, digits = NA, matrix = "rowmajor",
      null = "null", na = "null"))
    p0n <- p0; p1n <- p1
    p0n$engine$ratings[[1]][[2]] <- 999   # list form (builder output)
    p1n$engine$ratings[1, 2] <- 999       # matrix form (file read)
    for (fld in c("settings", "engine", "investments")) {
      expect_equal(j(p1n[[fld]]), j(p0n[[fld]]), info = fld)
    }
    # provenance: the file's own annotation survives untouched, and
    # exactly the edited leaf gained a new one
    expect_setequal(names(p1$annotations),
                    c("engine/loyalty", "engine/ratings/0/1"))
    expect_equal(p1$annotations[["engine/loyalty"]]$rationale,
                 "Board workshop, March")
    expect_equal(p1$annotations[["engine/ratings/0/1"]]$source,
                 "stated")
  })
})

test_that("the panel's jump lands the survey on the driving question", {
  spec <- question_spec()
  shiny::testServer(mod_survey_server, args = list(spec = spec), {
    invisible(output$section_ui)      # observers exist before inputs
    session$setInputs(mode = "ma", n_products = 2, n_attributes = 1,
                      n_buckets = 1)
    session$flushReact()
    ret <- session$getReturned()
    ret$jump("loyalty")
    session$flushReact()
    html <- as.character(output$progress$html)
    expect_match(html, "What drives choice")
  })
})


test_that("typing an ordinary answer never rebuilds the section (cursor stays put)", {
  spec <- question_spec()
  shiny::testServer(mod_survey_server, args = list(spec = spec), {
    invisible(output$section_ui)
    session$setInputs(mode = "ma")
    session$flushReact()
    invisible(output$section_ui)          # structural change re-rendered
    n0 <- session$getReturned()$.render_count()

    # ordinary answers: absorbed, but the DOM is left alone
    session$setInputs(n_products = 3)     # structural: allowed to rebuild
    session$flushReact()
    invisible(output$section_ui)
    n1 <- session$getReturned()$.render_count()
    expect_gt(n1, n0)

    for (k in 1:2) { invisible(output$section_ui)
                     session$setInputs(nxt = k) }
    invisible(output$section_ui)
    n2 <- session$getReturned()$.render_count()
    session$setInputs(market_potential = 5)
    session$setInputs(market_potential = 50)
    session$setInputs(market_potential = 500)   # keystrokes
    session$flushReact()
    invisible(output$section_ui)
    expect_equal(session$getReturned()$.render_count(), n2)
    expect_equal(session$getReturned()$answers()$market_potential, 500)
  })
})

test_that("typing a product name is absorbed without rebuilding the naming screen", {
  spec <- question_spec()
  shiny::testServer(mod_survey_server, args = list(spec = spec), {
    invisible(output$section_ui)
    session$setInputs(mode = "ma", n_products = 2, n_attributes = 1,
                      n_buckets = 1)
    session$flushReact()
    session$setInputs(nxt = 1)
    invisible(output$section_ui)
    n0 <- session$getReturned()$.render_count()
    session$setInputs(product_name__0 = "A")
    session$setInputs(product_name__0 = "Ac")
    session$setInputs(product_name__0 = "Acme")
    session$flushReact()
    invisible(output$section_ui)
    expect_equal(session$getReturned()$.render_count(), n0)
    expect_equal(session$getReturned()$answers()$product_name__0, "Acme")
  })
})
