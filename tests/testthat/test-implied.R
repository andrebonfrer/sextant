test_that("clv matches the hand calculation", {
  # m = 40, r = 0.8, d = 0.1: 40 * 0.8 / (1.1 - 0.8) = 32 / 0.3
  expect_equal(clv(40, 0.8, 0.1), 32 / 0.3)
  expect_equal(clv(40, 0, 0.1), 0)
  expect_error(clv(40, 1, 0.1))
})

test_that("payback_periods matches a hand-summed schedule", {
  # m = 40, d = 0.1, cac = 100:
  # PV of margins: 36.36, 33.06, 30.05, ... cum: 36.36, 69.42, 99.47, 126.79
  # -> covered in period 4
  expect_equal(payback_periods(40, 0.1, 100), 4L)
  expect_equal(payback_periods(40, 0.1, 99), 3L)
  expect_equal(payback_periods(40, 0, 100), 3L)     # 100/40 -> ceiling
  expect_equal(payback_periods(40, 0.1, 0), 0L)
  expect_equal(payback_periods(10, 0.1, 200), Inf)  # perpetuity = 100 < 200
})

test_that("implied_summary computes MA lifetime values and rough-up", {
  spec <- question_spec()
  a <- ma_answers()
  rows <- implied_summary(spec, a)
  labels <- vapply(rows, `[[`, "", "label")
  expect_true(any(grepl("Lifetime value: Line A", labels)))
  expect_true(any(grepl("Portfolio rough-up", labels)))
  # Line A: margin (10-6)*1 = 4; retention proxy e/(e+2) = .4761;
  # clv = 4 * .4761 / (1.1 - .4761)
  r <- exp(1) / (exp(1) + 2)
  v <- 4 * r / (1.1 - r)
  la <- rows[[which(grepl("Line A", labels))]]
  expect_equal(la$value, paste0("$", round(v)))
  expect_true("loyalty" %in% la$drivers)
})

test_that("implied_summary computes funnel buyer value from the stay rate", {
  spec <- question_spec()
  rows <- implied_summary(spec, funnel_answers())
  labels <- vapply(rows, `[[`, "", "label")
  expect_true(any(grepl("Lifetime value of a buyer", labels)))
  # margin (12-7)*1 = 5; stay at Buy = .85; clv = 5*.85/(1.1-.85) = 17
  b <- rows[[which(grepl("buyer", labels))[1]]]
  expect_equal(b$value, "$17")
})

test_that("implied_summary is total: every partial answer state returns, never errors", {
  spec <- question_spec()
  partials <- list(
    list(mode = "ma", n_products = 2),                       # the crash
    list(mode = "ma", n_products = 2, n_attributes = 1,
         line_price__0 = 10),                                # costs missing
    list(mode = "ma", n_products = 3, loyalty = 1),          # no economics
    list(mode = "funnel", n_stages = 3),                     # nothing else
    list(mode = "funnel", n_stages = 2, buyer_stage = "Gone",# renamed away
         funnel_price = 10, funnel_cost = 5,
         stage_stay__0 = 50, discount_rate = 10, frequency = 1)
  )
  for (a in partials) {
    expect_no_error(rows <- implied_summary(spec, a))
    expect_true(is.list(rows))
  }
})

test_that("the implied panel renders live at the exact partial state a user types through", {
  shiny::testServer(app_server, {
    invisible(output$`survey-section_ui`)
    session$setInputs(`survey-mode` = "ma")
    session$flushReact()
    invisible(output$`survey-section_ui`)
    session$setInputs(`survey-n_products` = 2)
    session$flushReact()
    expect_no_error(invisible(output$`implied-rows`))
    expect_no_error(invisible(output$`implied-payback`))
    expect_no_error(invisible(output$`implied-engine_ui`))
  })
})

test_that("spinner-style entry (stepping, clearing to NA) never crashes the panel", {
  shiny::testServer(app_server, {
    invisible(output$`survey-section_ui`)
    session$setInputs(`survey-mode` = "ma")
    session$flushReact()
    invisible(output$`survey-section_ui`)
    for (v in list(1, 2, NA, 3)) {      # click, click, clear, click
      session$setInputs(`survey-n_products` = v)
      session$flushReact()
      expect_no_error(invisible(output$`implied-rows`))
      expect_no_error(invisible(output$`implied-payback`))
    }
  })
})

test_that("legibility rows translate coefficients into shares, hand-checked", {
  spec <- question_spec()
  a <- ma_answers()
  rows <- implied_summary(spec, a)
  labels <- vapply(rows, `[[`, "", "label")

  # hand calculation of the softmax shares:
  # u = beta*price + sum(w * rating); beta=-0.3, w=(1,1)
  u <- c(-0.3 * 10 + 4 + 3, -0.3 * 9 + 3 + 4, -0.3 * 10 + 3 + 3)
  sh <- exp(u - max(u)); sh <- sh / sum(sh)

  i <- which(labels == "Implied starting shares")
  expect_length(i, 1)
  expect_match(rows[[i]]$value, sprintf("%.0f%%", 100 * sh[1]))
  expect_match(rows[[i]]$detail, "Rival X")
  expect_true("price_sensitivity" %in% rows[[i]]$drivers)

  # +1 rating point on the top-weight attribute: 100 * s(1-s) * w
  pp <- 100 * sh[1] * (1 - sh[1]) * 1
  j <- grep("One rating point", labels)
  expect_match(rows[[j]]$value, sprintf("%+.1f pp", pp), fixed = TRUE)

  # $1 price cut: 100 * s(1-s) * |beta|
  ppp <- 100 * sh[1] * (1 - sh[1]) * 0.3
  k <- grep("price cut", labels)
  expect_match(rows[[k]]$value, sprintf("%+.1f pp", ppp), fixed = TRUE)
})

test_that("legibility rows stay silent until ratings and importance exist", {
  spec <- question_spec()
  rows <- implied_summary(spec, list(mode = "ma", n_products = 2,
                                     line_owned__0 = TRUE,
                                     line_price__0 = 10,
                                     line_cost__0 = 6))
  labels <- vapply(rows, `[[`, "", "label")
  expect_false(any(grepl("Implied starting shares", labels)))
})
