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
