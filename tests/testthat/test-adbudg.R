test_that("fit_adbudg recovers known parameters deterministically", {
  a <- 100; b <- 20; cc <- 1.5; d <- 8
  r <- function(x) b + (a - b) * x^cc / (d + x^cc)
  fit <- fit_adbudg(zero = b, current = r(1), increased = r(2),
                    saturation = a, x_current = 1, x_increased = 2)
  expect_equal(fit$c, cc, tolerance = 1e-10)
  expect_equal(fit$d, d, tolerance = 1e-10)
  expect_equal(fit$predict(3), r(3), tolerance = 1e-10)
  # deterministic: identical on repeat
  fit2 <- fit_adbudg(b, r(1), r(2), a)
  expect_identical(fit$c, fit2$c)
})

test_that("non-monotone anchors are refused", {
  expect_error(fit_adbudg(zero = 30, current = 25, increased = 60,
                          saturation = 100), "rise strictly")
  expect_error(fit_adbudg(zero = 10, current = 50, increased = 40,
                          saturation = 100), "rise strictly")
})
