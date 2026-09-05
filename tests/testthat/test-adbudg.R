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

test_that("an anchor_set builds its linear read and preserves the fit", {
  spec <- list(
    version = 1,
    collections = list(buckets = list(count = "n_buckets",
                                      name = "bucket_name")),
    sections = list(list(id = "s", title = "S", questions = list(
      list(id = "n_buckets", section = "s", param_path = NULL,
           wording = "How many spending proposals are on the table?",
           type = "number", bounds = list(min = 1, max = 8),
           transform = "to_integer", help = "Count them."),
      list(id = "mode_q", section = "s", param_path = "/mode",
           wording = "Which picture of demand fits?", type = "choice",
           options = list(list(label = "ma", value = "ma")),
           transform = "identity", help = "Sets the mode."),
      list(id = "resp", section = "s",
           param_path = "/investments/uplift/0/{bucket_index}",
           wording = "Of 100 customers, how many respond at each spend level?",
           type = "anchor_set", family = "adbudg",
           anchors = list("zero", "current", "increased", "saturation"),
           writes = list("/investments/uplift/0/{bucket_index}"),
           transform = "count_of_100_to_proportion",
           bounds = list(min = 0, max = 100),
           help = "Anchors at no spend, planned spend, double, and unlimited.",
           repetition = "per_bucket")
    )))
  )
  ans <- list(mode = "ma", n_buckets = 1, bucket_name__0 = "Push",
              resp__0__zero = 10, resp__0__current = 30,
              resp__0__increased = 42, resp__0__saturation = 60)
  p <- build_params(spec, ans)
  expect_equal(p$investments$uplift[[1]][[1]], 0.2)   # 0.30 - 0.10
  a <- p$annotations[["investments/uplift/0/0"]]
  expect_equal(a$source, "stated")
  expect_match(a$rationale, "ADBUDG")
  expect_match(a$rationale, "linear read")

  # non-monotone anchors are refused through the same path
  bad <- ans; bad$resp__0__increased <- 20
  expect_error(build_params(spec, bad), "rise strictly")
})
