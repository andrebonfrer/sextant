test_that("a scripted multi-attribute answer set builds a file write_params accepts", {
  spec <- question_spec()
  p <- build_params(spec, ma_answers())
  expect_equal(p$mode, "ma")
  expect_equal(p$settings$discount_rate, 0.1)
  expect_equal(p$engine$nest_similarity, 0.6)
  expect_equal(unlist(p$engine$brands),
               c("Line A", "Line B", "Rival X"))
  expect_equal(p$engine$ratings[[1]][[2]], 3)
  expect_equal(p$investments$uplift[[2]][[2]], 0.4)
  expect_equal(unlist(p$investments$price_change), c(0, 0))
  out <- tempfile(fileext = ".json")
  expect_invisible(write_params(p, out))
  back <- jsonlite::fromJSON(out, simplifyVector = TRUE)
  expect_equal(back$engine$owned, c(TRUE, TRUE, FALSE))
  expect_equal(back$investments$targets[1], "Line A")
})

test_that("a scripted funnel answer set builds, with rows summing to one", {
  spec <- question_spec()
  p <- build_params(spec, funnel_answers())
  expect_equal(p$mode, "funnel")
  expect_equal(p$engine$buyer_stage, 3L)
  for (i in 1:3) {
    expect_equal(sum(unlist(p$engine$transition[[i]])), 1,
                 tolerance = 1e-12, info = paste("row", i))
  }
  # residual rule: for the first stage, recycling to the start IS
  # staying, so its diagonal absorbs both (.6 stay + .2 residual)
  expect_equal(p$engine$transition[[1]][[1]], 0.8)
  # a middle stage shows the rule distinctly: advance .3, stay .5,
  # residual .2 recycles to column 1
  expect_equal(p$engine$transition[[2]][[1]], 0.2)
  # last stage: no advance question existed; stay .85, recycle .15
  expect_equal(p$engine$transition[[3]][[3]], 0.85)
  expect_equal(p$engine$transition[[3]][[1]], 0.15)
  out <- tempfile(fileext = ".json")
  expect_invisible(write_params(p, out))
})

test_that("overfull funnel rows are refused with a plain message", {
  spec <- question_spec()
  a <- funnel_answers()
  a$stage_advance__0 <- 70; a$stage_stay__0 <- 60
  expect_error(build_params(spec, a), "exceeds 100 of 100")
})

test_that("pointer_set grows nested structures at 0-based indices", {
  x <- pointer_set(list(), "/a/b/2", 9)
  expect_equal(x$a$b[[3]], 9)
  expect_null(x$a$b[[1]])
  x <- pointer_set(x, "/a/b/0", 1)
  expect_equal(x$a$b[[1]], 1)
})
