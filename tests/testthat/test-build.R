ma_answers <- function() {
  list(
    mode = "ma", n_products = 3, n_attributes = 2, n_buckets = 2,
    product_name__0 = "Line A", product_name__1 = "Line B",
    product_name__2 = "Rival X",
    attribute_name__0 = "Quality", attribute_name__1 = "Service",
    bucket_name__0 = "Push A", bucket_name__1 = "House ads",
    market_potential = 100000, frequency = 1, quantity = 1,
    periods = 5, discount_rate = 10,
    line_owned__0 = TRUE, line_owned__1 = TRUE, line_owned__2 = FALSE,
    line_parent__0 = "House", line_parent__1 = "House",
    line_parent__2 = "Rival",
    nest_similarity = 50,
    importance__0 = 1, importance__1 = 1,
    rating__0__0 = 4, rating__0__1 = 3, rating__1__0 = 3,
    rating__1__1 = 4, rating__2__0 = 3, rating__2__1 = 3,
    price_sensitivity = -0.3, loyalty = 1,
    line_price__0 = 10, line_price__1 = 9, line_price__2 = 10,
    line_cost__0 = 6, line_cost__1 = 5.5, line_cost__2 = 6,
    bucket_spend__0 = 20000, bucket_spend__1 = 30000,
    bucket_target__0 = "Line A", bucket_target__1 = "Line B",
    bucket_uplift_ma__0__0 = 0.5, bucket_uplift_ma__0__1 = 0,
    bucket_uplift_ma__1__0 = 0, bucket_uplift_ma__1__1 = 0.4
  )
}

funnel_answers <- function() {
  list(
    mode = "funnel", n_stages = 3, n_buckets = 1,
    stage_name__0 = "See", stage_name__1 = "Try", stage_name__2 = "Buy",
    bucket_name__0 = "Push",
    market_potential = 100000, frequency = 1, quantity = 1,
    periods = 6, discount_rate = 10,
    funnel_price = 12, funnel_cost = 7,
    stage_count__0 = 50000, stage_count__1 = 20000,
    stage_count__2 = 8000,
    stage_advance__0 = 20, stage_advance__1 = 30,
    stage_stay__0 = 60, stage_stay__1 = 50, stage_stay__2 = 85,
    buyer_stage = "Buy",
    bucket_spend__0 = 1000,
    bucket_uplift_funnel__0__0 = 5, bucket_uplift_funnel__0__1 = 0
  )
}

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

  # and the file is a real Gyro scenario to sextant's own reader too
  back <- read_scenario(out)
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
