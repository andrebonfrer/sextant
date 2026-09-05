q <- function(type, transform, bounds = NULL, id = "t") {
  list(id = id, instance_id = id, type = type, transform = transform,
       bounds = bounds)
}

test_that("transforms round-trip on boundary values", {
  c100 <- q("count_of_100", "count_of_100_to_proportion",
            list(min = 0, max = 100))
  expect_equal(apply_transform(0, c100), 0)
  expect_equal(apply_transform(100, c100), 1)
  expect_equal(apply_transform(37, c100), 0.37)

  pct <- q("percent", "percent_to_proportion", list(min = 0, max = 100))
  expect_equal(apply_transform(100, pct), 1)

  expect_equal(apply_transform(4.6, q("number", "to_integer")), 5L)
  expect_true(apply_transform(TRUE, q("choice", "to_logical")))
  expect_false(apply_transform("false", q("choice", "to_logical")))
})

test_that("bounds refuse out-of-range and non-numeric answers", {
  c100 <- q("count_of_100", "count_of_100_to_proportion",
            list(min = 0, max = 100))
  expect_error(apply_transform(101, c100), "above the maximum")
  expect_error(apply_transform(-1, c100), "below the minimum")
  expect_error(apply_transform("many", c100), "not a number")
})

test_that("sister share maps monotonically onto within-brand similarity", {
  s <- q("count_of_100", "sister_share_to_similarity",
         list(min = 5, max = 100))
  expect_equal(apply_transform(50, s), 0.6)   # the coffee-demo value
  expect_equal(apply_transform(100, s), 0.2)  # clamped floor
  expect_gt(apply_transform(10, s), apply_transform(90, s))
})

test_that("stage_to_index resolves labels via collections", {
  ctx <- list(collections = list(stages = c("See", "Try", "Buy")))
  st <- q("choice", "stage_to_index")
  expect_equal(apply_transform("Buy", st, ctx), 3L)
  expect_error(apply_transform("Lost", st, ctx), "Unknown stage")
})
