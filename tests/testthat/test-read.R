test_that("read_params validates and parses; pointer_get walks the shapes", {
  p <- build_params(question_spec(), ma_answers())
  f <- tempfile(fileext = ".json")
  write_params(p, f)
  back <- read_params(f)
  expect_equal(back$mode, "ma")
  expect_true(is.matrix(back$engine$ratings))
  expect_equal(pointer_get(back, "/engine/ratings/1/1"), 4)
  expect_equal(pointer_get(back, "/engine/brands/2"), "Rival X")
  expect_equal(pointer_get(back, "/settings/discount_rate"), 0.1)

  writeLines('{"mode": "btyd"}', f2 <- tempfile(fileext = ".json"))
  expect_error(read_params(f2), "Not a valid parameter file")
})

test_that("file_to_answers inverts what it can and surfaces the rest", {
  spec <- question_spec()
  a0 <- ma_answers()
  p <- build_params(spec, a0)
  f <- tempfile(fileext = ".json"); write_params(p, f)
  got <- file_to_answers(spec, read_params(f))

  expect_equal(got$structural$n_products, 3)
  expect_equal(got$answers$discount_rate, 10)        # proportion -> count
  expect_equal(got$answers$nest_similarity, 50)      # similarity -> share
  expect_equal(got$answers$rating__1__1, 4)
  expect_equal(got$answers$line_owned__2, FALSE)
  expect_equal(got$answers$bucket_target__0, "Line A")
  expect_equal(got$answers$buyer_stage %||% "absent", "absent")

  # a full rebuild from the recovered answers reproduces the file
  p2 <- build_params(spec, got$answers)
  for (fld in c("settings", "engine", "investments")) {
    a_json <- jsonlite::toJSON(p[[fld]], auto_unbox = TRUE, digits = NA)
    b_json <- jsonlite::toJSON(p2[[fld]], auto_unbox = TRUE, digits = NA)
    expect_equal(as.character(a_json), as.character(b_json), info = fld)
  }
})

test_that("funnel answers round-trip through a file too", {
  spec <- question_spec()
  p <- build_params(spec, funnel_answers())
  f <- tempfile(fileext = ".json"); write_params(p, f)
  got <- file_to_answers(spec, read_params(f))
  expect_equal(got$answers$stage_stay__2, 85)
  expect_equal(got$answers$stage_advance__0, 20)
  expect_equal(got$answers$buyer_stage, "Buy")
  p2 <- build_params(spec, got$answers)
  expect_equal(unlist(p2$engine$transition),
               unlist(p$engine$transition), tolerance = 1e-12)
})
