test_that("get_schema prefers an installed Gyro, else the snapshot", {
  p <- get_schema()
  expect_true(file.exists(p))
  if (requireNamespace("Gyro", quietly = TRUE)) {
    expect_equal(normalizePath(p),
                 normalizePath(system.file("schema",
                                           "company-params.schema.json",
                                           package = "Gyro")))
  } else {
    expect_match(p, "sextant")
  }
})

test_that("the bundled snapshot has not drifted from installed Gyro", {
  skip_if_not_installed("Gyro")
  live <- system.file("schema", "company-params.schema.json",
                      package = "Gyro")
  snap <- app_sys("schema", "company-params.schema.json")
  expect_identical(readLines(snap), readLines(live))
})

test_that("write_params writes valid files and refuses invalid ones", {
  p <- build_params(question_spec(), ma_answers())
  out <- tempfile(fileext = ".json")
  expect_invisible(write_params(p, out))
  expect_true(file.exists(out))
  expect_equal(jsonlite::fromJSON(out)$mode, "ma")

  bad <- p
  bad$engine$nest_similarity <- 1.7
  out2 <- tempfile(fileext = ".json")
  err <- expect_error(write_params(bad, out2), "nothing written")
  expect_match(conditionMessage(err), "nest_similarity")
  expect_false(file.exists(out2))
})
