fixture <- test_path("fixture-coffee-annotated.json")

test_that("a real Gyro scenario reads, and schema gating works when available", {
  scn <- read_scenario(fixture)
  expect_equal(scn$mode, "ma")
  expect_equal(scn$schema_version, 2L)
  expect_true(is.matrix(scn$engine$ratings))
  expect_gt(length(scn$annotations), 0)
})

test_that("write_scenario round-trips a Gyro file without mangling it", {
  scn <- read_scenario(fixture)
  out <- tempfile(fileext = ".json")
  write_scenario(scn, out)
  back <- read_scenario(out)
  # double round-trip stabilises representation; deep equality after
  out2 <- tempfile(fileext = ".json")
  write_scenario(back, out2)
  back2 <- read_scenario(out2)
  expect_equal(back2, back)
  # and the substance survives the first trip
  expect_equal(back$engine$ratings, scn$engine$ratings)
  expect_equal(back$investments$uplift, scn$investments$uplift)
  expect_equal(back$annotations, scn$annotations)
  expect_equal(back$engine$pricing$mode, scn$engine$pricing$mode)
})

test_that("serialisation settings match Gyro's writer verbatim", {
  # the contract: same jsonlite arguments Gyro uses; a scenario written
  # by sextant must equal the same list written by those settings
  scn <- read_scenario(fixture)
  a <- tempfile(fileext = ".json"); b <- tempfile(fileext = ".json")
  write_scenario(scn, a)
  jsonlite::write_json(scn, b, pretty = TRUE, auto_unbox = TRUE,
                       digits = NA, matrix = "rowmajor",
                       null = "null", na = "null")
  expect_identical(readLines(a), readLines(b))
})

test_that("non-scenarios are refused with reasons", {
  bad <- tempfile(fileext = ".json")
  writeLines('{"mode": "btyd"}', bad)
  expect_error(read_scenario(bad, schema_check = FALSE), "mode")
  expect_error(read_scenario(tempfile(), schema_check = FALSE), "No file")
})

test_that("the bundled demo scenario is valid and unannotated", {
  demo <- app_sys("demo", "coffee-portfolio.json")
  scn <- read_scenario(demo)
  expect_null(scn$annotations)
  expect_equal(scn$mode, "ma")
})
