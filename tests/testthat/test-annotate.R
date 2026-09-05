scn <- read_scenario(test_path("fixture-coffee-annotated.json"))
demo <- read_scenario(app_sys("demo", "coffee-portfolio.json"))

test_that("annotation_targets lists judgements with high-stakes flags", {
  t <- annotation_targets(demo)
  expect_true(all(c("settings/discount_rate", "engine/price_sensitivity",
                    "engine/nest_similarity", "investments/uplift/0") %in%
                    t$path))
  expect_true(t$high_stakes[t$path == "engine/price_sensitivity"])
  expect_true(t$high_stakes[t$path == "investments/uplift/2"])
  expect_false(t$high_stakes[t$path == "settings/periods"])
  # the pure price-cut bucket surfaces its price change as high-stakes
  expect_true("investments/price_change/2" %in% t$path)
  expect_true(all(is.na(t$source)))   # demo ships unannotated
})

test_that("existing Gyro annotations are read into the targets table", {
  t <- annotation_targets(scn)
  ps <- t[t$path == "engine/price_sensitivity", ]
  expect_equal(ps$source, "assumed")
  expect_match(ps$rationale, "grab-and-go")
  b0 <- t[t$path == "engine/brands/0", ]
  expect_equal(b0$location, "AU metro grocery")
})

test_that("set/get/drop annotation round-trip and validate", {
  s2 <- set_annotation(demo, "engine/price_sensitivity", "benchmark",
                       rationale = "Conjoint study, T1 2026",
                       location = "AU metro")
  a <- get_annotation(s2, "engine/price_sensitivity")
  expect_equal(a$source, "benchmark")
  expect_equal(a$location, "AU metro")
  s3 <- drop_annotation(s2, "engine/price_sensitivity")
  expect_null(get_annotation(s3, "engine/price_sensitivity"))
  expect_null(s3$annotations)   # last one removed -> block absent

  expect_error(set_annotation(demo, "engine/price_sensitivity", "guessed"))
  expect_error(set_annotation(demo, "no/such/path", "stated"),
               "not an annotatable")
})

test_that("annotations set by sextant survive a file round-trip", {
  s2 <- set_annotation(demo, "engine/nest_similarity", "assumed",
                       rationale = "Pending switching data")
  out <- tempfile(fileext = ".json")
  write_scenario(s2, out)
  back <- read_scenario(out)
  expect_equal(get_annotation(back, "engine/nest_similarity")$source,
               "assumed")
})

test_that("coverage counts, splits by source, and flags the red list", {
  cov0 <- annotation_coverage(demo)
  expect_equal(cov0$n_annotated, 0)
  expect_true("engine/price_sensitivity" %in% cov0$missing_high_stakes)

  s2 <- set_annotation(demo, "engine/price_sensitivity", "assumed",
                       "placeholder")
  s2 <- set_annotation(s2, "engine/nest_similarity", "benchmark",
                       "switching panel")
  cov <- annotation_coverage(s2)
  expect_equal(cov$n_annotated, 2)
  expect_equal(unname(cov$by_source[["assumed"]]), 1)
  expect_true("engine/price_sensitivity" %in% cov$assumed_high_stakes)
  expect_false("engine/nest_similarity" %in% cov$missing_high_stakes)
})
