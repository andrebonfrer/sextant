test_that("the provenance report renders coverage, tables and red flags", {
  scn <- read_scenario(test_path("fixture-coffee-annotated.json"))
  rpt <- provenance_report(scn)
  expect_type(rpt, "character")
  expect_match(rpt, "Parameter provenance")
  expect_match(rpt, "Coverage:")
  expect_match(rpt, "High-stakes judgements")
  expect_match(rpt, "grab-and-go")            # rationale carried through
  expect_match(rpt, "AU metro grocery")       # location carried through
  expect_match(rpt, "rest on assumption")     # price sensitivity is assumed+HS
  expect_match(rpt, "Not yet annotated")
})

test_that("a fully naked scenario reports its missing high-stakes list", {
  demo <- read_scenario(app_sys("demo-scenarios", "coffee-portfolio.json"))
  rpt <- provenance_report(demo)
  expect_match(rpt, "no provenance at all")
  expect_match(rpt, "0 of|0%|\\(0%\\)")
})
