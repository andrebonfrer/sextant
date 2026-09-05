test_that("the bundled question spec is valid", {
  spec <- question_spec()
  expect_gte(length(spec$sections), 5)
})

test_that("every schema-required parameter is reachable from some question", {
  spec <- question_spec()
  schema <- jsonlite::fromJSON(get_schema(), simplifyVector = FALSE)

  # question path templates, brace placeholders stripped to prefixes,
  # split by the mode that reveals them
  qs <- unlist(lapply(spec$sections, `[[`, "questions"),
               recursive = FALSE)
  prefix_of <- function(p) sub("/\\{.*$", "", p)
  paths_for_mode <- function(m) {
    keep <- vapply(qs, function(q) {
      d <- q$depends_on
      is.null(q$param_path) == FALSE &&
        (is.null(d) || !identical(d$question, "mode") ||
           identical(d$equals, m))
    }, TRUE)
    unique(vapply(qs[keep], function(q) prefix_of(q$param_path), ""))
  }
  builder_supplied <- "/schema_version"

  covered <- function(req_path, prefixes) {
    any(startsWith(req_path, prefixes)) ||
      req_path %in% builder_supplied
  }

  for (m in c("ma", "funnel")) {
    prefixes <- paths_for_mode(m)
    # root-level requireds
    for (k in schema$required) {
      if (k %in% c("settings", "engine", "investments")) next
      expect_true(covered(paste0("/", k), prefixes),
                  label = paste(m, "root", k))
    }
    for (k in schema$properties$settings$required) {
      expect_true(covered(paste0("/settings/", k), prefixes),
                  label = paste(m, "settings", k))
    }
    branch <- Filter(function(x) {
      identical(x$`if`$properties$mode$const, m)
    }, schema$allOf)[[1]]
    for (k in branch$then$properties$engine$required) {
      expect_true(covered(paste0("/engine/", k), prefixes),
                  label = paste(m, "engine", k))
    }
    for (k in schema$properties$investments$required) {
      expect_true(covered(paste0("/investments/", k), prefixes),
                  label = paste(m, "investments", k))
    }
  }
})

test_that("instantiation expands repetition, honours depends_on and skip_last", {
  spec <- question_spec()
  ans <- list(mode = "funnel", n_stages = 3, n_buckets = 1,
              stage_name__0 = "See", stage_name__1 = "Try",
              stage_name__2 = "Buy", bucket_name__0 = "Push")
  inst <- instantiate_questions(spec, ans)
  ids <- vapply(inst, `[[`, "", "instance_id")
  expect_true("stage_advance__0" %in% ids)
  expect_false("stage_advance__2" %in% ids)   # skip_last on stages
  expect_true("stage_stay__2" %in% ids)
  expect_false(any(grepl("^rating__", ids)))  # ma questions hidden
  adv0 <- inst[[which(ids == "stage_advance__0")]]
  expect_equal(adv0$param_path, "/engine/transition/0/1")
  expect_match(adv0$wording, "See")
  buyer <- inst[[which(ids == "buyer_stage")]]
  expect_equal(vapply(buyer$options, `[[`, "", "label"),
               c("See", "Try", "Buy"))
})

test_that("from_owned_products offers only owned lines", {
  spec <- question_spec()
  ans <- list(mode = "ma", n_products = 3, n_attributes = 1,
              n_buckets = 1,
              product_name__0 = "A", product_name__1 = "B",
              product_name__2 = "Rival",
              line_owned__0 = TRUE, line_owned__1 = TRUE,
              line_owned__2 = FALSE,
              attribute_name__0 = "Q", bucket_name__0 = "Ads")
  inst <- instantiate_questions(spec, ans)
  ids <- vapply(inst, `[[`, "", "instance_id")
  tgt <- inst[[which(ids == "bucket_target__0")]]
  expect_equal(vapply(tgt$options, `[[`, "", "value"), c("A", "B"))
})

test_that("an anchor_set question validates in the spec format", {
  tmp <- tempfile(fileext = ".yml")
  yaml::write_yaml(list(
    version = 1, collections = list(),
    sections = list(list(id = "s", title = "S", questions = list(list(
      id = "curve", section = "s", param_path = NULL,
      wording = "Of 100 customers, how many respond at each level?",
      type = "anchor_set", family = "adbudg",
      anchors = list("zero", "current", "increased", "saturation"),
      writes = list("/nowhere/yet"),
      transform = "identity", help = "Unwired until the schema has a home for it."
    ))))), tmp)
  expect_silent(spec <- question_spec(tmp))
  bad <- yaml::read_yaml(tmp)
  bad$sections[[1]]$questions[[1]]$family <- "logistic"
  tmp2 <- tempfile(fileext = ".yml")
  yaml::write_yaml(bad, tmp2)
  expect_error(question_spec(tmp2), "response family")
})
