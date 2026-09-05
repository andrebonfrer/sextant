test_that("extract_json handles clean, fenced, prose-wrapped and empty input", {
  clean <- '{"mode": {"answer": "ma", "source": "assumed", "rationale": "r"}}'
  expect_equal(extract_json(clean)$data$mode$answer, "ma")

  fenced <- paste0("Here you go:\n```json\n", clean, "\n```\nDone.")
  expect_equal(extract_json(fenced)$data$mode$answer, "ma")

  expect_equal(extract_json("no json here")$notes, "no JSON object found")
  expect_equal(length(extract_json("")$data), 0)
  expect_equal(length(extract_json(NULL)$data), 0)
})

test_that("extract_json recovers complete entries from truncated output", {
  truncated <- paste0(
    '{"mode": {"answer": "ma", "source": "assumed", "rationale": "x"},',
    '"n_products": {"answer": 3, "source": "assumed", "rationale": "y"},',
    '"loyalty": {"answer": 1.2, "source": "bench')
  got <- extract_json(truncated)
  expect_equal(got$data$mode$answer, "ma")
  expect_equal(got$data$n_products$answer, 3)
  expect_null(got$data$loyalty)
  expect_match(paste(got$notes, collapse = " "), "truncated")
})

test_that("validate_prefill drops out-of-bounds and unknown ids; never clamps", {
  spec <- question_spec()
  data <- list(
    mode = list(answer = "ma", source = "assumed", rationale = ""),
    n_products = list(answer = 2, source = "assumed", rationale = ""),
    n_attributes = list(answer = 1, source = "assumed", rationale = ""),
    n_buckets = list(answer = 1, source = "assumed", rationale = ""),
    product_name__0 = list(answer = "A", source = "assumed",
                           rationale = ""),
    product_name__1 = list(answer = "B", source = "assumed",
                           rationale = ""),
    attribute_name__0 = list(answer = "Q", source = "assumed",
                             rationale = ""),
    bucket_name__0 = list(answer = "Ads", source = "assumed",
                          rationale = ""),
    rating__0__0 = list(answer = 7, source = "assumed",
                        rationale = "too high"),
    rating__1__0 = list(answer = 4, source = "benchmark",
                        rationale = "Category tracker norm"),
    made_up__0 = list(answer = 1, source = "assumed", rationale = ""),
    loyalty = list(answer = 1, source = "guessed", rationale = "")
  )
  v <- validate_prefill(spec, data)
  expect_null(v$answers$rating__0__0)                 # dropped, not clamped
  expect_equal(v$answers$rating__1__0, 4)
  expect_equal(v$sources$rating__1__0, "benchmark")
  expect_null(v$answers$made_up__0)
  expect_equal(v$sources$loyalty, "assumed")          # coerced
  expect_match(paste(v$notes, collapse = " "), "above the maximum")
  expect_match(paste(v$notes, collapse = " "), "not a question here")
  expect_match(paste(v$notes, collapse = " "), "not allowed")
  expect_equal(v$rationales[["rating__1__0__why"]],
               "Category tracker norm")
})

test_that("anchor sub-answers validate through their parent question", {
  spec <- list(
    version = 1,
    collections = list(buckets = list(count = "n_buckets",
                                      name = "bucket_name")),
    sections = list(list(id = "s", title = "S", questions = list(
      list(id = "n_buckets", section = "s", param_path = NULL,
           wording = "How many proposals?", type = "number",
           bounds = list(min = 1, max = 8), transform = "to_integer",
           help = "h"),
      list(id = "resp", section = "s",
           param_path = "/investments/uplift/0/{bucket_index}",
           wording = "Of 100, how many respond?", type = "anchor_set",
           family = "adbudg",
           anchors = list("zero", "current", "increased", "saturation"),
           writes = list("/investments/uplift/0/{bucket_index}"),
           transform = "count_of_100_to_proportion",
           bounds = list(min = 0, max = 100), help = "h",
           repetition = "per_bucket")
    )))
  )
  data <- list(
    n_buckets = list(answer = 1, source = "assumed", rationale = ""),
    resp__0__zero = list(answer = 10, source = "assumed",
                         rationale = ""),
    resp__0__current = list(answer = 130, source = "assumed",
                            rationale = "over")
  )
  v <- validate_prefill(spec, data)
  expect_equal(v$answers$resp__0__zero, 10)
  expect_null(v$answers$resp__0__current)   # out of bounds -> dropped
})

test_that("ai_available reflects the environment; the chat refuses without a key", {
  old <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old), add = TRUE)
  expect_false(ai_available())
  expect_error(anthropic_chat("hi"), "ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "test-key-not-real")
  expect_true(ai_available())
})
