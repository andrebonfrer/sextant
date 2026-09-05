# Scripted answer sets shared by the build and contract tests.

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
