#' Quick implications (closed form)
#'
#' The implied-quantities panel exists so managers correct implications,
#' not coefficients. Everything here is closed-form arithmetic on the
#' current answers, computed inside sextant - no engine. Where the
#' schema lacks a parameter the classic formula wants, a documented
#' proxy stands in and the label says so:
#'
#' * **Retention.** The funnel's buyer-stage stay probability is a real
#'   repeat rate. The multi-attribute model has no retention parameter,
#'   so the proxy is the loyalty-implied repeat probability among
#'   evenly matched lines, \eqn{e^{loyalty}/(e^{loyalty} + J - 1)}.
#' * **Shares.** A softmax of the answered utilities (ratings times
#'   importance, price times sensitivity) - the model's own first
#'   period, before dynamics.
#' * **Acquisition cost.** The schema has none, so payback takes the
#'   cost as an input (the panel offers a scratch field for it).
#'
#' @param margin Contribution per customer per period.
#' @param retention Per-period repeat probability in `[0, 1)`.
#' @param discount Per-period discount rate.
#' @return [clv()]: the discounted value of the retained stream from
#'   next period on, \eqn{m \cdot r / (1 + d - r)}.
#' @export
clv <- function(margin, retention, discount) {
  stopifnot(retention >= 0, retention < 1, discount >= 0)
  margin * retention / (1 + discount - retention)
}

#' @rdname clv
#' @param cac Cost to win one customer.
#' @return [payback_periods()]: the smallest whole number of periods
#'   whose discounted margins cover `cac`, or `Inf` when they never do.
#' @export
payback_periods <- function(margin, discount, cac) {
  stopifnot(margin > 0, discount >= 0, cac >= 0)
  if (cac == 0) return(0L)
  if (discount == 0) return(as.integer(ceiling(cac / margin)))
  if (margin / discount <= cac) return(Inf)   # perpetuity can't cover it
  t <- log(margin / (margin - cac * discount)) / log(1 + discount)
  as.integer(ceiling(t - 1e-9))
}

# Coerce one answer to exactly one number: absent, empty, multi-valued
# or non-numeric answers all become NA, so partial survey states flow
# through the arithmetic guards instead of detonating in `if`/`||`.
# @noRd
num1 <- function(x) {
  v <- suppressWarnings(as.numeric(x %||% NA))
  if (length(v) != 1) return(NA_real_)
  v
}

# Softmax with max-subtraction.
# @noRd
softmax <- function(u) {
  e <- exp(u - max(u))
  e / sum(e)
}

#' @rdname clv
#' @param spec,answers The question spec and current answers.
#' @return [implied_summary()]: a list of rows, each
#'   `list(label, value, detail, drivers)` where `drivers` names the
#'   question ids the number leans on (for one-click revisiting).
#' @export
implied_summary <- function(spec, answers) {
  coll <- spec_collections(spec, answers)
  d <- num1(answers$discount_rate) / 100
  freq <- num1(answers$frequency)
  qty <- num1(answers$quantity)
  mp <- num1(answers$market_potential)
  rows <- list()
  if (!identical(answers$mode, "ma") &&
      !identical(answers$mode, "funnel")) {
    return(rows)
  }
  add <- function(label, value, detail, drivers) {
    rows[[length(rows) + 1L]] <<- list(label = label, value = value,
                                       detail = detail,
                                       drivers = drivers)
  }
  money <- function(x) paste0("$", format(round(x), big.mark = ","))

  if (identical(answers$mode, "ma")) {
    J <- length(coll$products)
    if (J == 0 || !is.finite(d) || !is.finite(freq)) return(rows)
    loy <- num1(answers$loyalty)
    r <- if (is.finite(loy)) exp(loy) / (exp(loy) + J - 1) else NA
    beta <- num1(answers$price_sensitivity)
    K <- length(coll$attributes)
    u <- vapply(seq_len(J) - 1L, function(i) {
      p <- num1(answers[[paste0("line_price__", i)]])
      s <- if (is.finite(beta) && is.finite(p)) beta * p else 0
      for (k in seq_len(K) - 1L) {
        w <- num1(answers[[paste0("importance__", k)]])
        x <- num1(answers[[paste0("rating__", i, "__", k)]])
        if (is.finite(w) && is.finite(x)) s <- s + w * x
      }
      s
    }, 0)
    shares <- softmax(u)
    portfolio <- 0
    for (i in seq_len(J) - 1L) {
      if (!isTRUE(answers[[paste0("line_owned__", i)]])) next
      p <- num1(answers[[paste0("line_price__", i)]])
      cc <- num1(answers[[paste0("line_cost__", i)]])
      if (!is.finite(p) || !is.finite(cc) || !is.finite(r)) next
      m <- (p - cc) * freq * (qty %||% 1)
      v <- clv(m, r, d)
      add(paste0("Lifetime value: ", coll$products[i + 1]), money(v),
          sprintf("margin %s/period x retention proxy %.0f%%",
                  money(m), 100 * r),
          c(paste0("line_price__", i), paste0("line_cost__", i),
            "loyalty", "discount_rate"))
      if (is.finite(mp)) portfolio <- portfolio + mp * shares[i + 1] * v
    }
    if (portfolio > 0) {
      add("Portfolio rough-up", money(portfolio),
          sprintf("market of %s x implied first-period shares x CLV",
                  format(mp, big.mark = ",")),
          c("market_potential", "rating__0__0", "importance__0"))
    }
  } else {
    S <- length(coll$stages)
    if (S == 0) return(rows)
    buyer <- answers$buyer_stage
    bi <- if (!is.null(buyer)) match(buyer, coll$stages) else S
    if (is.na(bi)) bi <- S            # renamed stages: fall back sanely
    stay <- num1(answers[[paste0("stage_stay__", bi - 1)]]) / 100
    p <- num1(answers$funnel_price)
    cc <- num1(answers$funnel_cost)
    if (is.finite(stay) && is.finite(p) && is.finite(cc) &&
        is.finite(d) && is.finite(freq)) {
      m <- (p - cc) * freq * (qty %||% 1)
      v <- clv(m, stay, d)
      add("Lifetime value of a buyer", money(v),
          sprintf("margin %s/period x repeat rate %.0f%% (stay at '%s')",
                  money(m), 100 * stay, coll$stages[bi]),
          c("funnel_price", "funnel_cost",
            paste0("stage_stay__", bi - 1), "discount_rate"))
      cnt <- num1(answers[[paste0("stage_count__", bi - 1)]])
      if (is.finite(cnt)) {
        add("Buyer pool rough-up", money(cnt * v),
            sprintf("%s current buyers x that lifetime value",
                    format(cnt, big.mark = ",")),
            c(paste0("stage_count__", bi - 1)))
      }
    }
  }
  rows
}
