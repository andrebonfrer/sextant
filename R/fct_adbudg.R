#' ADBUDG calibration from anchor answers
#'
#' The `anchor_set` question type bundles four behavioural anchors about
#' a response to spend - the response at zero spend, at current spend,
#' at a meaningfully increased spend, and at saturation - and fits them
#' deterministically to the ADBUDG family (Little 1970):
#' \deqn{r(x) = b + (a - b) \frac{x^c}{d + x^c}}
#' with \eqn{b = r(0)}, \eqn{a = r(\infty)}, and \eqn{c, d} solved in
#' closed form from the two interior anchors.
#'
#' **Not yet wired to any parameter**: the Gyro v0.5.0 schema declares
#' no response-curve family - bucket effects are linear uplifts at a
#' stated spend - so there are no `param_path`s for fitted curve
#' parameters to write to. The fitter ships implemented and tested; the
#' question spec accepts the type; wiring waits on a schema that has
#' somewhere for the answer to live.
#'
#' @param zero,current,increased,saturation Responses at spend 0, at
#'   `x_current`, at `x_increased`, and in the limit of unlimited spend.
#' @param x_current,x_increased The two positive spend levels the
#'   interior anchors refer to (`x_increased > x_current > 0`).
#' @return A list with elements `a`, `b`, `c`, `d` and a `predict`
#'   function of spend.
#' @export
fit_adbudg <- function(zero, current, increased, saturation,
                       x_current = 1, x_increased = 2) {
  stopifnot(x_increased > x_current, x_current > 0)
  b <- zero; a <- saturation
  if (!(a > increased && increased > current && current > b)) {
    stop("Anchors must rise strictly: zero < current < increased < ",
         "saturation.", call. = FALSE)
  }
  # (r - b) / (a - r) = x^c / d  at both interior anchors
  g1 <- (current - b) / (a - current)
  g2 <- (increased - b) / (a - increased)
  c_hat <- log(g2 / g1) / log(x_increased / x_current)
  d_hat <- x_current^c_hat / g1
  list(
    a = a, b = b, c = c_hat, d = d_hat,
    predict = function(x) b + (a - b) * x^c_hat / (d_hat + x^c_hat)
  )
}
