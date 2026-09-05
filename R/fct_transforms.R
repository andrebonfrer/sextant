#' Answer-to-parameter transforms
#'
#' Every question declares a named transform mapping its behavioural
#' answer onto the parameter the file stores. The registry
#' `sextant_transforms` implements them; [apply_transform()] checks the
#' question's bounds on the raw answer, then applies the transform.
#'
#' `sister_share_to_similarity` deserves its footnote: the question asks,
#' of 100 customers who leave one of your lines, how many land on a
#' sister line; the parameter is Gyro's within-brand similarity
#' \eqn{\lambda \in (0.2, 1]}. The mapping `1 - 0.8 * share` (clamped) is
#' a documented calibration convention, monotone and deterministic - an
#' even 50 lands on 0.6, the value Gyro's coffee demo uses - not a
#' model-derived inversion, which would depend on the shares themselves.
#'
#' @name transforms
NULL

#' @rdname transforms
#' @export
sextant_transforms <- list(
  identity = function(x, ctx = NULL) x,
  to_integer = function(x, ctx = NULL) as.integer(round(as.numeric(x))),
  to_logical = function(x, ctx = NULL) {
    isTRUE(x) || identical(x, "true") || identical(x, "TRUE")
  },
  percent_to_proportion = function(x, ctx = NULL) as.numeric(x) / 100,
  count_of_100_to_proportion = function(x, ctx = NULL) as.numeric(x) / 100,
  sister_share_to_similarity = function(x, ctx = NULL) {
    max(0.2, min(1, 1 - 0.8 * (as.numeric(x) / 100)))
  },
  stage_to_index = function(x, ctx = NULL) {
    i <- match(x, ctx$collections$stages)
    if (is.na(i)) stop("Unknown stage '", x, "'.", call. = FALSE)
    as.integer(i)
  }
)

#' @rdname transforms
#' @param answer A raw answer.
#' @param question An instantiated question (for bounds, type, transform).
#' @param ctx Context list (collections) for transforms that need it.
#' @return The parameter value.
#' @export
apply_transform <- function(answer, question, ctx = NULL) {
  b <- question$bounds
  if (!is.null(b) && question$type %in%
        c("number", "percent", "currency", "count_of_100")) {
    x <- suppressWarnings(as.numeric(answer))
    if (is.na(x)) {
      stop(question$instance_id %||% question$id,
           ": answer is not a number.", call. = FALSE)
    }
    if (!is.null(b$min) && x < b$min) {
      stop(question$instance_id %||% question$id, ": ", x,
           " is below the minimum of ", b$min, ".", call. = FALSE)
    }
    if (!is.null(b$max) && x > b$max) {
      stop(question$instance_id %||% question$id, ": ", x,
           " is above the maximum of ", b$max, ".", call. = FALSE)
    }
  }
  f <- sextant_transforms[[question$transform %||% "identity"]]
  f(answer, ctx)
}
