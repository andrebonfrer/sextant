#' Build a parameter file from answers
#'
#' [build_params()] assembles a complete Gyro parameter list from a
#' question spec and a set of answers: it instantiates the questions,
#' applies each answer's transform to its JSON-pointer target, and then
#' applies the deterministic assembly rules the pointer grammar cannot
#' express - zero-filling unanswered matrix cells, and the funnel
#' residual rule (whatever share of a stage neither advances nor stays
#' drops back to the first stage, so every transition row sums to one by
#' construction). The result is ready for [write_params()].
#'
#' @param spec A spec from [question_spec()].
#' @param answers Named list keyed by `instance_id` (structural answers
#'   by plain id).
#' @return A parameter list.
#' @export
build_params <- function(spec, answers) {
  coll <- spec_collections(spec, answers)
  inst <- instantiate_questions(spec, answers)
  mode <- answers$mode %||% stop("'mode' must be answered.", call. = FALSE)

  params <- list(
    schema_version = 2L,
    app = "sextant",
    app_version = as.character(utils::packageVersion("sextant")),
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    metadata = list(scenario_name = "", author = "",
                    notes = "Elicited with sextant."),
    mode = mode,
    settings = list(),
    engine = list(),
    investments = list()
  )

  ctx <- list(collections = coll)
  for (q in inst) {
    if (is.null(q$param_path)) next
    if (identical(q$type, "anchor_set")) {
      params <- apply_anchor_set(params, q, answers, ctx)
      next
    }
    a <- answers[[q$instance_id]]
    if (is.null(a)) next
    params <- pointer_set(params, q$param_path,
                          apply_transform(a, q, ctx))
  }

  J <- length(coll$products); K <- length(coll$attributes)
  B <- length(coll$buckets); S <- length(coll$stages)

  if (identical(mode, "ma")) {
    params$engine$intercepts <- params$engine$intercepts %||%
      as.list(rep(0, J))
    params$engine$max_rating <- params$engine$max_rating %||% 5
    params$engine$uplift_rows <- NULL
    params$investments$uplift <- fill_matrix(params$investments$uplift,
                                             K, B, 0)
  } else {
    params$engine$transition <- fill_matrix(params$engine$transition,
                                            S, S, 0)
    for (i in seq_len(S)) {
      row <- vapply(params$engine$transition[[i]],
                    function(v) as.numeric(v %||% 0), 0)
      resid <- 1 - sum(row)
      if (resid < -1e-9) {
        stop("Stage '", coll$stages[i], "': advancing plus staying ",
             "exceeds 100 of 100 customers.", call. = FALSE)
      }
      row[1] <- row[1] + max(resid, 0)
      params$engine$transition[[i]] <- as.list(row)
    }
    params$investments$uplift <- fill_matrix(params$investments$uplift,
                                             max(S - 1, 1), B, 0)
  }
  params$investments$price_change <- params$investments$price_change %||%
    as.list(rep(0, B))
  params
}

# Set a value at a JSON pointer ("/a/b/0"), growing nested lists as
# needed; numeric segments are 0-based indices.
# @noRd
pointer_set <- function(x, pointer, value) {
  segs <- strsplit(sub("^/", "", pointer), "/", fixed = TRUE)[[1]]
  set_rec <- function(node, segs, value) {
    if (length(segs) == 0) return(value)
    s <- segs[[1]]
    idx <- suppressWarnings(as.integer(s))
    if (!is.na(idx)) {
      i <- idx + 1L
      if (is.null(node)) node <- list()
      if (length(node) < i) {
        node <- c(node, vector("list", i - length(node)))
      }
      node[[i]] <- set_rec(node[[i]], segs[-1], value)
    } else {
      if (is.null(node)) node <- list()
      node[[s]] <- set_rec(node[[s]], segs[-1], value)
    }
    node
  }
  set_rec(x, segs, value)
}

# Ensure a list-of-rows matrix of dimension nrow x ncol, filling missing
# cells with `fill`.
# @noRd
fill_matrix <- function(m, nrow, ncol, fill = 0) {
  if (is.null(m)) m <- list()
  if (length(m) < nrow) m <- c(m, replicate(nrow - length(m), list(),
                                            simplify = FALSE))
  for (i in seq_len(nrow)) {
    row <- m[[i]] %||% list()
    if (length(row) < ncol) {
      row <- c(row, vector("list", ncol - length(row)))
    }
    m[[i]] <- lapply(row, function(v) v %||% fill)
  }
  m
}

# Linear wiring of an anchor_set question, pending a response-curve
# family in the Gyro schema: the four anchor answers (each passed
# through the question's transform) are fitted to ADBUDG - which also
# enforces that they rise sensibly - and the value written to the
# question's param_path is the linear read at the planned spend,
# response(current) - response(zero). The full fitted curve is preserved
# in the file's annotations block so nothing elicited is lost; when a
# schema with curve parameters exists, the same anchors re-fit
# losslessly.
# @noRd
apply_anchor_set <- function(params, q, answers, ctx) {
  keys <- paste0(q$instance_id, "__", c("zero", "current", "increased",
                                        "saturation"))
  raw <- lapply(keys, function(k) answers[[k]])
  if (any(vapply(raw, is.null, TRUE))) return(params)
  tq <- q; tq$type <- "count_of_100"   # bounds-check each anchor answer
  v <- vapply(raw, function(a) as.numeric(apply_transform(a, tq, ctx)), 0)
  fit <- fit_adbudg(zero = v[1], current = v[2], increased = v[3],
                    saturation = v[4], x_current = 1, x_increased = 2)
  params <- pointer_set(params, q$param_path, v[2] - v[1])
  ann <- params$annotations %||% list()
  ann[[sub("^/", "", q$param_path)]] <- list(
    source = "stated",
    rationale = sprintf(
      paste("ADBUDG anchors (zero/current/2x/saturation =",
            "%.4g/%.4g/%.4g/%.4g); fitted a=%.4g b=%.4g c=%.4g d=%.4g;",
            "wrote the linear read at planned spend (current - zero)",
            "pending a response-curve family in the Gyro schema."),
      v[1], v[2], v[3], v[4], fit$a, fit$b, fit$c, fit$d)
  )
  params$annotations <- ann
  params
}

#' Assemble the annotated draft from survey state
#'
#' [build_params()] plus the provenance layer: every answered question
#' whose source the survey tracked gets an annotation on its parameter
#' path (`stated` or `benchmark`, with the optional rationale), except
#' where the builder already wrote one (the anchor-set auto-annotation
#' wins). Answers whose source is `file` - loaded and untouched - get
#' no new annotation: the file already says what it says.
#'
#' @param spec,answers As in [build_params()].
#' @param sources Named list, instance id -> "stated"/"benchmark"/"file".
#' @param rationales Named list, `<instance id>__why` -> free text.
#' @return An annotated parameter list ready for [write_params()].
#' @export
assemble_draft <- function(spec, answers, sources = list(),
                           rationales = list(),
                           prior_annotations = list()) {
  p <- build_params(spec, answers)
  inst <- instantiate_questions(spec, answers)
  # start from the loaded file's provenance; the builder's anchor
  # auto-annotations overlay it, and this session's tracked sources
  # overlay in turn on the leaves the user actually touched
  ann <- prior_annotations
  for (k in names(p$annotations %||% list())) {
    ann[[k]] <- p$annotations[[k]]
  }
  for (q in inst) {
    if (is.null(q$param_path)) next
    s <- sources[[q$instance_id]]
    if (is.null(s) || identical(s, "file")) next
    key <- sub("^/", "", q$param_path)
    if (!is.null((p$annotations %||% list())[[key]])) next  # anchor wins
    entry <- list(source = s)
    why <- trimws(rationales[[paste0(q$instance_id, "__why")]] %||% "")
    if (nzchar(why)) entry$rationale <- why
    ann[[key]] <- entry
  }
  if (length(ann)) p$annotations <- ann
  p
}
