#' Read a parameter file back into survey answers
#'
#' [read_params()] is the mirror of [write_params()]: schema-validated
#' parse of an existing file. [file_to_answers()] repopulates the survey
#' from it - structural answers are derived from the file's shape, and
#' each instantiated question whose transform is invertible gets its
#' answer back (a proportion becomes a count of 100 again, a similarity
#' becomes a sister-share, a stage index becomes its label). Where a
#' transform is not invertible - the anchor-set linear read is the case
#' in point - the parameter value is surfaced directly instead, to be
#' shown read-only with its source until re-elicited.
#'
#' @param path Path of a parameter file.
#' @return `read_params()`: the parameter list (vectors and matrices
#'   simplified).
#' @export
read_params <- function(path) {
  if (!file.exists(path)) stop("No file at '", path, "'.", call. = FALSE)
  v <- sextant_validator(get_schema())
  json <- paste(readLines(path, warn = FALSE), collapse = "\n")
  ok <- v(json, verbose = TRUE, greedy = TRUE)
  if (!isTRUE(ok)) {
    err <- attr(ok, "errors")
    where <- err$instancePath %||% err$dataPath %||% ""
    where[!nzchar(where)] <- "(root)"
    stop("Not a valid parameter file:\n",
         paste0("- ", where, ": ", err$message, collapse = "\n"),
         call. = FALSE)
  }
  jsonlite::fromJSON(json, simplifyVector = TRUE, simplifyMatrix = TRUE)
}

# Inverses for the transforms that have them.
# @noRd
sextant_inverses <- list(
  identity = function(v, ctx = NULL) v,
  to_integer = function(v, ctx = NULL) as.integer(v),
  to_logical = function(v, ctx = NULL) isTRUE(v),
  percent_to_proportion = function(v, ctx = NULL) as.numeric(v) * 100,
  count_of_100_to_proportion = function(v, ctx = NULL) {
    as.numeric(v) * 100
  },
  sister_share_to_similarity = function(v, ctx = NULL) {
    round(100 * (1 - as.numeric(v)) / 0.8)
  },
  stage_to_index = function(v, ctx = NULL) {
    ctx$collections$stages[as.integer(v)]
  }
)

#' @rdname read_params
#' @export
invertible_transform <- function(name) {
  name %in% names(sextant_inverses)
}

# Read a value at a JSON pointer from a simplified params list;
# numeric segments index into vectors, matrices or lists (0-based).
# @noRd
pointer_get <- function(x, pointer) {
  segs <- strsplit(sub("^/", "", pointer), "/", fixed = TRUE)[[1]]
  node <- x
  for (k in seq_along(segs)) {
    s <- segs[[k]]
    idx <- suppressWarnings(as.integer(s))
    if (is.null(node)) return(NULL)
    if (!is.na(idx)) {
      i <- idx + 1L
      if (is.matrix(node)) {
        # a matrix pointer is /row/col: consume the next segment too
        j <- suppressWarnings(as.integer(segs[[k + 1L]])) + 1L
        if (i > nrow(node) || j > ncol(node)) return(NULL)
        return(node[i, j])
      }
      if (i > length(node)) return(NULL)
      node <- if (is.list(node)) node[[i]] else node[i]
    } else {
      node <- node[[s]]
    }
  }
  node
}

#' @rdname read_params
#' @param spec A spec from [question_spec()].
#' @param params A parameter list from [read_params()].
#' @return `file_to_answers()`: a list with `answers` (repopulated,
#'   invertible ones only), `direct` (per instance: the raw parameter
#'   value where the transform is not invertible), `structural`
#'   answers derived from the file's shape, and the file's existing
#'   `annotations`, so provenance survives a load-edit-save cycle.
#' @export
file_to_answers <- function(spec, params) {
  structural <- list(mode = params$mode)
  eng <- params$engine
  if (identical(params$mode, "ma")) {
    structural$n_products <- length(eng$brands)
    structural$n_attributes <- length(eng$attributes)
    for (i in seq_along(eng$brands)) {
      structural[[paste0("product_name__", i - 1)]] <- eng$brands[i]
    }
    for (i in seq_along(eng$attributes)) {
      structural[[paste0("attribute_name__", i - 1)]] <- eng$attributes[i]
    }
    own <- eng$owned %||% c(TRUE, rep(FALSE, length(eng$brands) - 1))
    for (i in seq_along(own)) {
      structural[[paste0("line_owned__", i - 1)]] <- isTRUE(own[i])
    }
  } else {
    structural$n_stages <- length(eng$stages)
    for (i in seq_along(eng$stages)) {
      structural[[paste0("stage_name__", i - 1)]] <- eng$stages[i]
    }
  }
  structural$n_buckets <- length(params$investments$buckets)
  for (i in seq_along(params$investments$buckets)) {
    structural[[paste0("bucket_name__", i - 1)]] <-
      params$investments$buckets[i]
  }

  ctx <- list(collections = spec_collections(spec, structural))
  inst <- instantiate_questions(spec, structural)
  answers <- structural
  direct <- list()
  for (q in inst) {
    if (is.null(q$param_path)) next
    if (q$instance_id %in% names(structural)) next
    v <- pointer_get(params, q$param_path)
    if (is.null(v)) next
    if (identical(q$type, "anchor_set") ||
        !invertible_transform(q$transform)) {
      direct[[q$instance_id]] <- v
    } else {
      answers[[q$instance_id]] <- sextant_inverses[[q$transform]](v, ctx)
    }
  }
  list(answers = answers, direct = direct, structural = structural,
       annotations = params$annotations %||% list())
}
