#' Survey drafts: crash insurance and portability
#'
#' A draft is the survey's working state - answers, sources, rationales,
#' AI flags, and any provenance carried in from a loaded file - as one
#' JSON object. It is deliberately not a parameter file: it needs no
#' completeness and passes no schema gate, because half-finished is the
#' whole point. Two carriers share this format: the browser's own
#' storage (autosaved as you type, offered back on return) and a
#' downloadable draft file (portable across machines; the app's file
#' loader recognises it and restores instead of validating).
#'
#' @param answers,sources,rationales,ai,direct,prior_annotations The
#'   survey state as returned by the survey module.
#' @return `draft_payload()`: a list ready for JSON serialisation.
#' @export
draft_payload <- function(answers, sources = list(), rationales = list(),
                          ai = character(), direct = list(),
                          prior_annotations = list()) {
  list(
    sextant_draft = 1L,
    saved_at = format(Sys.time(), "%Y-%m-%d %H:%M"),
    answers = answers,
    sources = sources,
    rationales = rationales,
    ai = as.list(ai),
    direct = direct,
    prior_annotations = prior_annotations
  )
}

#' @rdname draft_payload
#' @param x A parsed JSON object.
#' @return `is_draft()`: `TRUE` when `x` is a sextant draft.
#' @export
is_draft <- function(x) {
  is.list(x) && !is.null(x$sextant_draft)
}

# Serialise / parse; parse returns NULL on anything unusable.
# @noRd
draft_to_json <- function(payload) {
  as.character(jsonlite::toJSON(payload, auto_unbox = TRUE,
                                digits = NA, null = "null"))
}

# @noRd
draft_from_json <- function(text) {
  x <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
                error = function(e) NULL)
  if (!is_draft(x)) return(NULL)
  x$ai <- as.character(unlist(x$ai %||% list()))
  x
}
