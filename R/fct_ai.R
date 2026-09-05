#' The AI layer: prefill and per-question assist
#'
#' Hard rule, enforced by construction: the model answers the
#' questionnaire, never the schema. Prompts are assembled from the
#' question spec (wording, types, bounds, the instantiation convention)
#' and answers come back keyed by question instance id; they then pass
#' through exactly the same bounds checks and transforms as a human
#' answer, and anchor-set questions are answered as anchors with the
#' curve fitted by the same deterministic R code. Out-of-bounds answers
#' are dropped, not clamped. Nothing the model produces is written
#' anywhere until a human reviews and saves.
#'
#' The API key is read from the `ANTHROPIC_API_KEY` environment variable
#' at call time only - never stored, never logged. The model is
#' configurable via `options(sextant.model = ...)`.
#'
#' @name ai
NULL

#' Is the AI layer available?
#' @return `TRUE` when an API key is present in the environment.
#' @rdname ai
#' @export
ai_available <- function() {
  nzchar(Sys.getenv("ANTHROPIC_API_KEY"))
}

# @noRd
ai_model <- function() {
  getOption("sextant.model", "claude-sonnet-4-6")
}

# The single network touchpoint; everything above it is mockable.
# Returns the concatenated text content of the model's reply.
# @noRd
anthropic_chat <- function(user, system = NULL, max_tokens = 4000) {
  key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(key)) {
    stop("No ANTHROPIC_API_KEY in the environment.", call. = FALSE)
  }
  body <- list(
    model = ai_model(),
    max_tokens = max_tokens,
    messages = list(list(role = "user", content = user))
  )
  if (!is.null(system)) body$system <- system
  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(`x-api-key` = key,
                       `anthropic-version` = "2023-06-01",
                       `content-type` = "application/json",
                       .redact = "x-api-key") |>
    httr2::req_body_json(body) |>
    httr2::req_error(body = function(resp) {
      # surface the API's message; the key never appears in it
      tryCatch(httr2::resp_body_json(resp)$error$message,
               error = function(e) NULL)
    }) |>
    httr2::req_perform()
  content <- httr2::resp_body_json(resp)$content
  paste(vapply(Filter(function(x) identical(x$type, "text"), content),
               `[[`, "", "text"),
        collapse = "")
}

#' Extract a JSON object from model output, defensively
#'
#' Strips code fences, tolerates prose around the object, and recovers
#' what it can from truncated output by trimming back to the last
#' complete entry. Returns a list (possibly empty) plus parse notes -
#' it never errors on bad input.
#'
#' @param text Raw model output.
#' @return `list(data = <named list>, notes = <character>)`.
#' @rdname ai
#' @export
extract_json <- function(text) {
  notes <- character()
  if (is.null(text) || !nzchar(trimws(text %||% ""))) {
    return(list(data = list(), notes = "empty model output"))
  }
  t <- gsub("```json|```", "", text)
  first <- regexpr("{", t, fixed = TRUE)
  last <- max(gregexpr("}", t, fixed = TRUE)[[1]])
  if (first < 0 || last < first) {
    return(list(data = list(), notes = "no JSON object found"))
  }
  candidate <- substr(t, first, last)
  try_parse <- function(x) {
    tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE),
             error = function(e) NULL)
  }
  parsed <- try_parse(candidate)
  if (is.null(parsed)) {
    # the last brace in truncated output often closes an inner entry,
    # leaving the outer object open: try closing it before trimming
    for (extra in c("}", "}}")) {
      parsed <- try_parse(paste0(candidate, extra))
      if (!is.null(parsed)) {
        notes <- c(notes,
                   "output looked truncated; closed the open object")
        break
      }
    }
  }
  if (is.null(parsed)) {
    # trim back to the last complete top-level entry
    cuts <- rev(gregexpr("},", candidate, fixed = TRUE)[[1]])
    for (cut in cuts) {
      if (cut < 2) next
      parsed <- try_parse(paste0(substr(candidate, 1, cut), "}"))
      if (!is.null(parsed)) {
        notes <- c(notes, "output looked truncated; recovered the
complete entries")
        break
      }
    }
  }
  if (is.null(parsed)) {
    return(list(data = list(), notes = "unparseable model output"))
  }
  if (!is.list(parsed)) parsed <- list()
  list(data = parsed, notes = notes)
}

# Serialise the questionnaire for the prompt: base questions with
# wording, type, bounds, options, repetition and the instance-id
# convention the answers must use.
# @noRd
question_payload <- function(spec) {
  qs <- unlist(lapply(spec$sections, `[[`, "questions"),
               recursive = FALSE)
  lines <- vapply(qs, function(q) {
    parts <- c(
      paste0("id: ", q$id),
      paste0("asks: ", q$wording),
      paste0("type: ", q$type),
      if (!is.null(q$bounds)) {
        paste0("bounds: ", q$bounds$min %||% "-", " to ",
               q$bounds$max %||% "-")
      },
      if (is.list(q$options)) {
        paste0("choices: ",
               paste(vapply(q$options, function(o)
                 as.character(o$value), ""), collapse = " | "))
      },
      if (!is.null(q$repetition)) {
        paste0("repeats: ", paste(unlist(q$repetition), collapse = " x "))
      },
      if (!is.null(q$depends_on)) {
        paste0("only when ", q$depends_on$question, " = ",
               q$depends_on$equals)
      }
    )
    paste(parts, collapse = "; ")
  }, "")
  paste(lines, collapse = "\n")
}

# @noRd
prefill_system_prompt <- function() {
  paste(
    "You fill in a structured elicitation questionnaire about a company",
    "and its market from a written brief. Rules, all hard:",
    "(1) You answer the QUESTIONNAIRE, never model parameters directly.",
    "Each answer is in the question's own units and within its bounds.",
    "(2) An unanswered question is correct behaviour; an invented answer",
    "is not. If the brief and general industry knowledge cannot ground",
    "an answer, omit that id entirely.",
    "(3) source is 'benchmark' ONLY when the answer rests on a stated,",
    "nameable industry norm - name it in the rationale. Otherwise",
    "source is 'assumed'.",
    "(4) Repeating questions use ids like rating__1__0: the base id,",
    "then 0-based indices in the repetition order given. Structural",
    "counts and names (mode, n_products, product_name__i, ...) come",
    "first and everything else must be consistent with them.",
    "(5) For anchor_set questions, answer the four anchors as",
    "id__zero, id__current, id__increased, id__saturation, in the",
    "question's own units; never emit curve parameters.",
    "(6) Reply with ONE strict JSON object and nothing else - no fences,",
    "no prose. Each key is a question instance id; each value is",
    '{"answer": ..., "source": "benchmark"|"assumed", "rationale": "..."}.'
  )
}

#' Prefill the questionnaire from a written brief
#'
#' Assembles a prompt from the question spec and the brief, requests
#' strict JSON, parses defensively, and validates every returned answer
#' against its question's bounds - out-of-bounds or unrecognised
#' answers are dropped with a note, never clamped or repaired.
#'
#' @param brief Free-text company/market brief.
#' @param spec A spec from [question_spec()].
#' @return `list(answers, sources, rationales, notes)` ready to seed
#'   the survey as AI-proposed; empty when nothing grounded.
#' @rdname ai
#' @export
prefill_from_brief <- function(brief, spec = question_spec()) {
  stopifnot(is.character(brief), nzchar(trimws(brief)))
  raw <- anthropic_chat(
    user = paste0("THE BRIEF:\n", brief, "\n\nTHE QUESTIONNAIRE:\n",
                  question_payload(spec)),
    system = prefill_system_prompt()
  )
  parsed <- extract_json(raw)
  validate_prefill(spec, parsed$data, notes = parsed$notes)
}

# Shared validation for prefill and per-question assist output: keep
# only recognised instance ids whose answers pass their question's
# bounds; coerce unknown sources to 'assumed'; collect notes.
# @noRd
validate_prefill <- function(spec, data, notes = character()) {
  answers <- list(); sources <- list(); rationales <- list()
  if (length(data) == 0) {
    return(list(answers = answers, sources = sources,
                rationales = rationales, notes = notes))
  }
  # structural answers first, so instantiation matches the model's own
  structure_ids <- c("mode", "n_products", "n_attributes", "n_stages",
                     "n_buckets")
  structural <- list()
  for (id in names(data)) {
    base <- sub("__.*$", "", id)
    if (base %in% structure_ids ||
        base %in% c("product_name", "attribute_name", "stage_name",
                    "bucket_name")) {
      structural[[id]] <- data[[id]]$answer %||% data[[id]]
    }
  }
  inst <- instantiate_questions(spec, structural)
  by_id <- stats::setNames(inst, vapply(inst, `[[`, "", "instance_id"))
  anchor_parent <- function(id) sub("__(zero|current|increased|saturation)$",
                                    "", id)

  for (id in names(data)) {
    entry <- data[[id]]
    if (!is.list(entry)) entry <- list(answer = entry)
    q <- by_id[[id]] %||% by_id[[anchor_parent(id)]]
    if (is.null(q)) {
      notes <- c(notes, paste0(id, ": not a question here; dropped"))
      next
    }
    a <- entry$answer
    if (is.null(a)) next
    tq <- q
    if (identical(q$type, "anchor_set")) tq$type <- "count_of_100"
    ok <- tryCatch({ apply_transform(a, tq,
                                     list(collections =
                                            spec_collections(spec,
                                                             structural)))
                     TRUE },
                   error = function(e) {
                     notes <<- c(notes, paste0(id, ": ",
                                               conditionMessage(e),
                                               " Dropped."))
                     FALSE
                   })
    if (!ok) next
    answers[[id]] <- a
    s <- entry$source %||% "assumed"
    if (!s %in% c("benchmark", "assumed")) {
      notes <- c(notes, paste0(id, ": source '", s,
                               "' is not allowed; recorded as assumed"))
      s <- "assumed"
    }
    sources[[id]] <- s
    why <- trimws(entry$rationale %||% "")
    if (nzchar(why)) rationales[[paste0(id, "__why")]] <- why
  }
  list(answers = answers, sources = sources, rationales = rationales,
       notes = notes)
}

#' Suggest an answer for one question
#'
#' Sends the brief plus that single question; returns a suggested
#' answer with rationale and a typical range, bounds-validated the same
#' way. Nothing is inserted anywhere by this function.
#'
#' @param question An instantiated question.
#' @rdname ai
#' @export
ai_suggest_question <- function(brief, question, spec = question_spec()) {
  raw <- anthropic_chat(
    user = paste0(
      "THE BRIEF:\n", brief, "\n\nONE QUESTION:\n",
      "asks: ", question$wording, "\n",
      "type: ", question$type, "; bounds: ",
      question$bounds$min %||% "-", " to ",
      question$bounds$max %||% "-", "\n\n",
      "Reply with ONE strict JSON object only: ",
      '{"answer": ..., "source": "benchmark"|"assumed", ',
      '"rationale": "...", "typical_range": "..."}. ',
      "If you cannot ground an answer, reply {}."),
    system = prefill_system_prompt(),
    max_tokens = 600
  )
  parsed <- extract_json(raw)
  d <- parsed$data
  if (length(d) == 0 || is.null(d$answer)) return(NULL)
  # bounds-validate directly against the question we already hold -
  # unlike prefill, no instantiation context is needed or available
  tq <- question
  if (identical(question$type, "anchor_set")) tq$type <- "count_of_100"
  ok <- tryCatch({ apply_transform(d$answer, tq, NULL); TRUE },
                 error = function(e) FALSE)
  if (!ok) return(NULL)                  # out of bounds: no suggestion
  s <- d$source %||% "assumed"
  if (!s %in% c("benchmark", "assumed")) s <- "assumed"
  list(answer = d$answer,
       source = s,
       rationale = d$rationale %||% "",
       typical_range = d$typical_range %||% "",
       notes = parsed$notes)
}
