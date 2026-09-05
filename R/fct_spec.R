#' The question spec: load, validate, instantiate
#'
#' [question_spec()] loads and validates `inst/elicitation/questions.yml`.
#' [instantiate_questions()] turns its templates into concrete asked
#' questions, given the structural answers (counts and names): repetition
#' over products / attributes / buckets / stages (or their cross), 0-based
#' index placeholders, `depends_on` show/hide, `skip_last` on the stage
#' dimension, and dynamic choice options (`from_stages`,
#' `from_owned_products`). `per_segment` is a legal repetition value in
#' the format but unused: the Gyro v0.5.0 schema has no segments.
#'
#' @param path Path to a questions YAML (default: the bundled spec).
#' @return `question_spec()`: the parsed spec list.
#' @export
question_spec <- function(path = app_sys("elicitation", "questions.yml")) {
  spec <- yaml::read_yaml(path)
  problems <- character()
  note <- function(...) problems <<- c(problems, paste0(...))

  qs <- unlist(lapply(spec$sections, function(s) {
    lapply(s$questions, function(q) { q$.section_id <- s$id; q })
  }), recursive = FALSE)
  ids <- vapply(qs, `[[`, "", "id")
  if (anyDuplicated(ids)) {
    note("duplicate question ids: ",
         paste(unique(ids[duplicated(ids)]), collapse = ", "))
  }
  types <- c("number", "percent", "currency", "count_of_100", "choice",
             "anchor_set", "text")
  reps <- c("per_product", "per_attribute", "per_bucket", "per_stage",
            "per_segment")
  for (q in qs) {
    if (!q$type %in% types) note(q$id, ": unknown type '", q$type, "'")
    if (!identical(q$section, q$.section_id)) {
      note(q$id, ": section field disagrees with its enclosing section")
    }
    if (is.null(q$wording) || !nzchar(q$wording)) note(q$id, ": no wording")
    if (is.null(q$help)) note(q$id, ": no help sentence")
    tr <- q$transform %||% "identity"
    if (!tr %in% names(sextant_transforms)) {
      note(q$id, ": unknown transform '", tr, "'")
    }
    for (r in as.character(q$repetition %||% character())) {
      if (!r %in% reps) note(q$id, ": unknown repetition '", r, "'")
    }
    if (identical(q$type, "anchor_set")) {
      for (f in c("family", "anchors", "writes")) {
        if (is.null(q[[f]])) note(q$id, ": anchor_set needs '", f, "'")
      }
      if (!identical(q$family, "adbudg")) {
        note(q$id, ": unknown response family '", q$family, "'")
      }
    }
  }
  if (length(problems)) {
    stop("Invalid question spec:\n- ",
         paste(problems, collapse = "\n- "), call. = FALSE)
  }
  spec
}

# Collections (labels) implied by structural answers. Names fall back to
# ordinals until the naming questions are answered.
# @noRd
spec_collections <- function(spec, answers) {
  out <- list()
  for (nm in names(spec$collections)) {
    cfg <- spec$collections[[nm]]
    n <- suppressWarnings(as.integer(answers[[cfg$count]] %||% 0))
    if (is.na(n) || n < 1) { out[[nm]] <- character(); next }
    labels <- vapply(seq_len(n) - 1L, function(i) {
      v <- answers[[paste0(cfg$name, "__", i)]]
      if (is.null(v) || !nzchar(trimws(as.character(v)))) {
        paste(tools::toTitleCase(sub("s$", "", nm)), i + 1)
      } else trimws(as.character(v))
    }, "")
    out[[nm]] <- labels
  }
  out
}

# @noRd
dep_satisfied <- function(dep, answers) {
  if (is.null(dep)) return(TRUE)
  v <- answers[[dep$question]]
  if (!is.null(dep$equals)) return(identical(v, dep$equals))
  if (!is.null(dep$one_of)) return(!is.null(v) && v %in% dep$one_of)
  TRUE
}

#' @rdname question_spec
#' @param spec A spec from [question_spec()].
#' @param answers Named list of answers so far (structural answers drive
#'   instantiation; instance answers use ids like `rating__0__1`).
#' @return `instantiate_questions()`: a data-frame-like list of concrete
#'   questions, each with `instance_id`, resolved `wording`,
#'   `param_path`, `type`, `transform`, `bounds`, `options`, and the
#'   originating `id`.
#' @export
instantiate_questions <- function(spec, answers) {
  coll <- spec_collections(spec, answers)
  dim_map <- c(per_product = "products", per_attribute = "attributes",
               per_bucket = "buckets", per_stage = "stages",
               per_segment = "segments")
  out <- list()

  for (s in spec$sections) for (q in s$questions) {
    if (!dep_satisfied(q$depends_on, answers)) next
    reps <- as.character(q$repetition %||% character())
    dims <- lapply(reps, function(r) {
      labels <- coll[[dim_map[[r]]]] %||% character()
      seq_along(labels) - 1L
    })
    if (length(reps) && any(vapply(dims, length, 1L) == 0)) next
    grid <- if (length(reps)) {
      as.matrix(do.call(expand.grid, dims))
    } else {
      matrix(integer(), nrow = 1, ncol = 0)
    }

    for (g in seq_len(nrow(grid))) {
      idx <- as.integer(grid[g, ])
      names(idx) <- reps
      if (isTRUE(q$skip_last) && "per_stage" %in% reps) {
        if (idx[["per_stage"]] == length(coll$stages) - 1L) next
      }
      ph <- list()
      for (r in reps) {
        base <- sub("^per_", "", r)          # product, attribute, ...
        lab <- coll[[dim_map[[r]]]][idx[[r]] + 1L]
        ph[[base]] <- lab
        ph[[paste0(base, "_index")]] <- idx[[r]]
        ph[[paste0(base, "_ordinal")]] <- idx[[r]] + 1L
        if (base == "stage") ph$stage_next_index <- idx[[r]] + 1L
      }
      fill <- function(x) {
        if (is.null(x)) return(NULL)
        for (k in names(ph)) {
          x <- gsub(paste0("{", k, "}"), as.character(ph[[k]]), x,
                    fixed = TRUE)
        }
        x
      }
      opts <- q$options
      if (identical(opts, "from_stages")) {
        opts <- lapply(coll$stages,
                       function(l) list(label = l, value = l))
      } else if (identical(opts, "from_owned_products")) {
        owned <- vapply(seq_along(coll$products) - 1L, function(i) {
          isTRUE(answers[[paste0("line_owned__", i)]])
        }, TRUE)
        opts <- lapply(coll$products[owned],
                       function(l) list(label = l, value = l))
      }
      out[[length(out) + 1L]] <- list(
        id = q$id,
        instance_id = paste(c(q$id, idx), collapse = "__"),
        section = q$section,
        wording = fill(q$wording),
        help = fill(q$help),
        param_path = fill(q$param_path),
        technical = fill(q$technical),
        type = q$type,
        transform = q$transform %||% "identity",
        bounds = q$bounds,
        options = opts,
        benchmark = q$benchmark
      )
    }
  }
  out
}
