#' Provenance annotations on scenario parameters
#'
#' Gyro scenario files carry an optional, engine-ignored `annotations`
#' block: per parameter path, a `source` (one of
#' `"stated"`, `"benchmark"`, `"assumed"`), a free-text `rationale`, and
#' an optional `location`. These functions are sextant's core: list the
#' parameters worth annotating, attach and remove annotations, and
#' measure coverage with particular attention to the high-stakes
#' judgements - the ones a budget conclusion leans on hardest.
#'
#' Paths use `/`-separated segments with 0-based indices for per-line and
#' per-bucket entries (e.g. `engine/brands/0`,
#' `investments/uplift/1`), matching the convention Gyro's annotated
#' demo established.
#'
#' @name annotate
NULL

#' Annotation source levels
#' @rdname annotate
#' @export
SEXTANT_SOURCES <- c("stated", "benchmark", "assumed")

#' List the annotatable parameters of a scenario
#'
#' Returns one row per parameter judgement worth documenting: its path,
#' a human label, a compact preview of the current value, whether it is
#' high-stakes (price sensitivity, within-brand similarity, funnel
#' transitions, and every bucket's uplift and price change - the inputs
#' Gyro's conclusions are most sensitive to), and its current annotation
#' if any.
#'
#' @param scenario A scenario list from [read_scenario()].
#' @return A data frame with columns `path`, `label`, `value`,
#'   `high_stakes`, `source`, `rationale`, `location`.
#' @export
annotation_targets <- function(scenario) {
  s <- scenario$settings
  rows <- list()
  add <- function(path, label, value, hs = FALSE) {
    rows[[length(rows) + 1]] <<- data.frame(
      path = path, label = label, value = preview(value),
      high_stakes = hs, stringsAsFactors = FALSE
    )
  }

  add("settings/market_potential", "Market potential", s$market_potential)
  add("settings/periods", "Planning horizon (periods)", s$periods)
  add("settings/frequency", "Purchase frequency", s$frequency)
  add("settings/quantity", "Quantity per purchase", s$quantity)
  add("settings/discount_rate", "Discount rate", s$discount_rate)

  e <- scenario$engine
  if (identical(scenario$mode, "ma")) {
    brands <- e$brands
    for (i in seq_along(brands)) {
      add(paste0("engine/brands/", i - 1),
          paste0("Line: ", brands[i]), brands[i])
    }
    add("engine/ratings", "Attribute ratings", e$ratings)
    add("engine/importance", "Attribute importance", e$importance)
    add("engine/price", "Prices", e$price)
    add("engine/unit_costs", "Unit costs", e$unit_costs)
    add("engine/intercepts", "Brand intercepts", e$intercepts)
    add("engine/loyalty", "Loyalty (inertia)", e$loyalty)
    add("engine/price_sensitivity", "Price sensitivity",
        e$price_sensitivity, hs = TRUE)
    if (!is.null(e$nest_similarity)) {
      add("engine/nest_similarity", "Within-brand similarity",
          e$nest_similarity, hs = TRUE)
    }
    if (!is.null(e$initial_shares)) {
      add("engine/initial_shares", "Starting shares", e$initial_shares)
    }
  } else {
    add("engine/transition", "Stage transition matrix", e$transition,
        hs = TRUE)
    add("engine/initial_counts", "Starting stage counts",
        e$initial_counts)
    add("engine/price", "Price", e$price)
    add("engine/unit_cost", "Unit cost", e$unit_cost)
  }

  inv <- scenario$investments
  buckets <- inv$buckets
  for (b in seq_along(buckets)) {
    add(paste0("investments/uplift/", b - 1),
        paste0("Uplift judgement: ", buckets[b]),
        as.matrix(inv$uplift)[, b], hs = TRUE)
    pc <- inv$price_change
    if (!is.null(pc) && length(pc) >= b && abs(pc[b]) > 0) {
      add(paste0("investments/price_change/", b - 1),
          paste0("Price change: ", buckets[b]), pc[b], hs = TRUE)
    }
    add(paste0("investments/spend_per_period/", b - 1),
        paste0("Spend: ", buckets[b]), inv$spend_per_period[b])
  }

  out <- do.call(rbind, rows)
  ann <- scenario$annotations %||% list()
  pick <- function(p, f) {
    a <- ann[[p]]
    if (is.null(a) || is.null(a[[f]])) NA_character_ else a[[f]]
  }
  out$source <- vapply(out$path, pick, "", f = "source")
  out$rationale <- vapply(out$path, pick, "", f = "rationale")
  out$location <- vapply(out$path, pick, "", f = "location")
  rownames(out) <- NULL
  out
}

# Compact one-line preview of a parameter value.
# @noRd
preview <- function(v) {
  if (is.matrix(v)) {
    return(sprintf("%dx%d matrix", nrow(v), ncol(v)))
  }
  if (is.numeric(v) && length(v) > 4) {
    return(paste0(paste(signif(v[1:4], 4), collapse = ", "), ", \u2026"))
  }
  if (is.numeric(v)) {
    return(paste(signif(v, 6), collapse = ", "))
  }
  paste(as.character(v), collapse = ", ")
}

#' Attach, read or remove an annotation
#'
#' @param scenario A scenario list.
#' @param path A parameter path from [annotation_targets()].
#' @param source One of [SEXTANT_SOURCES]: `"stated"` (someone with
#'   standing said so), `"benchmark"` (an external or measured
#'   reference), `"assumed"` (a working guess awaiting evidence).
#' @param rationale Free text: why this value, and on whose authority.
#' @param location Optional free text locating the judgement (a market,
#'   a segment, a geography).
#' @return `set_annotation()` and `drop_annotation()` return the
#'   modified scenario; `get_annotation()` returns the annotation list
#'   or `NULL`.
#' @rdname annotate
#' @export
set_annotation <- function(scenario, path, source,
                           rationale = "", location = NULL) {
  source <- match.arg(source, SEXTANT_SOURCES)
  known <- annotation_targets(scenario)$path
  if (!path %in% known) {
    stop("'", path, "' is not an annotatable parameter of this scenario. ",
         "See annotation_targets().", call. = FALSE)
  }
  entry <- list(source = source)
  rationale <- trimws(rationale %||% "")
  if (nzchar(rationale)) entry$rationale <- rationale
  if (!is.null(location) && nzchar(trimws(location))) {
    entry$location <- trimws(location)
  }
  ann <- scenario$annotations %||% list()
  ann[[path]] <- entry
  scenario$annotations <- ann
  scenario
}

#' @rdname annotate
#' @export
get_annotation <- function(scenario, path) {
  (scenario$annotations %||% list())[[path]]
}

#' @rdname annotate
#' @export
drop_annotation <- function(scenario, path) {
  ann <- scenario$annotations %||% list()
  ann[[path]] <- NULL
  if (length(ann) == 0) scenario$annotations <- NULL
  else scenario$annotations <- ann
  scenario
}

#' Annotation coverage of a scenario
#'
#' How defensible is this parameter file? Counts annotated targets,
#' splits them by source, and singles out the two red flags for a
#' reviewer: high-stakes judgements with no annotation at all, and
#' high-stakes judgements marked `assumed` - the load-bearing guesses.
#'
#' @param scenario A scenario list.
#' @return A list: `n_targets`, `n_annotated`, `pct`, `by_source`
#'   (named counts), `missing` (paths), `missing_high_stakes` (paths),
#'   `assumed_high_stakes` (paths).
#' @export
annotation_coverage <- function(scenario) {
  t <- annotation_targets(scenario)
  annotated <- !is.na(t$source)
  hs <- t$high_stakes
  list(
    n_targets = nrow(t),
    n_annotated = sum(annotated),
    pct = if (nrow(t)) sum(annotated) / nrow(t) else 0,
    by_source = table(factor(t$source[annotated],
                             levels = SEXTANT_SOURCES)),
    missing = t$path[!annotated],
    missing_high_stakes = t$path[hs & !annotated],
    assumed_high_stakes = t$path[hs & annotated &
                                   t$source == "assumed"]
  )
}
