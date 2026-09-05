#' Provenance report for a scenario
#'
#' A markdown document to sit beside the ROI table in a budget meeting:
#' who vouches for which number. Leads with coverage, then the
#' high-stakes judgements (value, source, location, rationale), then
#' everything else annotated, then the naked list - parameters no one
#' has yet vouched for. Pairs with the decision log in Gyro's manager
#' worksheet.
#'
#' @param scenario A scenario list from [read_scenario()].
#' @return A single character string of markdown.
#' @export
provenance_report <- function(scenario) {
  t <- annotation_targets(scenario)
  cov <- annotation_coverage(scenario)
  meta <- scenario$metadata %||% list()

  esc <- function(x) gsub("\\|", "\\\\|", x)
  row_md <- function(r) {
    sprintf("| %s | %s | %s | %s | %s |",
            esc(r$label), esc(r$value),
            r$source,
            if (is.na(r$location)) "" else esc(r$location),
            if (is.na(r$rationale)) "" else esc(r$rationale))
  }
  section_table <- function(rows) {
    if (nrow(rows) == 0) return("*(none)*")
    paste(c("| Parameter | Value | Source | Location | Rationale |",
            "|---|---|---|---|---|",
            vapply(seq_len(nrow(rows)),
                   function(i) row_md(rows[i, ]), "")),
          collapse = "\n")
  }

  annotated <- !is.na(t$source)
  hs_rows <- t[t$high_stakes & annotated, , drop = FALSE]
  other_rows <- t[!t$high_stakes & annotated, , drop = FALSE]

  flags <- character()
  if (length(cov$missing_high_stakes)) {
    flags <- c(flags, sprintf(
      "**%d high-stakes judgement(s) have no provenance at all:** %s.",
      length(cov$missing_high_stakes),
      paste(sprintf("`%s`", cov$missing_high_stakes), collapse = ", ")))
  }
  if (length(cov$assumed_high_stakes)) {
    flags <- c(flags, sprintf(
      "**%d high-stakes judgement(s) rest on assumption:** %s. The
conclusions lean on these; treat them as the first candidates for
evidence.",
      length(cov$assumed_high_stakes),
      paste(sprintf("`%s`", cov$assumed_high_stakes), collapse = ", ")))
  }
  if (!length(flags)) {
    flags <- "No high-stakes judgement is unaccounted for."
  }

  name <- meta$scenario_name %||% ""
  paste0(
    "# Parameter provenance: ",
    if (nzchar(name)) name else "(unnamed scenario)", "\n\n",
    if (nzchar(meta$author %||% "")) {
      paste0("Scenario author: ", meta$author, ". ")
    } else "",
    "Report generated ", format(Sys.Date()), " by sextant.\n\n",
    sprintf("**Coverage: %d of %d parameters annotated (%.0f%%)** \u2014 %s stated, %s benchmarked, %s assumed.\n\n",
            cov$n_annotated, cov$n_targets, 100 * cov$pct,
            cov$by_source[["stated"]], cov$by_source[["benchmark"]],
            cov$by_source[["assumed"]]),
    paste(flags, collapse = "\n\n"), "\n\n",
    "## High-stakes judgements\n\n",
    "The inputs Gyro's conclusions are most sensitive to.\n\n",
    section_table(hs_rows), "\n\n",
    "## Other annotated parameters\n\n",
    section_table(other_rows), "\n\n",
    "## Not yet annotated\n\n",
    if (length(cov$missing)) {
      paste(sprintf("- `%s`", cov$missing), collapse = "\n")
    } else "*(none \u2014 full coverage)*",
    "\n\n---\n*Gyro scenario schema v2; annotations are engine-ignored",
    " provenance. sextant computes no equity.*\n"
  )
}
