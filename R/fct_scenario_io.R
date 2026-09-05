#' Read and write Gyro scenario files
#'
#' sextant's half of the file contract with Gyro. [read_scenario()] parses
#' a scenario JSON the way Gyro does (vectors and matrices simplified);
#' [write_scenario()] serialises with settings identical to Gyro's writer,
#' so a file that round-trips through sextant differs only where you
#' changed it. When the `jsonvalidate` package is installed, reads are
#' checked against [get_schema()] first
#' (`schema_check = TRUE`).
#'
#' sextant never computes on these files - it reads them, annotates them,
#' and writes them back for Gyro to price.
#'
#' @param path File path of a Gyro scenario JSON.
#' @param schema_check Validate against [get_schema()] on read
#'   (default `TRUE`).
#' @return `read_scenario()`: the scenario as a list. `write_scenario()`:
#'   `path`, invisibly.
#' @export
read_scenario <- function(path, schema_check = TRUE) {
  if (!file.exists(path)) {
    stop("No file at '", path, "'.", call. = FALSE)
  }
  if (isTRUE(schema_check)) {
    check_scenario_schema(path)
  }
  x <- jsonlite::fromJSON(path, simplifyVector = TRUE,
                          simplifyMatrix = TRUE)
  problems <- character()
  if (!identical(x$mode, "ma") && !identical(x$mode, "funnel")) {
    problems <- c(problems, "mode must be 'ma' or 'funnel'")
  }
  for (f in c("settings", "engine", "investments")) {
    if (!is.list(x[[f]])) problems <- c(problems, paste("missing", f))
  }
  sv <- suppressWarnings(as.integer(x$schema_version %||% NA))
  if (is.na(sv) || sv > 2L) {
    problems <- c(problems,
                  "schema_version missing or newer than sextant understands (2)")
  }
  if (length(problems)) {
    stop("Not a readable Gyro scenario:\n- ",
         paste(problems, collapse = "\n- "), call. = FALSE)
  }
  x
}

#' @rdname read_scenario
#' @param scenario A scenario list.
#' @export
write_scenario <- function(scenario, path) {
  jsonlite::write_json(
    scenario, path,
    pretty = TRUE, auto_unbox = TRUE, digits = NA,
    matrix = "rowmajor", null = "null", na = "null"
  )
  invisible(path)
}

# Validate a file against the bundled Gyro schema when jsonvalidate is
# available; no-op otherwise. Errors carry the validator's messages.
# @noRd
check_scenario_schema <- function(path) {
  schema <- get_schema()
  if (!nzchar(schema)) return(invisible(TRUE))
  v <- sextant_validator(schema)
  json <- paste(readLines(path, warn = FALSE), collapse = "\n")
  ok <- v(json, verbose = TRUE, greedy = TRUE)
  if (!isTRUE(ok)) {
    err <- attr(ok, "errors")
    where <- err$instancePath %||% err$dataPath %||% ""
    where[!nzchar(where)] <- "(root)"
    stop("Scenario failed schema validation:\n",
         paste0("- ", where, ": ", err$message, collapse = "\n"),
         call. = FALSE)
  }
  invisible(TRUE)
}
