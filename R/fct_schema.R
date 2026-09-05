#' The parameter-file schema, and validated writing
#'
#' sextant's only integration with Gyro is the company-parameter JSON
#' defined by Gyro's `inst/schema/company-params.schema.json`.
#' [get_schema()] returns the schema to validate against: the installed
#' Gyro package's live copy when Gyro is present, otherwise the snapshot
#' bundled with sextant (pinned byte-identical at Gyro v0.5.0; see
#' `inst/schema/PROVENANCE.md`). [write_params()] validates a parameter
#' list against that schema and refuses to write an invalid file,
#' surfacing the validator's messages.
#'
#' @return `get_schema()`: the path to the schema file in use.
#' @export
get_schema <- function() {
  if (requireNamespace("Gyro", quietly = TRUE)) {
    live <- system.file("schema", "company-params.schema.json",
                        package = "Gyro")
    if (nzchar(live)) return(live)
  }
  app_sys("schema", "company-params.schema.json")
}

#' @rdname get_schema
#' @param params A parameter list (a scenario, in Gyro's terms).
#' @param path File path to write to.
#' @return `write_params()`: `path`, invisibly; errors without writing
#'   if the parameters do not validate.
#' @export
write_params <- function(params, path) {
  json <- jsonlite::toJSON(params, pretty = TRUE, auto_unbox = TRUE,
                           digits = NA, matrix = "rowmajor",
                           null = "null", na = "null")
  v <- sextant_validator(get_schema())
  ok <- v(json, verbose = TRUE, greedy = TRUE)
  if (!isTRUE(ok)) {
    err <- attr(ok, "errors")
    where <- err$instancePath %||% err$dataPath %||% ""
    where[!nzchar(where)] <- "(root)"
    stop("Parameters failed schema validation; nothing written:\n",
         paste0("- ", where, ": ", err$message, collapse = "\n"),
         call. = FALSE)
  }
  writeLines(json, path)
  invisible(path)
}

# One compiled validator per schema path per session.
# @noRd
sextant_validator <- local({
  cache <- list()
  function(schema_path) {
    key <- normalizePath(schema_path)
    if (is.null(cache[[key]])) {
      cache[[key]] <<- jsonvalidate::json_validator(schema_path,
                                                    engine = "ajv")
    }
    cache[[key]]
  }
})
