# sextant (development version)

Initial development. Provenance companion to Gyro (from the initial
build):

* Scenario IO with Gyro serialisation parity (`read_scenario()`,
  `write_scenario()`); reads are validated against the bundled copy of
  Gyro's schema when `jsonvalidate` is available.
* Annotation engine: `annotation_targets()` with high-stakes flags,
  `set_annotation()` / `get_annotation()` / `drop_annotation()`,
  `annotation_coverage()` with the two reviewer red lists (unannotated
  high-stakes; high-stakes resting on assumption).
* `provenance_report()`: the markdown handout for the budget meeting.
* Shiny app (`run_app()`): Load / Annotate / Report, with the annotated
  scenario and report as exports; ships Gyro's coffee-portfolio demo
  unannotated as the exercise.
