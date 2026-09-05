# sextant (development version)

* Question spec (`inst/elicitation/questions.yml`): behavioural
  elicitation covering every schema-required Gyro parameter in both
  modes, with repetition, dependencies, benchmarks, and tested
  transforms; `build_params()` assembles answers into a valid file and
  `write_params()` refuses to write an invalid one.
* Schema contract: `get_schema()` prefers an installed Gyro and falls
  back to a snapshot pinned byte-identical at Gyro v0.5.0, with a
  drift test; Gyro sits in Suggests only.
* `fit_adbudg()`: deterministic ADBUDG calibration from four anchors,
  implemented and tested but unwired - the v0.5.0 schema declares no
  response-curve family to write to.
* From the initial build, retained: scenario IO with Gyro serialisation parity (`read_scenario()`,
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
