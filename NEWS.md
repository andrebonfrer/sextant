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
