# sextant (development version)

* Survey UI (Prompt 2): section-by-section interview rendered from the
  question spec, with progress, repetition, dependencies, and
  benchmark defaults shown with their source notes; answers equal to
  the shown default record `benchmark`, anything typed records
  `stated`, and an optional "why?" captures the rationale. The anchor
  widget fits the ADBUDG curve live and plots the manager's own points.
* Implied-quantities panel, always live: closed-form lifetime values,
  the portfolio rough-up, and a payback scratchpad, each with a
  one-click jump back to its driving answer; a "Run full model" button
  appears when Gyro is installed and shows the engine's equity beside
  the closed forms.
* Load-and-edit: an existing valid file repopulates the survey where
  transforms invert; edits flip provenance to `stated`; the file's own
  annotations survive a load-edit-save cycle, and exactly the edited
  leaves gain new ones.

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
