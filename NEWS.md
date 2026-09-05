# sextant (development version)

* Field-driven round (thanks Andre): sections scroll back to the top on
  navigation; every key judgement carries its technical parameter name
  alongside the plain wording (the no-jargon guardrail, revised by its
  author); the within-brand similarity question now states its purpose
  - the cannibalisation split; the implied panel gains legibility rows
  that translate coefficients into consequences (implied starting
  shares, share points per rating point and per dollar of price); and
  drafts - the browser autosaves as you type and offers to resume on
  return, and a draft file can be downloaded and later dropped on the
  loader, which restores it instead of validating it.

* AI layer (Prompt 3), propose-review-commit only: prefill_from_brief()
  answers the questionnaire - never the schema - from a free-text
  brief, with omission preferred over invention; answers pass the same
  bounds and transforms as a human's, out-of-bounds proposals are
  dropped with a note (never clamped), and anchor questions are
  answered as anchors with the curve fitted by the same deterministic
  R code. Proposals load into the survey visibly flagged with their
  rationales; any human edit flips the answer to stated; nothing is
  written without review and an explicit save. A per-question suggest
  control proposes one answer with rationale and typical range,
  inserted only on an explicit Use click. Key from ANTHROPIC_API_KEY
  only; without it the AI features hide behind a short notice and the
  survey works fully. All tests mock the API at its single seam.

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
