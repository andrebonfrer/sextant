# sextant

Elicitation for [Gyro](https://github.com/andrebonfrer/Gyro) parameter
files. Gyro prices managerial judgements in customer-equity terms;
sextant is the structured interview that produces those judgements as a
valid parameter file — behavioural questions in plain language, never
"enter the coefficient". A gyroscope keeps you stable; a sextant tells
you where you are.

Parameter files compatible with Gyro >= 0.5.0 (schema_version 2).

sextant's only integration with Gyro is that file contract; it depends
on no Gyro internals and computes no equity.

## What is here now

The interview and its engine:

| Area | Functions |
|---|---|
| Schema contract | `get_schema()` (installed Gyro preferred, pinned snapshot otherwise), `write_params()` (validates, refuses invalid files) |
| Question spec | `question_spec()`, `instantiate_questions()` — `inst/elicitation/questions.yml` holds 8 sections of behavioural questions covering every schema-required parameter in both modes |
| Answers to parameters | `apply_transform()`, `build_params()` — JSON-pointer assembly with the funnel residual rule (rows sum to one by construction) |
| Response calibration | `fit_adbudg()` — deterministic ADBUDG fit from four anchors; wired as a linear read at planned spend, full fit preserved in the file's annotations |
| Survey app | `run_app()` — the interview with live implied quantities (closed form), anchor curve plotting, load-and-edit with provenance that survives the round trip |

## The contract

`inst/schema/company-params.schema.json` is a byte-identical copy of
the schema published by Gyro v0.5.0 (see `inst/schema/PROVENANCE.md`);
Gyro's repository is the source of truth, `get_schema()` prefers an
installed Gyro's live copy, and a drift test fails if the two ever
differ.
