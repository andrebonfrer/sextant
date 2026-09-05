# sextant

Parameter provenance for [Gyro](https://github.com/andrebonfrer/Gyro)
scenarios. Gyro prices managerial judgements in customer-equity terms;
**sextant records where those judgements come from** — so the budget
conversation can distinguish a number someone will vouch for from a
working guess. A gyroscope keeps you stable; a sextant tells you where
you are.

sextant computes no equity and never will. Its whole job is the
engine-ignored `annotations` block that Gyro's scenario schema (v2)
accepts: per parameter path, a `source`, a `rationale`, and an optional
`location`.

## The loop

1. Build and decide a scenario in Gyro; save it from the Integrations
   tab.
2. Open it in sextant. Every annotatable judgement appears in one table,
   with the **high-stakes** ones flagged — price sensitivity,
   within-brand similarity, funnel transitions, and every bucket's
   uplift and price change: the inputs Gyro's conclusions lean on
   hardest.
3. Attach provenance to each: **stated** (someone with standing said
   so), **benchmark** (measured or external reference), or **assumed**
   (a working guess awaiting evidence), with rationale and location.
4. Read the Report tab: coverage, and the two red lists — high-stakes
   judgements with no provenance, and high-stakes judgements resting on
   assumption.
5. Export the annotated scenario (it loads straight back into Gyro,
   byte-compatible) and the provenance report (markdown, for the
   meeting).

## Functions

| Area | Functions |
|---|---|
| Scenario IO | `read_scenario()`, `write_scenario()` — Gyro-parity serialisation, schema-gated reads when `jsonvalidate` is installed |
| Annotation | `annotation_targets()`, `set_annotation()`, `get_annotation()`, `drop_annotation()`, `SEXTANT_SOURCES` |
| Review | `annotation_coverage()`, `provenance_report()` |
| App | `run_app()` |

## Install & run

```r
remotes::install_github("andrebonfrer/sextant")
sextant::run_app()
```

Load the bundled demo (Gyro's coffee-portfolio scenario, annotations
stripped) and annotate it; or bring your own scenario file.

## The contract

`inst/schema/company-params.schema.json` is a verbatim copy of the
schema published by Gyro 0.5.0 — Gyro's repository is the source of
truth; sextant's copy is version-matched to scenario `schema_version`
2. The serialisation settings of `write_scenario()` are asserted in
tests to be byte-identical to Gyro's writer, so a round-trip through
sextant changes a file only where you changed it.
