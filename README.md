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
| AI layer | `prefill_from_brief()`, `ai_suggest_question()` — propose-review-commit: the model answers the questionnaire under the same bounds and transforms, proposals arrive flagged with rationales, and a human edit or save is always the last word. Requires `ANTHROPIC_API_KEY`; fully optional |

## Install and run

```r
install.packages("remotes")
remotes::install_github("andrebonfrer/sextant")
sextant::run_app()
```

All hard dependencies are CRAN packages. On Linux, `jsonvalidate`
needs the V8 engine (`apt install libv8-dev` or the `r-cran-v8`
binary) before install; macOS and Windows binaries need nothing extra.

Two optional extras change what you see:

* **Gyro installed** (private repository - collaborators only):
  `get_schema()` switches to Gyro's live schema, the drift test arms,
  and the implied panel gains its "Run full model" button. Without it,
  the bundled schema snapshot and the closed-form panel do everything
  else.
* **`ANTHROPIC_API_KEY` set** (get one from the Anthropic Console;
  put `ANTHROPIC_API_KEY=...` in `~/.Renviron` and restart R): the AI
  prefill panel and per-question suggest appear. Without it, a short
  notice and a fully functional survey.

## Hosting on shinyapps.io

From a clone of this repository:

```r
install.packages("rsconnect")
# one-time: account token from the shinyapps.io dashboard
rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
rsconnect::deployApp(appName = "sextant", appTitle = "sextant")
```

The repository is deploy-ready as is: `.renvignore` keeps the
dependency scan away from the Gyro-touching file (a private package
the server could never install), so the hosted app runs with the
bundled schema and the closed-form panel - the designed degradation.

Think before shipping an API key to a public app: on the free tier
every app is public, and a key deployed with
`deployApp(envVars = "ANTHROPIC_API_KEY")` means anyone with the URL
can spend it. Hosting without the key (survey fully functional, AI
off) is the sensible default; add the key only on a plan with
app-level authentication.

## The contract

`inst/schema/company-params.schema.json` is a byte-identical copy of
the schema published by Gyro v0.5.0 (see `inst/schema/PROVENANCE.md`);
Gyro's repository is the source of truth, `get_schema()` prefers an
installed Gyro's live copy, and a drift test fails if the two ever
differ.
