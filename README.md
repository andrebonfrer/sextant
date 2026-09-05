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

