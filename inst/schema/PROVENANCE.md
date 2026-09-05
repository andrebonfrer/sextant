# Schema snapshot provenance

`company-params.schema.json` in this directory is a byte-identical copy
of the schema published by **Gyro v0.5.0**, extracted from that tag
(`git show v0.5.0:inst/schema/company-params.schema.json`). The file
itself declares `schema_version` up to **2**.

Gyro's repository is the source of truth. This snapshot exists so
sextant works without Gyro installed; when Gyro *is* installed,
`get_schema()` prefers Gyro's live copy, and a drift test fails if the
two ever differ. The snapshot is kept byte-identical deliberately
(provenance lives here rather than inside the JSON, which admits no
comments) so that equality with Gyro's file is a meaningful test.
