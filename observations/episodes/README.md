# Observation episode manifests

Files in this directory are internal planning, evidence, and production
records. They are not public posts and no command in the observation pipeline
publishes them.

Validate a record from the repository root with:

```text
cargo run -p hornvale -- observations validate --manifest observations/episodes/HV-001.json
```

Caption drafts remain advisory. `evidence_status` records evidence review;
`editorial_status` separately records the exact video and copy package's
lifecycle. A manifest is not publishable until `editorial_status` is
`approved` and `approval` records reviewer `Nathan` with a UTC timestamp in
`YYYY-MM-DDTHH:MM:SSZ` form. Publication itself is manual.

Keep comparison references in `comparison_reference`; they are internal
research metadata and must not replace the public title or observation
sentence.

The opening pilot is indexed in
`observations/batches/2026-09-opening-batch.md`. Its manifests deliberately
use only the currently exportable underworld producer and spatial grammar.
The accompanying files in `observations/captions/` are casual drafts, not
reviewed or approved copy. The seven-package approved reserve remains pending
Nathan's manual review of each exact video and caption package.
