# Observation episode manifests

Files in this directory are internal planning, evidence, and production
records. They are not public posts and no command in the observation pipeline
publishes them.

Validate a record from the repository root with:

```text
cargo run -p hornvale -- observations validate --manifest observations/episodes/HV-001.json
```

Caption drafts remain advisory. A manifest is not publishable until its
`evidence_status` is `approved` and its `approval` field records Nathan's
review of the exact video and copy package. Publication itself is manual.

Keep comparison references in `comparison_reference`; they are internal
research metadata and must not replace the public title or observation
sentence.
