# Calibration loads the census fixture

**Status:** Accepted (2026-07-10) · **Decider:** Nathan

In the context of the `windows/lab` calibration suite recomputing two 500-seed
censuses (`DRIFT`, `MEETING`) behind a `LazyLock` on every run — ~145s that
dominated the *entire* workspace `cargo test` wall-clock and, when several dev
contexts ran concurrently, thrashed the machine — we decided that
**calibration loads each census from a committed, drift-checked `rows.csv`
fixture instead of recomputing it.** `lab run` now publishes the full per-seed
table (`book/src/laboratory/generated/<study>/rows.csv`) beside the summary and
charts; the existing CI "Artifacts are current" step regenerates and
`git diff`s it, so `load_rows(fixture)` equals `run(&study)` by construction.

**Context.** The census is a pure deterministic function of its study, so its
output is materialisable exactly like the almanacs, maps, and registry dumps
the project already commits and drift-checks — the standing pattern for
"expensive deterministic output we do not want to recompute." The reader
(`hornvale_lab::load_rows`) takes the schema — metric names, order, and each
metric's `SummaryKind` — from the study, never from the CSV, so `MetricValue`
reconstruction is lossless rather than guessed from a field's shape.
Alternatives were weighed and rejected: `cargo-nextest` (its process-per-test
model re-initialises the `LazyLock` census once *per test* — strictly worse for
this suite, not better); a fuzzy on-disk cache (a stale cache could silently
pass against old worlds — the fixture dissolves that hazard, because drift is
exactly what CI checks); tag crates such as `test-tag` (blocked by the
serde-only dependency allowlist). Determinism is what makes the whole scheme
sound and safe.

**Consequence.** The 22 calibration assertions run in ~0.05s locally instead of
~145s; workspace `cargo test` no longer carries the census on every
intermediate run. The guarantee splits cleanly across CI: the artifact step
proves the committed fixture is *fresh* (regenerate + `git diff`), and the
calibration tests prove the fresh fixture reproduces *ground truth*. A
`#[ignore]`d guard, `census_fixture_matches_live_run`, pins
`load_rows(fixture) == run(&study)` directly for anyone wanting the full proof
locally (`cargo test -p hornvale-lab --test calibration -- --ignored`). One
cost is accepted knowingly: a developer who changes worldgen and runs only
`cargo test` — skipping the artifact drift check — sees calibration pass
against the stale fixture until CI catches it, the same contract every other
committed artifact already carries. After a worldgen change, regenerate with
`lab run`.

**See also.** Decision `ci-checks-500-seed-censuses` (the two fixtures are the
CI-checked census half); decision 0011 (studies are data, metrics are code);
the Laboratory overview self-check section
(`book/src/laboratory/overview.md`). Slug filename per decision 0026.
