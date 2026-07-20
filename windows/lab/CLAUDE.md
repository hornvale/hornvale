# CLAUDE.md — working in `windows/lab/`

The lab is Hornvale's measurement instrument: it runs **studies** (data) that
compute **metrics** (code) over generated worlds, producing the censuses the
book publishes and the calibration evidence. Read the root `CLAUDE.md`
"Process" section first.

## Studies are data, metrics are code (decision 0011)

- A **study** is a JSON file in `studies/` (seeds × pins × which metrics to
  read). It carries no logic. Adding or changing a study is a data edit.
- A **metric** is a Rust `Metric { name, doc, summary, extract }` entry in the
  `registry()` in `metrics.rs`. There are ~150+ of them. The registry is
  drift-checked — a metric's name/doc/output is a published contract.
- Studies **preregister their hypotheses** (decision 0016); the
  `preregistration_guard` test enforces that a study can't be quietly edited
  to match a result. Don't disable an ignored study with a result-quieting
  reason ("flaky", "TODO") — the guard rejects exactly that.

## nextest runs each test in its own PROCESS

This is the single most surprising fact for optimizing lab tests. `cargo
nextest` (decision 0040, the gate runner) isolates each test in a separate
process, so **a global cache / `OnceLock` fixture never persists across
tests** — every test that builds a world builds it from scratch. You cannot
speed the suite by memoizing world construction across tests. The levers that
*do* exist: build to the shallowest sufficient `BuildDepth` (see
`windows/worldgen/CLAUDE.md`), and the world-gen speedups in the kernel.

## Censuses regenerate locally now, ~7 min (decision 0063, supersedes 0046)

- The everyday gate still stays fast by skipping censuses: `regenerate-
  artifacts.sh` runs them only under `HV_CENSUS=1` (a plain `make rebaseline`
  skips them). But since [The Local Census](../../book/src/chronicle/the-local-census.md)
  the census is cheap — the all-metric per-world cost fell ~285 → ~8 CPU-s
  (the metric + genesis-naming paths stopped re-sculpting terrain) — so the
  full ~2000-world census regenerates **locally in ~7 min** on the 40-core box.
- The sanctioned refresh is therefore `HV_CENSUS=1 bash
  scripts/regenerate-artifacts.sh`, run once per campaign at the pre-merge
  close, keeping the census fixtures (`book/src/laboratory/generated/*/rows.csv`)
  **current with main** — not lagging. `make regen-remote` (AWS) is retired to
  abandoned — this box is the single canonical platform (decision 0063; AWS
  differs on ~0.1% of discrete-count metrics, so it can't be a parallel ref).
- Calibration loads the drift-checked fixture, not a live recompute (decision
  0032).

## `metrics.rs` is large and splittable

~5.4k lines. Clean seams: `views.rs` (the `WorldView`→`FullView` build-rung
chain), `phonotactics.rs` (the validator), `homophony.rs`, and the `registry()`
broken by its existing `// ---` family headers (Ground/Words/Branches/Tone/
BIO). The per-species metric literals are copy-paste (only a species string
changes) — a `per_species_metric` helper would collapse dozens. Worth doing;
same merge-hot caveat as worldgen's `lib.rs`.
