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

## Censuses are regenerated remotely, never locally (decisions 0045/0046)

- The local gate stays < 5 min by **never** regenerating censuses.
  `regenerate-artifacts.sh` skips them unless `HV_CENSUS=1`, which only the
  AWS spot box sets (`make regen-remote`).
- The committed census fixtures (`book/src/laboratory/generated/*/rows.csv`)
  therefore **lag** after a worldgen change until the next pre-merge AWS regen.
  That lag is the chosen trade — don't "fix" a stale census locally.
- Calibration loads the drift-checked fixture, not a live recompute (decision
  0032).

## `metrics.rs` is large and splittable

~5.4k lines. Clean seams: `views.rs` (the `WorldView`→`FullView` build-rung
chain), `phonotactics.rs` (the validator), `homophony.rs`, and the `registry()`
broken by its existing `// ---` family headers (Ground/Words/Branches/Tone/
BIO). The per-species metric literals are copy-paste (only a species string
changes) — a `per_species_metric` helper would collapse dozens. Worth doing;
same merge-hot caveat as worldgen's `lib.rs`.
