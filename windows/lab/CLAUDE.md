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

## `timings.rs` measures the BUILD, not the world (decision 0088)

The Laboratory is the measurement window, so the suite's own clock lives here:
`timings.rs` parses nextest's `libtest-json-plus` durations, folds/hysteresizes
the committed per-host baseline, and computes both alarms. `cli/` is bin-only,
so its tests cannot host shared code — that is why this module is in `lab` and
not next to `cli/tests/timings_alarm.rs`, which is only the failing-test
surface. Two consequences when editing it:

- **The wall-clock ban does not reach it, and the tags say so.** Durations are
  tagged `bare-ok(diagnostic-value)` — measurements *of* the implementation,
  never of the world. Do not copy that class into anything a world can see. A
  `pub fn` here returning `String` or `Result<_, String>` needs
  `bare-ok(prose: return)`; four consecutive tasks forgot it during execution,
  so assume the tag rather than the vigilance.
- **The pure functions are the contract.** `fold_below_floor`,
  `apply_hysteresis`, `suite_shift`, `per_test_shifts` and the enforcement
  polarity are unit-tested directly, not only through `make ci`. The contention
  gate was shipped *inverted* once and passed spec-compliance review; the fix
  was to make its polarity a pure function with a test that fails on
  re-inversion. Keep new decisions in that shape.

## Censuses regenerate locally now, ~7 min (decision 0063, supersedes 0046)

- The everyday gate still stays fast by skipping censuses: `regenerate-
  artifacts.sh` runs them only under `HV_CENSUS=1` (a plain `make rebaseline`
  skips them). But since [The Local Census](../../book/src/chronicle/the-local-census.md)
  the census is cheap — the all-metric per-world cost fell ~285 → ~8 CPU-s
  (the metric + genesis-naming paths stopped re-sculpting terrain) — so the
  full ~2000-world census regenerates **locally in ~7 min** on the 40-core box.
- The sanctioned refresh is therefore **`scripts/census-run.sh`**, run once
  per campaign at the pre-merge close, keeping the census fixtures
  (`book/src/laboratory/generated/*/rows.csv`) **current with main** — not
  lagging. Use the wrapper, not `HV_CENSUS=1 bash
  scripts/regenerate-artifacts.sh`: since decision
  [0081](../../docs/decisions/0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md)
  every entry point serializes against other heavy runs on the box, but only
  `census-run.sh` also records the run in `docs/timings.md`.
  `scripts/census-run.sh status` says whether one is running. `make regen-remote` (AWS) is retired to
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
