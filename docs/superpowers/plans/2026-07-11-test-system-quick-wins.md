# Test-system quick wins — implementation plan

> **Status: COMPLETE (2026-07-11).** All five tasks landed and reviewed;
> chronicle entry `book/src/chronicle/the-fast-gate.md`, retrospective
> `docs/retrospectives/the-fast-gate.md`.

Five small improvements from the test-system ideonomy batch (recorded in the
idea registry, this branch), ordered by leverage. Each task is independent,
lands as its own commit on `test-system-ideas`, and flips its registry row
`raw` → `shipped`.

Measured context (2026-07-11, M1 Max, solo): workspace suite ~65 s wall;
`fixture_staleness` probe ~8 s; the `#[ignore]`d `branches_fixture_matches_live_run`
guard ~343 s debug; censuses run ~5× faster in release than debug.

## Global Constraints

- Every commit passes the full gate: `cargo fmt --check`,
  `cargo clippy --workspace --all-targets -- -D warnings`,
  `cargo test --workspace` (`make gate` runs all three, cost-ordered).
- Dependencies: `serde` + `serde_json` ONLY. No new crates. No
  `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` (workspace clippy.toml
  enforces). No wall-clock time anywhere.
- Every new `pub` item, field, and variant gets a one-line doc comment
  (`#![warn(missing_docs)]` is deny-by-clippy at the gate).
- Determinism is constitutional: any derived test selection (windows,
  samples) must be a pure function of committed bytes — never git state,
  time, or environment.
- Each task flips its row in `book/src/frontier/idea-registry.md` from
  `raw` to `shipped` and appends the code path to the row's Where cell;
  after any registry edit run
  `cargo test -p hornvale --test docs_consistency`.
- Typed quantities per repo convention; dimensionless ratios stay bare
  `f64`. New primitives at `pub` boundaries need a `type-audit:` verdict
  tag (`bare-ok(<class>)`) — mirror the tags on neighboring code, and run
  `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` once
  before committing any task that adds a `pub` boundary.
- Commit message style: `area(scope): summary` + body explaining why,
  naming the registry slug.

## Task 1: Per-package opt-level for hot leaf crates (TOOL-hot-crate-opt)

**Goal**: The debug-profile test suite gets release-grade speed in the
crates where tests burn CPU, without slowing everyone's compiles.

**Files**: `Cargo.toml` (workspace root), `book/src/frontier/idea-registry.md`.

**Steps**:
1. Baseline: `time cargo test -p hornvale-lab --test fixture_staleness`
   (expect ~8 s order).
2. Add to the workspace `Cargo.toml`:
   ```toml
   [profile.dev.package.hornvale-language]
   opt-level = 2
   ```
   with a one-line comment stating why (tests sweep 1,000-seed language
   evolution; O2 here buys release-grade test speed while every other
   crate keeps fast debug compiles).
3. Re-measure `fixture_staleness`. If the probe did not improve
   materially (< 2×), also try `hornvale-terrain`, `hornvale-astronomy`,
   `hornvale-climate`, `hornvale-worldgen` one at a time and keep ONLY
   the entries that measurably pay (report each measurement).
4. Run the full guard once with the final config and report its wall
   time: `time cargo test -p hornvale-lab --test branches_family_calibration -- --ignored`
   (baseline 343 s; expect a large drop).
5. Byte-identity proof (constitutional): the guard and probes compare
   live opt-2 output against release-generated fixtures — their passing
   IS cross-profile identity evidence. Additionally run
   `cargo test -p hornvale --test release_determinism -- --ignored` and
   report the result.

**Success criteria**: guard wall time drops substantially (report exact
numbers); all byte-identity checks pass; full gate green; registry row
flipped; measurements quoted in the commit body.

## Task 2: The drift-scan probe window (TOOL-drift-scan-probes)

**Goal**: The staleness probes stop always checking seeds 0–2; they probe
0–2 PLUS a rotating 3-seed contiguous window whose position is a pure
function of the committed fixture bytes, so successive fixture
regenerations sweep different slices of seed space.

**Files**: `windows/lab/tests/fixture_staleness.rs`,
`book/src/frontier/idea-registry.md`.

**Design constraints**:
- Window start derived ONLY from the committed `rows.csv` bytes (e.g.
  `hornvale_kernel::Seed` derivation over the CSV content, then a bounded
  draw) — never git state, wall clock, or env.
- Window lies in `[3, span - 3]` for any study span ≥ 7 (the branches
  study has span 1,000; the censuses 500); if a study's span is smaller
  than 7, fall back to the fixed window only.
- Probing runs as a sub-study: `Study.seeds` has `from`/`count` — clone
  the study with `from = window_start, count = 3` for the rotating half
  (mirror the existing `mini.seeds.count` truncation trick).
- Keep the existing synthetic self-tests passing; ADD self-tests
  (PROC-6 pattern: prove the mechanism, not just the current tree):
  window is deterministic for fixed bytes, in bounds, and moves when the
  bytes change.

**Success criteria**: probe covers 6 seeds per census (0–2 + rotating 3);
new self-tests pass; probe wall time stays acceptable (report before/after;
with Task 1 landed it should be well under the old 8 s); full gate green;
registry row flipped.

## Task 3: The failure black-box (TOOL-failure-black-box)

**Goal**: When a swept-seed test fails, the developer gets the failing
world and a repro command on disk, not just a seed number in a panic
message.

**Files**: `windows/lab/src/blackbox.rs` (new), `windows/lab/src/lib.rs`
(module + re-export), `windows/lab/tests/fixture_staleness.rs`,
`windows/lab/tests/branches_family_calibration.rs`,
`book/src/frontier/idea-registry.md`.

**Design**:
- `pub fn record_failure(study: &Study, seed: u64, pin_set_label: &str) -> std::io::Result<std::path::PathBuf>`
  in a new `blackbox` module: rebuild the world exactly as the study
  runner does for that seed + pin set (REUSE the runner's existing
  world-construction path — read `windows/lab/src/runner.rs` first; do
  not duplicate its pin-application logic), serialize the world
  (quantized, same as `World`'s normal serialization) to
  `target/failures/<study-name>-seed<seed>-<pin-set>.json`, and write a
  sibling `.repro.txt` containing the equivalent
  `cargo run -p hornvale -- new --seed <seed> [pins] --out world.json`
  line. Return the world path.
- Integration (prove the pattern, don't rototill): the staleness probe's
  mismatch panic calls it best-effort (an `io` failure must not mask the
  real panic — degrade to the old message) and names the written path in
  the panic text. In `branches_family_calibration.rs`, wire ONE test
  (`lexicon_regular_family_holds_on_every_swept_seed`) to record the
  first failing seed before panicking.
- Unit test: `record_failure` writes JSON that `serde_json` parses back
  to a `World` equal to one built directly for the same seed + pins.

**Success criteria**: unit test passes; both integrations compile and the
suite stays green (the integrations only fire on failure); doc comments on
every new pub item; full gate green; registry row flipped.

## Task 4: Exhaustive pin micro-enumeration (TOOL-exhaustive-enumeration)

**Goal**: The finite, discrete corner of pin space is enumerated
completely instead of sampled: sky {constant, generated} × rotation
{normal, locked} × neighbor {blue-giant, red-giant, white-dwarf,
orange-giant, red-dwarf, sun-like} × supercontinent {true, false} at
seed 42 (48 combos). Continuous pins (plates, ocean-fraction, moons) are
excluded — say so in the test's module doc.

**Files**: `windows/worldgen/tests/pin_enumeration.rs` (new),
`book/src/frontier/idea-registry.md`.

**Design**:
- Read how the CLI hands pins to `hornvale-worldgen` (`cli/src/main.rs`
  lines ~127–160 and the worldgen public API) and drive the same public
  path.
- For each combo assert: the build returns `Ok(world)` or a loud
  `Err(GenesisError…)` — never a panic. For every `Ok`, build twice and
  assert the serialized ledgers are byte-identical (determinism at every
  enumerated point).
- Report (in the test, as a final assertion message or doc comment) how
  many combos build vs refuse — measured, not preregistered; do not
  force either count.
- Runtime rule: measure the test's wall time. If it exceeds ~15 s in the
  gate, keep a 12-combo representative subset in the default test and
  move the full 48-combo product behind `#[ignore]`
  (`full_pin_product_is_enumerated`) for the scheduled/pre-merge run —
  state which path you took in the report.

**Success criteria**: test passes; no panics across the product;
determinism holds at every buildable combo; wall-time rule applied and
reported; full gate green; registry row flipped.

## Task 5: The affected-only gate (TOOL-gate-fast)

**Goal**: `make gate-fast` runs only the test subset a change can affect,
mechanizing CLAUDE.md's scoping prose. The full `make gate` stays the
commit gate — this is an iteration tool only.

**Files**: `scripts/gate-fast.sh` (new), `Makefile`,
`book/src/frontier/idea-registry.md`.

**Design**:
- Changed set = `git diff --name-only $(git merge-base main HEAD)` plus
  uncommitted changes (`git status --porcelain`).
- Path → crate mapping by directory: `kernel/` → everything (stop:
  full gate); `domains/<x>/` → that domain + `hornvale-worldgen` +
  every window + `hornvale` (cli); `windows/worldgen/` → worldgen + all
  windows + cli; other `windows/<x>/` → that window + cli; `cli/` → cli.
  Anything OUTSIDE those directories (root `Cargo.toml`, `Makefile`,
  `book/`, `docs/`, `studies/`, `scripts/`, `.github/`) → fall back to
  the full gate. Overapproximation is fine; missing a dependent is not.
- Always run `cargo fmt --check` first and `cargo clippy` scoped to the
  same package set (`-p` flags). Print the chosen packages and the
  reason (which changed paths mapped to them) before running.
- `make gate-fast` target wired into the Makefile help header.
  `shellcheck scripts/gate-fast.sh` must pass.
- Demo in the report: run it on this branch (registry + plan changes
  only → should choose the full-gate fallback path and say why), and
  once with a synthetic touch of `domains/language/src/lib.rs`
  (undo after) showing the scoped selection.

**Success criteria**: shellcheck clean; both demo runs behave per the
mapping; Makefile help lists it; full gate green; registry row flipped.
