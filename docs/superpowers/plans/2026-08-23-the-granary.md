# The Granary Implementation Plan

> **REQUIRED SUB-SKILL:** Use the executing-plans skill to implement this plan task-by-task.

**Goal:** Give the deep-history bake seasonal resolution — an authored,
latitude-derived harvest curve that stores integrate and raid/founding checks
sample at sub-year phases — so committed history events carry sub-year
timestamps and the founder-handle discrimination tail can be measured for
retirement.

**Architecture:** Three additions to `windows/worldgen/src/history_bake.rs`
(the ~9.4k-line bake): (1) a new pure `harvest` module producing a seasonal
production multiplier from `(latitude, biome_class, day_of_year)`; (2) the
existing granary `stores` becoming a running stock integrated over ~12
sub-year phases per epoch; (3) event timestamps refined from whole-year to
`year + phase/12` in the SAME f64 bake-year unit they carry today (grain
change, not unit change — decision-ledger #4). Raid strength/margin/
defensibility math is untouched; only when the raid rule is asked changes.

**Tech Stack:** Rust (edition 2024, workspace rules: serde/libm only,
BTreeMap discipline, `total_cmp`, `#![warn(missing_docs)]`). No new draws —
the curve consumes no stream, so `history/bake/v2`'s consumption order is
untouched (its LABEL still bumps to v3 because committed history changes).

**Spec:** `docs/superpowers/specs/2026-08-23-the-granary-design.md`

**Working conventions for every task:**
- Worktree: `.claude/worktrees/the-granary` (branch `campaign/the-granary`).
- Iterate cost-ordered: fmt + clippy first, scope tests to the crate
  (`cargo test -p hornvale-worldgen --test suite -- <filter>`), full gates
  belong to the sluice, not intermediate runs.
- Every commit passes `make gate-commit` before it lands (kernel-layer edits
  rebuild more units; expect the warm-tree gate to cost minutes here since
  worldgen depends on domains).
- NEVER regenerate census goldens locally — lefford only, at close (decision
  0063/0079).

---

## Phase A — the harvest curve (pure, no bake wiring)

### Task 1: The curve module

**TDD scenario:** new feature — full TDD cycle.

**Files:**
- Create: `windows/worldgen/src/harvest.rs`
- Modify: `windows/worldgen/src/lib.rs` (add `pub mod harvest;`)
- Test: in-module `#[cfg(test)]` in `harvest.rs` (house style: grid-mechanics
  tests live beside the code; see CLAUDE.md's fixture/split rule)

**Step 1: Write the failing tests**

```rust
// windows/worldgen/src/harvest.rs (test module)
#[test]
fn curve_is_zero_in_winter_and_peaks_at_harvest() {
    let c = Curve::new(LatDeg(45.0), BiomeClass::TemperateForest);
    let peak_doy = c.peak_day_of_year();
    assert!(c.at(peak_doy) >= c.at_every_other_quarter_sample(), "..."); // shape asserts below
    assert!(c.at(day_of_winter(45.0)) < WINTER_FLOOR); // near-zero through winter
}

#[test]
fn southern_hemisphere_is_half_a_year_out_of_phase() {
    let n = Curve::new(LatDeg(45.0), BiomeClass::TemperateForest);
    let s = Curve::new(LatDeg(-45.0), BiomeClass::TemperateForest);
    assert!((n.peak_day_of_year() - s.peak_day_of_year()).abs() - DAYS_PER_YEAR / 2.0 < 1e-9);
}
```

(The exact assertion bodies are the implementer's to finalize; the PROPERTIES
the tests must pin are: winter floor near zero, single annual peak, peak-day
antisymmetry under latitude negation exact to float equality after the
half-year shift, and `at` periodic with period one year.)

**Step 2: Run to verify failure**

Run: `cargo test -p hornvale-worldgen --lib harvest`
Expected: FAIL (module/type does not exist).

**Step 3: Implement**

```rust
/// Authored seasonal production multiplier in [0, AMPLITUDE_MAX].
pub struct Curve { latitude: LatDeg, biome: BiomeClass }
impl Curve {
    pub fn new(latitude: LatDeg, biome: BiomeClass) -> Self;
    pub fn at(&self, day_of_year: f64) -> f64;       // pure, periodic
    pub fn peak_day_of_year(&self) -> f64;
    pub fn amplitude(&self) -> f64;                   // authored per biome class
}
```

Shape: a clamped half-wave over the growing season whose onset/peak derive
from latitude (reuse whatever latitude term the terrain/climate side already
computes — find the existing insolation/latitude axis first and cite it in
the module doc; do NOT author a second latitude computation). Amplitude is a
small authored table keyed by biome class, documented values, one line of
justification each. All floats through `libm` where transcendentals appear;
`total_cmp` in any sort; doc comment on every pub item (`missing_docs`).

**Step 4: Run to verify pass**

Run: `cargo test -p hornvale-worldgen --lib harvest`
Expected: PASS. Then `make quick`.

**Step 5: Commit**

```bash
git add windows/worldgen/src/harvest.rs windows/worldgen/src/lib.rs
git commit -m "feat(history): authored latitude-phase harvest curve (The Granary T1)"
```

---

## Phase B — stores integrate the curve

### Task 2: Per-community curve parameters at open

**TDD scenario:** modifying tested code — locate and run the affected tests
first (`cargo test -p hornvale-worldgen --test suite -- bake`), then extend.

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` — `Bake::open` (~line 2239)
  and the `Community`/`BakeOccupation` state structs
- Test: `windows/worldgen/tests/suite/history_bake.rs` (add cases)

**Steps:** Add curve params (computed once at open from the community site's
latitude and its cell's biome class — both already reachable from `Bake`'s
`geo`/caps inputs; if biome class is NOT reachable, stop and surface it rather
than approximating: the amplitude table keys on class, and a silent nearest-
match would corrupt criterion 3's contrast). Assert in tests: two communities
opened at ±lat sites carry curves exactly half a year out of phase; a
community carries its site's amplitude class.

Commit: `feat(history): communities carry their harvest curve at open (T2)`.

### Task 3: Running-store integration over sub-year phases

**TDD scenario:** modifying tested code — this is the campaign's most
dangerous edit; the existing tribute/growth battery MUST stay green except
where this plan explicitly moves a value.

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` — the epoch loop in `bake()`
  (~line 3666), `step_community`, and the store fields
- Test: `windows/worldgen/tests/suite/history_bake.rs`; in-module mutation
  checks per house style

**Design constraints (from spec §3.2–3.4):**
- PHASES_PER_YEAR = 12, a named const with a justification doc.
- Total annual production per community matches today's logistic increment —
  the curve redistributes WHEN food arrives, never HOW MUCH. Test: summed
  phase accrual equals the old annual increment to within float tolerance on
  a scripted scenario.
- Stores integrate: accrual per phase from the curve, consumption bleeding
  per phase; clamp at zero; starvation path exercised by a synthetic
  low-yield community.
- Phase order fixed ascending; communities in the existing snapshot order;
  BTreeMap discipline throughout; `total_cmp` everywhere.

**Mutation-proofing (autopilot rule):** every test written here must first be
shown RED against the un-phased code (or against a deliberately broken phase
count), and the red must be behavioural, not a compile error.

Commit: `feat(history): stores integrate the harvest curve over 12 phases (T3)`.

### Task 4: Raid and founding placement at phases

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` — the epoch loop (raid checks
  move into the phase loop; `maybe_raid` signature gains the phase timestamp),
  founding/relocate stamping sites
- Test: `windows/worldgen/tests/suite/history_bake.rs` + a scripted
  two-community scenario test

**Constraints:** the raid rule's internals (strength, margin, defensibility,
dominance gate) are UNTOUCHED — diff review must show no change above
`maybe_raid`'s call into its helpers other than the timestamp argument.
Timestamps are `year + phase/12` in the SAME f64 bake-year unit as today's
`founded`/`ended` (ledger #4: grain change, not unit change). Scripted test:
a raider whose stores bottom out mid-year raids in the depleted phase, not at
the epoch boundary.

Commit: `feat(history): raids and foundings stamped at sub-year phases (T4)`.

---

## Phase C — contracts and artifacts

### Task 5: Stream label bump

**Files:**
- Modify: `domains/history/src/streams.rs:35` — `history/bake/v2` →
  `history/bake/v3`, doc updated citing this spec (epoch suffix, never rename
  — decision 0006)

**Steps:** bump label; run the pin-isolation / byte-identity batteries that
read the label; EXPECT the history byte-identity suite to go red (committed
history legitimately changed) — that red is recorded in the commit message as
the intended consequence, and Task 7 regenerates the fixtures.

Commit: `chore(streams): history/bake/v3 — The Granary phase timestamps (T5)`.

### Task 6: Founder-collision byproduct measurement

**Files:**
- Modify: `domains/history/src/flesh.rs::founder_handle` — behind the
  measurement, not yet deleted
- Modify: `windows/worldgen/tests/suite/founder_collision.rs` — add the
  tail-less key arm

**Steps:** Add a test-only variant computing the identity key WITHOUT the
`ended`/`peak` tail, run over seeds 0–2999 (the Ell scaffold's range).
Outcome branches (write the branch table into the test doc):
- 0 collisions → retire the tail in `founder_handle` (delete STEP 2's tail
  folds), update the function doc's trimmability paragraph, update registry
  rows at close.
- >0 collisions → keep the tail, record the count and worlds in the test doc
  AND the ledger; the byproduct question ships its answer either way.
Do NOT guess which branch holds — run it.

Commit: `test(founder): measure the tail-less handle on 0-2999 (T6)`.

### Task 7: Regenerate committed artifacts

**Files:** everything `scripts/regenerate-artifacts.sh` writes EXCEPT census.

```bash
make rebaseline          # regenerates almanacs, galleries, dumps, audits
git diff --stat          # READ the drift before accepting it
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$') || true  # expected NON-empty
git add -A && git commit -m "chore(artifacts): regenerate after The Granary phases (T7)"
```

Branch table: gallery/almanac numbers move → expected, commit; type-audit
report moves → expected (pub items changed in harvest.rs), commit;
`docs/generated-paths.txt` itself should not need edits; ANY census CSV move
locally → STOP, wrong path, lefford owns that.

### Task 8: The preregistered study

**Files:**
- Create: `studies/the-granary.study.json` (seeds 0–999,
  `BuildDepth::Settlements`) — studies are data; metrics are code
- Create: metric code per the lab's existing metric-registration pattern
  (find it via `cargo run -p hornvale -- lab list-metrics` before writing)
- Emit per-raid `(seed, community, day-of-year, local-amplitude)` rows

**Readout (after the study runs):** criteria 1–2 decide the hypothesis;
criterion 3 reports descriptively (ratified fallback). Results go in the
chronicle verbatim, null or not — a falsified prediction is a finding
(decision 0016's spirit; do not retune the curve to rescue the hypothesis).

### Task 9: Close-out

- Census refresh ON LEFFORD by Nathan (standing rule; not this agent's call).
- Chronicle entry (`book/src/chronicle/the-granary.md`), book freshness sweep,
  retrospective (`docs/retrospectives/the-granary.md`),
  idea-registry updates: `MEM-founder-key-trim` row resolved/amended per T6's
  outcome; NEW row for layer 2 (inter-annual memory / Viking hypothesis, new
  draws, v3-stream consumption); temporary-grain caveat visible to the
  WorldTime migration session.
- `make sluice-stage BRANCH=campaign/the-granary REF=<full-sha>` at each plan-
  stage boundary; merge via `make sluice` at close.
