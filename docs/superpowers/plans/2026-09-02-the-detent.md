# The Detent Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Hold the fear path's terrain verdicts for the session instead of re-sampling every visited room of every roster member every tick, byte-identically, and re-measure H4 on the instruments it was frozen on.

**Architecture:** A session-lived `GroundHazards` memo (`hornvale_kernel::derived::Derived<Facet, Hazards>`, `Validity::Pure`) behind a `RefCell`, threaded into `LocaleTerrain` by an additive builder and read inside `LocaleTerrain::hazards`; a per-entity read-side `FrighteningGround` index in the resident store, advanced from the `Trail` by a consumed-prefix cursor, over which `build_emitter_scan` and `hazard_memory_memo`'s emitter-free path become O(new sightings); counting witnesses that are red on the merge base before they are green.

**Tech Stack:** Rust 2024, std only (`serde`/`serde_json`/`libm` are the only external crates); `BTreeMap`/`BTreeSet`/`Vec`, never `HashMap`; nextest for the suite; `--release` examples for the readout.

**Spec:** `docs/superpowers/specs/2026-09-02-the-detent-design.md` — read it before any task. **Ledger:** `docs/superpowers/ledgers/2026-09-02-the-detent.md` — every task's report is appended there by the controller.

## Global Constraints

- **Byte-identical.** No fact, predicate, stream label or epoch changes. Every `f64` the fear path produces is the same value in the same summation order (spec §0, §5). A task that moves a committed artifact other than `docs/audits/type-audit-report.md` stops and reports.
- **Dependencies:** `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap`/`HashSet`, no wall clock** (`std::time::Instant` is banned in tests; the `--release` examples already carry `#[allow(clippy::disallowed_types)] // benchmark harness` where they time).
- **Every `pub` item, field and variant carries a one-line doc comment** (`#![warn(missing_docs)]`).
- **Every primitive at a `pub` boundary carries a `type-audit:` tag** — `/// type-audit: bare-ok(count)` for counters, `bare-ok(count: return)` for a returned count, `bare-ok(flag: return)` for a returned `bool`, `bare-ok(index: return)` for an index, `bare-ok(ratio: <name>)` for a `[0,1]` `f64`. `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` is in the commit gate and is default-deny.
- **`docs/audits/type-audit-report.md` drifts on any `pub` change**: regenerate it in the SAME commit with `cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md`. Never text-merge it after an absorption; regenerate.
- **The lexicon guard** (`cli/tests/suite/lexicon_guard.rs`) counts every bare `cell` token, including `std::cell::RefCell` and `std::cell::Cell`. Write the waiver ON THE SAME LINE as the token, as `resident.rs:1196` does: `pub type OwnedGround = std::cell::RefCell<GroundHazards>; // lexicon: std::cell::RefCell is the standard library's interior-mutability type — not a place at all`. Run `cargo fmt` and then re-check that fmt did not wrap the waiver onto its own line. Never put `Cell` in a `pub` signature.
- **No `#[ignore]` on any new test.** Decision 0093 requires a `claim:`-shaped reason and `cli/tests/suite/heavy_tier.rs` freezes the untokenised-ignore roster; this plan never needs one — a witness that must be red before it is green is written red inside the task that makes it green, and the red run is recorded in the ledger.
- **Renaming a test is a commit-gate change** (`docs/timings/subfloor-roster.tsv` selects by exact name). Name witnesses once.
- **A witness has a denominator.** Every assertion of the form "X is zero" or "X ≤ N" sits beside an `assert!(denominator > 0)` that proves the path was reached.
- **Commands the implementer runs:** `cargo fmt`, `cargo clippy -p hornvale-vessel --all-targets -- -D warnings`, scoped tests `cargo test -p hornvale-vessel --test suite -- <filter>`, and `make gate-commit` before every commit (the pre-commit hook runs it for any Rust path). Never `--no-verify`.
- **Commit messages** end with the trailer line `Claude-Session: https://claude.ai/code/session_01VZbMTxSHFDJLtmyAUVztbc`. Write them with `git commit -F <file>`, never a heredoc containing an apostrophe.
- **Work in the campaign worktree** `.claude/worktrees/the-detent` on branch `campaign/the-detent`. `cd` there first in every shell; verify with `git rev-parse --abbrev-ref HEAD`.
- **Quiet box for timings, any box for counts.** Counts are the gated instrument; timings belong to Task 9 and require all three load averages recorded before and after every run.

---

## File map

| file | responsibility | tasks |
|---|---|---|
| `windows/vessel/tests/common/mod.rs` | shared test helpers; gains `CountingTerrain` | 1 |
| `windows/vessel/tests/suite/the_detent.rs` (new) + `mod the_detent;` in `tests/suite.rs` | the campaign's witnesses: the bench-shape builder, H5/H6 counts, the campaign-time hash constants, rules 2/4/5, M1 | 1, 2, 4, 6, 8 |
| `windows/vessel/tests/suite/ledger_hash_witness.rs` | three helpers become `pub(crate)` so `the_detent.rs` can mint constants from the SAME scripts | 2 |
| `windows/vessel/src/ground.rs` (new) + `pub mod ground;` in `src/lib.rs` | `GroundHazards`, `OwnedGround`: the room memo | 3 |
| `windows/vessel/src/liveness.rs` | `LocaleTerrain` gains `ground` + `with_ground`; `hazards` reads it; `build_emitter_scan` and `hazard_memory_memo` over the index; `believed_hazard_memo` deleted | 3, 6, 7 |
| `windows/vessel/src/liveness_tests/emitter_scan.rs` (new) | the in-crate `EmitterScan` tests, moved out of `liveness.rs` via `#[path]`; the FOLD-equals-SCAN oracle | 6, 7 |
| `windows/vessel/src/resident.rs` | `FrighteningGround` + store accessors + two `ReadWitness` counters | 5, 8 |
| `windows/vessel/src/session.rs` | one field `ground`, six `.with_ground(&self.ground)` calls, four accessors | 4 |
| `windows/lab/src/health.rs` | `run_simulation` owns one `OwnedGround` per run | 4 |
| `windows/vessel/examples/session_length_scaling.rs`, `agent_scaling.rs` | own one `OwnedGround` per run; M1 columns | 4, 8 |
| `docs/superpowers/specs/2026-09-02-the-detent-design.md` §11 | the readout | 9 |

---

### Task 1: The counting instrument, and the count that is red today

**Files:**
- Modify: `windows/vessel/tests/common/mod.rs` (append)
- Create: `windows/vessel/tests/suite/the_detent.rs`
- Modify: `windows/vessel/tests/suite.rs` (add `mod the_detent;` in alphabetical position, between `mod the_blocking;` and `mod the_first_mark;`)

**Interfaces:**
- Consumes: `hornvale_vessel::liveness::{Terrain, Hazards, derive_npcs, hazard_memory_memo, DriveMovements, LocaleTerrain, PrimaryAfraidMemo, HomeNavCache, SUSTENANCE, AGENT_AT, DRANK, RESTED, SLEPT, EATEN}`, `hornvale_vessel::resident::{OwnedFolds, ResidentFolds}`, `hornvale_locale::LocaleContext`, `hornvale_worldgen::{build_world, SkyChoice, SettlementPins}`, `common::build`.
- Produces: `common::CountingTerrain<'a>` (wraps `&'a dyn Terrain`; `hazards_calls()`, `water_calls()`, `temperature_calls()`, `elevation_calls()`, `reset()`); `the_detent::BenchShape` and `the_detent::bench_shape(seed, ticks, agents) -> BenchShape` — the exact `session_length_scaling` construction, reusable by every later task; `the_detent::probe_index(&BenchShape) -> usize` (the roster member with the most `agent-at` facts).

- [ ] **Step 1: Add `CountingTerrain` to `tests/common/mod.rs`**

Append (after `world_that_draws_a_creature`). The counters are `std::cell::Cell<u64>` in PRIVATE fields with the lexicon waiver on each line; the accessors expose `u64`.

```rust
/// A `Terrain` that delegates every read to `inner` and counts the calls —
/// The Detent's instrument. Counts are deterministic and load-independent,
/// which is why the campaign's gated witnesses assert on them rather than
/// on wall time.
pub struct CountingTerrain<'a> {
    inner: &'a dyn Terrain,
    hazards: std::cell::Cell<u64>, // lexicon: std::cell::Cell is the standard library's interior-mutability counter — not a place
    water: std::cell::Cell<u64>, // lexicon: std::cell::Cell is the standard library's interior-mutability counter — not a place
    temperature: std::cell::Cell<u64>, // lexicon: std::cell::Cell is the standard library's interior-mutability counter — not a place
    elevation: std::cell::Cell<u64>, // lexicon: std::cell::Cell is the standard library's interior-mutability counter — not a place
}

impl<'a> CountingTerrain<'a> {
    /// Wrap `inner`, all counters at zero.
    pub fn new(inner: &'a dyn Terrain) -> Self {
        Self {
            inner,
            hazards: Default::default(),
            water: Default::default(),
            temperature: Default::default(),
            elevation: Default::default(),
        }
    }
    /// How many `hazards()` calls since construction or the last `reset`.
    pub fn hazards_calls(&self) -> u64 {
        self.hazards.get()
    }
    /// How many `is_fresh_water()` calls since construction or the last `reset`.
    pub fn water_calls(&self) -> u64 {
        self.water.get()
    }
    /// How many `temperature()` calls since construction or the last `reset`.
    pub fn temperature_calls(&self) -> u64 {
        self.temperature.get()
    }
    /// How many `elevation()` calls since construction or the last `reset`.
    pub fn elevation_calls(&self) -> u64 {
        self.elevation.get()
    }
    /// Zero every counter.
    pub fn reset(&self) {
        self.hazards.set(0);
        self.water.set(0);
        self.temperature.set(0);
        self.elevation.set(0);
    }
}

impl Terrain for CountingTerrain<'_> {
    fn elevation(&self, room: &Facet) -> f64 {
        self.elevation.set(self.elevation.get() + 1);
        self.inner.elevation(room)
    }
    fn is_fresh_water(&self, room: &Facet) -> bool {
        self.water.set(self.water.get() + 1);
        self.inner.is_fresh_water(room)
    }
    fn temperature(&self, room: &Facet, day: WorldTime) -> f64 {
        self.temperature.set(self.temperature.get() + 1);
        self.inner.temperature(room, day)
    }
    fn solar_altitude(&self, room: &Facet, day: WorldTime) -> Option<f64> {
        self.inner.solar_altitude(room, day)
    }
    fn day_ticks(&self) -> Option<hornvale_kernel::units::TickSpan> {
        self.inner.day_ticks()
    }
    fn forage_value(&self, room: &Facet) -> f64 {
        self.inner.forage_value(room)
    }
    fn hazards(&self, room: &Facet) -> Hazards {
        self.hazards.set(self.hazards.get() + 1);
        self.inner.hazards(room)
    }
    fn is_built(&self, room: &Facet) -> bool {
        self.inner.is_built(room)
    }
    fn is_cold(&self, room: &Facet) -> bool {
        self.inner.is_cold(room)
    }
    fn prey_value(&self, room: &Facet) -> f64 {
        self.inner.prey_value(room)
    }
}
```

Add the imports `common/mod.rs` lacks (`Facet`, `WorldTime`, `Terrain`, `Hazards`); read its existing `use` block first. If `Terrain` gains a method between now and then, the compiler names it — delegate it.

- [ ] **Step 2: Write `the_detent.rs` with the bench-shape builder and the first witness**

The builder is `session_length_scaling.rs`'s `run()` construction, verbatim in substance (read `windows/vessel/examples/session_length_scaling.rs:1090-1210` first and copy its predicate registration, `derive_npcs`, store, day arithmetic and per-tick `DriveMovements` exactly — the point of the witness is to be the bench's shape).

```rust
//! The Detent's witnesses (spec §4 H5/H6, §3 rules 2/4/5, M1). Counts, not
//! clocks: every number here is deterministic on every box.

use crate::common;
use hornvale_kernel::{EntityId, Facet, Ledger, RoomMeshMemo, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_vessel::body::Body;
use hornvale_vessel::liveness::{
    AGENT_AT, DRANK, DriveMovements, EATEN, HomeNavCache, LocaleTerrain, PrimaryAfraidMemo,
    RESTED, SLEPT, SUSTENANCE, derive_npcs, hazard_memory_memo,
};
use hornvale_vessel::resident::{OwnedFolds, ResidentFolds};

/// Spec §4 H5's shape: seed 42, 50 derived agents, 60 ticks.
pub const H5_SEED: u64 = 42;
pub const H5_AGENTS: usize = 50;
pub const H5_TICKS: usize = 60;

/// Everything the bench holds after `ticks` ticks, so a witness can probe it.
pub struct BenchShape {
    pub world: hornvale_kernel::World,
    pub ctx: LocaleContext,
    pub ledger: Ledger,
    pub npcs: Vec<Body>,
    pub folds: OwnedFolds,
    pub mesh_memo: RoomMeshMemo,
    pub day: WorldTime,
    pub day_ticks: Option<hornvale_kernel::units::TickSpan>,
    /// Per tick, the `hazards()` calls the tick's own walk made, in order.
    pub hazards_per_tick: Vec<u64>,
    /// Per tick, the facts committed.
    pub facts_per_tick: Vec<usize>,
}

/// `session_length_scaling.rs`'s construction, counted: the world at `seed`,
/// `agents` derived bodies, `ticks` ticks of `DriveMovements::step_with_occupancy`
/// over one caller-owned store, mesh memo and nav cache, with every tick's
/// terrain wrapped in a `CountingTerrain`.
pub fn bench_shape(seed: u64, ticks: usize, agents: usize) -> BenchShape {
    let world = common::build(seed).expect("the seed builds a world");
    let ctx = LocaleContext::build(&world).expect("the locale context builds");
    let home_settlement = hornvale_settlement::village_info(&world)
        .expect("the flagship always exists")
        .id;
    let day_ticks = hornvale_worldgen::sky_of(&world)
        .ok()
        .and_then(|sky| sky.calendar().cloned())
        .and_then(|c| c.day_ticks());
    let mut ledger = world.ledger.clone();
    let mut registry = world.registry.clone();
    for (pred, doc) in [
        (AGENT_AT, "an agent's position on a day"),
        (DRANK, "an agent satisfied its sustenance goal"),
        (RESTED, "an agent rested on a day, for this many ticks"),
        (SLEPT, "an agent slept on a day, for this many ticks"),
        (EATEN, "an agent ate (eased its hunger) on a day"),
    ] {
        registry
            .register_predicate(pred, false, doc)
            .expect("the drive predicates register identically every run");
    }
    let npcs = derive_npcs(&world, &ctx, &mut ledger, agents, home_settlement);
    assert_eq!(npcs.len(), agents, "derive_npcs must yield the roster asked for");
    let mut mesh_memo = RoomMeshMemo::new();
    let mut home_nav_cache = HomeNavCache::new();
    let folds = OwnedFolds::new(ResidentFolds::new());
    let mut day = WorldTime::from_std_days(0.5).expect("0.5 is a finite day count");
    let mut hazards_per_tick = Vec::with_capacity(ticks);
    let mut facts_per_tick = Vec::with_capacity(ticks);
    for _ in 0..ticks {
        let from = day;
        day = WorldTime::from_ticks(day.ticks() + WorldTime::TICKS_PER_STD_DAY);
        let mesh_snapshot = mesh_memo.clone();
        let base = LocaleTerrain::with_fields(&ctx, None, None, None, None, Some(&mesh_snapshot));
        let terrain = common::CountingTerrain::new(&base);
        let sys = DriveMovements {
            npcs: npcs.clone(),
            from,
            to: day,
            params: SUSTENANCE,
            day_ticks,
            terrain: &terrain,
            folds: &folds,
        };
        let (facts, _occupancy) =
            sys.step_with_occupancy(&ledger, &mut mesh_memo, &mut home_nav_cache);
        facts_per_tick.push(facts.len());
        for fact in facts {
            ledger.commit(fact, &registry).expect("a drive-movements fact commits");
        }
        hazards_per_tick.push(terrain.hazards_calls());
    }
    BenchShape { world, ctx, ledger, npcs, folds, mesh_memo, day, day_ticks, hazards_per_tick, facts_per_tick }
}

/// The roster member with the most `agent-at` facts — the bench's probe rule.
pub fn probe_index(shape: &BenchShape) -> usize {
    let counts: Vec<usize> = shape
        .npcs
        .iter()
        .map(|b| shape.ledger.facts_of(b.entity, AGENT_AT).count())
        .collect();
    counts
        .iter()
        .enumerate()
        .max_by_key(|(_, c)| **c)
        .map(|(i, _)| i)
        .expect("the roster is non-empty")
}

/// The counts one `hazard_memory_memo` call makes on the probe, with a FRESH
/// `PrimaryAfraidMemo` (the bench's shape) and then a second call on the same
/// memo (production's per-creature read after the tick's scan exists).
pub struct ProbeCounts {
    pub fresh_hazards: u64,
    pub warm_hazards: u64,
    pub scans_delta: u64,
    pub with_emitters_delta: u64,
    pub replays_delta: u64,
    pub shunned: usize,
}

pub fn probe_counts(shape: &BenchShape) -> ProbeCounts {
    let pi = probe_index(shape);
    let npc = &shape.npcs[pi];
    let mesh = shape.mesh_memo.clone();
    let base = LocaleTerrain::with_fields(&shape.ctx, None, None, None, None, Some(&mesh));
    let terrain = common::CountingTerrain::new(&base);
    let w0 = {
        let s = shape.folds.borrow();
        let w = s.witness();
        (w.emitter_scans(), w.emitter_scans_with_emitters(), w.alarm_replays())
    };
    let mut memo = PrimaryAfraidMemo::new();
    let first = hazard_memory_memo(&shape.ledger, &shape.folds, npc, shape.day, &terrain, &shape.npcs, &mut memo);
    let fresh_hazards = terrain.hazards_calls();
    let w1 = {
        let s = shape.folds.borrow();
        let w = s.witness();
        (w.emitter_scans(), w.emitter_scans_with_emitters(), w.alarm_replays())
    };
    terrain.reset();
    let second = hazard_memory_memo(&shape.ledger, &shape.folds, npc, shape.day, &terrain, &shape.npcs, &mut memo);
    assert_eq!(first, second, "two reads of one instant over one ledger must agree");
    ProbeCounts {
        fresh_hazards,
        warm_hazards: terrain.hazards_calls(),
        scans_delta: w1.0 - w0.0,
        with_emitters_delta: w1.1 - w0.1,
        replays_delta: w1.2 - w0.2,
        shunned: first.shunned.len(),
    }
}

/// The witness Task 1 commits: floors and prints, no threshold yet. Task 4
/// and Task 6 add H5's assertions in the task that makes each green, and
/// record the red they saw first.
#[test]
fn h5_witness_the_hazard_reads_terrain_samples_on_the_bench_shape() {
    let shape = bench_shape(H5_SEED, H5_TICKS, H5_AGENTS);
    let counts = probe_counts(&shape);
    let distinct_rooms: usize = {
        let mut store = shape.folds.borrow_mut();
        let (visits, _) = store.latest_visit_and_trail(&shape.ledger);
        shape.npcs.iter().map(|b| visits.of(b.entity).len()).sum()
    };
    let last_tick = *shape.hazards_per_tick.last().expect("ticks ran");
    println!("--- H5 witness: seed {H5_SEED}, {H5_AGENTS} agents, tick {H5_TICKS} ---");
    println!(
        "probe: FRESH memo {} hazards() calls, WARM memo {}, scans +{} (with emitters +{}), \
         alarm replays +{}, shunned {}",
        counts.fresh_hazards, counts.warm_hazards, counts.scans_delta,
        counts.with_emitters_delta, counts.replays_delta, counts.shunned
    );
    println!(
        "whole tick {H5_TICKS}: {last_tick} hazards() calls, {} facts committed; roster distinct rooms {distinct_rooms}",
        shape.facts_per_tick.last().copied().unwrap_or(0)
    );
    // Denominators: the path was reached, the population is the one the
    // mechanism is worst for, and the tick did real work.
    assert!(distinct_rooms > 0, "the roster visited no rooms — the walk did nothing");
    assert!(counts.scans_delta == 1, "one fresh read must build exactly one emitter scan");
    assert!(counts.fresh_hazards > 0, "the fresh read must sample terrain at all, or the count is zero of zero");
    assert!(last_tick > 0, "the last tick must have sampled terrain, or the tick did not run the fear path");
    assert!(shape.facts_per_tick.iter().sum::<usize>() > 0, "no facts committed over the run");
}
```

- [ ] **Step 3: Run it and record the numbers**

Run: `cd .claude/worktrees/the-detent && cargo test -p hornvale-vessel --test suite -- the_detent --nocapture 2>&1 | tee /tmp/detent-t1.txt`
Expected: PASS, printing FRESH ≈ 22,302 and whole-tick ≈ 44,694 (the spec's spike numbers, ±0 — the construction is deterministic; a different number means the construction differs from the bench and the task stops to find out why). Paste the two printed lines into the task report.

- [ ] **Step 4: fmt, clippy, gate, commit**

```bash
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/tests/common/mod.rs windows/vessel/tests/suite/the_detent.rs windows/vessel/tests/suite.rs
git commit -F- <<'EOF'
test(vessel): The Detent Task 1 -- the counting terrain and the H5 witness, floors only

Claude-Session: https://claude.ai/code/session_01VZbMTxSHFDJLtmyAUVztbc
EOF
```

---

### Task 2: The campaign-time hash constants, with a positive control

**Files:**
- Modify: `windows/vessel/tests/suite/ledger_hash_witness.rs` — make `fnv1a`, `run_fixed_script`, `run_emitter_witness`, `EmitterRun` (and its fields) and `EMITTER_SEED` `pub(crate)`; nothing else changes there.
- Modify: `windows/vessel/tests/suite/the_detent.rs` (append)

**Interfaces:**
- Consumes: `crate::ledger_hash_witness::{fnv1a, run_fixed_script, run_emitter_witness, EmitterRun, EMITTER_SEED}`, `hornvale_vessel::session::{Session, PossessOpts}`.
- Produces: three `pub(crate) const` values in `the_detent.rs` — `DETENT_SEED_42_LEDGER: u64`, `DETENT_EMITTER_LEDGER: u64`, `DETENT_EMITTER_HAZARD: u64` — and two tests that assert them. Task 9 retires them (decision 0541).

- [ ] **Step 1: Widen the four helpers to `pub(crate)`** in `ledger_hash_witness.rs`. Do not touch their bodies or the file's doc.

- [ ] **Step 2: Append the constants and two tests to `the_detent.rs`**, with the constants set to `0` for the first run:

```rust
use crate::ledger_hash_witness::{EMITTER_SEED, fnv1a, run_emitter_witness, run_fixed_script};
use hornvale_vessel::session::{PossessOpts, Session};

/// CAMPAIGN-TIME constants (decision 0541): minted at Task 2 from the merge
/// base, re-recorded MAIN-FIRST after every absorption, retired at close.
/// They equal "the whole walk's behaviour on one seed" and redden on ANY
/// behaviour change by any campaign; that is their job for exactly as long
/// as this campaign's pre-fix code exists to diverge from.
pub(crate) const DETENT_SEED_42_LEDGER: u64 = 0;
pub(crate) const DETENT_EMITTER_LEDGER: u64 = 0;
pub(crate) const DETENT_EMITTER_HAZARD: u64 = 0;

#[test]
fn the_detent_seed_42_walk_matches_the_campaign_time_constant() {
    let world = common::build(42).expect("seed 42 builds");
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 starts");
    run_fixed_script(&mut session);
    let hash = fnv1a(session.session_ledger_json().as_bytes());
    println!("the-detent seed-42 ledger hash: {hash:#018x}");
    assert_eq!(hash, DETENT_SEED_42_LEDGER, "the seed-42 walk moved — a fold changed a creature's route");
}

#[test]
fn the_detent_emitter_walk_matches_the_campaign_time_constants() {
    let world = common::build(EMITTER_SEED).expect("the emitter seed builds");
    let run = run_emitter_witness(&world);
    println!(
        "the-detent emitter: ledger {:#018x} hazard {:#018x} over {} bodies, {} shunned, {} dread, {} replays",
        run.ledger_hash, run.hazard_hash, run.bodies, run.shunned, run.dread, run.replays
    );
    assert!(run.replays > 0, "the emitter seed must reach the past-day affect replay or this constant witnesses the terrain-only path");
    assert!(run.shunned > 0, "the hazard digest must be non-empty");
    assert_eq!(run.ledger_hash, DETENT_EMITTER_LEDGER);
    assert_eq!(run.hazard_hash, DETENT_EMITTER_HAZARD);
}
```

- [ ] **Step 3: Run both, twice, and mint the constants.** Run: `cargo test -p hornvale-vessel --test suite -- the_detent_ --nocapture 2>&1 | tee /tmp/detent-t2a.txt`. Expected: both FAIL against `0`, printing the hashes. Run again; the printed hashes must be identical (determinism). Paste the three values into the constants. Run a third time: PASS.

- [ ] **Step 4: The positive control.** Choose a mutation in the fear path that still type-checks and that you EXPECT to move a creature's route — do not take one from this plan; read `feels_frightening`, `threat_field`, `DANGER_ACT` in `liveness.rs` and pick one. Apply it with `python3 scripts/mutate.py` (it refuses unless the target text is found exactly once), rebuild, run both tests: at least one hash must MOVE. If neither moves, the mutation did not reach a route on either script — pick another; a control that cannot fail is not a control. Restore with `git checkout -- windows/vessel/src/liveness.rs`, rebuild, re-run: PASS. Record in the report: the mutation, the moved value(s), and the restored green.

- [ ] **Step 5: fmt, clippy, gate, commit**

```bash
git add windows/vessel/tests/suite/ledger_hash_witness.rs windows/vessel/tests/suite/the_detent.rs
git commit -F- <<'EOF'
test(vessel): The Detent Task 2 -- campaign-time hash constants with a positive control

Claude-Session: https://claude.ai/code/session_01VZbMTxSHFDJLtmyAUVztbc
EOF
```

**Stage 1 closes here.** Controller: push, `make sluice-stage BRANCH=campaign/the-detent REF=<full-sha>`; if `campaign/the-rack` has landed on main, absorb it first (spec rule 6), regenerate `docs/audits/type-audit-report.md`, and re-record the three constants MAIN-FIRST (a detached worktree of `origin/main` with this test file copied in, then the merged tree; the two must agree).

---

### Task 3: `GroundHazards` — the room memo, and the terrain that reads it

**Files:**
- Create: `windows/vessel/src/ground.rs`
- Modify: `windows/vessel/src/lib.rs` (add `pub mod ground;` in alphabetical position)
- Modify: `windows/vessel/src/liveness.rs:582-700` (`LocaleTerrain` struct, `with_fields`, and `impl Terrain for LocaleTerrain`'s `hazards` at ~741)
- Modify: `docs/audits/type-audit-report.md` (regenerate)

**Interfaces:**
- Consumes: `hornvale_kernel::derived::Derived`, `hornvale_kernel::Facet`, `crate::liveness::Hazards`.
- Produces:

```rust
pub struct GroundHazards { /* private */ }
impl GroundHazards {
    pub fn new() -> Self;
    /// The memoised hazards at `room`, computing and holding them on a miss.
    pub fn hazards_or_insert_with(&mut self, room: &Facet, compute: impl FnOnce() -> Hazards) -> Hazards;
    pub fn len(&self) -> usize;        // type-audit: bare-ok(count: return)
    pub fn is_empty(&self) -> bool;    // type-audit: bare-ok(flag: return)
    pub fn hits(&self) -> u64;         // type-audit: bare-ok(count: return)
    pub fn misses(&self) -> u64;       // type-audit: bare-ok(count: return)
    pub fn evict_all(&mut self);
}
pub type OwnedGround = std::cell::RefCell<GroundHazards>; // lexicon: (same-line waiver, see Global Constraints)
impl<'a> LocaleTerrain<'a> {
    pub fn with_ground(self, ground: &'a OwnedGround) -> Self;
}
```

- [ ] **Step 1: Write the failing unit tests in `ground.rs`** (a `#[cfg(test)] mod tests` at the bottom):

```rust
#[cfg(test)]
mod tests {
    use super::*;
    fn room(face: u8) -> Facet { Facet::new(face, &[]).expect("a top-level face is a facet") }
    fn hz(v: f64) -> Hazards { Hazards { uncanny: v, heat: 0.0, cold: 0.0, predator: 0.0 } }

    #[test]
    fn a_miss_computes_and_a_hit_does_not() {
        let mut g = GroundHazards::new();
        let mut computed = 0;
        let a = g.hazards_or_insert_with(&room(0), || { computed += 1; hz(0.5) });
        let b = g.hazards_or_insert_with(&room(0), || { computed += 1; hz(0.9) });
        assert_eq!(computed, 1, "the second read must not recompute");
        assert_eq!(a, b, "the held value is the first one computed");
        assert_eq!((g.hits(), g.misses(), g.len()), (1, 1, 1));
    }

    #[test]
    fn evicting_everything_recomputes_the_same_value() {
        let mut g = GroundHazards::new();
        let a = g.hazards_or_insert_with(&room(3), || hz(0.25));
        g.evict_all();
        assert!(g.is_empty());
        let b = g.hazards_or_insert_with(&room(3), || hz(0.25));
        assert_eq!(a, b);
        assert_eq!(g.misses(), 2);
    }
}
```

Check `Facet`'s constructor name in `kernel/src/room.rs` before using `Facet::new`; the tests in `resident_folds.rs` build rooms with a `room(face, path)` helper — copy its shape.

- [ ] **Step 2: Run to see them fail** (`cargo test -p hornvale-vessel ground::` — FAIL: module missing).

- [ ] **Step 3: Implement `ground.rs`**

```rust
//! The room memo (The Detent, spec §2.1): what the TERRAIN determines about a
//! room, held for the session. The world-derived half of the adaptive cache
//! whose ledger-derived half is [`crate::resident`] — a `Validity::Pure`
//! tenant of the kernel's `Derived` store (decision 0206), keyed by the room
//! alone.
//!
//! # One memo per `(LocaleContext, predator field)`
//!
//! `Terrain::hazards` reads the room, the locale's climate/regime/geosphere
//! and the session's predator field, none of which change after
//! `Session::start`; the key is the room and the REST is supplied by
//! ownership — the session that owns the context and the field owns this
//! memo, and hands it to exactly the terrains built over them. That is the
//! same rule `SustenanceMemo` states for the temperature field. A caller
//! with a second terrain over a different field builds a second memo; the
//! two-terrain test in `tests/suite/the_detent.rs` is what refuses sharing.
//!
//! # What could make it wrong, and why it cannot happen silently
//!
//! A hazard that depends on the day. `Terrain::hazards` takes no `day` by
//! contract ("a slow field"); a seasonal hazard would change that signature,
//! and this memo's key is the first thing the compiler refuses. A predator
//! field that moves per tick would need `Validity::Ledger` or a rebuild per
//! tick; today it is computed once per session (`Session.predator`).

use crate::liveness::Hazards;
use hornvale_kernel::Facet;
use hornvale_kernel::derived::Derived;

/// The per-room hazard memo. See the module doc.
#[derive(Debug, Default)]
pub struct GroundHazards {
    /// `room -> hazards(room)`, `Validity::Pure`, never invalidated.
    memo: Derived<Facet, Hazards>,
}

impl GroundHazards {
    /// An empty memo.
    pub fn new() -> Self {
        Self::default()
    }

    /// The hazards at `room`: the held value on a hit, else `compute()`,
    /// held from then on. The value returned on a miss IS the value held, so
    /// a later hit returns the identical `f64`s.
    pub fn hazards_or_insert_with(&mut self, room: &Facet, compute: impl FnOnce() -> Hazards) -> Hazards {
        if let Some(h) = self.memo.get(room) {
            return *h;
        }
        let h = compute();
        self.memo.insert(room.clone(), h);
        h
    }

    /// Rooms held.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize { self.memo.len() }
    /// Whether nothing is held.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool { self.memo.is_empty() }
    /// Reads served without computing, ever.
    /// type-audit: bare-ok(count: return)
    pub fn hits(&self) -> u64 { self.memo.hits() }
    /// Reads that computed, ever — the field samples taken.
    /// type-audit: bare-ok(count: return)
    pub fn misses(&self) -> u64 { self.memo.misses() }
    /// Drop every held room (chaos eviction; unobservable by construction).
    pub fn evict_all(&mut self) { self.memo.evict_all() }
}

/// The memo behind interior mutability, so `&self` terrain readers can fill
/// it — the `OwnedFolds` shape (The Pawl, spec §2.2).
pub type OwnedGround = std::cell::RefCell<GroundHazards>; // lexicon: std::cell::RefCell is the standard library's interior-mutability type — not a place at all
```

Check whether `Derived::get` on a miss increments `misses` (read `kernel/src/derived.rs:110-130`); if it does, `misses()` already counts field samples and nothing more is needed. If it does not, count them in `hazards_or_insert_with` yourself.

- [ ] **Step 4: Thread it through `LocaleTerrain`** in `liveness.rs`:

Add the field after `cache`:
```rust
    /// The session-owned room memo (The Detent, spec §2.1), if the caller
    /// has one: `hazards` reads and fills it. `None` is byte-identical to
    /// `Some` — the memo holds exactly the values the field blend returns.
    ground: Option<&'a crate::ground::OwnedGround>,
```
Initialise `ground: None` in `with_fields` and in `new` (read both constructors). Add the builder immediately after `with_fields`:
```rust
    /// [`Self::with_fields`] plus the session's room memo (The Detent).
    /// Additive: every existing construction site is unchanged and reads
    /// the field unmemoised.
    pub fn with_ground(mut self, ground: &'a crate::ground::OwnedGround) -> Self {
        self.ground = Some(ground);
        self
    }
```
Rewrite `hazards` so the blend is a closure and the memo, if present, owns the call:
```rust
    fn hazards(&self, room: &Facet) -> Hazards {
        let compute = || {
            let (uncanny, heat, cold) = self
                .ctx
                .hazards_at_cached(room, self.cache)
                .unwrap_or((0.0, 0.0, 0.0));
            let predator = self
                .predator
                .and_then(|field| self.ctx.blend_at_cached(room, field, self.cache))
                .unwrap_or(0.0);
            Hazards { uncanny, heat, cold, predator }
        };
        match self.ground {
            // One guard, dropped before anything else runs: `compute` never
            // re-enters this memo, so the borrow cannot nest.
            Some(ground) => ground.borrow_mut().hazards_or_insert_with(room, compute),
            None => compute(),
        }
    }
```
Keep the two existing comments on the blend (the-Bane and the-Quarry lines) inside the closure.

- [ ] **Step 5: The two-terrain test** in `tests/suite/the_detent.rs` (spec rule 3). It must fail against a SHARED memo first: write it so the assertion is about the memo's ownership, and check the failing shape by temporarily sharing one memo across both terrains before writing the passing form.

```rust
#[test]
fn a_room_memo_belongs_to_one_predator_field_and_a_second_field_gets_its_own() {
    use hornvale_vessel::ground::{GroundHazards, OwnedGround};
    let world = common::build(42).expect("seed 42 builds");
    let (session, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 starts");
    let ctx = LocaleContext::build(&world).expect("ctx");
    // A room where the predator field is non-zero: search the flagship's
    // neighbourhood through the session's own predator-bearing terrain.
    let with_field = session.terrain_for_tests();
    let probe = session
        .bodies()
        .iter()
        .flat_map(|b| std::iter::once(b.home.clone()).chain(b.home.neighbors()))
        .find(|r| with_field.hazards(r).predator > 0.0)
        .expect("some room near the roster carries predator pressure — the field is non-zero on the flagship since The Quarry");
    let without = LocaleTerrain::with_fields(&ctx, None, None, None, None, None);
    let field_only = with_field.hazards(&probe);
    let bare = without.hazards(&probe);
    assert!(field_only.predator > bare.predator, "the two terrains must disagree on this room, or the test cannot see sharing");

    // Ownership: each terrain its own memo -> each answers as itself.
    let g1: OwnedGround = OwnedGround::new(GroundHazards::new());
    let g2: OwnedGround = OwnedGround::new(GroundHazards::new());
    let t1 = session.terrain_for_tests().with_ground(&g1);
    let t2 = LocaleTerrain::with_fields(&ctx, None, None, None, None, None).with_ground(&g2);
    assert_eq!(t1.hazards(&probe), field_only);
    assert_eq!(t2.hazards(&probe), bare);
    assert_eq!((g1.borrow().misses(), g2.borrow().misses()), (1, 1));

    // The refused shape, demonstrated: one memo handed to both terrains makes
    // the second terrain answer with the first's field. This is the aliasing
    // the ownership rule forbids, shown rather than assumed.
    let shared: OwnedGround = OwnedGround::new(GroundHazards::new());
    let s1 = session.terrain_for_tests().with_ground(&shared);
    let s2 = LocaleTerrain::with_fields(&ctx, None, None, None, None, None).with_ground(&shared);
    let _ = s1.hazards(&probe);
    assert_eq!(s2.hazards(&probe), field_only, "a shared memo aliases — which is why a memo is owned by one (context, field)");
}
```

`Session::terrain_for_tests` does not exist: add it in Task 4's session edits as a `pub fn terrain_for_tests(&self) -> LocaleTerrain<'_>` that returns exactly what `terrain_here` returns (read `session.rs:6215`), with a doc saying it is the test seam for the terrain a session reads. Until Task 4 lands, this test does not compile — so write it in Task 4, not here. (Kept in this task's text so the reader sees what Step 4's builder is for.)

- [ ] **Step 6: Chaos eviction on the memo**, in `the_detent.rs` (compiles now — needs no session): at every read of the H5 bench shape's probe, `evict_all` between reads and assert the `HazardMemory` is unchanged. Write it as the bench-shape helper's twin: run `bench_shape(42, 20, 10)` (a cheap shape), build a `GroundHazards`, and for each of 30 reads alternate `evict_all()` on and off; compare every `HazardMemory` to the first. Assert the memo's `misses()` grew across evictions (denominator: eviction happened) and that `len()` after the last un-evicted read equals the number of distinct rooms and neighbours the probe's read touched (print it).

- [ ] **Step 7: fmt, clippy, type-audit report, gate, commit**

```bash
cargo fmt && cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add windows/vessel/src/ground.rs windows/vessel/src/lib.rs windows/vessel/src/liveness.rs windows/vessel/tests/suite/the_detent.rs docs/audits/type-audit-report.md
git commit -F- <<'EOF'
feat(vessel): The Detent Task 3 -- GroundHazards, the room memo LocaleTerrain reads

Claude-Session: https://claude.ai/code/session_01VZbMTxSHFDJLtmyAUVztbc
EOF
```

---

### Task 4: Thread the memo into the session, the lab and the benches; H5's first assertion goes red then green

**Files:**
- Modify: `windows/vessel/src/session.rs` — the field block near line 966 (`folds`), `start`'s initialiser near line 1817, the six `LocaleTerrain::with_fields(` sites (lines ~2135, ~2738, ~6215, ~7331, ~8371, and any the grep finds), and the accessor block near line 2767.
- Modify: `windows/lab/src/health.rs:240-262`
- Modify: `windows/vessel/examples/session_length_scaling.rs:1139, 1181, 1240, 1264` and `agent_scaling.rs:381-418`
- Modify: `windows/vessel/tests/suite/the_detent.rs`
- Modify: `docs/audits/type-audit-report.md` (regenerate)

**Interfaces:**
- Produces on `Session`: `pub fn terrain_for_tests(&self) -> LocaleTerrain<'_>`, `pub fn resident_ground_len(&self) -> usize`, `pub fn resident_ground_hits(&self) -> u64`, `pub fn resident_ground_misses(&self) -> u64` (each with a `type-audit:` tag).
- Bench and lab: one `OwnedGround` per run, in scope with `folds`.

- [ ] **Step 1: Write the red assertion first.** In `the_detent.rs`'s H5 witness, `probe_counts` builds its terrain with `LocaleTerrain::with_fields(...)`; change `bench_shape` to own `pub ground: OwnedGround` (created beside `folds`) and to build every tick's terrain with `.with_ground(&shape.ground)`; make `probe_counts` build its terrain with `.with_ground(&shape.ground)` too, and record `misses` before and after each read. Add to the witness:

```rust
    // H5, first clause (spec §4): field samples. A second fresh-memo read
    // in the same tick samples the field ZERO times once the memo exists.
    assert_eq!(counts.warm_samples, 0, "H5: a repeated read must take no field samples");
    assert_eq!(counts.second_fresh_samples, 0, "H5: a second FRESH-memo read must take no field samples either");
```
where `ProbeCounts` gains `warm_samples: u64` and `second_fresh_samples: u64` (misses deltas), and `probe_counts` makes a THIRD call with another fresh `PrimaryAfraidMemo` to measure the second one. Run: the two new assertions FAIL on this tree? No — with the memo threaded in `bench_shape` they pass immediately, so the RED must be taken BEFORE threading: run the witness once with `bench_shape` still building terrains WITHOUT `.with_ground` (misses read from a memo nothing fills → the assertion cannot see samples). So instead the red is taken on `hazards_calls()`: before threading, record and print `fresh_hazards` (22,302) and `warm_hazards` (1,089) — those ARE the field samples on a tree with no memo. Write both assertions, run once with threading, and put in the report the pre-threading counts from Task 1's output as the red. This is the one place the plan accepts Task 1's printed numbers as the red rather than a failing assertion, because the assertion's subject (memo misses) does not exist on the pre-fix tree.

- [ ] **Step 2: The whole-tick assertion**, red on the pre-memo count, green after: in `bench_shape`, record per tick `ground.borrow().misses()` deltas as `samples_per_tick` beside `hazards_per_tick`. Add:

```rust
    let last_samples = *shape.samples_per_tick.last().expect("ticks ran");
    println!("whole tick {H5_TICKS}: {last_samples} field samples against {last_tick} hazards() calls");
    assert!(last_samples * 10 <= last_tick, "H5: the tick's field samples must be at most a tenth of its hazards() calls ({last_samples} vs {last_tick})");
    assert!(last_samples <= 4_469, "H5: at most 4,469 field samples in tick 60 (from 44,694)");
```

- [ ] **Step 3: Session.** Add the field (doc it beside `folds`, same reasoning: session-lived, interior mutability for `&self` readers, discardable):
```rust
    /// The session-lived room memo (The Detent, spec §2.1): what the terrain
    /// determines about a room, held for the session and read by every
    /// `LocaleTerrain` this session builds. World-derived, never serialized,
    /// discardable at any instant. One per `(LocaleContext, predator field)`,
    /// which this session owns both of.
    ground: crate::ground::OwnedGround,
```
Initialise in `start`: `ground: crate::ground::OwnedGround::new(crate::ground::GroundHazards::new()),`. At every `LocaleTerrain::with_fields(` site in `session.rs`, append `.with_ground(&self.ground)` to the expression (`grep -n 'LocaleTerrain::with_fields(' windows/vessel/src/session.rs` — do all of them; the count today is six). Add the accessors beside `resident_alarm_replays`:
```rust
    /// The terrain this session reads, for tests that need the same one.
    pub fn terrain_for_tests(&self) -> LocaleTerrain<'_> { self.terrain_here() }
    /// Rooms the session's room memo holds.
    /// type-audit: bare-ok(count: return)
    pub fn resident_ground_len(&self) -> usize { self.ground.borrow().len() }
    /// Room-memo reads served without a field sample, ever.
    /// type-audit: bare-ok(count: return)
    pub fn resident_ground_hits(&self) -> u64 { self.ground.borrow().hits() }
    /// Room-memo reads that sampled the field, ever.
    /// type-audit: bare-ok(count: return)
    pub fn resident_ground_misses(&self) -> u64 { self.ground.borrow().misses() }
```

- [ ] **Step 4: The lab and the benches.** `health.rs` `run_simulation`: create `let ground = hornvale_vessel::ground::OwnedGround::new(hornvale_vessel::ground::GroundHazards::new());` beside its `folds`/`mesh_memo`, and append `.with_ground(&ground)` to its `with_fields` call (line ~260). `session_length_scaling.rs`: same beside `folds` (line ~1139); append `.with_ground(&ground)` to the per-tick terrain (~1181), `probe_terrain` (~1240) and `fatigue_terrain` (~1264). `agent_scaling.rs`: same in `run_rung` beside `folds` (~393), append to the terrain at ~418. **Do not change the hazard probe's fresh-memo-per-call shape** — H4's comparability depends on it.

- [ ] **Step 5: The two-terrain test from Task 3 Step 5** now compiles: add it to `the_detent.rs`, run it, and record whether the "refused shape" clause actually observes aliasing (it must; if `s2.hazards(&probe)` equals `bare`, the memo is not being read — stop).

- [ ] **Step 6: Run everything that touches these files**: `cargo test -p hornvale-vessel --test suite -- the_detent resident_folds ledger_hash_witness --nocapture`, `cargo test -p hornvale-lab`. The two hash constants must still pass (byte-identity). Then `make gate-commit`.

- [ ] **Step 7: Regenerate the type-audit report, commit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git add windows/vessel/src/session.rs windows/lab/src/health.rs windows/vessel/examples/session_length_scaling.rs windows/vessel/examples/agent_scaling.rs windows/vessel/tests/suite/the_detent.rs docs/audits/type-audit-report.md
git commit -F- <<'EOF'
feat(vessel): The Detent Task 4 -- the session, the lab and the benches own one room memo; H5 field samples

Claude-Session: https://claude.ai/code/session_01VZbMTxSHFDJLtmyAUVztbc
EOF
```

**Stage 2 closes here** (spec rule 1: re-count). Controller: push, stage gate, absorb main if it moved, re-record constants main-first if anything on main changed a walk.

---

### Task 5: `FrighteningGround` — the per-entity verdict index

**Files:**
- Modify: `windows/vessel/src/resident.rs` (new type after `SustenanceMemo`; a field in `ResidentFolds`; one accessor; unit tests in the file's `#[cfg(test)]` module if it has one, else a new one at the bottom)
- Modify: `docs/audits/type-audit-report.md` (regenerate)

**Interfaces:**
- Consumes: `Trail::of`, `Trail::prefix_len`.
- Produces:

```rust
pub struct FrighteningGround { /* private: BTreeMap<EntityId, GroundEntry> */ }
impl FrighteningGround {
    /// Judge every sighting after the entity's cursor through `judge`, hold
    /// the verdicts, move the cursor to the trail's end. Returns how many
    /// rooms were judged this call.
    /// type-audit: bare-ok(count: return)
    pub fn advance(&mut self, entity: EntityId, trail: &[(WorldTime, Facet)], judge: &mut dyn FnMut(&Facet) -> bool) -> u64;
    /// The frightening rooms first visited at or before `t`, ascending by first visit.
    pub fn frightening_at(&self, entity: EntityId, t: WorldTime) -> &[(WorldTime, Facet)];
    /// The verdict already held for `room`, if any.
    /// type-audit: bare-ok(flag: return)
    pub fn verdict(&self, entity: EntityId, room: &Facet) -> Option<bool>;
    /// Rooms judged for `entity`, both verdicts.
    /// type-audit: bare-ok(count: return)
    pub fn judged(&self, entity: EntityId) -> usize;
    /// Every entity's judged-room count summed — M1's entry count.
    /// type-audit: bare-ok(count: return)
    pub fn entries(&self) -> usize;
}
impl ResidentFolds {
    pub fn latest_visit_trail_and_ground(&mut self, ledger: &Ledger) -> (&LatestVisit, &Trail, &mut FrighteningGround, &mut ReadWitness);
}
```

- [ ] **Step 1: Failing unit tests** (in `resident.rs`'s test module; build trails by hand as `Vec<(WorldTime, Facet)>` — see how `resident_folds.rs`'s `hand_built_upto` makes rooms and instants, and copy its `room()` helper shape):

```rust
    #[test]
    fn advance_judges_each_room_once_and_holds_the_verdict() {
        let e = EntityId::new(7).expect("id");
        let t = |d: i64| WorldTime::from_ticks(d * WorldTime::TICKS_PER_STD_DAY);
        let trail = vec![(t(1), room(0, &[1])), (t(2), room(0, &[2])), (t(3), room(0, &[1]))];
        let mut g = FrighteningGround::default();
        let mut asked = Vec::new();
        let judged = g.advance(e, &trail, &mut |r| { asked.push(r.clone()); *r == room(0, &[2]) });
        assert_eq!(judged, 2, "two distinct rooms, the revisit is not re-asked");
        assert_eq!(asked.len(), 2);
        assert_eq!(g.advance(e, &trail, &mut |_| panic!("nothing new to judge")), 0);
        assert_eq!(g.frightening_at(e, t(3)), &[(t(2), room(0, &[2]))]);
        assert_eq!(g.frightening_at(e, t(1)), &[] as &[(WorldTime, Facet)], "at t(1) the frightening room was not yet first-visited");
        assert_eq!(g.verdict(e, &room(0, &[1])), Some(false));
        assert_eq!(g.verdict(e, &room(0, &[9])), None);
        assert_eq!((g.judged(e), g.entries()), (2, 2));
    }

    #[test]
    fn a_later_advance_over_a_longer_trail_only_judges_the_new_rooms() {
        let e = EntityId::new(8).expect("id");
        let t = |d: i64| WorldTime::from_ticks(d * WorldTime::TICKS_PER_STD_DAY);
        let mut trail = vec![(t(1), room(0, &[1]))];
        let mut g = FrighteningGround::default();
        g.advance(e, &trail, &mut |_| true);
        trail.push((t(2), room(0, &[1])));
        trail.push((t(3), room(0, &[5])));
        let judged = g.advance(e, &trail, &mut |r| *r == room(0, &[5]));
        assert_eq!(judged, 1);
        assert_eq!(g.frightening_at(e, t(3)).len(), 2);
    }

    #[test]
    fn discarding_the_index_and_rebuilding_gives_the_same_prefixes() {
        let e = EntityId::new(9).expect("id");
        let t = |d: i64| WorldTime::from_ticks(d * WorldTime::TICKS_PER_STD_DAY);
        let trail: Vec<(WorldTime, Facet)> = (1..=12).map(|i| (t(i), room(0, &[(i % 5) as u8]))).collect();
        let judge = |r: &Facet| r.path().last().copied().unwrap_or(0) % 2 == 1;
        let mut whole = FrighteningGround::default();
        whole.advance(e, &trail, &mut { let j = judge; move |r| j(r) });
        for cut in 1..=12 {
            let mut fresh = FrighteningGround::default();
            fresh.advance(e, &trail[..cut], &mut { let j = judge; move |r| j(r) });
            fresh.advance(e, &trail, &mut { let j = judge; move |r| j(r) });
            assert_eq!(fresh.frightening_at(e, t(12)), whole.frightening_at(e, t(12)), "cut at {cut}");
        }
    }
```
Check `Facet`'s accessor for its path (`path()` or similar) in `kernel/src/room.rs` and adjust the judge; the point is a verdict that varies by room.

- [ ] **Step 2: Run to see them fail.** `cargo test -p hornvale-vessel resident::` — FAIL: type missing.

- [ ] **Step 3: Implement**, after `SustenanceMemo`:

```rust
/// One entity's held verdicts about the ground it has stood on (The Detent,
/// spec §2.2).
#[derive(Debug, Default)]
struct GroundEntry {
    /// Trail entries already judged; the cursor.
    consumed: usize,
    /// Rooms judged frightening, ascending by FIRST visit, one entry per room.
    frightening: Vec<(WorldTime, Facet)>,
    /// Every room judged, with its verdict.
    judged: BTreeMap<Facet, bool>,
}

/// A read-side index of the frightening verdict per `(entity, room)` — the
/// answer `threat_field(room, niche) × mettle ≥ DANGER_ACT` gives, which over
/// a fixed terrain is a constant per pair. Advanced from the entity's
/// [`Trail`] by a consumed-prefix cursor (the [`MemoPartition`] shape); the
/// predicate is the CALLER's, applied at read, so `LedgerFold::absorb` still
/// sees only facts (The Pawl's `Alarm`-is-not-a-tenant ruling, kept). A
/// pure function of `(ledger prefix, terrain)`; discardable at any instant.
///
/// `frightening` is ordered by first visit so that "the frightening rooms
/// at a past `t`" is a `partition_point` prefix — the lab's waking-instant
/// reads need exactly that (spec §2.2).
#[derive(Debug, Default)]
pub struct FrighteningGround {
    by_entity: BTreeMap<EntityId, GroundEntry>,
}

impl FrighteningGround {
    /// Judge the sightings after the cursor, hold the verdicts, move the
    /// cursor to the trail's end. `judge` is asked ONCE per new room.
    /// type-audit: bare-ok(count: return)
    pub fn advance(&mut self, entity: EntityId, trail: &[(WorldTime, Facet)], judge: &mut dyn FnMut(&Facet) -> bool) -> u64 {
        let entry = self.by_entity.entry(entity).or_default();
        let mut judged = 0_u64;
        for (day, room) in &trail[entry.consumed.min(trail.len())..] {
            if entry.judged.contains_key(room) {
                continue;
            }
            let verdict = judge(room);
            entry.judged.insert(room.clone(), verdict);
            judged += 1;
            if verdict {
                // The trail is ascending by (day, room), so this is the
                // room's first visit and the list stays ascending by first
                // visit without a sort.
                entry.frightening.push((*day, room.clone()));
            }
        }
        entry.consumed = trail.len();
        judged
    }

    /// The frightening rooms first visited at or before `t`.
    pub fn frightening_at(&self, entity: EntityId, t: WorldTime) -> &[(WorldTime, Facet)] {
        match self.by_entity.get(&entity) {
            Some(e) => {
                let n = e.frightening.partition_point(|(d, _)| *d <= t);
                &e.frightening[..n]
            }
            None => &[],
        }
    }

    /// The verdict already held for `room`, if any.
    /// type-audit: bare-ok(flag: return)
    pub fn verdict(&self, entity: EntityId, room: &Facet) -> Option<bool> {
        self.by_entity.get(&entity).and_then(|e| e.judged.get(room).copied())
    }

    /// Rooms judged for `entity`, both verdicts.
    /// type-audit: bare-ok(count: return)
    pub fn judged(&self, entity: EntityId) -> usize {
        self.by_entity.get(&entity).map_or(0, |e| e.judged.len())
    }

    /// Every entity's judged rooms, summed — M1's entry count.
    /// type-audit: bare-ok(count: return)
    pub fn entries(&self) -> usize {
        self.by_entity.values().map(|e| e.judged.len()).sum()
    }
}
```

Add `frightening_ground: FrighteningGround,` to `ResidentFolds` (it is NOT a `Folded`; it does not advance in `advance()` — it advances at read through its own cursor, like `sustenance_memo`), and the accessor beside `latest_visit_and_witness`:

```rust
    /// The visit lists, the trail, the verdict index and the witness — the
    /// hazard path's read (The Detent).
    pub fn latest_visit_trail_and_ground(&mut self, ledger: &Ledger) -> (&LatestVisit, &Trail, &mut FrighteningGround, &mut ReadWitness) {
        self.advance(ledger);
        (self.latest_visit.state(), self.trail.state(), &mut self.frightening_ground, &mut self.witness)
    }
```
Also a read-only `pub fn frightening_ground(&self) -> &FrighteningGround` for M1.

**The trail-order claim in `advance` is load-bearing**: `Trail` is ascending by `(day, room)`, so the first occurrence of a room in trail order IS its earliest day. Assert it in a unit test with a trail whose rooms interleave (room A day 1, room B day 2, room A day 3) — `frightening_at` must carry A at day 1, not 3.

- [ ] **Step 4: Run, fmt, clippy, type-audit report, gate, commit.**

```bash
git add windows/vessel/src/resident.rs docs/audits/type-audit-report.md
git commit -F- <<'EOF'
feat(vessel): The Detent Task 5 -- FrighteningGround, the per-entity verdict index in the store

Claude-Session: https://claude.ai/code/session_01VZbMTxSHFDJLtmyAUVztbc
EOF
```

---

### Task 6: The scan and the emitter-free read over the index; H5's second reading and H6

**Files:**
- Modify: `windows/vessel/src/liveness.rs` — `build_emitter_scan` (~1241-1330), `hazard_memory_memo` (~1507-1660)
- Create: `windows/vessel/src/liveness_tests/emitter_scan.rs`, wired from `liveness.rs` by `#[cfg(test)] #[path = "liveness_tests/emitter_scan.rs"] mod emitter_scan_tests;`
- Modify: `windows/vessel/tests/suite/the_detent.rs`

**Interfaces:**
- Consumes: `FrighteningGround::{advance, frightening_at}`, `ResidentFolds::latest_visit_trail_and_ground`, `threat_field`, `mettle_factor`, `DANGER_ACT`, `feels_frightening`.
- Produces: the same `EmitterScan` and `HazardMemory` values as before, byte for byte — the oracle in `emitter_scan.rs` is the proof.

- [ ] **Step 1: Preserve the oracle FIRST.** Before touching either function, copy the CURRENT bodies of `build_emitter_scan`'s pass 1+2 and of `hazard_memory_memo`'s emitter-free loop into `liveness_tests/emitter_scan.rs` as `fn scan_oracle(roster, ledger, folds, terrain, t) -> (Vec<bool> /* is_emitter */, BTreeSet<Facet> /* alarm_source_rooms */)` and `fn emitter_free_oracle(latest: &BTreeMap<Facet, WorldTime>, npc, terrain) -> BTreeSet<Facet>`, verbatim except for the witness calls. The module is in-crate (`super::*` sees `threat_field`, `mettle_factor`, `DANGER_ACT`, `feels_frightening`, `EmitterScan`), which is why it lives under `src/` and not `tests/suite/` — `pub(crate)` is invisible to an integration test crate, so the registry row's "move the tests to `tests/suite/resident_folds.rs`" cannot be done as written; this is the correction (ledger it).

- [ ] **Step 2: Write the failing FOLD-equals-SCAN test** in `emitter_scan.rs`: on the H5 bench shape at ticks 5, 10, 15 (build a small in-crate copy of `bench_shape` — 10 agents, 15 ticks, no counting), for every roster member and for `t` = the tick's instant AND `t` = an instant one day earlier (the lab's past-instant shape), assert `build_emitter_scan(...)`'s `emitters` entity list and `alarm_source_rooms` equal the oracle's, and `hazard_memory_memo(...)`'s `shunned`/`dread` equal the oracle's on the emitter-free path. Floors: at least one member is an emitter on some tick OR the test also runs on `EMITTER_SEED`'s derived roster (it must — seed 42's derived roster has no emitter; seed 6's has one); at least one room judged frightening somewhere (print counts). This test passes TODAY (the oracle is today's code) — that is expected; it goes red only if Step 3 breaks equality, which is its job.

- [ ] **Step 3: Rewrite `build_emitter_scan`** pass 1+2 over the index:

```rust
    // Pass 1+2, over the verdict index (The Detent, spec §2.3): each
    // member's rooms first visited since its last read are judged ONCE
    // through the caller's terrain (which carries the room memo), and the
    // held verdicts answer `ever` and the halo. `home` is judged through the
    // same memo. One guard, dropped before the predicate ran? No — the
    // predicate needs no store, so the guard is held across the advance;
    // `judge` reads only `terrain` and the member, never the store.
    let mut alarm_source_rooms: std::collections::BTreeSet<Facet> = std::collections::BTreeSet::new();
    let mut is_emitter: Vec<bool> = Vec::with_capacity(roster.len());
    {
        let mut store = folds.borrow_mut();
        let (_, trail, ground, witness) = store.latest_visit_trail_and_ground(ledger);
        for m in roster {
            let mettle = mettle_factor(m.boldness);
            let mut frightening = |room: &Facet| threat_field(room, &m.threat_niche, terrain) * mettle >= DANGER_ACT;
            let judged = ground.advance(m.entity, trail.of(m.entity), &mut frightening);
            witness.note_ground_judged(judged);
            let mut ever = frightening(&m.home);
            if ever { note_halo(&mut alarm_source_rooms, &m.home); }
            for (_, p) in ground.frightening_at(m.entity, t) {
                ever = true;
                note_halo(&mut alarm_source_rooms, p);
            }
            is_emitter.push(ever);
        }
    }
```
with `fn note_halo(set: &mut BTreeSet<Facet>, p: &Facet)` inserting `p` and `p.neighbors()`. **Two things to check against the oracle, and the test in Step 2 is what checks them:** (a) the old pass judged `rooms_at(m, t)` — rooms whose FIRST visit is ≤ t — and `frightening_at(m, t)` is exactly that set's frightening subset; (b) the old code judged `home` every call; judging it through the memo is one lookup. The home verdict is NOT held in the index (it is not a sighting); it goes through the room memo like any read.

`ReadWitness` gains `note_ground_judged(&mut self, n: u64)` and `ground_judged(&self) -> u64` (type-audit `bare-ok(count)`), the H6 instrument.

- [ ] **Step 4: Rewrite the emitter-free path** of `hazard_memory_memo`:

```rust
    if scan.emitters.is_empty() {
        // The emitter-free common case, over the index (The Detent, spec
        // §2.3): with terrain static, "some visit at or before t was
        // frightening" is "first visit ≤ t and the room is frightening",
        // which is exactly the index's prefix at `t`. Same set, same order
        // (a BTreeSet sorts on insert), same verdict per room.
        let mut store = folds.borrow_mut();
        let (_, trail, ground, witness) = store.latest_visit_trail_and_ground(ledger);
        let mettle = mettle_factor(npc.boldness);
        let mut frightening = |room: &Facet| feels_frightening(threat_field(room, &npc.threat_niche, terrain), 0.0, npc.boldness);
        let judged = ground.advance(npc.entity, trail.of(npc.entity), &mut frightening);
        witness.note_ground_judged(judged);
        let _ = mettle;
        for (_, room) in ground.frightening_at(npc.entity, t) {
            mem.shunned.insert(room.clone());
        }
        return mem;
    }
```
**Stop and read before writing this:** the scan's predicate is `threat_field × mettle ≥ DANGER_ACT` and the read's is `feels_frightening(threat, 0.0, boldness)` = `(threat × mettle).clamp(0,1) ≥ DANGER_ACT`. They agree unless `threat × mettle > 1.0` clamps — and `DANGER_ACT` is 0.3, so a clamped value is still ≥ 0.3 and the verdicts agree on every input. Both callers therefore share ONE index per entity with ONE predicate: use `feels_frightening(..., 0.0, ...)` in both places, and write that agreement as a unit test in `emitter_scan.rs` over a sweep of `threat ∈ [0, 3]` and `boldness ∈ [0, 1]` (the two functions agree at every point, including the clamp region). If they ever disagree at some point, the index needs two predicates and the plan is wrong — stop and ledger it.

The `latest` map is still needed for the EMITTER path (untouched), so keep its computation but move it below the emitter-free early return only if the witness's `note_hazard` is still recorded FIRST for every call — read the existing comment ("Spec §3 rule 6's witness is taken in the same guard, and FIRST"); keep that ordering: witness first, then the early return, then `latest` for the emitter path.

- [ ] **Step 5: Run the oracle test** (`cargo test -p hornvale-vessel emitter_scan_tests`) — PASS means equality held; then run the H5 witness and the two hash constants; then add H5's second reading and H6 to the witness:

```rust
    // H5, second reading (Stage 3): not just field samples — the CALLS are
    // gone too, because the index answers without asking the terrain.
    assert_eq!(counts.warm_hazards, 0, "H5: a repeated read makes no hazards() calls at all");
    assert!(last_tick <= 4_469, "H5: the whole tick makes at most 4,469 hazards() calls (from 44,694)");
    // H6: the scan's work per tick is O(new sightings). Judged rooms per
    // tick across ticks 15 -> 60 grow strictly slower than the roster's
    // distinct rooms; the margin is printed.
    let judged_15 = shape.judged_per_tick[14];
    let judged_60 = shape.judged_per_tick[59];
    let rooms_15 = shape.distinct_rooms_per_tick[14];
    let rooms_60 = shape.distinct_rooms_per_tick[59];
    println!("H6: judged/tick {judged_15} -> {judged_60}; distinct rooms {rooms_15} -> {rooms_60}");
    assert!(judged_15 > 0 && judged_60 > 0, "H6 denominator: the scan judged rooms on both ticks");
    assert!((judged_60 as f64 / judged_15 as f64) < (rooms_60 as f64 / rooms_15 as f64), "H6: judged rooms per tick must grow slower than the roster's distinct rooms");
```
`bench_shape` records `judged_per_tick` (delta of `witness.ground_judged()`) and `distinct_rooms_per_tick` (sum of `visits.of(e).len()` per tick). Run: green. Record the red: check out the Task 5 commit's `liveness.rs` (`git stash` is shared across worktrees — do NOT use it; use `git show <task-5-sha>:windows/vessel/src/liveness.rs > /tmp/pre.rs` and swap it in by copy, run the witness, restore by `git checkout -- windows/vessel/src/liveness.rs`, and REBUILD before trusting the next green — a restored source with a stale binary is a known trap). Paste the red output in the report.

- [ ] **Step 6: fmt, clippy, gate, commit**

```bash
git add windows/vessel/src/liveness.rs windows/vessel/src/liveness_tests/emitter_scan.rs windows/vessel/src/resident.rs windows/vessel/tests/suite/the_detent.rs docs/audits/type-audit-report.md
git commit -F- <<'EOF'
feat(vessel): The Detent Task 6 -- the emitter scan and the emitter-free read advance over the verdict index

Claude-Session: https://claude.ai/code/session_01VZbMTxSHFDJLtmyAUVztbc
EOF
```

---

### Task 7: The rides — the in-file `EmitterScan` tests move out, and the dead memo goes

**Files:**
- Modify: `windows/vessel/src/liveness.rs` — delete `believed_hazard_memo` (~1476-1490) and every mention of it in doc comments (`grep -n believed_hazard_memo windows/vessel/src/*.rs windows/vessel/tests/suite/*.rs`); move the existing in-file `EmitterScan` equivalence tests (find them: `grep -n 'fn .*emitter_scan\|EmitterScan' windows/vessel/src/liveness.rs` inside `#[cfg(test)] mod tests`) into `liveness_tests/emitter_scan.rs`.
- Modify: `docs/audits/type-audit-report.md` (regenerate — a `pub fn` was deleted)

- [ ] **Step 1:** Delete `believed_hazard_memo`; build; fix any doc link (`[`believed_hazard_memo`]`) the compiler or `cargo doc` flags — replace with `hazard_memory_memo(...).shunned`.
- [ ] **Step 2:** Move the tests; each moved test keeps its name exactly (the subfloor roster selects by name). Run `cargo test -p hornvale-vessel emitter_scan_tests` and `cargo test -p hornvale-vessel liveness::tests` — the moved tests run from the new module, and `liveness.rs`'s line count drops (record before/after with `wc -l`).
- [ ] **Step 3:** fmt, clippy, type-audit report, gate, commit:

```bash
git commit -F- <<'EOF'
refactor(vessel): The Detent Task 7 -- EmitterScan tests leave liveness.rs; believed_hazard_memo deleted

Claude-Session: https://claude.ai/code/session_01VZbMTxSHFDJLtmyAUVztbc
EOF
```

---

### Task 8: Rules 2, 4, 5 and M1 — the numbers the readout and stage 4 need

**Files:**
- Modify: `windows/vessel/src/resident.rs` — `ReadWitness` gains `note_emitter_timeline_copied(&mut self, entries: u64)` / `emitter_timeline_copied(&self) -> u64`
- Modify: `windows/vessel/src/liveness.rs` — `build_emitter_scan` pass 3 records the copied length
- Modify: `windows/vessel/src/session.rs` — accessors `resident_ground_judged_entries(&self) -> usize`, `resident_emitter_timeline_copied(&self) -> u64`
- Modify: `windows/vessel/tests/suite/the_detent.rs` (append three witnesses)
- Modify: `windows/vessel/examples/session_length_scaling.rs` — M1 columns per band: `ground_len`, `ground_bytes`, `index_entries`, `index_bytes`

- [ ] **Step 1: Rule 2, the affect replay on the seed-6 possession shape.** A witness that starts a session on `EMITTER_SEED`, runs `run_emitter_script`, and prints per tick: `resident_alarm_replays()` delta, `resident_ground_misses()` delta, `resident_ground_hits()` delta, and the hazards() calls made INSIDE replays — count those by reading `resident_ground_hits()+misses()` before and after `session.hazard_memories()` while the replay counter moves. Assert `replays > 0` (denominator — this is the shape that reaches it). Print the share: calls attributable to replays over all calls in the hazard read. No threshold: the number goes in the ledger and decides rule 2's branch.

- [ ] **Step 2: Rule 4, the timeline copy on seed 6's 50-agent roster.** In `build_emitter_scan` pass 3, after `trail.of(m.entity)[..upto].to_vec()`, record `witness.note_emitter_timeline_copied(upto as u64)`. Witness: `bench_shape(EMITTER_SEED, 60, 50)` and print entries copied per tick at ticks 15/30/60 and the roster's trail sum; assert `with_emitters > 0` (denominator). No threshold; the branch is decided in the ledger (spec rule 4: under 1% of the tick's allocation or flat with history → keep the copy).

- [ ] **Step 3: Rule 5, past-instant reads on the lab shape.** Extend `resident_folds.rs`'s existing rule-6 witness's shape-2 block (the `run_simulation` shape, `affect_of_memo_occupied` at a waking instant) — do NOT rename that test; write a NEW test in `the_detent.rs` that runs the same construction for 10 ticks with `WorldTime::from_std_days((day - 1.0) + waking_offset)` instants exactly as `health.rs:169-186` does, and asserts `resident_hazards_in_the_past() > 0` (the index's prefix machinery has a production caller) and that every `hazard_memory_memo` result at a past instant equals the same call served by a FRESH store (discard-and-rebuild at a past instant). Print the count.

- [ ] **Step 4: M1.** Add to `session_length_scaling.rs`'s `Band` struct and its printed table: `ground_len` (`ground.borrow().len()`), `ground_bytes` (`ground_len × (size_of::<Facet>() + size_of::<Hazards>())` — state the formula in the column doc; it is an estimate of held data, not an allocator measurement), `index_entries` (`folds.borrow().frightening_ground().entries()`), `index_bytes` (`entries × (size_of::<Facet>() + size_of::<bool>() + size_of::<(WorldTime, Facet)>())`, same caveat). Run the bench once in `--release` on any box to confirm the columns print (M1 is a count; the timing columns are Task 9's).

- [ ] **Step 5:** fmt, clippy, type-audit report, gate, commit:

```bash
git commit -F- <<'EOF'
test(vessel): The Detent Task 8 -- the affect-replay share, the timeline copy, past-instant reads, and M1 bytes

Claude-Session: https://claude.ai/code/session_01VZbMTxSHFDJLtmyAUVztbc
EOF
```

**Stage 3 closes here.** Controller: push, stage gate, absorb main, re-record constants main-first if a walk moved.

---

### Task 9: The readout, and the close

**Files:**
- Modify: `docs/superpowers/specs/2026-09-02-the-detent-design.md` (append §11)
- Modify: `windows/vessel/tests/suite/the_detent.rs` (retire the constants per decision 0541: replace the `assert_eq!` against each constant with a two-fresh-runs-agree assertion plus the printed hash, keep every floor, and move the constants and their positive control into the file's doc as a dated record — `ledger_hash_witness.rs`'s doc is the template)
- Modify: `book/src/frontier/idea-registry.md` (the corrected row gains the readout's numbers; `TOOL-emitter-scan-tests-out-of-liveness` and `TOOL-believed-hazard-memo-is-dead` flip to `shipped` with a Where cell naming this campaign and the `#[path]` correction)
- Create: `docs/decisions/0626-*.md`, `0627-*.md`, `0628-*.md` (spec §8), `book/src/chronicle/the-detent.md`, `docs/retrospectives/the-detent.md`; the `book/src/SUMMARY.md` chronicle entry and `docs/retrospectives/README.md` index line; `book/src/open-questions.md` re-score.

- [ ] **Step 1: The control.** A detached worktree of the campaign's merge base (`git merge-base origin/main campaign/the-detent`) with the three examples' constants verified identical (`AGENTS = 50`, `TICKS = 200`, `FOLD_REPS = 200`, `BAND = 20`) and NO `ground` (grep confirms). Build both trees `--release`.
- [ ] **Step 2: The runs**, interleaved control/campaign, on a quiet box (1-minute load ≤ 10 before and after or the run is set aside and listed): `session_length_scaling` ≥ 3 valid runs a side; `agent_scaling` ≥ 2 paired runs; `fold_depth_sweep` once a side. Record every run with its loads in §11, including the discarded.
- [ ] **Step 3: §11**, in The Pawl's §12 layout: the quiet-box rule as applied; the decisive H4 column with `k`, r², elasticity, `C`, final-band µs/call per run and side; the whole tick and attribution; the verdict table against §4 (H4 a/b — both against the frozen 73–97 ms and the same-box control, named; H2 c; H3; H5; H6; the falsifier with the crossover); the level from `agent_scaling`; `fold_depth_sweep` as the no-regression control; M1's bytes on all three shapes; rules 2 and 4's branches as decided from Task 8's numbers.
- [ ] **Step 4: Retire the constants** (decision 0541) and re-run the witness file.
- [ ] **Step 5: The close artifacts** — invoke the `closing-a-campaign` skill and follow it: decisions 0626–0628 (0629 if rule 2 fired), chronicle, retrospective, registry flips, Confidence Gradient re-score, followups, the null census (`make sluice-census`), then the G6 package and `make sluice`.

---

## Self-review

**Spec coverage.** §2.1 → Tasks 3, 4. §2.2 → Task 5. §2.3 → Task 6. §2.4 → Task 7. §2.5 (per-shape counts) → Tasks 1, 8. §3 rule 1 → Task 4 (re-count); rule 2 → Task 8 Step 1; rule 3 → Task 3 Step 5 / Task 4 Step 5; rule 4 → Task 8 Step 2; rule 5 → Task 8 Step 3; rule 6 → controller at every stage boundary; rule 7 → Task 9 (`session_cost.rs` re-pin) — **gap:** no task names `turn_budget.rs`, which is The Rack's and unmerged; the controller checks at absorption. §4 H4/H2c/H3/falsifier → Task 9; H5 → Tasks 4, 6; H6 → Task 6; M1 → Task 8 Step 4 and Task 9. §5 hash witnesses → Task 2 (mint) and Task 9 (retire); FOLD-equals-SCAN → Task 6 Step 2; chaos eviction → Task 3 Step 6; the day-contract note → `ground.rs` module doc. §6 stage gates → after Tasks 2, 4, 8. §8 decisions → Task 9. §9 registry → Task 9.

**Placeholders.** None found; every code step carries code. Two places tell the implementer to READ before writing (the `Facet` constructor; `Derived::get`'s miss accounting) rather than assert a fact this plan did not verify.

**Type consistency.** `OwnedGround` = `RefCell<GroundHazards>` throughout; `with_ground(self, &'a OwnedGround)`; `hazards_or_insert_with(&mut self, &Facet, impl FnOnce() -> Hazards) -> Hazards`; `FrighteningGround::{advance, frightening_at, verdict, judged, entries}`; `ResidentFolds::latest_visit_trail_and_ground` returns `(&LatestVisit, &Trail, &mut FrighteningGround, &mut ReadWitness)` in Tasks 5 and 6 alike; `ReadWitness::{note_ground_judged, ground_judged, note_emitter_timeline_copied, emitter_timeline_copied}`; `Session::{terrain_for_tests, resident_ground_len, resident_ground_hits, resident_ground_misses, resident_ground_judged_entries, resident_emitter_timeline_copied}`.

**One correction made while reviewing.** Task 4 Step 1's red-then-green cannot use a failing assertion, because the assertion's subject (memo misses) does not exist before the memo does; the plan says so and takes Task 1's printed count as the red for that clause only. Task 6's red IS a failing assertion (calls), taken by swapping in the pre-Task-6 `liveness.rs` by copy, never by `git stash`.
