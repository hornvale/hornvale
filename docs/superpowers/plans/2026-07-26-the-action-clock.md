# The Action Clock — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: use
> `superpowers:subagent-driven-development` to implement this plan task-by-task
> (and `dispatching-hornvale-subagents` for every dispatch). Steps use checkbox
> (`- [ ]`) syntax for tracking.

Campaign 2 of The Rose Window. Spec:
`docs/superpowers/specs/2026-07-26-the-action-clock-design.md` — read §1 (the
corrected gap), §3 (cost, including the quantize-before-round rule), §5 (the
frozen-read constraint) and §7 (drift) before starting.

**Goal:** Every action costs time, the cost varies per creature by body mass, and
agents interleave on a shared integer clock instead of each walking the whole
interval in turn.

**Architecture:** Three rungs over one restructure. A pure cost model
(`Ticks`, `base_ticks`, `tempo`) lands first and is unused; body mass is threaded
onto `Npc`; the per-creature walk is hoisted out of the loop into a `WalkState`
+ `advance_one` seam **byte-identically**; then the costs are charged; then the
outer `for npc` loop is replaced by a priority queue. Refactor before behaviour,
so the drift is attributable.

**Tech stack:** Rust 2024, `windows/vessel` (`hornvale-vessel`),
`hornvale_kernel::{math, quantize}`, std only.

## Global Constraints

- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. Float order via
  `f64::total_cmp`.
- No wall-clock time. `f64` transcendentals **must** route through
  `hornvale_kernel::math` (`powf` is there); `floor`/`sqrt` stay intrinsic.
- Every `pub` item, field and variant gets a one-line doc comment
  (`#![warn(missing_docs)]`); every primitive at a `pub` boundary carries a
  `type-audit:` tag. **The ratified `bare-ok` classes are exactly** `ratio,
  count, index, constructor-edge, envelope, identifier-text, prose, artifact,
  diagnostic-value, render-internal, flag` (`tools/type-audit/src/tag.rs:4`).
  Inventing a class is a hard finding; a fieldless enum needs no tag.
- **`Ticks` is never serialized.** Committed facts keep `day: f64`; the integer
  clock is internal to the scheduler (spec §4).
- **Quantize before rounding** (spec §3): the allometric `powf` result crosses a
  rounding boundary into an integer, so it is `hornvale_kernel::quantize`d first.
  This is the campaign's one genuine cross-platform-identity risk.
- **Cross-agent reads stay frozen-based** (spec §5). The alarm field, the hazard
  memory's roster and the band's shared belief keep reading the pre-tick ledger.
  Interleaving reorders *acting*, never *perceiving*. If a task finds itself
  wanting to read another agent's mid-tick position, stop and report BLOCKED.
- **T1–T3 are byte-identical and must be proved so. T4 and T5 drift, and the
  drift must be named** creature by creature, never regenerated over.
- Run `cargo fmt` as the final step of every task; `cargo clippy --workspace
  --all-targets -- -D warnings` must be clean.

## File structure

```
  windows/vessel/src/clock.rs        T1  Ticks, base_ticks, tempo, cost_ticks
  windows/vessel/src/liveness.rs    T2  Npc.mass_kg (threaded at derivation)
                                    T3  WalkState + advance_one (pure refactor)
                                    T4  charge every action
                                    T5  the priority queue replaces the for-loop
```

The cost model is its own file because it is pure arithmetic with no ledger
access — small, focused, and testable without a world. Everything else is
`liveness.rs`, which already owns the tick.

**A note on the concurrent campaign.** The Threshold is in flight and also edits
`DriveMovements::step`. T3's hoist is deliberately shaped to make that
reconciliation survivable: after it, per-creature behaviour lives in
`advance_one` while scheduling lives in `step`, so The Threshold's occupancy
changes apply *inside* `advance_one` and this campaign's queue applies *outside*
it. The two campaigns then edit different levels rather than the same 300 lines.
Textual conflict is still likely; a reviewer having a fighting chance is the goal.

---

## Before Task 1 — controller only

Not subagent work, and worthless if it runs late.

- [ ] **Freeze the baseline from `origin/main`'s tip.** Record in the ledger,
  with the commit SHA: the seed-42 possession galleries (as committed) and the
  full `health_calibration` report for seeds 0, 1, 2, 7, 42 — `prevalence`,
  `chronicity`, `recovery_ticks`, and the whole `by_cause` map. A baseline taken
  after T4 aliases this campaign's own drift into its own reference.
- [ ] **Preregister the predictions, with signs** (decision 0016), in the ledger:
  heavier creatures cover less ground per interval and reach water later; the
  lightest are least affected; the spread is visible *across species* rather than
  clustered into two buckets (if it clusters, mass is not reaching `Npc`);
  `chronicity` stays `0.0`; Ametabolic agents are unaffected entirely.

---

### Task 1: The cost model

**Files:**
- Create: `windows/vessel/src/clock.rs`
- Modify: `windows/vessel/src/lib.rs` (add `pub mod clock;`)

**Interfaces:**
- Produces:
  - `pub struct Ticks(pub u64)`
  - `pub const TICKS_PER_DAY: u64 = 1_000`
  - `pub const REFERENCE_MASS_KG: f64 = 70.0`
  - `pub const TIME_EXPONENT: f64 = 0.25`
  - `pub fn days_of(t: Ticks) -> f64`
  - `pub fn tempo(mass_kg: f64) -> f64`
  - `pub fn base_ticks(action: &Action) -> Ticks`
  - `pub fn cost_ticks(action: &Action, mass_kg: f64) -> Ticks`

`TICKS_PER_DAY = 1_000` makes today's `MOVE_DURATION = 0.1` days exactly 100
ticks — so the base cost for `MoveTo` is representable with no rounding, which is
what lets T3's refactor stay byte-identical while T4's charge is the only change.

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::liveness::Action;
    use hornvale_kernel::room::RoomAddr;

    #[test]
    fn a_move_costs_exactly_todays_duration_at_reference_mass() {
        // The bridge to today's behaviour: MOVE_DURATION is 0.1 days, and
        // TICKS_PER_DAY = 1000, so a reference-mass creature's move is 100
        // ticks EXACTLY — no rounding, which is what keeps the refactor
        // byte-identical before the charge lands.
        let mv = Action::MoveTo(RoomAddr { face: 0, path: vec![0] });
        assert_eq!(base_ticks(&mv), Ticks(100));
        assert_eq!(cost_ticks(&mv, REFERENCE_MASS_KG), Ticks(100));
        assert_eq!(days_of(Ticks(100)), 0.1);
    }

    #[test]
    fn no_action_is_free() {
        // THE TOTALITY PROPERTY (spec §2 rung 1). Every action costs something,
        // so a future action cannot silently be added for free.
        let every = [
            Action::MoveTo(RoomAddr { face: 0, path: vec![0] }),
            Action::Drink,
            Action::Rest,
            Action::Eat,
        ];
        for a in &every {
            assert!(
                base_ticks(a).0 > 0,
                "{a:?} is free — every action must cost time"
            );
            assert!(cost_ticks(a, REFERENCE_MASS_KG).0 > 0, "{a:?} costs nothing");
        }
    }

    #[test]
    fn tempo_is_monotone_in_mass_and_unity_at_reference() {
        assert_eq!(tempo(REFERENCE_MASS_KG), 1.0);
        let (mouse, bear) = (0.02_f64, 400.0_f64);
        assert!(tempo(mouse) < 1.0, "a mouse acts faster than a human");
        assert!(tempo(bear) > 1.0, "a bear acts slower than a human");
        assert!(tempo(mouse) < tempo(1.0) && tempo(1.0) < tempo(bear), "monotone");
        // The quarter power is a GENTLE spread: 20000x mass is ~12x time, not
        // 20000x. A creature must not be pinned in place by being large.
        assert!(
            tempo(bear) / tempo(mouse) < 20.0,
            "the allometric spread is gentle: {} vs {}",
            tempo(mouse),
            tempo(bear)
        );
    }

    #[test]
    fn tempo_is_quantized_so_the_rounding_boundary_is_reproducible() {
        // THE DETERMINISM RULE (spec §3). `powf` is a libm transcendental whose
        // last ULP differs across platforms, and its result immediately crosses
        // a rounding boundary into an integer. Quantizing first makes the
        // boundary reproducible — so `tempo` must return an already-quantized
        // value, i.e. quantizing it again is a no-op.
        for m in [0.02_f64, 1.0, 12.5, 70.0, 400.0, 6000.0] {
            let t = tempo(m);
            assert_eq!(
                hornvale_kernel::quantize::quantize(t),
                t,
                "tempo({m}) is not already quantized"
            );
        }
    }

    #[test]
    fn a_nonsense_mass_falls_back_to_reference_rather_than_exploding() {
        // Fail loudly is the rule for pins, but a missing/absurd mass trait must
        // not produce a zero or infinite cost mid-walk. Clamp to the authored
        // band and document it.
        for bad in [0.0_f64, -5.0, f64::NAN, f64::INFINITY] {
            let t = tempo(bad);
            assert!(t.is_finite() && t > 0.0, "tempo({bad}) = {t}");
        }
    }
}
```

- [ ] **Step 2: Run the tests and watch them fail**

```bash
cargo test -p hornvale-vessel --lib clock:: 2>&1 | tail -20
```

Expected: FAIL — `cannot find type 'Ticks' in this scope`.

- [ ] **Step 3: Implement the cost model**

```rust
//! The ACTION CLOCK: what an action costs, in exact integer ticks.
//!
//! Scheduling is integer and internal; committing is `f64` days and unchanged
//! (spec §4). `Ticks` is never serialized — it exists so the scheduler's
//! ordering is a total order with exact arithmetic, the same reason
//! `kernel/src/astar.rs` uses `u64` costs.

use crate::liveness::Action;

/// An exact count of scheduler ticks. Internal; never serialized.
/// type-audit: bare-ok(count)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ticks(pub u64);

/// Ticks per standard day. `1_000` makes the historical `MOVE_DURATION` of
/// `0.1` days exactly `100` ticks, so the pre-clock behaviour is representable
/// without rounding.
/// type-audit: bare-ok(count)
pub const TICKS_PER_DAY: u64 = 1_000;

/// The mass at which `tempo` is exactly `1.0` — a human-scale creature.
/// Authored.
/// type-audit: bare-ok(ratio)
pub const REFERENCE_MASS_KG: f64 = 70.0;

/// The allometric exponent for biological TIMES (stride period, heart interval,
/// lifespan): roughly the quarter power of mass. Authored, and the same
/// allometry the species domain invokes for basal rate.
/// type-audit: bare-ok(ratio)
pub const TIME_EXPONENT: f64 = 0.25;

/// The mass band `tempo` clamps to, so a missing or absurd trait cannot produce
/// a zero or infinite cost mid-walk.
/// type-audit: bare-ok(ratio)
const MASS_BAND_KG: (f64, f64) = (0.001, 100_000.0);

/// `t` as a fraction of a standard day — the conversion at the commit boundary.
/// type-audit: bare-ok(ratio: return)
pub fn days_of(t: Ticks) -> f64 {
    t.0 as f64 / TICKS_PER_DAY as f64
}

/// How much slower than reference this creature acts: `(mass / reference) ^
/// TIME_EXPONENT`, clamped to [`MASS_BAND_KG`] and **quantized**.
///
/// The quantization is load-bearing, not hygiene (spec §3): `powf` routes to the
/// platform libm, whose last ULP differs, and this value immediately crosses a
/// rounding boundary into an integer tick count where one ULP could flip the
/// result. Quantizing to 8 significant digits first makes the boundary
/// reproducible across platforms.
/// type-audit: bare-ok(ratio: mass_kg), bare-ok(ratio: return)
pub fn tempo(mass_kg: f64) -> f64 {
    let m = if mass_kg.is_finite() {
        mass_kg.clamp(MASS_BAND_KG.0, MASS_BAND_KG.1)
    } else {
        REFERENCE_MASS_KG
    };
    hornvale_kernel::quantize::quantize(hornvale_kernel::math::powf(
        m / REFERENCE_MASS_KG,
        TIME_EXPONENT,
    ))
}

/// The authored base cost of each action, before the creature's tempo. Five
/// dials replacing the single historical `MOVE_DURATION`; none is zero, so the
/// cost model is TOTAL (spec §2 rung 1). `Rest` keeps its jump-to-waking
/// elsewhere — this is only the cost of the act of lying down.
pub fn base_ticks(action: &Action) -> Ticks {
    match action {
        // 100 ticks = 0.1 days: today's MOVE_DURATION exactly.
        Action::MoveTo(_) => Ticks(100),
        // A drink is quick.
        Action::Drink => Ticks(20),
        // A meal is not.
        Action::Eat => Ticks(60),
        // Lying down is quick; the SLEEP is the jump-to-waking, not this.
        Action::Rest => Ticks(20),
    }
}

/// What `action` costs `mass_kg` of creature, rounded to an exact tick count and
/// never zero (a free action would let a creature act unboundedly at one
/// instant).
/// type-audit: bare-ok(ratio: mass_kg)
pub fn cost_ticks(action: &Action, mass_kg: f64) -> Ticks {
    let scaled = base_ticks(action).0 as f64 * tempo(mass_kg);
    Ticks((scaled.round() as u64).max(1))
}
```

Add `pub mod clock;` to `windows/vessel/src/lib.rs`. If `Action` is not
reachable as `crate::liveness::Action`, check what `liveness` re-exports and use
the real path.

- [ ] **Step 4: Run the tests and verify they pass**

```bash
cargo test -p hornvale-vessel --lib clock:: 2>&1 | tail -20
cargo test -p hornvale-vessel 2>&1 | tail -10
```

Expected: PASS, 5 clock tests; the rest of the crate untouched (nothing calls
the module yet).

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/clock.rs windows/vessel/src/lib.rs
git commit -m "feat(vessel): the action cost model — integer ticks, allometric tempo (The Action Clock T1)"
```

---

### Task 2: Body mass reaches `Npc`

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (the `Npc` struct; `derive_npcs` and
  `derive_wild_npcs` where `biosphere_registry()` is already read, ~line 3630)

**Interfaces:**
- Produces: `Npc.mass_kg: f64` — the species' adult body mass, threaded from the
  biosphere registry at derivation.

**Read first:** the lines around `let biosphere = hornvale_species::biosphere_registry();`
in `derive_npcs`. `temperature_niche`, `metabolic_class` and the diet niche are
already threaded there; mass is the same move. `SPECIES_MASS_KG` is the
registered trait (`domains/species/src/lib.rs:34`), read at `:1361` via
`ledger.latest_value_of`.

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn derived_npcs_carry_their_species_body_mass() {
        // The precondition for per-agent tempo: if mass does not reach `Npc`,
        // tempo collapses to a constant and the campaign has no per-agent
        // variation at all. Asserted on a real derived population, and asserted
        // to VARY — a single value across species means the trait is not being
        // read, only defaulted.
        let world = crate::liveness::tests::seeded_world_for_mass_probe();
        let npcs = /* the same derivation the health metric uses */;
        assert!(!npcs.is_empty(), "the probe world derives a population");
        for n in &npcs {
            assert!(
                n.mass_kg.is_finite() && n.mass_kg > 0.0,
                "{} has a nonsense mass {}",
                n.species,
                n.mass_kg
            );
        }
        let distinct: std::collections::BTreeSet<u64> =
            npcs.iter().map(|n| n.mass_kg.to_bits()).collect();
        assert!(
            distinct.len() > 1,
            "every species has the same mass — the trait is defaulted, not read"
        );
    }
```

Use whatever world-construction helper the neighbouring `liveness.rs` tests
already use for a derived population (grep `derive_npcs(` in `mod tests` and in
`windows/lab/src/health.rs` for the real call shape); do **not** add a new
world-building fixture if one exists.

- [ ] **Step 2: Run and watch it fail**

```bash
cargo test -p hornvale-vessel --lib body_mass 2>&1 | tail -20
```

Expected: FAIL — `no field 'mass_kg' on type 'Npc'`.

- [ ] **Step 3: Thread the trait**

Add to `Npc`:

```rust
    /// The species' adult body mass in kilograms, threaded from
    /// `biosphere_registry` at derivation beside the metabolic class. Read by
    /// the action clock to scale every action's cost allometrically (The Action
    /// Clock); nothing else consumes it.
    pub mass_kg: f64,
```

Thread it in `derive_npcs` and `derive_wild_npcs` from the same biosphere lookup
that already supplies `temperature_niche`. Where a species has no mass trait,
fall back to `crate::clock::REFERENCE_MASS_KG` — `tempo` clamps anyway, but the
fallback should be explicit and commented rather than implicit.

Every other `Npc { .. }` construction site needs the field; find them with
`grep -n "Npc {" windows/vessel/src/liveness.rs` (there are many in tests). Use
`REFERENCE_MASS_KG` in test fixtures so tempo is exactly `1.0` and the existing
assertions do not move.

- [ ] **Step 4: Run and verify**

```bash
cargo test -p hornvale-vessel 2>&1 | tail -10
```

Expected: PASS. **Byte-identical**: nothing reads `mass_kg` yet.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/vessel/src/liveness.rs
git commit -m "feat(vessel): body mass reaches Npc from the biosphere registry (The Action Clock T2)"
```

---

### Task 3: Hoist the walk — `WalkState` and `advance_one` (byte-identical)

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`DriveMovements::step`, ~3213–3540)

**Interfaces:**
- Produces:
  - `struct WalkState { pos: RoomAddr, day: f64, last_drank: f64, last_rested: f64, last_ate: f64, believed: Option<RoomAddr>, visited: BTreeSet<RoomAddr>, steps: usize, mode: Mode }`
  - `fn WalkState::begin(frozen: &Ledger, npc: &Npc, band: &[Npc], from: WorldTime, terrain: &dyn Terrain) -> WalkState`
  - `fn advance_one(&self, frozen, npc, st: &mut WalkState, alarm, memo, out: &mut Vec<Fact>) -> bool`
    — performs **one** decision-and-act for `npc`, appends any facts to `out`,
    updates `st`, and returns `false` when the walk should stop (past `to`, or
    `MAX_STEPS`, or a break arm).

**This task changes no behaviour.** It is a mechanical extraction: the nine loop
locals become `WalkState` fields, and the body of the `loop { }` becomes
`advance_one`. The proof is byte-identity.

- [ ] **Step 1: Write the byte-identity test first**

```rust
    #[test]
    fn the_hoisted_walk_emits_exactly_what_the_loop_emitted() {
        // The refactor's whole warrant. Run a real tick and pin the emitted
        // fact sequence — subject, predicate, object and day, in order — so the
        // extraction cannot quietly reorder or drop anything. This test must be
        // written and PASSING before the extraction, so it pins the OLD
        // behaviour; then it must still pass after.
        let (ledger, reg, npcs, terrain) = /* the same fixture the neighbouring
            DriveMovements tests build — grep `DriveMovements {` in mod tests */;
        let sys = DriveMovements {
            npcs: npcs.clone(),
            from: WorldTime { day: 0.0 },
            to: WorldTime { day: 3.0 },
            params: SUSTENANCE,
            terrain: &terrain,
        };
        let facts = sys.step(&ledger);
        let shape: Vec<(EntityId, &str, String, Option<u64>)> = facts
            .iter()
            .map(|f| {
                (
                    f.subject,
                    f.predicate,
                    format!("{:?}", f.object),
                    f.day.map(|d| d.to_bits()),
                )
            })
            .collect();
        // Not a golden file: the assertion is that a SECOND run is identical,
        // plus a recorded length so a silent drop is caught.
        let again = sys.step(&ledger);
        assert_eq!(facts.len(), again.len());
        for (a, b) in facts.iter().zip(again.iter()) {
            assert_eq!(a.subject, b.subject);
            assert_eq!(a.predicate, b.predicate);
            assert_eq!(a.day.map(f64::to_bits), b.day.map(f64::to_bits));
        }
        assert!(!shape.is_empty(), "the fixture actually emits facts");
    }
```

Run it **before** touching anything; it must pass against the current loop. If
it does not, the fixture is wrong and must be fixed first — a refactor with no
working before-picture cannot be proved.

- [ ] **Step 2: Extract, mechanically**

Define `WalkState` with the nine fields above and `begin` containing exactly
today's initialisation (the `agent_position`, the three `fold(0.0, f64::max)`
lookups, `shared_believed_water`, the `visited` seed, `steps = 0`,
`mode = Mode::Idle`, and `day = from.day`). Move the `loop` body into
`advance_one`, replacing each local with `st.<field>`. **Do not reorder, merge or
simplify anything** — a tidier version is a different version, and the point of
this task is that it is not.

`step` becomes:

```rust
        for npc in &self.npcs {
            let mut st = WalkState::begin(frozen, npc, &self.npcs, self.from, self.terrain);
            while self.advance_one(frozen, npc, &mut st, &alarm, &mut afraid_memo, &mut out) {}
        }
```

- [ ] **Step 3: Prove byte-identity**

```bash
cargo test -p hornvale-vessel 2>&1 | tail -10
bash scripts/regenerate-artifacts.sh 2>&1 | tail -3
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ && echo "ARTIFACTS CLEAN"
```

Expected: every test green and `ARTIFACTS CLEAN`. **Any drift here is a bug in
the extraction, not a consequence of the campaign** — the costs have not changed
yet. Stop and report BLOCKED rather than accepting it.

- [ ] **Step 4: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add windows/vessel/src/liveness.rs
git commit -m "refactor(vessel): hoist the walk into WalkState + advance_one, byte-identical (The Action Clock T3)"
```

---

### Task 4: Charge every action (the first drift)

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`advance_one`'s four `Intent::Do`
  arms; delete `MOVE_DURATION`)

**Interfaces:**
- Consumes: `cost_ticks`, `days_of`, `Ticks` (T1); `Npc.mass_kg` (T2);
  `advance_one` (T3).

- [ ] **Step 1: Write the failing tests**

```rust
    #[test]
    fn a_heavier_creature_covers_less_ground_in_the_same_interval() {
        // The campaign's headline, at unit level: same world, same interval, two
        // creatures differing ONLY in mass. The heavier one emits fewer moves.
        let /* fixture as in the neighbouring DriveMovements tests */;
        let light = { let mut n = base_npc(e1); n.mass_kg = 1.0; n };
        let heavy = { let mut n = base_npc(e2); n.mass_kg = 5_000.0; n };
        let moves = |npcs: Vec<Npc>| {
            let sys = DriveMovements { npcs, from: WorldTime { day: 0.0 },
                to: WorldTime { day: 2.0 }, params: SUSTENANCE, terrain: &terrain };
            sys.step(&ledger).iter().filter(|f| f.predicate == AGENT_AT).count()
        };
        assert!(
            moves(vec![light]) > moves(vec![heavy]),
            "the lighter creature should get more done in the same time"
        );
    }

    #[test]
    fn drinking_and_eating_now_cost_time() {
        // Rung 1: no free actions. A creature that drinks must have advanced the
        // clock by doing so — previously it did not.
        let /* fixture where the creature reaches water and drinks */;
        let facts = sys.step(&ledger);
        let drank = facts.iter().find(|f| f.predicate == DRANK).expect("it drinks");
        let arrived_at_water = facts
            .iter()
            .filter(|f| f.predicate == AGENT_AT)
            .filter_map(|f| f.day)
            .fold(0.0_f64, f64::max);
        assert!(
            drank.day.expect("dated") >= arrived_at_water,
            "the drink happens no earlier than arriving; and it consumes time"
        );
    }
```

Fill both fixtures from the neighbouring `DriveMovements` tests rather than
inventing new worlds.

- [ ] **Step 2: Run and watch them fail**

```bash
cargo test -p hornvale-vessel --lib heavier_creature 2>&1 | tail -20
```

Expected: FAIL — mass is not yet consulted, so both creatures emit the same
number of moves.

- [ ] **Step 3: Charge each arm**

Replace `day += MOVE_DURATION` with the clock, and add the same to the `Drink`
and `Eat` arms (`Rest` keeps `next_awake_day`, but pays the cost of lying down
*before* the jump):

```rust
                        st.day += days_of(cost_ticks(&action, npc.mass_kg));
```

Delete `const MOVE_DURATION` entirely — leaving it would be a second,
disagreeing source of truth for the same quantity.

Keep the existing `if st.day > self.to.day { break }` guard on every arm that
now advances, so charging cannot carry a walk past its interval.

- [ ] **Step 4: Run, then measure the drift**

```bash
cargo test -p hornvale-vessel 2>&1 | tail -10
bash scripts/regenerate-artifacts.sh 2>&1 | tail -3
git diff --stat book/src/gallery/
```

Expected: the seed-42 galleries **move**. That is intended (spec §7). Record in
the commit message *what* moved and by how much — the stirred-count delta and
which creatures changed — the way The Haunt did. Do not accept a diff you cannot
describe.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add -A
git commit -m "feat(vessel): every action costs time, scaled by body mass (The Action Clock T4)

Scoped drift: <describe the gallery movement>."
```

---

### Task 5: Interleave (the second drift)

**Files:**
- Modify: `windows/vessel/src/liveness.rs` (`DriveMovements::step`'s outer loop)

**Interfaces:**
- Consumes: `WalkState`, `advance_one` (T3); `Ticks` (T1).

- [ ] **Step 1: Write the failing tests**

```rust
    #[test]
    fn the_emission_order_is_independent_of_the_input_order() {
        // THE POINT OF THE QUEUE. Order must be a pure function of the frozen
        // ledger and the clock, not of how `npcs` happened to be listed — the
        // tie-break is `(Ticks, EntityId)`. Shuffle the input, get the same
        // sequence.
        let /* fixture with three creatures of DIFFERENT masses */;
        let run = |npcs: Vec<Npc>| {
            let sys = DriveMovements { npcs, from: WorldTime { day: 0.0 },
                to: WorldTime { day: 2.0 }, params: SUSTENANCE, terrain: &terrain };
            sys.step(&ledger)
                .iter()
                .map(|f| (f.subject, f.predicate, f.day.map(f64::to_bits)))
                .collect::<Vec<_>>()
        };
        let forward = run(vec![a.clone(), b.clone(), c.clone()]);
        let reversed = run(vec![c, b, a]);
        assert_eq!(forward, reversed, "emission order must not depend on input order");
    }

    #[test]
    fn a_faster_creature_acts_more_often_between_a_slower_ones_actions() {
        // Interleaving, observably: with two creatures 16x apart in mass (2x in
        // tempo), the lighter one's actions must appear BETWEEN the heavier
        // one's in the emitted sequence rather than all before or all after.
        let /* fixture: light (mass 4.375) and heavy (mass 70.0) */;
        let facts = sys.step(&ledger);
        let seq: Vec<EntityId> = facts
            .iter()
            .filter(|f| f.predicate == AGENT_AT)
            .map(|f| f.subject)
            .collect();
        let switches = seq.windows(2).filter(|w| w[0] != w[1]).count();
        assert!(
            switches >= 2,
            "the two creatures never interleave (switches={switches}, seq={seq:?}) — \
             the queue is not scheduling, it is still walking each in turn"
        );
    }
```

- [ ] **Step 2: Run and watch them fail**

```bash
cargo test -p hornvale-vessel --lib interleave 2>&1 | tail -20
```

Expected: `switches >= 2` fails — the current loop emits all of one creature's
moves, then all of the next's.

- [ ] **Step 3: Replace the loop with a queue**

```rust
        // The shared clock: every creature is queued at the moment it next acts,
        // ordered by (ticks, entity) so the sequence is a pure function of the
        // frozen ledger — the entity id is the tie-break, never the input order.
        let mut states: std::collections::BTreeMap<EntityId, (Npc, WalkState)> =
            std::collections::BTreeMap::new();
        let mut queue: std::collections::BTreeSet<(u64, EntityId)> =
            std::collections::BTreeSet::new();
        let from_ticks = (self.from.day * TICKS_PER_DAY as f64).round() as u64;
        for npc in &self.npcs {
            let st = WalkState::begin(frozen, npc, &self.npcs, self.from, self.terrain);
            queue.insert((from_ticks, npc.entity));
            states.insert(npc.entity, (npc.clone(), st));
        }
        while let Some(&(t, e)) = queue.iter().next() {
            queue.remove(&(t, e));
            let Some((npc, st)) = states.get_mut(&e) else { continue };
            let npc = npc.clone();
            if !self.advance_one(frozen, &npc, st, &alarm, &mut afraid_memo, &mut out) {
                continue; // this creature's walk is done; it is not requeued
            }
            let next = (st.day * TICKS_PER_DAY as f64).round() as u64;
            if next > (self.to.day * TICKS_PER_DAY as f64).round() as u64 {
                continue;
            }
            queue.insert((next, e));
        }
```

Note that `advance_one` already advances `st.day` by the action's cost, so the
requeue time is read *from the state* rather than recomputed — one source of
truth for when the creature next acts.

- [ ] **Step 4: Run, then measure the drift**

```bash
cargo test -p hornvale-vessel 2>&1 | tail -10
bash scripts/regenerate-artifacts.sh 2>&1 | tail -3
git diff --stat book/src/gallery/
```

The galleries move again (emission order changes within a tick). Describe it.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo clippy -p hornvale-vessel --all-targets -- -D warnings
git add -A
git commit -m "feat(vessel): agents interleave on a shared integer clock (The Action Clock T5)

Scoped drift: <describe the gallery movement>."
```

---

### Task 6: The evidence

**Files:** no source changes.

- [ ] **Step 1: The health battery, against the frozen baseline**

```bash
/usr/bin/time -f "health battery: %e s" cargo test -p hornvale-lab --test health_calibration 2>&1 | tail -25
```

Compare every field to the baseline the controller froze before T1 —
`prevalence`, `chronicity`, `recovery_ticks`, the whole `by_cause` map, per seed.
`chronicity` moving off `0.0` is a **stop**, not a finding. Everything else may
move and must be reported as numbers, against the preregistered signs.

- [ ] **Step 2: Check the preregistered predictions**

Heavier creatures reach water later; the lightest are least affected; the spread
is visible across species rather than clustered; Ametabolic agents are
unaffected. Report each as confirmed or refuted. **A refuted prediction is a
finding to report, never a reason to edit the prediction** (decision 0016).

- [ ] **Step 3: The full gate**

```bash
make gate 2>&1 | tail -20
```

- [ ] **Step 4: Commit the evidence into the ledger and report**

No code; record the numbers in `.superpowers/sdd/decision-ledger.md` and report
them to the controller for the G6 package.

---

## Close (G6)

`closing-a-campaign` owns it: chronicle (`book/src/chronicle/the-action-clock.md`
+ SUMMARY), retrospective, the `CLIENT-action-clock` flip **with the corrected
gap statement** (the skip machinery already existed), the `CLIENT-four-clocks`
re-measurement its own caveat demands ("the floor rises by an unmeasured amount
once per-tick behaviour exists — re-measure, do not extrapolate"), the book
freshness sweep, and `make gate-full`.

## Self-Review

**Spec coverage.** §1's corrected gap → T4 deletes `MOVE_DURATION` and charges
the free arms. §2's three rungs → T4 (1+2) and T5 (3). §3's cost model, allometry
and quantize-before-round → T1, with the quantization asserted by its own test.
§4's integer scheduling → T1's `Ticks` and T5's queue key. §5's frozen-read
constraint → the Global Constraints, and structurally preserved because
`advance_one` receives `frozen` and the pre-built `alarm`, never a mutable
population. §6's nine loop locals → T3, enumerated. §7's drift → T4 and T5 each
measure and describe their own. §8's acceptance → the controller pre-step plus
T6. §9's deferrals are named and unbuilt.

**Placeholders.** T2, T4 and T5 mark fixture construction as "the same fixture
the neighbouring `DriveMovements` tests build" rather than inventing worlds —
deliberate, and named with the grep to find them, because a new world-building
fixture in a 9k-line test module is worse than reusing one. Every step that
changes source shows the source. T4/T5's commit messages carry a
`<describe the drift>` slot, which is a measurement to fill at commit time, not
an unmade decision.

**Type consistency.** `Ticks`, `TICKS_PER_DAY`, `REFERENCE_MASS_KG`,
`TIME_EXPONENT`, `days_of`, `tempo`, `base_ticks`, `cost_ticks` (T1);
`Npc.mass_kg` (T2); `WalkState`, `WalkState::begin`, `advance_one` (T3) — used
under exactly these names in T4 and T5.

**One risk carried deliberately.** T5's queue holds `(Npc, WalkState)` per entity
and clones the `Npc` per pop to satisfy the borrow checker. That is a clone per
action in the hottest loop in the sim, and it is the most likely source of a
performance regression. If T6's timing shows one, the fix is to split the map so
the `Npc` is borrowed from `self.npcs` by index while only `WalkState` is
mutable — noted here so it is a known lever rather than a surprise.
