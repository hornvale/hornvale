# The Vestige Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Derive the underworld's **historical residue** — sealed wards, abandoned delvings, buried undercities, dormant gate-scars — by *joining* the settlement-abandonment history + terrain, read backward from the narrated past. A pure derived layer at the worldgen composition root.

**Architecture:** A new `windows/worldgen/src/vestige.rs`. **People-made** residue is derived from the committed occupation history (each `OccupationRecord` → a `Vestige`, its seal-state/valence/hazard read from alive-vs-dead + cause + age). **Pre-human** residue (gate-scars, natural seals) is derived from terrain's deep numinous zones via reused hash-noise. Both stack per-cell as a palimpsest. Residue exposes a derived `vestige_dread` field (the `predator_pressure` pattern) — the hazard-field wiring itself is deferred. Nothing mutates; nothing is committed.

**Tech Stack:** Rust (edition 2024), `hornvale-worldgen` (composition root; `GeneratedTerrain`, the ledger, the `deposit_of`/`predator_pressure` seams), `hornvale-history` (`OccupationRecord`, the `OCC_*` predicates), `hornvale-kernel` (`World`, `Ledger`, `CellMap`, `CellId`). No new dependencies.

## Global Constraints

- **No epoch / byte-identity:** no committed byte changes. `cli/tests/lens_purity.rs` stays green. Residue **reads the ledger read-only and commits NO new facts.**
- **Derived-historical provenance:** residue is a pure function of the seed via the narrated history + terrain; **narrated backward, never simulated forward** (the Lorenz discipline).
- **No metaphysics:** a gate-scar is a physical/historical wound with a `numinous` hazard + dread; the *entity* behind it stays reserved.
- **Determinism:** same seed + pins → byte-identical residue & census rows. No `HashMap`/`HashSet` (use `BTreeMap`/`Vec`); float sorts `total_cmp`; no wall-clock (history time is `f64` standard days).
- **Reads history ⇒ `BuildDepth::Full`** for any residue metric/extractor.
- **No new sequential stream draws** — pre-human placement reuses terrain's `features_noise_seed()` (hash-noise only). No new stream label needed.
- **Typed quantities + type-audit:** every new `pub`-boundary primitive carries a `type-audit:` tag (ratios/dread `bare-ok(ratio)`, days `bare-ok(diagnostic-value)`, bools `bare-ok(flag)`; enums none). CI default-deny.
- **Docs:** `#![warn(missing_docs)]`; `cargo fmt` final before commit.
- **Naming:** the worldgen type/fns are `Vestige`/`vestiges_at`/`vestige_dread` — NOT `residue_of` (which already exists as `hornvale_history::flesh::residue_of`, a different per-occupation archaeological-finds thing).

**Gate note at execution time:** `origin/main` carries the census-fixture calibration tests red (The Deep + The Lode metrics owed a census regen); The Vestige's new metrics add to that set until the out-of-band regen. Distinguish new failures from that known set (git-stash-verify on base if unsure).

**Verification:** scoped `cargo test -p hornvale-worldgen <name>`; byte-identity `cargo test -p hornvale --test lens_purity`; type-audit `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`; gate `make gate`.

---

### Task 1: Lift the occupation-reconstruction into a shared worldgen query

**Files:**
- Modify: `windows/worldgen/src/history_emit.rs` (add `pub fn occupation_records(world) -> Vec<OccupationRecord>` and `pub fn occupations_at(world, cell) -> Vec<OccupationRecord>`, reconstructing from the ledger — lift the logic from `windows/almanac/src/history.rs::record_of`/`layers_at`)
- Modify: `windows/worldgen/src/lib.rs` (re-export the two new fns)
- Modify: `windows/almanac/src/history.rs` (replace the private `record_of`/`layers_at` bodies with calls to the lifted worldgen fns — DRY, and it re-verifies the lift against the shipped almanac)
- Test: inline test in `history_emit.rs`

**Interfaces:**
- Consumes: `hornvale_history::{OccupationRecord, record::*, HISTORY_NOW, IS_OCCUPATION, OCC_SITE, OCC_FOUNDED, OCC_ENDED, OCC_PEOPLE, OCC_PEAK, OCC_TECH, OCC_FUNCTION, OCC_CAUSE, OCC_ENDED_BY, OCC_FOUNDED_FROM, OCC_NOTABILITY}`, `World`, `Ledger::{find, value_of, facts_about}`, `CellId`, `Value`.
- Produces: `worldgen::occupation_records(world: &World) -> Vec<OccupationRecord>` (all occupations, in commit order), `worldgen::occupations_at(world: &World, cell: CellId) -> Vec<OccupationRecord>` (occupations on a cell, oldest-founded first).

- [ ] **Step 1: Read the existing private reconstruction** — open `windows/almanac/src/history.rs:165-235` (`layers_at`, `record_of`). This is the working reconstruction (it decodes exactly how `history_emit` encoded each `OCC_*` fact — including how `Function`/`CauseOfEnd`/`TechHorizon`/`Notability` are serialized). Copy its decode logic verbatim into the new worldgen fns; do not re-derive the encoding by guessing.

- [ ] **Step 2: Write the failing test** — in `history_emit.rs` tests:

```rust
#[test]
fn occupation_records_reconstruct_from_the_ledger() {
    let world = /* build a Full seed-42 world via the crate's test helper */;
    let recs = occupation_records(&world);
    assert!(!recs.is_empty(), "seed 42 has occupations");
    // a reconstructed record round-trips its site + lifecycle
    let r = &recs[0];
    assert!(r.founded >= 0.0);
    // occupations_at groups by cell
    let at = occupations_at(&world, r.site);
    assert!(at.iter().any(|o| o.founded == r.founded));
}
```
(Use the worldgen test helper that builds a `BuildDepth::Full` world — grep the crate's tests for how `deposit_of`/`stratigraphy` tests build one.)

- [ ] **Step 3: Run red** → `cannot find function occupation_records`.

- [ ] **Step 4: Implement** — in `history_emit.rs`, port `record_of`/`layers_at` to `pub`:

```rust
/// Reconstruct every committed occupation from the ledger, in commit order.
/// (Lifted from the almanac's private reconstruction so both share one decoder.)
pub fn occupation_records(world: &World) -> Vec<OccupationRecord> {
    world
        .ledger
        .find(hornvale_history::IS_OCCUPATION)
        .map(|f| f.subject)
        .filter_map(|id| reconstruct_occupation(world, id))
        .collect()
}

/// Occupations on a cell, oldest-founded first (the palimpsest layers).
pub fn occupations_at(world: &World, cell: hornvale_kernel::CellId) -> Vec<OccupationRecord> {
    let mut v: Vec<_> = occupation_records(world)
        .into_iter()
        .filter(|o| o.site == cell)
        .collect();
    v.sort_by(|a, b| a.founded.total_cmp(&b.founded));
    v
}

// `reconstruct_occupation(world, id) -> Option<OccupationRecord>`: the verbatim
// port of almanac::history::record_of — decode each OCC_* fact off `id`.
```
Re-export both from `lib.rs`. Then update `almanac/src/history.rs` `layers_at`/`record_of` to delegate to `hornvale_worldgen::{occupations_at, ...}` (or keep `record_of` private but call the shared reconstructor) — confirm the almanac tests still pass.

- [ ] **Step 5: Run green** (`cargo test -p hornvale-worldgen occupation`, `cargo test -p hornvale-almanac`), fmt, type-audit, `lens_purity` green, commit.

```bash
git add windows/worldgen/src/history_emit.rs windows/worldgen/src/lib.rs windows/almanac/src/history.rs
git commit -m "refactor(worldgen,almanac): lift occupation-record reconstruction to a shared worldgen query"
```

---

### Task 2: The `Vestige` types + people-made derivation

**Files:**
- Create: `windows/worldgen/src/vestige.rs`
- Modify: `windows/worldgen/src/lib.rs` (`mod vestige; pub use`)
- Test: inline in `vestige.rs`

**Interfaces:**
- Consumes: `hornvale_history::record::{OccupationRecord, Function, CauseOfEnd, Notability}`, `KindId`.
- Produces: `VestigeKind`, `SealState`, `Valence`, `HazardKind`, `Vestige { kind, seal_state, valence, hazard, dread, warning_legibility, founded_day }`, `vestige_from_occupation(occ: &OccupationRecord, now: f64) -> Vestige`.

- [ ] **Step 1: Write the failing tests**:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_history::record::{CauseOfEnd, Function, Notability};

    fn occ(function: Function, ended: Option<f64>, cause: Option<CauseOfEnd>, notability: Notability) -> hornvale_history::record::OccupationRecord {
        // build a minimal OccupationRecord (match the real struct's fields)
    }

    #[test]
    fn a_living_ward_is_maintained_and_venerated() {
        let v = vestige_from_occupation(&occ(Function::Fort, None, None, Notability::Seat), 2000.0);
        assert_eq!(v.seal_state, SealState::Maintained);
        assert_eq!(v.valence, Valence::Venerated);
    }

    #[test]
    fn an_ancient_ruin_is_breached_and_forgotten_with_a_lost_warning() {
        let v = vestige_from_occupation(&occ(Function::Mine, Some(50.0), Some(CauseOfEnd::Fled), Notability::Common), 2000.0);
        assert_eq!(v.seal_state, SealState::Breached);
        assert_eq!(v.valence, Valence::Forgotten);
        assert!(v.warning_legibility < 0.2, "ancient warning decayed to near-illegible");
    }

    #[test]
    fn a_plague_end_yields_a_pestilent_hazard() {
        let v = vestige_from_occupation(&occ(Function::Cult, Some(1900.0), Some(CauseOfEnd::Plague), Notability::Common), 2000.0);
        assert_eq!(v.hazard, HazardKind::Pestilent);
    }
}
```
(Match `OccupationRecord`'s real fields in the `occ` helper — read `domains/history/src/record.rs:90-124`.)

- [ ] **Step 2: Run red.**

- [ ] **Step 3: Implement** — `vestige.rs`:

```rust
//! Subsurface historical residue — sealed wards, abandoned delvings, buried
//! ruins, gate-scars (The Vestige). A derived reading of the narrated past
//! (settlement-abandonment history + terrain): no live mutation, no committed
//! facts, no metaphysics — the door and its dread, not the entity.

use hornvale_history::record::{CauseOfEnd, Function, Notability, OccupationRecord};

/// What a residue site is, by maker → purpose.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VestigeKind {
    /// Dormant dimensional wound in the primordial deep (pre-human).
    GateScar,
    /// The deep sealed its own chamber (pre-human, no maker).
    NaturalSeal,
    /// An abandoned mine / exhausted delving.
    AbandonedDelving,
    /// A buried ruin or, at Seat scale, an undercity / necropolis.
    BuriedRuin,
    /// A custodial ward or tomb (a Fort/Cult site).
    SealedVault,
}

/// How intact the containment is, read from the keeper's fate.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SealState { Maintained, Lapsing, Breached }

/// Remembered-sacred vs forgotten-dreaded — the same axis as seal-state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Valence { Venerated, Forgotten }

/// The kind of danger the residue now poses (feeds the dread field).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HazardKind { Structural, ToxicGas, Pestilent, Flooded, Numinous, Cursed }

/// A located residue feature (one layer of a cell's palimpsest).
/// type-audit: bare-ok(ratio: dread), bare-ok(ratio: warning_legibility), bare-ok(diagnostic-value: founded_day)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vestige {
    pub kind: VestigeKind,
    pub seal_state: SealState,
    pub valence: Valence,
    pub hazard: HazardKind,
    /// Dread the site radiates, [0,1]: forgotten + breached is highest.
    pub dread: f64,
    /// How readable the old warning still is, [0,1]: decays fast with age.
    pub warning_legibility: f64,
    /// The historical day it was founded (people-made); None for pre-human.
    pub founded_day: Option<f64>,
}

/// A ruin older than this (days since it ended) has an all-but-lost warning.
const WARNING_HALF_LIFE_DAYS: f64 = 300.0;

/// Derive a people-made vestige from one occupation, as of `now` (days).
/// Read backward: no simulation.
/// type-audit: bare-ok(diagnostic-value: now)
pub fn vestige_from_occupation(occ: &OccupationRecord, now: f64) -> Vestige {
    let kind = match (occ.function, occ.notability) {
        (Function::Mine, _) => VestigeKind::AbandonedDelving,
        (Function::Fort, _) | (Function::Cult, _) => VestigeKind::SealedVault,
        (_, Notability::Seat) => VestigeKind::BuriedRuin, // undercity scale (see size, later)
        _ => VestigeKind::BuriedRuin,
    };
    let (seal_state, valence) = match occ.ended {
        None => (SealState::Maintained, Valence::Venerated), // a living keeper
        Some(end) => {
            let age = (now - end).max(0.0);
            if age < WARNING_HALF_LIFE_DAYS {
                (SealState::Lapsing, Valence::Forgotten)
            } else {
                (SealState::Breached, Valence::Forgotten)
            }
        }
    };
    let hazard = match occ.cause {
        Some(CauseOfEnd::Plague) => HazardKind::Pestilent,
        Some(CauseOfEnd::Burned) => HazardKind::Cursed,
        _ => match occ.function {
            Function::Mine => HazardKind::Structural,
            _ => HazardKind::Structural,
        },
    };
    // Warning decays fast (the fastest of the three rates); recent = legible.
    let warning_legibility = match occ.ended {
        None => 1.0,
        Some(end) => (-((now - end).max(0.0)) / WARNING_HALF_LIFE_DAYS).exp(),
    };
    // Dread: forgotten + breached + hazardous is highest; venerated is low.
    let base = match valence {
        Valence::Venerated => 0.1,
        Valence::Forgotten => 0.6,
    };
    let dread = (base + 0.4 * (1.0 - warning_legibility)).clamp(0.0, 1.0);
    Vestige {
        kind,
        seal_state,
        valence,
        hazard,
        dread,
        warning_legibility,
        founded_day: Some(occ.founded),
    }
}
```
Wire `mod vestige; pub use vestige::{Vestige, VestigeKind, SealState, Valence, HazardKind, vestige_from_occupation};` in `lib.rs`.

- [ ] **Step 4: Run green; fmt; type-audit; commit.**

```bash
git add windows/worldgen/src/vestige.rs windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): The Vestige — types + people-made residue from occupation history"
```

---

### Task 3: Pre-human residue + the per-cell palimpsest stack

**Files:** Modify `windows/worldgen/src/vestige.rs` (pre-human derivation + `vestiges_at`); `lib.rs` (`pub use`). Test inline.

**Interfaces:**
- Consumes: `occupations_at` (Task 1), `vestige_from_occupation` (Task 2), `GeneratedTerrain` (`column_at` → `BandKind::Underneath`, `crust_age_at`, `globe().features_noise_seed()`, `geosphere`), `crust::sphere_fbm01`.
- Produces: `vestiges_at(world: &World, terrain: &GeneratedTerrain, cell: CellId) -> Vec<Vestige>` (oldest first: pre-human deepest, then people layers), `prehuman_vestige(terrain, cell) -> Option<Vestige>`.

- [ ] **Step 1: Write the tests** — a deep, ancient, numinous cell yields a pre-human `GateScar` (`Numinous`, `founded_day: None`); a shallow young cell does not; `vestiges_at` returns pre-human-then-people order and is deterministic.

- [ ] **Step 2: Run red.**

- [ ] **Step 3: Implement** — `prehuman_vestige` gates a `GateScar`/`NaturalSeal` on the deep numinous proxy (ancient `crust_age` + the column reaching `BandKind::Underneath`) via `sphere_fbm01(terrain.globe().features_noise_seed(), pos, freq, octaves) < prob` (reuse The Lode's presence pattern; **no new stream label**). `vestiges_at` assembles the stack: the pre-human layer (if any) first, then `occupations_at(world, cell).map(|o| vestige_from_occupation(&o, present_day))` — where `present_day` reads `HISTORY_NOW` (worldgen `present_day`/the `emit_now` value). Oldest-first (pre-human is deep-time-old; people by `founded`).

- [ ] **Step 4: Run green; fmt; type-audit; commit** `feat(worldgen): The Vestige — pre-human gate-scars + the per-cell palimpsest stack`.

---

### Task 4: The `vestige_dread` derived field (exposed; vessel wiring deferred)

**Files:** Modify `windows/worldgen/src/lib.rs` (add `vestige_dread`, mirroring `predator_pressure`). Test inline.

**Interfaces:**
- Consumes: `vestiges_at`, `GeneratedTerrain`, `Geosphere`, `World`.
- Produces: `pub fn vestige_dread(world: &World) -> Result<CellMap<f64>, BuildError>` — a seedless per-cell dread readout, `[0,1]`, the max dread over a cell's vestige stack.

- [ ] **Step 1: Test** — mirror a `predator_pressure` test: build a Full seed-42 world, assert `vestige_dread` returns a `CellMap<f64>` in `[0,1]`, and that a cell with a breached/forgotten vestige reads higher than an empty cell.

- [ ] **Step 2–4: Implement** mirroring `predator_pressure` (`lib.rs:970`): `CellMap::from_fn(geo, |cell| vestiges_at(world, &terrain, cell).iter().map(|v| v.dread).fold(0.0, f64::max))`. Seedless, derived, byte-identical across calls. **Do NOT edit the vessel** — this exposes the field as the hook; injecting it into `Hazards`/`LocaleTerrain` (the `predator` pattern) is a deferred follow-on (spec §9.2, the resolved G3 call). Commit `feat(worldgen): The Vestige — the derived vestige-dread field (exposed; vessel wiring deferred)`.

---

### Task 5: Determinism / byte-identity guard

**Files:** Modify a worldgen test file (or `vestige.rs` tests).

- [ ] **Step 1: Tests** — `vestiges_at` is deterministic (two Full builds of seed 42 → identical stacks for all cells); `lens_purity` green (residue reads the ledger read-only, commits no facts).
- [ ] **Step 2: Run** (should pass — guards existing behavior); confirm `cargo test -p hornvale --test lens_purity` green.
- [ ] **Step 3: fmt; commit** `test(worldgen): The Vestige — residue is a deterministic, non-committing derived read`.

---

### Task 6: The residue map lens

**Files:** Modify `domains/terrain/src/render.rs`? — **NO.** Residue is worldgen-derived (needs the ledger), so the lens can't live in terrain's render. Add the lens in a worldgen-reachable render path and wire the CLI. Study how `paleo-map`/`settlement-map` (which render worldgen-derived data) are wired in `cli/src/main.rs` — mirror that, not the terrain `--field` path.

- [ ] Steps: a `vestige_png(world) -> Vec<u8>` (categorical by kind/valence — venerated vs forgotten distinct; the clusters visible), mirroring `settlement_map`/`paleo_map`'s rasterizer; a `cmd_vestige_map` CLI subcommand (like `cmd_paleo_map`); well-formed/determinism test. Tag `bare-ok(artifact: return)`. Commit `feat(cli,worldgen): The Vestige — a residue map lens`.
  (Adaptation: confirm where `paleo_png` lives and whether residue rendering should sit beside it or in a new worldgen render module — match the paleo/settlement precedent.)

---

### Task 7: Census metrics

**Files:** Modify `windows/lab/src/metrics.rs`.

- [ ] Add metrics (all **`Extractor::Full`** — residue reads history): `vestige-density` (Numeric, residue sites / land), `forgotten-fraction` (Numeric, forgotten vs venerated over vestige-bearing cells), `dominant-hazard` (Categorical, modal `HazardKind` — exhaustive `hazard_name`), `mean-warning-legibility` (Numeric). Land-guarded; mirror `karst-fraction`/`dominant-rock`. **Bump `registry_metric_count_is_pinned` by the number added** (verify against the actual `registry().len()`; note "+N for The Vestige"). Registration test → red → add → green. **Do NOT run the census.** Commit `feat(lab): The Vestige — census metrics for residue density, forgotten-fraction, hazard, warning`.

---

### Task 8: The almanac "The Vestige" section

**Files:** Modify `windows/almanac/src/lib.rs` (`vestige_lines` field + render block after "The Lode"; the ~15 construction sites — most via `..sample_context()`); `windows/worldgen/src/lib.rs` (`vestige_lines_from(world, terrain)` wired into `almanac_context`).

- [ ] TDD suppression + ordering tests (renders after "## The Lode"); add the field + render block + `vestige_lines: Vec::new()` at every construction site the compiler flags; `vestige_lines_from` reports notable sealed wards, great abandoned delvings/undercities, the venerated deep-tombs, prominent gate-scars, and the forgotten-vs-venerated split (reading `vestiges_at` across the land, land-only → `Vec::new()`). `lens_purity` green. Commit `feat(almanac,worldgen): The Vestige — an almanac section for the underworld's residue`.

---

### Task 9: Artifacts, book, and campaign close

**Files:** `scripts/regenerate-artifacts.sh` (residue-lens gallery line); regenerate almanac goldens + residue lens + type-audit report + census; `book/src/chronicle/the-vestige.md` (+ SUMMARY); `book/src/frontier/idea-registry.md` (MAP-10 campaign-3 → shipped); `docs/retrospectives/the-vestige.md`.

- [ ] Steps mirror The Lode's close: add the `vestige-map` regen line; write the chronicle (book altitude, no registry IDs) + SUMMARY entry (after The Lode); flip MAP-10 "Campaign 3, The Vestige, spec'd" → "shipped"; Confidence-Gradient grep (likely N/A — document); retrospective (process lessons: the integration-layer nature, the ledger-reconstruction lift, the venerated↔forgotten valence, the dread-field expose-vs-wire call); `SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh` + drift check; the out-of-band census refresh (carve-out); `make gate`; commit `docs(the-vestige): chronicle, retrospective, registry flip, artifacts, close`.

---

## Self-Review

**1. Spec coverage** (spec § → task): §2 integration/worldgen-derived → T1/T2 (joins history+terrain); §3 the facet model (kind/maker/era/seal-state/valence/hazard/dread/warning) → T2/T3; §4 taxonomy (maker→purpose tree) → T2 (`VestigeKind`) + T3 (pre-human); §5 palimpsest stack + occupation cycle → T3 (`vestiges_at` layers) + T2 (alive→maintained/venerated = the cycle's phases); §6 three-rate decay / forgotten warning → T2 (`warning_legibility` fast decay, `WARNING_HALF_LIFE_DAYS`); §7 derivation (clusters at history×Lode×Deep) → T2 (history) + T3 (deep numinous); §8 determinism/no-facts → T5; §9 consumers (query, Dread coupling exposed, almanac, lens, census) → T3/T4/T6/T7/T8; §10 non-goals → nothing built. *Note: "clusters at The Lode's ore/cave provinces" is folded into the people-history derivation (settlements sat where the ore/caves were, per The Lode's placement) rather than a separate Lode read in T2/T3 — called out so the light touch is deliberate, not a gap.*

**2. Placeholder scan:** code steps carry real code; T2's core derivation is complete. Flagged adaptation points (explicit "read/match the real X", not vague): the `occ` test-helper + `reconstruct_occupation` decoder must match `record.rs`/`almanac::history::record_of` (T1/T2); the CLI residue-lens wiring must match the `paleo-map`/`settlement-map` precedent (T6); the metric-count bump verified against actual `registry().len()` (T7). Compiler is the net.

**3. Type consistency:** `Vestige{kind,seal_state,valence,hazard,dread,warning_legibility,founded_day}`, `VestigeKind`/`SealState`/`Valence`/`HazardKind`, `vestige_from_occupation`, `vestiges_at`, `vestige_dread`, `occupation_records`/`occupations_at` — names identical across T1–T9. Metric names match between T7 and T9. No collision with `hornvale_history::flesh::residue_of` (distinct `Vestige` namespace).
