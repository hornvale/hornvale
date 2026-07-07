# Campaign 4b: Emergent Society — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make a settlement's society emerge from its environment — a subsistence mode from its biome and a caste/role structure from surplus, scale, and threat — replacing the fixed goblin ladder; prove the enrichment thesis with a cascade test; and close Campaign 4 with a Census of Peoples and the tier-1 book chapters.

**Architecture:** `domains/culture` becomes tier 1 (kernel-only): given a settlement's environmental summary (a culture-owned `BiomeClass`, coastal flag, surplus, population, threat) it derives a `Subsistence` mode and an ordered role list, committed as facts. The composition root (`windows/worldgen`) maps the flagship's `climate::Biome` → `culture::BiomeClass` and computes surplus/threat from terrain + climate, then runs tier-1 culture genesis on the flagship. The Lab gains Census-of-Peoples metrics and a subsistence⇔biome calibration. Two vestigial 4a residues (the `located-in` predicate/field and the dead tier-0 Vale generator) are removed.

**Tech Stack:** Rust edition 2024; `hornvale-kernel` only in domains; std only; the existing Lab study/metric framework and CI drift harness.

## Global Constraints

- `serde` + `serde_json` only; **no new crates.** Randomness is the kernel's `Seed`/`Stream`.
- **Layering (enforced by `cli/tests/architecture.rs`):** `domains/culture` and `domains/settlement` depend on `hornvale-kernel` and **nothing else**. The flagship's `climate::Biome` reaches culture only as a culture-owned `BiomeClass` (plus bare `f64`/`bool`/`u32`), mapped at the composition root. `domains/culture` must not import terrain, climate, or astronomy.
- **Culture output is committed facts** (a `subsistence` fact + `has-caste` facts on the flagship), read back by the almanac/REPL/Lab — not reconstructed. Terrain/climate still reconstruct from seed + pins.
- **Determinism (constitutional):** same seed + pins → identical society and Census rows; **no `HashMap`/`HashSet`/`std::time`** (clippy-forbidden); `BTreeMap`/`Vec`, `total_cmp`.
- **Calibration is exact** (spec §10): subsistence must equal a known function of `(biome, coastal)` — validated as a test, the sibling of the band-count and belief-kind calibrations.
- **Stream labels are permanent contracts.** Culture adds none in 4b (subsistence/structure are pure functions of the environmental summary; population/name draws already exist in settlement). If a local-variation draw is added it is declared in `domains/culture/src` `streams` and published through `cli/src/streams.rs`.
- Every crate `#![warn(missing_docs)]`; every public item/field/variant a `///` doc comment.
- Edition 2024; `cargo fmt` final each task. **Full gate:** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, plus the CI artifact drift check.
- **Baseline:** 381 tests pass on `main` at the start of this plan (Campaign 4a merged). Verify counts directly after each task. Goblin-only stays (multi-species is the Year-2 psychology substrate).

---

## Cross-cutting design decisions (read before Task 1)

1. **Subsistence is an exact function of `(BiomeClass, coastal)`** so the calibration is exact:
   - `Forest → Farming`, `Grassland → Farming`, `Arid → Herding`, `Cold → Foraging`, `Barren → Foraging`.
   - Then a coastal override: if the inland verdict is `Herding` or `Foraging` and the cell is coastal, it becomes `Fishing` (a coast rescues a marginal hinterland). Forest/Grassland stay `Farming` even on the coast (rich hinterland).
2. **`BiomeClass` is culture-owned and coarse**; the root maps every `climate::Biome` into it. Marine biomes never reach culture (flagships are land), but the map is total (marine → `Barren`) so it can't panic.
3. **The role vocabulary stays goblin** but which roles appear varies: `chief` (always, top) + a subsistence worker (`farmer`/`herder`/`fisher`/`forager`, always) + `warrior` (threat), `artisan` (surplus + scale), `shaman` (surplus), `slave` (surplus + high scale). Ordered lowest→highest, mirroring the tier-0 `slave..chief` convention. A lean forager camp is `[forager, chief]`; a rich farm town is `[slave, farmer, artisan, shaman, chief]`.
4. **`surplus` = fertility(BiomeClass) × moisture**; **`threat` = flagship-cell unrest**; both `[0,1]`, computed at the root. `population` comes from the committed flagship fact (4a's `draw_population`).
5. **Settlement pins** (spec §9): a single minimal `--min-suitability F` pin (the placement floor), threaded like terrain pins and persisted as a scenario fact. The flagship-selection override is **deferred** (showpiece-only; not needed by the Census or any exit criterion) — documented, not silently dropped.
6. **The exit-demo cascade test** reuses astronomy rotation pins (spinning vs locked) and asserts the flagship's subsistence/structure reorganizes — no new pin needed.

---

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `domains/terrain/src/lib.rs` | Modify | Remove the dead tier-0 Vale `genesis` fn + its test (4a residue). |
| `domains/settlement/src/lib.rs` | Modify | Remove the vestigial `LOCATED_IN` const, its registration, and `VillageInfo.located_in` (4a residue); update `village_info` + tests. |
| `windows/almanac/src/lib.rs` | Modify | Drop `located_in` from the `VillageInfo` sample; add culture/subsistence lines to "The People". |
| `domains/culture/src/subsistence.rs` | Create | `Subsistence`, `BiomeClass`, `subsistence(class, coastal)`, `fertility(class)`. |
| `domains/culture/src/structure.rs` | Create | `EnvSummary`, `structure(env) -> Vec<String>` (emergent ordered role list). |
| `domains/culture/src/lib.rs` | Modify | Module wiring, re-exports, `SUBSISTENCE` predicate, tier-1 `genesis(world, flagship, &EnvSummary)` (replaces the fixed-ladder genesis), `subsistence_of`/`castes_of` accessors. |
| `windows/worldgen/src/lib.rs` | Modify | Map flagship `Biome`→`BiomeClass`, compute surplus/threat, build `EnvSummary`, call tier-1 culture genesis; settlement pins (`SettlementPins`) threaded through `build_world` + persisted + reconstructed; `culture_lines`/almanac field. |
| `cli/src/main.rs` | Modify | `--min-suitability` flag → `SettlementPins`; usage text. |
| `windows/lab/src/metrics.rs` | Modify | Census-of-Peoples metrics (settlement count, mean population, flagship subsistence/biome/coastal, structure size, endorheic coverage); metric-count test → new total. |
| `windows/lab/tests/calibration.rs` | Modify | Add the subsistence⇔biome calibration. |
| `studies/census-of-peoples.study.json` | Create | Author-time 10k headline census (or "all" metrics). |
| `.github/workflows/ci.yml` | Modify | (No new render; `census-lands-drift` rerun already covers the new metrics — confirm drift net includes them.) |
| `book/src/domains/settlement.md`, `book/src/domains/culture.md` | Modify | Promote to tier 1 with model cards. |
| `book/src/domains/overview.md` | Modify | Cascade table: Settlement/Culture rows to tier 1 (the Vale retired, society emergent). |
| `book/src/laboratory/study-003.md` | Create | The Census of Peoples chapter (comprehension-gated). |
| `book/src/chronicle/campaign-4b.md` | Create | Chronicle entry (folds the C4 close). |
| `book/src/SUMMARY.md` | Modify | Laboratory + chronicle entries. |
| `book/src/gallery/almanac-*.md`, `book/src/reference/*-generated.md`, `book/src/laboratory/generated/*` | Regenerate | Committed generated artifacts (new subsistence predicate, culture lines, Census metrics). |

Dataflow inside culture: `subsistence` (biome→mode) + `structure` (env→roles) → `genesis` (commits). Root: flagship biome/climate/terrain → `EnvSummary` → culture genesis.

---

### Task 1: Retire the 4a residue (dead Vale genesis + vestigial `located-in`)

**Files:**
- Modify: `domains/terrain/src/lib.rs`
- Modify: `domains/settlement/src/lib.rs`
- Modify: `windows/almanac/src/lib.rs`

**Interfaces:**
- Produces: `terrain::genesis` removed; `settlement::LOCATED_IN` and `VillageInfo.located_in` removed; `VillageInfo` is now `{ id, name, population }`.

- [ ] **Step 1: Remove the dead Vale generator.** In `domains/terrain/src/lib.rs`, delete `pub fn genesis(world: &mut World) -> Result<EntityId, LedgerError>` (the hand-placed Vale — no longer called after 4a) and the test that calls it (`genesis_commits_one_named_place` and `genesis_is_idempotent_on_facts` if they exercise it). Keep `places`, `PlaceInfo`, `IS_PLACE`, `BIOME`, `register_concepts`. Run `cargo test -p hornvale-terrain` — green.

- [ ] **Step 2: Remove `located-in`.** In `domains/settlement/src/lib.rs`: delete the `LOCATED_IN` const, its `register_predicate(LOCATED_IN, ...)` line in `register_concepts`, the `located_in` field from `VillageInfo`, and the `located_in` read in `village_info` (return `VillageInfo { id, name, population }`). Update settlement tests that constructed/asserted `located_in`.

- [ ] **Step 3: Fix the almanac fallout.** In `windows/almanac/src/lib.rs`, remove `located_in` from the `VillageInfo { ... }` literal in `sample_context()`. Run `cargo test -p hornvale-almanac`.

- [ ] **Step 4: Full gate + commit.**

Run: `cargo test --workspace && cargo clippy --workspace --all-targets -- -D warnings`
Expected: PASS (nothing writes or reads `located-in`; `VillageInfo` consumers use `id`/`name`/`population` only). `cargo fmt`.

```bash
git add domains/terrain/src/lib.rs domains/settlement/src/lib.rs windows/almanac/src/lib.rs
git commit -m "refactor: retire dead Vale genesis and vestigial located-in (4a residue)"
```

---

### Task 2: Culture — subsistence from biome

**Files:**
- Create: `domains/culture/src/subsistence.rs`
- Modify: `domains/culture/src/lib.rs` (`pub mod subsistence;` + re-exports)

**Interfaces:**
- Produces:
  - `enum Subsistence { Farming, Herding, Fishing, Foraging }` (`Clone, Copy, Debug, PartialEq, Eq`), `impl Subsistence { fn name(self) -> &'static str; fn worker(self) -> &'static str }` (kebab name; `worker` = `farmer`/`herder`/`fisher`/`forager`).
  - `enum BiomeClass { Forest, Grassland, Arid, Cold, Barren }` (same derives).
  - `fn subsistence(class: BiomeClass, coastal: bool) -> Subsistence` (cross-cutting decision 1).
  - `fn fertility(class: BiomeClass) -> f64` — Forest 0.9, Grassland 0.7, Arid 0.3, Cold 0.2, Barren 0.1.

- [ ] **Step 1: Write failing tests**

Create `domains/culture/src/subsistence.rs`:

```rust
//! Subsistence: a settlement's mode of making a living, an exact function of
//! its biome class and whether it reaches the coast. Farming in productive
//! land, herding in the arid interior, foraging on the cold/barren margins,
//! and fishing where a marginal hinterland meets the sea. The exactness is
//! the point — the Lab calibrates subsistence against biome (spec §10).

/// How a settlement makes its living.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Subsistence {
    /// Settled agriculture.
    Farming,
    /// Pastoral herding.
    Herding,
    /// Coastal fishing.
    Fishing,
    /// Hunting and gathering.
    Foraging,
}

/// A coarse biome category (culture-owned; the composition root maps every
/// `climate::Biome` into this so culture imports no domain).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BiomeClass {
    /// Forests, taiga, rainforests — productive, arable.
    Forest,
    /// Grasslands and savanna — arable/pastoral.
    Grassland,
    /// Deserts and shrubland — dry interior.
    Arid,
    /// Tundra and cold margins.
    Cold,
    /// Alpine, ice, or (defensively) marine — barren.
    Barren,
}

impl Subsistence {
    /// Kebab-case name (almanac, Lab, CSV).
    pub fn name(self) -> &'static str {
        match self {
            Subsistence::Farming => "farming",
            Subsistence::Herding => "herding",
            Subsistence::Fishing => "fishing",
            Subsistence::Foraging => "foraging",
        }
    }
    /// The base worker role this subsistence implies.
    pub fn worker(self) -> &'static str {
        match self {
            Subsistence::Farming => "farmer",
            Subsistence::Herding => "herder",
            Subsistence::Fishing => "fisher",
            Subsistence::Foraging => "forager",
        }
    }
}

/// Relative land fertility of a biome class, `[0, 1]`.
pub fn fertility(class: BiomeClass) -> f64 {
    match class {
        BiomeClass::Forest => 0.9,
        BiomeClass::Grassland => 0.7,
        BiomeClass::Arid => 0.3,
        BiomeClass::Cold => 0.2,
        BiomeClass::Barren => 0.1,
    }
}

/// The subsistence mode for a biome class and coastal access. Forest and
/// grassland farm regardless of coast; arid herds inland but fishes on the
/// coast; cold and barren forage inland but fish on the coast.
pub fn subsistence(class: BiomeClass, coastal: bool) -> Subsistence {
    let inland = match class {
        BiomeClass::Forest | BiomeClass::Grassland => Subsistence::Farming,
        BiomeClass::Arid => Subsistence::Herding,
        BiomeClass::Cold | BiomeClass::Barren => Subsistence::Foraging,
    };
    if coastal && matches!(inland, Subsistence::Herding | Subsistence::Foraging) {
        Subsistence::Fishing
    } else {
        inland
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn subsistence_is_an_exact_function_of_biome_and_coast() {
        assert_eq!(subsistence(BiomeClass::Forest, false), Subsistence::Farming);
        assert_eq!(subsistence(BiomeClass::Forest, true), Subsistence::Farming); // rich hinterland stays farming
        assert_eq!(subsistence(BiomeClass::Grassland, false), Subsistence::Farming);
        assert_eq!(subsistence(BiomeClass::Arid, false), Subsistence::Herding);
        assert_eq!(subsistence(BiomeClass::Arid, true), Subsistence::Fishing); // coast rescues the interior
        assert_eq!(subsistence(BiomeClass::Cold, false), Subsistence::Foraging);
        assert_eq!(subsistence(BiomeClass::Barren, true), Subsistence::Fishing);
        assert!(fertility(BiomeClass::Forest) > fertility(BiomeClass::Arid));
        assert_eq!(Subsistence::Fishing.worker(), "fisher");
    }
}
```

- [ ] **Step 2: Run to see it fail**

Run: `cargo test -p hornvale-culture subsistence 2>&1 | tail -5`
Expected: FAIL — undefined.

- [ ] **Step 3: (implementation is in Step 1's file)** — the code above is complete. Add `pub mod subsistence;` and `pub use subsistence::{BiomeClass, Subsistence, fertility, subsistence};` to `domains/culture/src/lib.rs`.

- [ ] **Step 4: Run + fmt + commit**

Run: `cargo test -p hornvale-culture && cargo clippy -p hornvale-culture --all-targets -- -D warnings`
Expected: PASS. `cargo fmt`.

```bash
git add domains/culture/src/subsistence.rs domains/culture/src/lib.rs
git commit -m "feat(culture): subsistence mode as an exact function of biome + coast"
```

---

### Task 3: Culture — emergent role structure

**Files:**
- Create: `domains/culture/src/structure.rs`
- Modify: `domains/culture/src/lib.rs` (`pub mod structure;` + re-exports)

**Interfaces:**
- Consumes: `subsistence::Subsistence`.
- Produces:
  - `struct EnvSummary { subsistence: Subsistence, surplus: f64, population: u32, threat: f64 }` (all `pub`, `Clone/Copy/Debug/PartialEq`).
  - `fn structure(env: &EnvSummary) -> Vec<String>` — ordered lowest→highest role list (cross-cutting decision 3).

- [ ] **Step 1: Write failing tests**

Create `domains/culture/src/structure.rs`:

```rust
//! Emergent social structure: which caste/role strata a settlement grows,
//! from its subsistence, surplus (fertility × water), population scale, and
//! threat. A lean forager camp is nearly egalitarian; a rich, populous farm
//! town differentiates into a full ladder. The role vocabulary stays goblin;
//! what varies is which rungs exist (replacing the tier-0 fixed ladder).

use crate::subsistence::Subsistence;

/// The environmental pressures that shape a settlement's structure.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EnvSummary {
    /// The settlement's subsistence mode.
    pub subsistence: Subsistence,
    /// Surplus in `[0, 1]` (fertility × water availability).
    pub surplus: f64,
    /// Population.
    pub population: u32,
    /// Threat in `[0, 1]` (tectonic unrest / frontier pressure).
    pub threat: f64,
}

/// The ordered role list (lowest → highest) a settlement grows. `chief` and
/// the subsistence worker always appear; `slave`/`artisan`/`shaman`/`warrior`
/// appear as surplus, scale, and threat cross thresholds.
pub fn structure(env: &EnvSummary) -> Vec<String> {
    let mut roles: Vec<String> = Vec::new();
    if env.surplus > 0.6 && env.population > 300 {
        roles.push("slave".to_string());
    }
    roles.push(env.subsistence.worker().to_string());
    if env.threat > 0.4 {
        roles.push("warrior".to_string());
    }
    if env.surplus > 0.6 && env.population > 200 {
        roles.push("artisan".to_string());
    }
    if env.surplus > 0.4 {
        roles.push("shaman".to_string());
    }
    roles.push("chief".to_string());
    roles
}

#[cfg(test)]
mod tests {
    use super::*;

    fn env(sub: Subsistence, surplus: f64, pop: u32, threat: f64) -> EnvSummary {
        EnvSummary { subsistence: sub, surplus, population: pop, threat }
    }

    #[test]
    fn lean_forager_camp_is_nearly_egalitarian() {
        let s = structure(&env(Subsistence::Foraging, 0.1, 30, 0.1));
        assert_eq!(s, vec!["forager".to_string(), "chief".to_string()]);
    }

    #[test]
    fn rich_populous_farm_town_is_stratified() {
        let s = structure(&env(Subsistence::Farming, 0.8, 500, 0.1));
        assert_eq!(
            s,
            vec!["slave", "farmer", "artisan", "shaman", "chief"]
                .into_iter().map(String::from).collect::<Vec<_>>()
        );
    }

    #[test]
    fn a_frontier_settlement_grows_warriors() {
        let s = structure(&env(Subsistence::Herding, 0.3, 120, 0.7));
        assert!(s.contains(&"warrior".to_string()));
        assert!(!s.contains(&"shaman".to_string()), "low surplus: no priest caste");
        assert_eq!(s.first().unwrap(), "herder");
        assert_eq!(s.last().unwrap(), "chief");
    }

    #[test]
    fn structure_always_has_a_worker_and_a_chief() {
        for sub in [Subsistence::Farming, Subsistence::Herding, Subsistence::Fishing, Subsistence::Foraging] {
            let s = structure(&env(sub, 0.0, 10, 0.0));
            assert_eq!(s.len(), 2);
            assert_eq!(s[0], sub.worker());
            assert_eq!(s[1], "chief");
        }
    }
}
```

- [ ] **Step 2–4: Run to fail, wire module, run to pass.** Add `pub mod structure;` and `pub use structure::{EnvSummary, structure};` to `domains/culture/src/lib.rs`. Run `cargo test -p hornvale-culture structure && cargo clippy -p hornvale-culture --all-targets -- -D warnings`; `cargo fmt`.

- [ ] **Step 5: Commit**

```bash
git add domains/culture/src/structure.rs domains/culture/src/lib.rs
git commit -m "feat(culture): emergent role structure from surplus, scale, and threat"
```

---

### Task 4: Culture — tier-1 genesis (replace the fixed ladder)

**Files:**
- Modify: `domains/culture/src/lib.rs`

**Interfaces:**
- Consumes: `EnvSummary`, `structure`, `Subsistence`.
- Produces:
  - `SUBSISTENCE` predicate (`"subsistence"`, functional Text), registered.
  - `genesis(world: &mut World, settlement: EntityId, env: &EnvSummary) -> Result<(), LedgerError>` — commits one `subsistence` fact and the emergent `has-caste` facts (replaces the old `genesis(world, village)`).
  - `subsistence_of(world, settlement) -> Option<String>` accessor. `castes_of` unchanged.
  - `CASTES` (the fixed array) is **removed** — nothing should reference it after this task (the repl/almanac read `castes_of`).

- [ ] **Step 1: Write failing tests.** Replace the culture tests module with tests that build an `EnvSummary`, call the new `genesis`, and assert: the committed castes equal `structure(&env)`; a `subsistence` fact is committed and readable via `subsistence_of`; a lean vs rich env yield different structures. Example:

```rust
#[test]
fn genesis_commits_the_emergent_structure_and_subsistence() {
    let mut w = World::new(Seed(42));
    register_concepts(&mut w.registry).unwrap();
    let s = w.ledger.mint_entity();
    let env = EnvSummary { subsistence: Subsistence::Farming, surplus: 0.8, population: 500, threat: 0.1 };
    genesis(&mut w, s, &env).unwrap();
    assert_eq!(castes_of(&w, s), structure(&env));
    assert_eq!(subsistence_of(&w, s).as_deref(), Some("farming"));
}
```

- [ ] **Step 2: Run to fail.** `cargo test -p hornvale-culture genesis_commits_the_emergent 2>&1 | tail -5` — FAIL.

- [ ] **Step 3: Implement.** In `domains/culture/src/lib.rs`: add `SUBSISTENCE` const + register it (functional Text) in `register_concepts`; remove the `CASTES` const; rewrite `genesis`:

```rust
/// Predicate: a settlement's subsistence mode (functional, Text).
pub const SUBSISTENCE: &str = "subsistence";
```
```rust
/// Tier-1 genesis: commit the settlement's subsistence mode and its emergent
/// caste/role structure (from `structure`). Replaces the tier-0 fixed ladder.
pub fn genesis(world: &mut World, settlement: EntityId, env: &EnvSummary) -> Result<(), LedgerError> {
    world.ledger.commit(
        Fact {
            subject: settlement,
            predicate: SUBSISTENCE.to_string(),
            object: Value::Text(env.subsistence.name().to_string()),
            place: None,
            day: Some(0.0),
            provenance: "culture".to_string(),
        },
        &world.registry,
    )?;
    for caste in structure(env) {
        world.ledger.commit(
            Fact {
                subject: settlement,
                predicate: HAS_CASTE.to_string(),
                object: Value::Text(caste),
                place: None,
                day: Some(0.0),
                provenance: "culture".to_string(),
            },
            &world.registry,
        )?;
    }
    Ok(())
}

/// The subsistence mode committed for a settlement, if any.
pub fn subsistence_of(world: &World, settlement: EntityId) -> Option<String> {
    match world.ledger.value_of(settlement, SUBSISTENCE) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}
```

Register `SUBSISTENCE` in `register_concepts` alongside `HAS_CASTE`.

- [ ] **Step 4: Run + fmt + commit.** `cargo test -p hornvale-culture` (the whole workspace will not compile — `build_world` still calls the old `genesis(world, flagship)`; that is rewired in Task 5, so gate only `-p hornvale-culture` here). `cargo clippy -p hornvale-culture --all-targets -- -D warnings`; `cargo fmt`.

```bash
git add domains/culture/src/lib.rs
git commit -m "feat(culture): tier-1 genesis — emergent structure + subsistence fact"
```

---

### Task 5: Composition-root wiring + settlement pin

**Files:**
- Modify: `windows/worldgen/src/lib.rs`
- Modify: `windows/almanac/src/lib.rs`
- Modify: `cli/src/main.rs`

**Interfaces:**
- Consumes: `culture::{BiomeClass, EnvSummary, subsistence, fertility, genesis}`, `climate::Biome`, flagship facts.
- Produces:
  - `fn biome_class(biome: hornvale_climate::Biome) -> hornvale_culture::BiomeClass` (total map, at the root).
  - `struct SettlementPins { min_suitability: Option<f64> }` + `pin_strings`/`parse_pin` (mirroring terrain pins), threaded as a 5th `build_world` argument, persisted as `scenario-pin`-style facts, reconstructed.
  - `build_world` runs tier-1 culture genesis on the flagship with an `EnvSummary`.
  - `fn culture_lines(world) -> Vec<String>` + `AlmanacContext.culture_lines`.

- [ ] **Step 1: Write failing tests** in worldgen: a generated seed-42 world's flagship has a non-empty `subsistence_of` and its castes equal the emergent structure for its environment; the flagship's subsistence matches `subsistence(biome_class(flagship_biome), flagship_coastal)`; two worlds differing only in rotation yield different flagship subsistence-or-structure (the enrichment cascade). Also: `--min-suitability 0.5` reduces the settlement count vs default. (Write concrete assertions mirroring 4a's `settlements_reorganize_between_spinning_and_locked`.)

- [ ] **Step 2: Run to fail.**

- [ ] **Step 3: Implement.** Add `biome_class` mapping (Forest: the four forests + taiga; Grassland: savanna + temperate-grassland; Arid: desert + shrubland; Cold: tundra; Barren: alpine + ice + all marine). In `build_world`'s flagship block, replace `hornvale_culture::genesis(&mut world, flagship)?` with: compute the flagship cell's `coastal`, `moisture`, `unrest` (from the already-reconstructed `terrain`/`climate`), `class = biome_class(climate.biome_at(flagship_cell))`, `sub = subsistence(class, coastal)`, `surplus = (fertility(class) * moisture).clamp(0,1)`, `threat = unrest`, `population` from the committed flagship fact (read `hornvale_settlement::POPULATION`), then `hornvale_culture::genesis(&mut world, flagship, &EnvSummary { subsistence: sub, surplus, population, threat })?`. (The flagship `CellId` = the placement whose cell matches; you already have `placements[0]` in scope — reuse it rather than re-reading facts.) Add `SettlementPins` (module or inline), thread through `build_world` signature + all call sites (CLI, lab `WorldView::build`, worldgen tests) + persist/reconstruct, and pass `min_suitability` (default 0.25) into `place(&sites, min_sep, floor)`. Add `culture_lines` (flagship subsistence + a one-line structure summary) + the almanac field + render in "The People".

- [ ] **Step 4: Fix call-site fallout.** `build_world` gained a 5th arg — update every caller: `cli/src/main.rs` (`cmd_new`, `cmd_concepts`), `windows/lab/src/metrics.rs` (`WorldView::build`), and all worldgen/repl tests. Add the `--min-suitability` flag parse in `cli/src/main.rs` (mirror `--ocean-fraction`).

- [ ] **Step 5: Full gate + commit.** `cargo test --workspace && cargo clippy --workspace --all-targets -- -D warnings`; `cargo fmt`.

```bash
git add windows/worldgen/src/lib.rs windows/almanac/src/lib.rs cli/src/main.rs windows/lab/src/metrics.rs cli/src/repl.rs
git commit -m "feat(worldgen): emergent culture on the flagship; --min-suitability pin"
```

---

### Task 6: Lab — the Census of Peoples

**Files:**
- Modify: `windows/lab/src/metrics.rs`
- Modify: `windows/lab/tests/calibration.rs`
- Create: `studies/census-of-peoples.study.json`

**Interfaces:**
- Consumes: `WorldView` (world + terrain + climate), the flagship's committed `subsistence`/`biome`/`population` facts, `hornvale_terrain::places`.
- Produces: new metrics + the subsistence⇔biome calibration.

New metrics (append to `registry()`; update the count test to the new total):
- `settlement-count` (Numeric) — `hornvale_terrain::places(&v.world).len()`.
- `mean-population` (Numeric) — mean of settlements' `POPULATION` facts.
- `flagship-subsistence` (Categorical) — `hornvale_culture::subsistence_of(&v.world, flagship)`; `Absent` if none.
- `flagship-biome` (Categorical) — the flagship place's `BIOME` fact.
- `flagship-coastal` (Flag) — recomputed from `v.terrain` at the flagship cell (any ocean neighbor).
- `flagship-structure-size` (Numeric) — `castes_of(flagship).len()` (stratification proxy).
- `endorheic-coverage` (Numeric) — fraction of land cells that are endorheic (`v.terrain`).

The flagship = `hornvale_settlement::village_info(&v.world)`. The flagship cell = its `CELL_ID` fact.

- [ ] **Step 1: Write failing tests** (metric extraction for seed 42; count test to new total). 
- [ ] **Step 2: Run to fail.**
- [ ] **Step 3: Implement the metrics** (mirror the 4a land metrics' style; `BTreeMap` if any counting; `total_cmp` for any sort).
- [ ] **Step 4: Add the subsistence⇔biome calibration** to `windows/lab/tests/calibration.rs`, loading `census-lands-drift.study.json`: for each non-refusal row, `flagship-subsistence` must equal `subsistence(biome_class(flagship-biome), flagship-coastal)` — a re-derivation from two independent metric columns (biome + coastal) versus the committed subsistence fact. Expose `biome_class`/`subsistence`/`BiomeClass` so the test can call them (the mapping lives in worldgen; re-export or duplicate the tiny biome→class match in the test — prefer re-exporting `world_builder::biome_class` and `hornvale_culture::subsistence`). Skip rows where `flagship-subsistence` is `Absent`.
- [ ] **Step 5: Create `studies/census-of-peoples.study.json`** (10k, `"all"`, author-time headline).
- [ ] **Step 6: Gate + commit.** `cargo test -p hornvale-lab && cargo clippy --workspace --all-targets -- -D warnings`; `cargo fmt`.

```bash
git add windows/lab/src/metrics.rs windows/lab/tests/calibration.rs studies/census-of-peoples.study.json windows/worldgen/src/lib.rs
git commit -m "feat(lab): Census of Peoples metrics + subsistence-biome calibration"
```

---

### Task 7: Book close-out + CI + artifact regeneration

**Files:**
- Modify: `book/src/domains/settlement.md`, `book/src/domains/culture.md`, `book/src/domains/overview.md`, `book/src/SUMMARY.md`
- Create: `book/src/laboratory/study-003.md`, `book/src/chronicle/campaign-4b.md`
- Modify: `.github/workflows/ci.yml` (confirm `census-lands-drift` + drift net cover the new metrics; no new render needed)
- Modify: `cli/src/streams.rs` (manifest test unchanged unless a label was added — culture added none; confirm)
- Regenerate: `book/src/gallery/almanac-*.md`, `book/src/reference/*-generated.md`, `book/src/laboratory/generated/census-lands-drift/*`

- [ ] **Step 1: Promote settlement + culture chapters to tier 1** with model cards (spec §5, §6): settlement placement (suitability, spacing, drainage-fed freshwater, flagship) and culture (subsistence⇔biome+coast; surplus/scale/threat → emergent structure; goblin-only; declared approximations: rule-table sociology, single species, no inter-settlement politics, static structure). Keep the tier-0 sections. Match `campaign-3c` chapter voice.
- [ ] **Step 2: Update the cascade overview** (`overview.md`) — Settlement/Culture rows to tier 1 (Vale retired; society emergent; culture consumes the flagship's environment).
- [ ] **Step 3: Write Study 003 (Census of Peoples)** — run the 10k `census-of-peoples` locally for real numbers (`cargo run -q --release -p hornvale -- lab run studies/census-of-peoples.study.json 2>/dev/null`, read `lab-out/census-of-peoples/rows.csv`). Comprehension-gated prose: the subsistence⇔biome calibration (exact) and the settled-fraction / subsistence-mix / structure-size distributions (the unknown numbers). Reference the CI-checked `census-lands-drift` charts; do not commit `census-of-peoples` artifacts (author-time; state this).
- [ ] **Step 4: Write the 4b chronicle** (`campaign-4b.md`) — the C4 close: society emerges from the land, the enrichment thesis proven (flagship subsistence/structure reorganizes under a different sky), the Census's numbers, the residue cleaned up. Note goblin-only and the deferred hooks (multi-species, inter-settlement politics, embark).
- [ ] **Step 5: SUMMARY + regenerate.** Add Study 003 + the 4b chronicle to `SUMMARY.md`. Regenerate every committed artifact **stdout-only** (the almanac gains culture lines; the registry gains the `subsistence` predicate; `census-lands-drift` gains the People metrics/charts). Verify `grep -rlE "Compiling|Finished \`|Running \`target" book/src` is empty and a second regeneration leaves `git diff --exit-code book/...` clean.
- [ ] **Step 6: Full gate + book build + commit.** `mdbook build book && cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, then the drift check.

```bash
git add book/ .github/workflows/ci.yml cli/src/streams.rs
git commit -m "docs(book): C4 close-out — tier-1 settlement/culture, Census of Peoples"
```

---

## Self-Review Notes

**Spec coverage (against §6, §7, §9, §10, §14):**
- §6 subsistence-anchored structure → Tasks 2–4 (subsistence, structure, genesis) + Task 5 (root builds `EnvSummary`). §7 enrichment cascade → Task 5's cascade test (flagship reorganizes under rotation pins). §9 settlement pins → Task 5's minimal `--min-suitability` (flagship-selection override explicitly deferred, documented). §10 Census of Peoples + subsistence⇔biome calibration → Task 6. §14 book close-out (tier-1 chapters, Census chapter, chronicle, freshness) → Task 7. Plus the two carried 4a minors → Task 1.

**Deferred (documented, not silently dropped):** the flagship-selection pin override (showpiece-only); deep culture for non-flagship settlements; inter-settlement politics/trade; multi-species (Year-2 substrate).

**Type consistency:** `Subsistence`/`BiomeClass`/`fertility`/`subsistence` (Task 2) → consumed by `EnvSummary`/`structure` (Task 3), `genesis` (Task 4), the root `biome_class`/`EnvSummary` build (Task 5), and the calibration (Task 6). `SUBSISTENCE` predicate + `subsistence_of` (Task 4) → read by Task 5 almanac lines and Task 6 metrics. `SettlementPins` (Task 5) threads a 5th `build_world` arg — Task 5 Step 4 fixes every call site (CLI, lab, tests).

**Interface checks flagged for implementers:** Task 5 reads the flagship `CellId`/population — reuse `placements[0]`/`placed[0]` already in `build_world` scope rather than re-reading facts (the plan says so). Task 6's calibration needs `biome_class` reachable from the test — re-export `world_builder::biome_class` (public) so the test calls the real mapping, keeping it non-tautological.

**Watch items (carried lessons):** verify test counts directly (baseline 381); the `build_world` 5th-arg change (Task 5) ripples to every caller — fix each, never trivialize; regenerate artifacts stdout-only; `CASTES` removal (Task 4) — confirm nothing outside culture referenced it (the repl/almanac use `castes_of`, not `CASTES`); `gen` is reserved in Rust 2024.
