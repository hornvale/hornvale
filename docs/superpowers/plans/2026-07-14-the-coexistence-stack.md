# The Coexistence Stack (MAP-22) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the-gathering's interim independent per-species condensation with a derived, deterministic multi-species coexistence stack over the carrying-capacity field: a per-cell per-species density stack, mass-based settlements with real composition, and three derived byproducts (strife / wilderness / refugia) — plus the axis-spanning menagerie that makes it demonstrable.

**Architecture:** One authored per-species primitive — `(mass, niche)` where `niche` is a resource-utilization vector — from which guild (overlap), trophic level (fractional, from the induced food web), and predation (mass-windowed) all DERIVE. The packer is a closed-form function of the-gathering's per-species K fields and these primitives; it draws nothing new from the seed. Built in two stages: (A) the engine, calibrated and β-frozen on the current 4 goblinoids; (B) the ~8–12 menagerie, re-measured against the frozen engine.

**Tech Stack:** Rust (edition 2024), `serde`/`serde_json` only. Kernel primitives: `Geosphere`, `CellId`, `CellMap<T>`, `WorldTime`, `Fact`/`ConceptRegistry`, `hornvale_kernel::math` (libm-routed transcendentals), `hornvale_kernel::quantize`. Test runner: `cargo nextest`. No new crates.

## Global Constraints

- **Determinism is constitutional.** Same seed + same pins → byte-identical worlds and artifacts. The packer draws **nothing** from the seed beyond what the K fields already consumed. No `HashMap`/`HashSet` (use `BTreeMap`/`BTreeSet`/`Vec`); float sorting via `total_cmp` with deterministic tie-breaks.
- **All `f64` transcendentals route through `hornvale_kernel::math`** (the `^β` power → `math::powf`, any `sqrt`/`cos` in overlap → `math::sqrt`/`math::cos`). Never call inherent `f64::powf`/`f64::sqrt` in the compute path. Quantize only at the emit boundary (`hornvale_kernel::quantize`), never in compute.
- **No wall-clock time.** Time is `WorldTime { day: f64 }` if referenced at all (this campaign is time-free).
- **Layering (enforced by `cli/tests/architecture.rs`):** `kernel/` → `domains/*` → `windows/*` → `cli/`. A domain crate depends on `hornvale-kernel` and **nothing else** — never another domain. `domains/demography` never imports `domains/species`; shared types (`Mass`, `ResourceVector`) therefore live in the kernel. `windows/worldgen` is the composition root and the only place that reads `SpeciesDef` and passes `(mass, niche)` into demography.
- **Every crate sets `#![warn(missing_docs)]`;** every public item, field, and variant gets a one-line doc comment.
- **Type audit (default-deny):** every primitive at a `pub` boundary carries a `type-audit:` verdict tag comment (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`). New newtypes (`Mass`, `ResourceVector`) are themselves the typed answer; their bare-`f64` internals get a tag. Verify with `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`.
- **`cargo fmt` is the final step before every commit.** The commit gate is `make gate` (fmt + clippy + `cargo nextest run --workspace` + doctests); the heavy census tier is `#[ignore]`d and NEVER regenerated locally (`SKIP_CENSUS=1`). Re-pin any golden fixture in the same commit that drifts it (do not defer to close).
- **Save-format contract:** frozen β and any packer constant are declared as named `const`s with a CALIBRATED provenance comment (mirror `carrying_capacity.rs`'s block). Deliberate regeneration uses an epoch suffix, never a rename.
- **Conform to the landed `Domain` trait (A1-domain-trait campaign).** New demography surface registers through the existing `DOMAINS` roster / `register_all` pattern where it emits predicates; check `domains/demography`'s current `Domain` impl before adding predicates.

---

## File Structure

**Kernel (shared vocabulary — both `species` and `demography` see it):**
- `kernel/src/units.rs` — MODIFY: add `Mass` newtype (follows `Temperature`).
- `kernel/src/ecology.rs` — CREATE: `ResourceAxis` (the open, registered basis with a `field`/`stock` kind), `ResourceVector` (sparse, zero-vector-legal), niche-overlap helper. Re-exported from `kernel/src/lib.rs`.

**`domains/species`:**
- `src/lib.rs` — MODIFY: `SpeciesDef += mass: Mass, niche: ResourceVector`; author the 4 goblinoids' values (Stage A) and the menagerie (Stage B).

**`domains/demography` (the packer):**
- `src/niche.rs` — CREATE: derive `guild_overlap`, `trophic_levels`, `predation` from `&[(u32, Mass, ResourceVector)]`.
- `src/footprint.rs` — CREATE: `home_range(Mass) -> f64` (cells-per-individual grain).
- `src/coexist.rs` — CREATE: the overlap-weighted K^β share + trophic coupling + soft-overflow → the per-cell density stack (`CoexistStack`).
- `src/stack_condense.rs` — CREATE: settlements from the stack — tier (mass) × composition + species-relative rendering. Replaces `condense_tagged` in `report`.
- `src/byproducts.rs` — CREATE: strife / wilderness / refugia derived fields.
- `src/lib.rs` — MODIFY: `DemographyReport` gains the stack, composition, and byproduct fields; `report()` calls the packer.
- `src/render.rs` — MODIFY: stack / strife / refugia debug PPMs.

**`windows/worldgen`:**
- `src/lib.rs` — MODIFY (~1900–2000): build `(u32, Mass, ResourceVector)` from `SpeciesDef`, call the packer, emit salient facts (dominant + bounded secondary species; named refugium; strife frontier).

**`windows/lab`:**
- `src/metrics.rs` + a new `studies/*.study.json` — the β→per-cell-diversity calibration; freeze β.

---

## STAGE A — the engine, calibrated on the 4 goblinoids

### Task A1: `Mass` newtype in the kernel units family

**Files:**
- Modify: `kernel/src/units.rs`
- Modify: `kernel/src/lib.rs` (re-export `Mass`)

**Interfaces:**
- Produces: `hornvale_kernel::Mass`; `Mass::new(kg: f64) -> Result<Mass, UnitError>` (rejects non-finite / negative); `Mass::kilograms(&self) -> f64`; `Mass::ratio_to(&self, other: Mass) -> f64` (mass ratio, dimensionless).

- [ ] **Step 1: Write the failing test.** Append to `kernel/src/units.rs` tests:
```rust
#[test]
fn mass_rejects_negative_and_reports_ratio() {
    assert!(Mass::new(-1.0).is_err());
    assert!(Mass::new(f64::NAN).is_err());
    let goblin = Mass::new(40.0).unwrap();
    let dragon = Mass::new(4000.0).unwrap();
    assert_eq!(goblin.kilograms(), 40.0);
    assert!((dragon.ratio_to(goblin) - 100.0).abs() < 1e-9);
}
```
- [ ] **Step 2: Run it, verify it fails.** `cargo test -p hornvale-kernel mass_rejects_negative` → FAIL (`Mass` undefined).
- [ ] **Step 3: Implement `Mass`** following the `Temperature` block (validating constructor, `UnitError` on non-finite/negative, doc comments on the type and its field, `#[derive(Clone, Copy, Debug, PartialEq)]`). Add the `type-audit: bare-ok(mass: kilograms)` tag on the wrapped field. Re-export from `lib.rs`.
- [ ] **Step 4: Run tests, verify pass.** `cargo test -p hornvale-kernel mass_` → PASS.
- [ ] **Step 5: Type-audit + fmt + commit.**
```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt
git add kernel/src/units.rs kernel/src/lib.rs
git commit -m "feat(kernel): Mass newtype (units family) for the coexistence stack"
```

### Task A2: `ResourceVector` + the open resource-axis basis

**Files:**
- Create: `kernel/src/ecology.rs`
- Modify: `kernel/src/lib.rs` (`mod ecology; pub use ecology::{ResourceAxis, ResourceKind, ResourceVector};`)

**Interfaces:**
- Produces:
  - `ResourceKind { Field, Stock }` (field = ambient/undepleted like photosynthate/mana; stock = depletable like prey/detritus, drives the trophic cap).
  - `ResourceAxis` — a registered basis member: `{ id: u16, label: &'static str, kind: ResourceKind }`. v1 constants: `PHOTOSYNTHATE`, `PLANT_FORAGE`, `ANIMAL_PREY`, `DETRITUS`, `MINERAL` (aquatic reserved). `pub fn v1_basis() -> &'static [ResourceAxis]`.
  - `ResourceVector` — a sparse map `BTreeMap<u16, f64>` (axis id → non-negative weight); `ResourceVector::new(weights: &[(ResourceAxis, f64)]) -> Result<ResourceVector, UnitError>` (rejects negative/non-finite; empty is legal = the zero vector); `weight(&self, axis: ResourceAxis) -> f64`; `is_zero(&self) -> bool`; `overlap(&self, other: &ResourceVector) -> f64` (Pianka symmetric niche overlap in `[0,1]`, `math::sqrt`-routed, `0.0` when either is zero).

- [ ] **Step 1: Write the failing tests** in `kernel/src/ecology.rs`:
```rust
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn zero_vector_is_legal_and_overlaps_nothing() {
        let z = ResourceVector::new(&[]).unwrap();
        assert!(z.is_zero());
        let herb = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap();
        assert_eq!(z.overlap(&herb), 0.0);
    }
    #[test]
    fn overlap_is_symmetric_unit_diagonal_and_disjoint_zero() {
        let a = ResourceVector::new(&[(PLANT_FORAGE, 0.5), (ANIMAL_PREY, 0.5)]).unwrap();
        let b = ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap();
        assert!((a.overlap(&a) - 1.0).abs() < 1e-9, "self-overlap is 1");
        assert!((a.overlap(&b) - b.overlap(&a)).abs() < 1e-12, "symmetric");
        let plants = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap();
        let meat = ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap();
        assert_eq!(plants.overlap(&meat), 0.0, "disjoint niches don't compete");
    }
    #[test]
    fn rejects_negative_weight() {
        assert!(ResourceVector::new(&[(MINERAL, -0.1)]).is_err());
    }
}
```
- [ ] **Step 2: Run, verify fail.** `cargo test -p hornvale-kernel -- ecology::` → FAIL.
- [ ] **Step 3: Implement `ecology.rs`.** Pianka overlap: `Σ_i p_i q_i / sqrt(Σ_i p_i² · Σ_i q_i²)`, iterating the union of axis ids from the two `BTreeMap`s; return `0.0` if either denominator factor is `0.0`. Route `sqrt` through `hornvale_kernel::math::sqrt`. Doc-comment every public item/variant/field. Type-audit tags: `bare-ok(ratio: weight)` on the map value, `bare-ok(identifier: id)` on axis id.
- [ ] **Step 4: Run, verify pass.** `cargo test -p hornvale-kernel -- ecology::` → PASS.
- [ ] **Step 5: Type-audit + fmt + commit.**
```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo fmt && git add kernel/src/ecology.rs kernel/src/lib.rs
git commit -m "feat(kernel): ResourceVector + open resource-axis basis (field/stock, zero-legal)"
```

### Task A3: author `mass` + `niche` on `SpeciesDef` (goblinoids)

**Files:**
- Modify: `domains/species/src/lib.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::{Mass, ResourceVector}` + the v1 axis constants.
- Produces: `SpeciesDef.mass: Mass`, `SpeciesDef.niche: ResourceVector`, populated for goblin/kobold/hobgoblin/bugbear.

- [ ] **Step 1: Write the failing test** in `domains/species/src/lib.rs` tests:
```rust
#[test]
fn goblinoids_carry_mass_and_a_nonzero_omnivore_niche() {
    let r = species_registry();
    for name in ["goblin", "kobold", "hobgoblin", "bugbear"] {
        let s = &r[name];
        assert!(s.mass.kilograms() > 0.0, "{name} has mass");
        assert!(!s.niche.is_zero(), "{name} eats something");
        // omnivores: both plant-forage and animal-prey present
        assert!(s.niche.weight(hornvale_kernel::PLANT_FORAGE) > 0.0);
        assert!(s.niche.weight(hornvale_kernel::ANIMAL_PREY) > 0.0);
    }
    // a modest, monotone mass band (bugbear largest, kobold smallest)
    assert!(r["bugbear"].mass.kilograms() > r["goblin"].mass.kilograms());
    assert!(r["goblin"].mass.kilograms() > r["kobold"].mass.kilograms());
}
```
(Use the actual registry accessor — confirm it is `species_registry()`/the roster fn while implementing.)
- [ ] **Step 2: Run, verify fail.** `cargo test -p hornvale-species goblinoids_carry_mass` → FAIL (no field `mass`).
- [ ] **Step 3: Add the two fields** to `SpeciesDef` (doc comments; `type-audit` tags are the newtypes themselves) and populate all four goblinoids: a modest mass band (e.g. kobold < goblin < hobgoblin < bugbear, in kg) and mixed omnivore niches (`PLANT_FORAGE` + `ANIMAL_PREY`, differing weights per species to give the overlap something to bite on). Update every other `SpeciesDef { .. }` literal in the crate (and `windows/lab/src/roster.rs`, `windows/lab/src/metrics.rs`'s test `twin`) so the workspace compiles — grep `SpeciesDef {` for the full set.
- [ ] **Step 4: Run, verify pass.** `cargo test -p hornvale-species` → PASS; `cargo build --workspace` compiles.
- [ ] **Step 5: fmt + commit.**
```bash
cargo fmt && git add domains/species windows/lab/src/roster.rs windows/lab/src/metrics.rs
git commit -m "feat(species): author mass + niche on the goblinoids (BIO-2 down-payment)"
```

### Task A4: niche → guild overlap (demography)

**Files:**
- Create: `domains/demography/src/niche.rs`
- Modify: `domains/demography/src/lib.rs` (`pub mod niche;`)

**Interfaces:**
- Consumes: `hornvale_kernel::{Mass, ResourceVector}`.
- Produces: `pub fn guild_overlap(species: &[(u32, ResourceVector)]) -> BTreeMap<(u32, u32), f64>` — symmetric overlap weight `w_ij ∈ [0,1]` for every ordered pair (including `w_ii = 1`), the competition weights coexist.rs consumes.

- [ ] **Step 1: Write the failing test** in `niche.rs`:
```rust
#[test]
fn guild_overlap_is_symmetric_with_unit_diagonal() {
    use hornvale_kernel::{ResourceVector, PLANT_FORAGE, ANIMAL_PREY};
    let sp = vec![
        (0u32, ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap()),
        (1u32, ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap()),
    ];
    let w = guild_overlap(&sp);
    assert_eq!(w[&(0, 0)], 1.0);
    assert_eq!(w[&(0, 1)], w[&(1, 0)]);
    assert_eq!(w[&(0, 1)], 0.0, "disjoint niches: no competition");
}
```
- [ ] **Step 2: Run, verify fail.** `cargo test -p hornvale-demography guild_overlap_is_symmetric` → FAIL.
- [ ] **Step 3: Implement `guild_overlap`** by calling `ResourceVector::overlap` over all pairs into a `BTreeMap<(u32,u32), f64>`. Doc-comment; type-audit tags on the bare `u32`/`f64`.
- [ ] **Step 4: Run, verify pass.** → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): derive guild overlap from niche vectors"`

### Task A5: niche → fractional trophic level (demography)

**Files:**
- Modify: `domains/demography/src/niche.rs`

**Interfaces:**
- Produces: `pub fn trophic_levels(species: &[(u32, ResourceVector)]) -> BTreeMap<u32, f64>` — fractional trophic level per species from the induced food web: a species eating only `PHOTOSYNTHATE`/`MINERAL` (abiotic, `ResourceKind::Field` at the base) is level 1; a `PLANT_FORAGE` eater is `1 + mean level of the producers it eats`; an `ANIMAL_PREY` eater is `1 + diet-weighted mean level of its animal prey`. `DETRITUS` is off-chain — a detritus-only feeder is marked level `1.0` but flagged (see `is_off_chain`). Deterministic fixpoint iteration (bounded passes, `total_cmp` ordering).
- Produces: `pub fn is_off_chain(niche: &ResourceVector) -> bool` — dominant axis is `DETRITUS`.

- [ ] **Step 1: Write the failing test** (encodes the ideonomy findings — omnivore between integers, autotroph at 1, detritivore off-chain):
```rust
#[test]
fn trophic_level_places_autotroph_omnivore_and_scavenger() {
    use hornvale_kernel::{ResourceVector, PHOTOSYNTHATE, PLANT_FORAGE, ANIMAL_PREY, DETRITUS};
    let sp = vec![
        (0u32, ResourceVector::new(&[(PHOTOSYNTHATE, 1.0)]).unwrap()),         // autotroph
        (1u32, ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap()),          // herbivore
        (2u32, ResourceVector::new(&[(PLANT_FORAGE, 0.5), (ANIMAL_PREY, 0.5)]).unwrap()), // omnivore
        (3u32, ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap()),           // predator
        (4u32, ResourceVector::new(&[(DETRITUS, 1.0)]).unwrap()),              // scavenger
    ];
    let lv = trophic_levels(&sp);
    assert!((lv[&0] - 1.0).abs() < 1e-9, "autotroph is level 1");
    assert!((lv[&1] - 2.0).abs() < 1e-9, "herbivore is level 2");
    assert!(lv[&2] > 2.0 && lv[&2] < 3.0, "omnivore between integers");
    assert!(lv[&3] > lv[&1], "predator above herbivore");
    assert!(is_off_chain(&sp[4].1), "scavenger is off-chain");
}
```
- [ ] **Step 2: Run, verify fail.** → FAIL.
- [ ] **Step 3: Implement.** Base level 1 for abiotic-axis feeders; iterate `level_s = 1 + Σ_axis(diet_fraction_axis · mean_level_of_axis_producers)` where an axis's "producers" are the species supplying that axis (autotrophs supply `PLANT_FORAGE`; any heterotroph body supplies `ANIMAL_PREY` — the "registers as a resource by its trophic identity" rule from spec §1.1). Fixed number of passes (e.g. species-count) for a deterministic fixpoint; `DETRITUS` weight excluded from the height recursion. Doc + type-audit tags.
- [ ] **Step 4: Run, verify pass.** → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): derive fractional trophic level from the induced food web"`

### Task A6: predation edges (mass-windowed) (demography)

**Files:**
- Modify: `domains/demography/src/niche.rs`

**Interfaces:**
- Consumes: `Mass`, `ResourceVector`, `trophic_levels`.
- Produces: `pub fn predation(species: &[(u32, Mass, ResourceVector)]) -> BTreeMap<u32, Vec<u32>>` — for each predator (active `ANIMAL_PREY` axis), the list of prey species whose `mass.ratio_to(predator)` falls in the window `[MIN_RATIO, MAX_RATIO]` (a predator eats bodies enough smaller than itself) **and** whose trophic level is strictly lower. Window constants declared with a CALIBRATED-provenance comment (predator/prey mass ratio; document the chosen band).

- [ ] **Step 1: Write the failing test:**
```rust
#[test]
fn predation_respects_the_mass_window_and_level_order() {
    use hornvale_kernel::{Mass, ResourceVector, PLANT_FORAGE, ANIMAL_PREY};
    let sp = vec![
        (0u32, Mass::new(40.0).unwrap(),   ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap()), // prey
        (1u32, Mass::new(4000.0).unwrap(), ResourceVector::new(&[(ANIMAL_PREY, 1.0)]).unwrap()),  // apex
        (2u32, Mass::new(4200.0).unwrap(), ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap()), // too big to be prey
    ];
    let pred = predation(&sp);
    assert_eq!(pred[&1], vec![0], "apex eats the small herbivore, not the co-huge grazer");
    assert!(pred.get(&0).map_or(true, |v| v.is_empty()), "the herbivore preys on nobody");
}
```
- [ ] **Step 2: Run, verify fail.** → FAIL.
- [ ] **Step 3: Implement** with the window + strict-lower-level filter; deterministic sorted prey lists. Doc + type-audit.
- [ ] **Step 4: Run, verify pass.** → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): mass-windowed predation edges over the derived food web"`

### Task A7: `home_range(mass)` → grain (demography)

**Files:**
- Create: `domains/demography/src/footprint.rs`
- Modify: `domains/demography/src/lib.rs` (`pub mod footprint;`)

**Interfaces:**
- Produces: `pub fn home_range(mass: Mass) -> f64` — cells-per-individual (Kleiber: super-linear in mass), `math::powf`-routed, `>= 0`. A big species → `> 1` cells/individual (fractional per-cell density); a small species → `< 1`.

- [ ] **Step 1: Write the failing test:**
```rust
#[test]
fn home_range_is_monotone_and_superlinear_in_mass() {
    use hornvale_kernel::Mass;
    let small = home_range(Mass::new(1.0).unwrap());
    let goblin = home_range(Mass::new(40.0).unwrap());
    let dragon = home_range(Mass::new(4000.0).unwrap());
    assert!(small < goblin && goblin < dragon, "bigger body → larger range");
    // super-linear: 100× mass → >100× range
    assert!(dragon / goblin > 100.0, "Kleiber super-linearity");
}
```
- [ ] **Step 2: Run, verify fail.** → FAIL.
- [ ] **Step 3: Implement** `home_range(mass) = A · powf(mass_kg, EXPONENT)` with `EXPONENT > 1` and `A` a scale constant, both `const` with provenance comment. Route through `math::powf`.
- [ ] **Step 4: Run, verify pass.** → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): Kleiber home-range grain from mass"`

### Task A8: the overlap-weighted K^β coexistence share (demography)

**Files:**
- Create: `domains/demography/src/coexist.rs`
- Modify: `domains/demography/src/lib.rs` (`pub mod coexist;`)

**Interfaces:**
- Consumes: `guild_overlap`, `home_range`, `hornvale_kernel::math::powf`.
- Produces: `pub fn cell_share(capacity: f64, present: &[(u32, f64)], overlap: &BTreeMap<(u32,u32), f64>, beta: f64, floor: f64) -> BTreeMap<u32, f64>` — the single-cell normalized share: `share_s = capacity · K_s^β / (Σ_j w_sj · K_j^β + floor^β)`, then any `share_s < floor` zeroed. `present` is `(species_id, K_s)` at this cell.

- [ ] **Step 1: Write the failing tests** (the monoculture↔oatmeal knob, and thinning):
```rust
#[test]
fn beta_slides_from_oatmeal_to_monoculture() {
    let overlap = full_overlap(&[0, 1]); // helper: w_ij = 1 for all pairs (same guild)
    let present = vec![(0u32, 2.0), (1u32, 1.0)]; // species 0 fitter
    let low = cell_share(1.0, &present, &overlap, 0.1, 1e-6);   // β→0: near-equal
    let high = cell_share(1.0, &present, &overlap, 20.0, 1e-6); // β→∞: winner-take-all
    let ratio_low = low[&0] / low[&1];
    assert!(ratio_low < 2.5, "low β shares broadly (oatmeal)");
    assert!(high[&1] < 1e-3 && high[&0] > 0.9, "high β → monoculture");
}
#[test]
fn more_same_guild_competitors_thin_everyone() {
    let two = cell_share(1.0, &vec![(0,1.0),(1,1.0)], &full_overlap(&[0,1]), 4.0, 1e-9);
    let three = cell_share(1.0, &vec![(0,1.0),(1,1.0),(2,1.0)], &full_overlap(&[0,1,2]), 4.0, 1e-9);
    assert!(three[&0] < two[&0], "a third competitor thins the first");
}
```
(Add a small `full_overlap(ids: &[u32]) -> BTreeMap<(u32,u32),f64>` test helper.)
- [ ] **Step 2: Run, verify fail.** → FAIL.
- [ ] **Step 3: Implement `cell_share`** with `math::powf` for every `K^β` and `floor^β`; deterministic iteration over the sorted `present` ids. Doc + type-audit tags.
- [ ] **Step 4: Run, verify pass.** → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): overlap-weighted K^β coexistence share (the master knob)"`

### Task A9: bidirectional trophic coupling (demography)

**Files:**
- Modify: `domains/demography/src/coexist.rs`

**Interfaces:**
- Consumes: `predation`, `trophic_levels`, `cell_share`.
- Produces: `pub fn couple_trophic(stack: &mut BTreeMap<u32, f64>, predation: &BTreeMap<u32, Vec<u32>>, levels: &BTreeMap<u32, f64>, shadow: f64)` — applied **highest-level-first**: (bottom-up) each predator's density is capped at `prey_biomass = Σ prey densities` (× a coefficient); (top-down) each prey's density is multiplied by `(1 - shadow · predator_density)` clamped `≥ 0` shadow floor. A predator with `prey_biomass == 0` collapses to the floor (the dormant-apex resting state).

- [ ] **Step 1: Write the failing tests:**
```rust
#[test]
fn apex_is_capped_by_prey_and_dormant_without_it() {
    let mut with_prey = BTreeMap::from([(0u32, 5.0), (1u32, 0.02)]); // prey 0, apex 1
    let pred = BTreeMap::from([(1u32, vec![0u32])]);
    let lv = BTreeMap::from([(0u32, 2.0), (1u32, 3.0)]);
    couple_trophic(&mut with_prey, &pred, &lv, 0.1);
    assert!(with_prey[&1] <= with_prey[&0], "apex density ≤ prey-supported cap");

    let mut no_prey = BTreeMap::from([(1u32, 0.02)]); // apex alone
    couple_trophic(&mut no_prey, &pred, &lv, 0.1);
    assert!(no_prey[&1] < 1e-6, "dormant apex self-limits with no prey");
}
#[test]
fn apex_shadow_suppresses_but_never_zeroes_prey() {
    let mut s = BTreeMap::from([(0u32, 5.0), (1u32, 0.02)]);
    let base_prey = s[&0];
    couple_trophic(&mut s, &BTreeMap::from([(1u32, vec![0u32])]), &BTreeMap::from([(0u32,2.0),(1u32,3.0)]), 0.5);
    assert!(s[&0] < base_prey && s[&0] > 0.0, "shadow suppresses, not zeroes");
}
```
- [ ] **Step 2: Run, verify fail.** → FAIL.
- [ ] **Step 3: Implement** the highest-level-first pass (sort species by `levels` descending, `total_cmp`, id tie-break). Doc the load-bearing ordering. Type-audit tags.
- [ ] **Step 4: Run, verify pass.** → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): bidirectional trophic coupling (prey cap + apex shadow)"`

### Task A10: soft-capacity overflow → emigration-pressure field (demography)

**Files:**
- Modify: `domains/demography/src/coexist.rs`

**Interfaces:**
- Consumes: `crate::flow::flow` (reused for spill), the per-cell stack.
- Produces: `pub fn emigration_pressure(geo: &Geosphere, demand: &CellMap<f64>, capacity: &CellMap<f64>) -> CellMap<f64>` — logistic overshoot `max(0, demand - capacity)` spilled to neighbours via the existing flow hydrology; the field the history campaign will read for displacement.

- [ ] **Step 1: Write the failing test:**
```rust
#[test]
fn overflow_is_zero_under_capacity_and_positive_when_crowded() {
    let geo = Geosphere::new(3);
    let cap = CellMap::from_fn(&geo, |_| 1.0);
    let under = CellMap::from_fn(&geo, |_| 0.5);
    let over = CellMap::from_fn(&geo, |c| if c.0 == 0 { 5.0 } else { 0.5 });
    let p_under = emigration_pressure(&geo, &under, &cap);
    let p_over = emigration_pressure(&geo, &over, &cap);
    assert!(geo.cells().all(|c| *p_under.get(c) == 0.0), "no pressure under capacity");
    assert!(*p_over.get(CellId(0)) > 0.0, "crowding makes pressure");
}
```
- [ ] **Step 2: Run, verify fail.** → FAIL.
- [ ] **Step 3: Implement** using `flow` to distribute overshoot. Doc + type-audit.
- [ ] **Step 4: Run, verify pass.** → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): soft-capacity overflow → emigration-pressure field"`

### Task A11: assemble the per-cell density stack (demography)

**Files:**
- Modify: `domains/demography/src/coexist.rs`

**Interfaces:**
- Consumes: A4–A10.
- Produces: `pub struct CoexistStack { pub density: Vec<(u32, CellMap<f64>)>, pub emigration_pressure: CellMap<f64> }` and `pub fn pack(geo: &Geosphere, per_species_k: &[(u32, CellMap<f64>)], species: &[(u32, Mass, ResourceVector)], beta: f64, floor: f64) -> CoexistStack` — the whole packer: derive overlap/levels/predation once, then per cell run `cell_share` (grain-scaled by `home_range`) → `couple_trophic`, and compute `emigration_pressure`. Draws nothing from the seed.

- [ ] **Step 1: Write the failing tests** (coexistence + determinism):
```rust
#[test]
fn pack_yields_coexistence_not_exclusion_and_is_deterministic() {
    let geo = Geosphere::new(3);
    let k0 = CellMap::from_fn(&geo, |_| 1.0);
    let k1 = CellMap::from_fn(&geo, |_| 0.6);
    let per = vec![(0u32, k0), (1u32, k1)];
    let sp = vec![
        (0u32, Mass::new(40.0).unwrap(), ResourceVector::new(&[(PLANT_FORAGE,1.0)]).unwrap()),
        (1u32, Mass::new(30.0).unwrap(), ResourceVector::new(&[(PLANT_FORAGE,1.0)]).unwrap()),
    ];
    let a = pack(&geo, &per, &sp, 4.0, 1e-6);
    let b = pack(&geo, &per, &sp, 4.0, 1e-6);
    assert_eq!(a.density, b.density, "byte-identical repack");
    let c = geo.cells().next().unwrap();
    let d0 = a.density.iter().find(|(t,_)| *t==0).unwrap().1.get(c);
    let d1 = a.density.iter().find(|(t,_)| *t==1).unwrap().1.get(c);
    assert!(*d0 > *d1 && *d1 > 0.0, "dominant leads but rival persists (coexistence)");
}
```
- [ ] **Step 2: Run, verify fail.** → FAIL.
- [ ] **Step 3: Implement `pack`** and `CoexistStack`. Doc + type-audit.
- [ ] **Step 4: Run, verify pass.** → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): assemble the per-cell coexistence density stack"`

### Task A12: settlements from the stack — tier × composition + species-relative rendering

**Files:**
- Create: `domains/demography/src/stack_condense.rs`
- Modify: `domains/demography/src/lib.rs` (`pub mod stack_condense;`)

**Interfaces:**
- Consumes: `CoexistStack`, `Mass`, `crate::condense::condense`, the founder floor idea.
- Produces:
  - `pub struct StackSettlement { pub cell: CellId, pub position: [f64;3], pub mass_total: f64, pub composition: Vec<(u32, f64)>, pub dominant: u32, pub rendered: Vec<(u32, HeadcountRender)> }`
  - `pub enum HeadcountRender { Count(u32), Lone, Colony(f64) }` — species-relative rendering (`Lone` when a species' mass-share is sub-one-body; `Colony` for hive/myconid extent).
  - `pub fn condense_stack(geo: &Geosphere, stack: &CoexistStack, mass_of: &BTreeMap<u32, Mass>, threshold: f64) -> Vec<StackSettlement>` — tier = `Σ_s density_s · mass_s` in the catchment (condense on the mass-weighted field); composition = the local density vector shape; every species keeps its founder attractor (retain the floor guarantee).

- [ ] **Step 1: Write the failing tests** (the two headline invariants):
```rust
#[test]
fn no_524_dragons_and_sub_one_share_renders_lone() {
    // one dragon-mass species with a tiny fractional density → "Lone", not a count
    // (construct a minimal stack; assert the render is HeadcountRender::Lone)
}
#[test]
fn every_species_retains_a_founder_settlement() {
    // a weak species outcompeted everywhere still founds its single strongest site
}
```
(Fill both with concrete `CoexistStack` fixtures during implementation — mirror `founder.rs`'s `peak_at` helper.)
- [ ] **Step 2: Run, verify fail.** → FAIL.
- [ ] **Step 3: Implement `condense_stack`** — mass-weight the stack, `condense` for attractors/catchments, read composition per attractor, render headcounts via `mass_share / mass_kg` with the `Lone`/`Colony` thresholds, and apply the founder floor per species. Doc + type-audit.
- [ ] **Step 4: Run, verify pass.** → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): settlements from the stack (tier × composition, species-relative)"`

### Task A13: the three byproducts — strife / wilderness / refugia (demography)

**Files:**
- Create: `domains/demography/src/byproducts.rs`
- Modify: `domains/demography/src/lib.rs` (`pub mod byproducts;`)

**Interfaces:**
- Produces: `pub struct Byproducts { pub strife: CellMap<f64>, pub wilderness: CellMap<f64>, pub refugia: Vec<(u32, CellMap<f64>)> }` and `pub fn byproducts(geo: &Geosphere, stack: &CoexistStack, per_species_k: &[(u32, CellMap<f64>)], floor: f64) -> Byproducts`:
  - **strife** = per cell, high where several overlapping-guild species have comparable K (a balanced-fitness contest measure — e.g. inverse Herfindahl of the same-guild `K^β` shares);
  - **wilderness** = `max(0, capacity - Σ densities)` fraction (packing fragmentation);
  - **refugia**[s] = cells where s's normally-dominant rival is below `floor` (hostility-zeroed) but s is not.

- [ ] **Step 1: Write the failing tests:**
```rust
#[test]
fn strife_peaks_at_balanced_fitness_not_at_high_k() {
    // two cells: (a) one dominant + one weak, (b) two equal → strife(b) > strife(a)
}
#[test]
fn refugia_track_hostility_zeroed_dominants() {
    // a cell where the strong species' K is floored but the weak's isn't → refugium for the weak
}
```
- [ ] **Step 2: Run, verify fail.** → FAIL.
- [ ] **Step 3: Implement `byproducts`.** Doc + type-audit.
- [ ] **Step 4: Run, verify pass.** → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): strife / wilderness / refugia derived byproducts"`

### Task A14: wire the packer into `DemographyReport`

**Files:**
- Modify: `domains/demography/src/lib.rs`, `domains/demography/src/render.rs`

**Interfaces:**
- Consumes: A11–A13.
- Produces: `DemographyReport` gains `pub stack: CoexistStack`, `pub stack_settlements: Vec<StackSettlement>`, `pub byproducts: Byproducts`; `report(geo, per_species_inputs, species: &[(u32, Mass, ResourceVector)], beta, floor, threshold)` calls `pack` → `condense_stack` → `byproducts`. Keep `per_species_k`; the old `settlements`/`condense_tagged` field is removed (its consumers move to `stack_settlements`).

- [ ] **Step 1: Update the existing `report_holds_k_fields_and_settlements` test** to the new signature + assert the stack and byproducts are populated; add a determinism test (`report(..) == report(..)`).
- [ ] **Step 2: Run, verify fail.** → FAIL (signature change).
- [ ] **Step 3: Rewire `report`;** extend `render.rs` with stack/strife/refugia PPM debug maps (mirror `density_ppm`). Remove the now-dead `condense_tagged` re-export (or keep `founder`'s floor logic if `condense_stack` reuses it — do not duplicate). Doc + type-audit.
- [ ] **Step 4: Run, verify pass.** `cargo test -p hornvale-demography` → PASS.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(demography): report builds the coexistence stack + byproducts"`

### Task A15: rewire worldgen settlement genesis + salient facts

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (~1900–2000; the `per_species_inputs` / `report(..).settlements` site at ~1951–1963), and predicate registration (conform to the `Domain` trait / `register_all` pattern).

**Interfaces:**
- Consumes: `SpeciesDef.{mass, niche}`, `DemographyReport.stack_settlements` + `byproducts`.
- Produces: settlement facts keyed on the dominant species (existing `PEOPLED_BY`), plus **new salient facts**: a bounded secondary-species list per settlement, a named refugium, a strife frontier. New predicate constants (registered per the Domain pattern). Composition vectors + fields stay in-memory (the-gathering doctrine) — never per-cell serialized.

- [ ] **Step 1: Write/extend a worldgen test** (`windows/worldgen/tests/species_worlds.rs` or a new `coexistence.rs`): a seed-42 world has settlements peopled by the goblinoids; determinism (same seed → identical settlements + facts); byte-identity of everything upstream of demography (sky/terrain/climate facts unchanged vs a pre-change golden).
- [ ] **Step 2: Run, verify fail.** → FAIL.
- [ ] **Step 3: Rewire** the genesis site to build `(u32, Mass, ResourceVector)` from `SpeciesDef`, call the new `report`, and emit `stack_settlements` + salient byproduct facts. Register new predicates through the Domain pattern. Doc every new predicate const with a type-audit tag.
- [ ] **Step 4: Run, verify pass.** `cargo test -p hornvale-worldgen` → PASS; `cargo nextest run --workspace` green.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(worldgen): settlement genesis reads the coexistence stack; salient facts"`

### Task A16: calibrate + freeze β on the goblinoids (preregistered)

**Files:**
- Modify: `windows/lab/src/metrics.rs` (a `per-cell-diversity` / rank-abundance metric), a new `studies/the-coexistence-stack.study.json`, and the β `const` (in `coexist.rs`) with a CALIBRATED provenance block.

**Interfaces:**
- Consumes: the packer; the Lab runner (mirror `windows/lab/tests/gathering_calibration.rs`).
- Produces: a preregistered β→diversity calibration, β frozen as a save-format constant; a Zipf-by-mass settlement check.

- [ ] **Step 1: Preregister** the target (mean per-cell rank-abundance slope: a few dominant + a graded tail) and the Zipf-by-mass expectation in the study JSON + a calibration test asserting the frozen β meets it on the current 4-goblinoid roster. Write it to fail first (β unset / placeholder).
- [ ] **Step 2: Run, verify fail.** `cargo test -p hornvale-lab coexistence_calibration` → FAIL.
- [ ] **Step 3: Sweep β** (Lab, local, SKIP_CENSUS), pick the value meeting the preregistered diversity target, and freeze it as `pub const BETA: f64` with the measured provenance comment (mirror `carrying_capacity.rs`). Re-pin any goblinoid-dependent fixture that shifts, in this commit.
- [ ] **Step 4: Run, verify pass.** Calibration + `cargo nextest run --workspace` green.
- [ ] **Step 5: fmt + commit.** `git commit -m "feat(lab): calibrate + freeze β to per-cell diversity (goblinoid baseline)"`

### Task A17: Stage-A artifacts + gate

**Files:**
- Generated artifacts (almanac/map/registry/manifest/lab), `docs/audits/type-audit-report.md`.

- [ ] **Step 1: Regenerate committed artifacts** (SKIP_CENSUS — never regenerate the census locally): follow the CI "Artifacts are current" step; regenerate the type-audit report.
- [ ] **Step 2: Run the full gate.** `make gate` → green (heavy tier skipped).
- [ ] **Step 3: Commit** any artifact drift. `git commit -m "regen(coexistence-stack): Stage-A artifacts + type-audit report"`
- [ ] **Step 4: Absorb main** (`make preflight`; on NO-GO merge main into the branch and re-run the gate) — Stage A is a plan-stage boundary. Not mid-measurement (β is frozen, so safe).

---

## STAGE B — the menagerie (GATED on Stage A + Nathan's roster sign-off)

> Stage B's per-species data (each menagerie member's `mass`, `niche`, psych/perception/articulation, family + proto) does not exist until drafted and approved. It is **not** placeheld here; Task B1 produces it and Task B2 commits the approved result. Everything downstream is a re-measurement against the Stage-A-frozen engine.

### Task B1: draft the ~8–12 axis-spanning menagerie (models author)

- [ ] **Step 1:** Draft ~8–12 species spanning `mass` (mouse-tier → dragon-tier), `niche` (≥1 `PHOTOSYNTHATE` autotroph, ≥1 `DETRITUS` scavenger/myconid, ≥1 pure `ANIMAL_PREY` apex, mixed omnivores), and psych/perception/articulation variety (nocturnal, keen-night-vision, tonal, exotic-manner, non-hierarchic). For each: name, family (+ proto vector if a new multi-member family), noun, all vectors, mass, niche. Produce as a review table.
- [ ] **Step 2 — GATE:** Present the drafted roster to Nathan for sign-off. **Do not proceed to B2 without approval.** Incorporate edits.

### Task B2: commit the approved roster to `domains/species`

- [ ] **Step 1:** Write a test asserting each new species is registered with a non-zero (or deliberately-zero, for a construct) niche + positive mass, and that `every_multi_member_family_has_a_proto` still holds.
- [ ] **Step 2:** Run, verify fail. → FAIL.
- [ ] **Step 3:** Add the approved `SpeciesDef` literals + family protos; update all `SpeciesDef {` call-sites to compile.
- [ ] **Step 4:** Run, verify pass; `cargo build --workspace`.
- [ ] **Step 5:** fmt + commit. `git commit -m "feat(species): the axis-spanning menagerie (Stage B roster)"`

### Task B3: re-measure against the frozen engine + re-pin fixtures

- [ ] **Step 1:** Run the preregistered β→diversity + Zipf-by-mass checks with the full roster — β is **re-measured, not re-tuned** (assert it still meets target, or record honestly if the richer roster shifts the diversity and needs a documented note). Verify trophic coupling is now **live** (an apex + its prey coexist; apex-shadow visible) and cross-scale rendering fires (`Lone`/`Colony`).
- [ ] **Step 2:** Re-pin every roster-dependent fixture that drifts (phonology, proto-language, lab metrics/schema, calibration) — in the drifting commits, not deferred.
- [ ] **Step 3:** `make gate` green (SKIP_CENSUS). Commit. `git commit -m "test(coexistence-stack): re-measure + re-pin against the menagerie (frozen engine)"`

### Task B4: campaign close

- [ ] **Step 1:** Chronicle entry (`book/src/chronicle/`), model card (numbers carried, per the night-sky evidence lesson), freshness sweep of stale chapters; re-score any Confidence-Gradient bet this resolves.
- [ ] **Step 2:** Register the idea-registry rows from spec §6 (resource-vector niche, β knob, trophic coupling, strife/wilderness/refugia, **metabolism over non-material resource-fields**, generative-species-at-scale). Run `cargo test -p hornvale --test docs_consistency`.
- [ ] **Step 3:** One-page retrospective (`docs/retrospectives/`); flip spec + plan shipped markers.
- [ ] **Step 4 — census (AWS, with Nathan's authorization):** warn Nathan, then regenerate the census on the AWS spot box (`make regen-remote`); re-pin census fixtures. Merge to main via the closing-a-campaign skill.

---

## Self-Review

- **Spec coverage:** §1.1 primitive → A1/A2/A3; guild/level/predation → A4/A5/A6; footprint → A7; K^β share → A8; trophic coupling → A9; soft-overflow → A10; the stack → A11; tier×composition + rendering → A12; byproducts → A13; report/worldgen wiring + salient facts → A14/A15; calibration/freeze β → A16; menagerie + staged re-measure → §2/§3 → B1–B3; forward-compat (open basis, field/stock, zero-vector) → A2; idea rows + close → B4. All spec sections map to a task.
- **Placeholder scan:** Stage A steps carry real test + implementation intent with exact signatures. Stage B data is *deferred by gate*, not placeheld (B1 produces it, B2 commits it) — this is correct staging, not a plan hole. A12/A13 test bodies are described precisely with the fixture pattern to mirror (`founder.rs::peak_at`); fill concretely at implementation.
- **Type consistency:** `Mass`/`ResourceVector` (kernel) flow into `niche.rs` (A4–A6), `footprint.rs` (A7), `coexist.rs` (A8–A11), `stack_condense.rs` (A12), `byproducts.rs` (A13), then `report` (A14) and worldgen (A15). `pack` → `CoexistStack` → `condense_stack`/`byproducts` names are consistent across A11–A14.
