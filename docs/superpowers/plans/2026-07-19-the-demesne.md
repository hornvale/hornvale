# The Demesne Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore the rank collapse in the habitat carrying capacity — make resource `supply` a per-axis spatial vector so different niches peak in different places, and peoples/creatures spread across distinct terrain.

**Architecture:** `niche_per_species_k` (worldgen) today collapses a species' 5-axis uptake vector to its scalar sum and multiplies one NPP field (`supply = base_carrying(cell) × total_uptake`). This plan replaces that with the axis dot product `Σ_axis uptake[axis]·supply_field[axis](cell)`, where `PHOTOSYNTHATE`'s field is the existing `base_carrying` (unchanged, keeps its conditioning) and `MINERAL`/`PLANT_FORAGE`/`DETRITUS` get their own spatial fields (mineral from The Ground's prospectivity, forage NPP-derived, detritus ambient). `ANIMAL_PREY` stays 0 (Stage 2). The uptake vectors, competition, trophic model, `dominant_species`, stronghold machinery, and `condense_stack` are untouched — they consume the richer per-species K automatically.

**Tech Stack:** Rust (edition 2024), std + serde only. Kernel: `Geosphere`, `CellId`, `CellMap<f64>`, `ResourceVector`, `v1_basis()`, and the axis consts `PHOTOSYNTHATE, PLANT_FORAGE, ANIMAL_PREY, DETRITUS, MINERAL`. Terrain: `GeneratedTerrain::prospectivity_at(cell) -> f64` ([0,1], The Ground). Demography: `carrying_capacity`, the CoexistStack, `byproducts::dominant_species`/strongholds. `cargo nextest`, `SKIP_CENSUS=1`, `tools/type-audit`.

## Global Constraints

- **CENSUS REGEN DEFERRED (Nathan): NEVER run `make regen-remote`/`make rebaseline`/a census/`HV_CENSUS=1`.** Every suite run uses `SKIP_CENSUS=1`. The 1000-seed census settlement-composition fixtures (`book/src/laboratory/generated/*/rows.csv`) LAG — the accepted trade. `scripts/regenerate-artifacts.sh` (skips censuses by default) is the only regen you run.
- **Genesis-changing:** placement moves. Likely **not a stream-label epoch** (placement is deterministic over K, no new seed draws — T3 confirms via the stream manifest). Artifacts (settlement gallery, downstream) re-baseline in-commit.
- **Emergent, not authored (0021):** the diversification keystone measures the generated placement; never author a kind's cell.
- **Determinism:** no `HashMap`/`HashSet` (`CellMap`/`BTreeMap`/`BTreeSet`/`Vec`); no RNG; all `f64` ordering via `total_cmp`.
- **Layering:** the new per-axis field builders live where their inputs are in scope (worldgen composition root reads terrain+climate; or a demography helper). Every public item documented; every pub-boundary primitive a valid `type-audit:` tag (ratified classes).
- **Run `cargo fmt` + `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` before every commit** (type-audit is CI-only). Cost-order fmt+clippy first.
- **Calibration:** field normalization constants and `carrying_capacity`/`CONDENSATION_THRESHOLD` are frozen save-format values; re-fit only if grounding requires, with a provenance comment.

## File Structure

- `windows/worldgen/src/lib.rs` — the per-axis supply-field builders (mineral from prospectivity, forage NPP-derived, detritus ambient) + the `niche_per_species_k` re-point (line ~601).
- `windows/worldgen/tests/` — a `demesne.rs`: the rank-restoration unit test + the emergent distinct-dominants keystone (preregistered) + the calibration checks.
- `windows/worldgen/tests/` or wherever `menagerie_fauna_hold_resource_partitioned_strongholds` lives — un-`#[ignore]` its Stage-1-reachable part (T3).
- book/docs — settlement gallery re-baseline, chronicle, retrospective, registry (T4).

---

## Task 1: the per-axis supply-field builders (mineral / forage / detritus)

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (add the field builders + `DETRITUS_AMBIENT`)
- Test: `windows/worldgen/tests/demesne.rs` (new)

**Interfaces:**
- Consumes: `GeneratedTerrain::prospectivity_at`, the NPP/`base_carrying` scale, `CellMap::from_fn`, `Geosphere`.
- Produces:
  - `pub fn mineral_supply_field(geo, terrain, scale: f64) -> CellMap<f64>` — prospectivity normalized to the supply scale.
  - `pub fn forage_supply_field(geo, base_carrying: &CellMap<f64>) -> CellMap<f64>` — an NPP-derived grazable field (a documented fraction of `base_carrying`).
  - `pub const DETRITUS_AMBIENT: f64` — the ambient detritus supply.

- [ ] **Step 1: Write the failing tests** (`demesne.rs`)

```rust
use hornvale_kernel::{CellMap, Geosphere, CellId};

#[test]
fn mineral_supply_tracks_prospectivity_spatially() {
    // On a real seed-42 world, the mineral field peaks where prospectivity peaks
    // and is 0 where prospectivity is 0 — a genuinely SPATIAL field, not a constant.
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    ).unwrap();
    // build the field via the same terrain the composition root uses; assert it VARIES
    // (>=2 distinct values across cells) and is monotone in prospectivity at two probe cells.
    // (bind the terrain handle + geo from the worldgen API, as niche_per_species_k does.)
}

#[test]
fn forage_supply_is_a_fraction_of_base_carrying_and_deterministic() {
    let geo = Geosphere::new(3);
    let base = CellMap::from_fn(&geo, |c| (c.0 as f64) * 0.1);
    let a = hornvale_worldgen::forage_supply_field(&geo, &base);
    let b = hornvale_worldgen::forage_supply_field(&geo, &base);
    for c in geo.cells() {
        assert_eq!(a.get(c), b.get(c));
        assert!(*a.get(c) <= *base.get(c), "forage is a fraction of primary production");
    }
}
```

- [ ] **Step 2: Run to verify failure** — `SKIP_CENSUS=1 cargo test -p hornvale-worldgen --test demesne` → FAIL (functions undefined).

- [ ] **Step 3: Implement the builders** (in `windows/worldgen/src/lib.rs`, near `niche_per_species_k`)

```rust
/// Ambient detritus supply (BIO-35 Stage 1): dead-matter resource is treated as
/// broadly available this stage — a small constant floor, not a spatial field.
/// A real spatial detritus field is a later refinement. type-audit: bare-ok(ratio)
pub const DETRITUS_AMBIENT: f64 = 0.2;

/// Fraction of primary production that is grazable plant forage. Plant-forage
/// supply tracks photosynthate spatially at a reduced amplitude.
/// type-audit: bare-ok(ratio)
const FORAGE_FRACTION: f64 = 0.5;

/// The `PLANT_FORAGE` supply field: a fraction of the NPP-based `base_carrying`
/// (grazable matter tracks primary production). Pure, deterministic.
pub fn forage_supply_field(geo: &Geosphere, base_carrying: &hornvale_kernel::CellMap<f64>)
    -> hornvale_kernel::CellMap<f64> {
    hornvale_kernel::CellMap::from_fn(geo, |c| base_carrying.get(c) * FORAGE_FRACTION)
}

/// The `MINERAL` supply field: The Ground's per-cell mineral prospectivity
/// ([0,1]) scaled to the supply range so it is comparable to `base_carrying` in
/// the weighted sum (`scale` = the mineral supply amplitude; the one calibration
/// knob, re-fit in T3). Pure, deterministic, `total_cmp`-free (a direct read).
pub fn mineral_supply_field(geo: &Geosphere, terrain: &GeneratedTerrain, scale: f64)
    -> hornvale_kernel::CellMap<f64> {
    hornvale_kernel::CellMap::from_fn(geo, |c| terrain.prospectivity_at(c) * scale)
}
```

Fill in the seed-42 test's field-construction + probe cells from the terrain handle (`terrain.prospectivity_at`) the way `niche_per_species_k` reaches terrain.

- [ ] **Step 4: Run to green** — `SKIP_CENSUS=1 cargo test -p hornvale-worldgen --test demesne`.

- [ ] **Step 5: fmt + clippy + type-audit + commit**

```bash
cargo fmt && cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/
git commit -m "feat(worldgen): per-axis supply fields — mineral (prospectivity), forage, detritus — the-demesne T1"
```

---

## Task 2: re-point `niche_per_species_k` to the axis dot product + the emergence keystone

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (`niche_per_species_k`, lines ~593-608)
- Test: `windows/worldgen/tests/demesne.rs`

**Interfaces:**
- Consumes: `mineral_supply_field`, `forage_supply_field`, `DETRITUS_AMBIENT` (T1); `v1_basis()`, the axis consts, `ResourceVector::weight`, `carrying_capacity`, `dominant_species`.
- Produces: the rank-restored per-species K; a settlement-dominant-diversity measurement helper.

- [ ] **Step 1: Write the failing rank-restoration unit test**

Extract the supply dot product into a pure, unit-testable helper (Step 3 uses it):

```rust
/// The per-axis resource supply for one niche at one cell (BIO-35): the dot
/// product of the species' uptake vector with the per-cell supply vector — the
/// rank-restored replacement for the old `base_carrying × Σuptake` scalar.
/// type-audit: bare-ok(ratio: return)
pub fn axis_supply(niche: &hornvale_kernel::ResourceVector,
                   per_axis: &[(hornvale_kernel::ResourceAxis, f64)]) -> f64 {
    per_axis.iter().map(|(axis, supply)| niche.weight(*axis) * supply).sum()
}
```

```rust
#[test]
fn different_uptake_vectors_peak_in_different_cells() {
    use hornvale_kernel::{ResourceVector, PHOTOSYNTHATE, MINERAL};
    // THE RANK-RESTORATION KEYSTONE: two cells — A photosynthate-rich, B mineral-rich.
    let cell_a = [(PHOTOSYNTHATE, 10.0), (MINERAL, 0.0)];
    let cell_b = [(PHOTOSYNTHATE, 0.0),  (MINERAL, 10.0)];
    let plant = ResourceVector::new(&[(PHOTOSYNTHATE, 1.0), (MINERAL, 0.0)]).unwrap();
    let rock  = ResourceVector::new(&[(PHOTOSYNTHATE, 0.0), (MINERAL, 1.0)]).unwrap();
    // the plant-eater's supply is higher in A; the rock-eater's is higher in B.
    assert!(axis_supply(&plant, &cell_a) > axis_supply(&plant, &cell_b), "plant-eater peaks in A");
    assert!(axis_supply(&rock,  &cell_b) > axis_supply(&rock,  &cell_a), "rock-eater peaks in B");
    // MUTATION GUARD: the OLD scalar `supply = base(cell) × Σuptake` would give both
    // niches the SAME cell ranking (base is identical per cell), so this pair of
    // strict inequalities cannot both hold under the collapsed model.
}
```

- [ ] **Step 2: Measure the seed-42 baseline FIRST (preregistration).** Before the re-point, measure the current distinct-dominant count on seed-42 (+ seeds 1,7,13,99): count `dominant_species` distinct kinds across cells, and the distinct `peopled-by` kinds among settlements. Record as `BASELINE_DOMINANTS_42` etc. consts + a comment (measured: seed-42 = 2 peopled dominants goblin/hobgoblin, 0 fauna dominants). Set `PREREGISTERED_MIN_DOMINANTS` from theory (a material rise — e.g. ≥ 4 distinct dominant kinds including ≥1 non-goblinoid and ≥1 fauna; frozen before the readout; the ruler counts a kind only if it dominates ≥ N cells, not one, to avoid the Confluence denominator artifact). Run the keystone test to see it FAIL against the current scalar supply.

- [ ] **Step 3: Re-point `niche_per_species_k`.** Replace the `total_uptake` scalar (lines 593-596) and line 601's `supply`:

```rust
    // The Confluence/Demesne: supply is a per-axis VECTOR — PHOTOSYNTHATE rides the
    // existing NPP-based base_carrying (keeps its conditioning); MINERAL/FORAGE/
    // DETRITUS get their own fields; ANIMAL_PREY is Stage 2 (0). The uptake vector
    // now SELECTS a spatial combination (rank restored), not just a scalar rescale.
    let mineral = mineral_supply_field(geo, terrain, MINERAL_SUPPLY_SCALE);
    let forage = forage_supply_field(geo, &base_carrying);
    // ... in the per-species map(), replacing total_uptake + the `supply` line:
            let k = hornvale_kernel::CellMap::from_fn(geo, |cell| {
                let s = substrate.get(cell);
                // Rank-restored supply via the extracted helper (Step 1). PHOTOSYNTHATE
                // rides base_carrying; ANIMAL_PREY is Stage 2 (0.0).
                use hornvale_kernel::{PHOTOSYNTHATE, PLANT_FORAGE, MINERAL, DETRITUS, ANIMAL_PREY};
                let per_axis = [
                    (PHOTOSYNTHATE, *base_carrying.get(cell)),
                    (PLANT_FORAGE, *forage.get(cell)),
                    (MINERAL, *mineral.get(cell)),
                    (DETRITUS, DETRITUS_AMBIENT),
                    (ANIMAL_PREY, 0.0),
                ];
                let supply = axis_supply(&bio.niche, &per_axis);
                let saturated = supply / (1.0 + supply);
                saturated
                    * cn.temperature.eval(s.temperature_c, floor_buf)
                    * cn.moisture.eval(s.moisture, floor_buf)
                    * cn.insolation.eval(s.insolation, floor_buf)
                    * cn.elevation.eval(s.elevation, 0.0)
            });
```

with `const MINERAL_SUPPLY_SCALE: f64 = 1.0;` (the mineral amplitude — the T3 calibration knob). Remove the now-unused `total_uptake`. Hoist `mineral`/`forage` out of the per-species loop (built once).

- [ ] **Step 4: Write + run the emergence keystone.** `settlements_and_dominants_diversify_on_seed_42`: build seed-42, count distinct dominant kinds + distinct `peopled-by` kinds, assert `>= PREREGISTERED_MIN_DOMINANTS` and that a non-goblinoid (kobold) AND a fauna kind (treant or xorn) appear as a dominant/stronghold holder. Run to green. If unmet, tune `MINERAL_SUPPLY_SCALE`/`FORAGE_FRACTION` (document) — never author a placement.

- [ ] **Step 5: fmt + clippy + type-audit + commit** (SKIP_CENSUS=1 on any world build).

```bash
cargo fmt && cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/
git commit -m "feat(worldgen): rank-restore per-species K to the axis dot product; dominants diversify — the-demesne T2"
```

---

## Task 3: re-calibrate + epoch surface + re-enable the strongholds test

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (normalization consts if re-fit), the strongholds test's `#[ignore]`.
- Test: the grounding + count + identity checks in `demesne.rs`.

- [ ] **Step 1: K-grounding + settlement-count check.** Re-run the biomass-by-latitude grounding (the-gathering discipline — the `gathering_calibration` / demography-report gradient) and the seed-42 settlement count. If the vector supply drifted K's magnitude (gradient out of tolerance, or count outside ~[100,400]), re-fit `MINERAL_SUPPLY_SCALE`/`FORAGE_FRACTION`/`CONDENSATION_THRESHOLD` back into band and re-pin provenance. Assert both in tolerance (SKIP_CENSUS=1).

- [ ] **Step 2: Epoch surface.** Confirm the settlement seed-derivation's draw ORDER is unchanged (placement is deterministic over K, no new draws — same as The Confluence). Verify the stream manifest is unchanged; document in a code comment + the ledger: derived-formula change, NOT a stream-label epoch. Only if a draw-order shift is found, add the epoch suffix and say so loudly.

- [ ] **Step 3: Same-seed byte-identity.** A test: same seed + pins → byte-identical world under the new behavior (build seed-42 twice, assert identical serialization).

- [ ] **Step 4: Re-enable the menagerie strongholds test to Stage-1 extent.** Find `menagerie_fauna_hold_resource_partitioned_strongholds` (`#[ignore]`d). Un-ignore its Stage-1-reachable assertions (≥ the preregistered distinct-dominant count + treant/xorn strongholds); keep the dragon/prey assertions split off and `#[ignore]`d with a "Stage 2 (prey field)" note. If treant/xorn strongholds don't materialize, report loudly (measure — don't fake).

- [ ] **Step 5: fmt + type-audit + commit** (SKIP_CENSUS=1).

```bash
SKIP_CENSUS=1 cargo test -p hornvale-worldgen -p hornvale-demography 2>&1 | tail -5
cargo fmt && cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/
git commit -m "calib(worldgen): re-ground K under the vector supply + Stage-1 strongholds re-enabled — the-demesne T3"
```

---

## Task 4: artifact re-baselines + chronicle/retro/registry + close

- [ ] **Step 1: Regenerate drifted committed artifacts** (SKIP_CENSUS=1 — NO census; census fixtures lag, deferred).

```bash
SKIP_CENSUS=1 bash scripts/regenerate-artifacts.sh
git diff --stat book/   # settlement gallery / downstream moved; census rows.csv NOT touched
```

Review + stage the intended drift (settlement composition changed — more diverse peoples).

- [ ] **Step 2: Chronicle + retrospective.** `book/src/chronicle/the-demesne.md` (+ SUMMARY): the story — the habitat K collapsed a 5-axis niche to one NPP scalar, so every niche shifted magnitude not place and the world went 100% goblinoid; restoring supply to a per-axis vector lets a niche direction peak in different terrain — kobold and the rock-eater (xorn) and the tree-folk (treant) hold ground; emergent, the first stage of living biomes (the food web / prey is next). **Name concepts, not registry IDs, in prose** (`docs_consistency`). `docs/retrospectives/the-demesne.md` (process lessons; promote followups incl. the Living-Biomes arc). In `book/src/frontier/idea-registry.md`: advance **BIO-35** (abiotic half shipped; prey/Stage-2 named), note the arc; never delete a row.

- [ ] **Step 3: Full gate.** `SKIP_CENSUS=1 make gate` green; `git diff --exit-code book/ docs/` clean after regen (except the intended drift, committed).

```bash
SKIP_CENSUS=1 make gate
git add -A && git commit -m "docs(the-demesne): settlement re-baseline + chronicle/retro/registry (close) — the-demesne T4"
```

- [ ] **Step 4: STOP — G6 hard stop.** Present the post-G3 ledger digest (the epoch/save-format #4 leading; census DEFERRED per Nathan). FF/push/pull are Nathan's calls under `closing-a-campaign`. **No census regen** (deferred).

---

## Self-review notes (author)

- **Spec coverage:** §3.1 supply vector → T1 (mineral/forage/detritus) + T2 (photosynthate=base_carrying, assembled); §3.2 wiring → T2 (line 601 re-point); §3.3 emergence → T2 keystone; §5 acceptance (1 fields→T1, 2 rank-restoration→T2, 3 distinct-dominants→T2, 4 strongholds→T3, 5 calibration→T3, 6 identity→T3) covered; §6 save-format (epoch surface, census DEFERRED, SKIP_CENSUS) → T3+T4+Global Constraints; §7 non-goals (prey Stage 2 = ANIMAL_PREY×0) enforced; §8 constants → T1/T3.
- **Census discipline:** no task runs a census/`make regen-remote`/`HV_CENSUS=1`; all runs `SKIP_CENSUS=1`; the census fixtures deferred (Nathan). This is the campaign's binding constraint.
- **Emergence:** T2's keystone measures the generated dominants against a preregistered baseline; never authors a placement.
- **Type consistency:** `mineral_supply_field(geo, terrain, scale)`, `forage_supply_field(geo, base_carrying)`, `DETRITUS_AMBIENT`, `MINERAL_SUPPLY_SCALE`, the axis-dot-product supply, consistent across T1/T2/T3. `niche_per_species_k`'s signature is unchanged (fields built internally).
- **The rank-restoration is the keystone:** T2 Step 1 unit test proves two uptake vectors peak in different cells — the mechanism, isolated; the mutation (revert to scalar) collapses them.
