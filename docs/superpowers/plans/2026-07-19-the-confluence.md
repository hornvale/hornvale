# The Confluence Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Settlements condense near fresh water — re-point the carrying-capacity field's freshwater term at the real river network so towns emerge on/adjacent-to rivers.

**Architecture:** A `river_proximity` field (BFS from The Freshet's `WaterKind::River` cells over the geosphere) replaces the smooth `drainage.max(moisture)` proxy in `carrying_inputs_of`, so `K` spikes near rivers and the existing up-gradient condensation pulls settlements there. Settlement placement stays **emergent** (0021). Because settlements move, this is a genesis change — an epoch + a census regen.

**Tech Stack:** Rust (edition 2024), std + serde only. Kernel: `Geosphere`, `CellId`, `CellMap<T>` (`from_fn`, `get`). Terrain: `TectonicGlobe.water_kind`, `WaterKind`, `GeneratedTerrain`. Demography: `carrying_capacity`, `CarryingInput`, `FRESHWATER_BONUS`. `cargo nextest`, `cargo test --doc`, `cargo clippy`, `cargo fmt`, `tools/type-audit`.

## Global Constraints

- **GENESIS-CHANGING: settlements move → an epoch + a CENSUS REGEN.** The census regen is **Nathan's explicit, manual, AWS step at merge** — the plan NEVER runs `make rebaseline` / `make regen-remote` / a census / `HV_CENSUS=1` locally or automatically. Local runs use `SKIP_CENSUS=1`. The census fixtures (`book/src/laboratory/generated/*/rows.csv`) lag until Nathan's pre-merge AWS regen — that lag is the chosen trade.
- **Emergent, not authored (0021):** settlement-near-water must fall out of the `K` field; never hand-place a town on a river.
- **Determinism:** no `HashMap`/`HashSet` (`CellMap`/`BTreeMap`/`BTreeSet`/`Vec`); no RNG; no wall-clock; all `f64` ordering via `total_cmp` (never native `<`/`>=`).
- **Layering:** terrain depends only on the kernel; worldgen (composition root) may read terrain + demography. Every public item/field documented; every pub-boundary primitive a valid `type-audit:` tag (ratified classes only).
- **Run `cargo fmt` + `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` before every commit** (type-audit is CI-only). Cost-order fmt+clippy first, then scoped tests.
- **Re-baseline discipline:** any committed artifact that drifts (settlement gallery, locale/scene, Surmise transcript) re-baselines **in the commit that drifts it** — except the census fixtures, which wait for Nathan's AWS regen.
- **Calibration:** `FRESHWATER_BONUS` (0.5) and `CONDENSATION_THRESHOLD` (10.0) are frozen save-format constants; re-fit only if the K-grounding / settlement-count check requires, and re-pin with a provenance comment.

Authored tuning constants: `RIVER_REACH` (proximity decay radius, tuned to *adjacency*), `MOISTURE_FLOOR_WEIGHT` (how much the moisture proxy survives as a floor).

---

## File Structure

- `domains/terrain/src/water.rs` (modify) — add `river_proximity(geo, water_kind, reach) -> CellMap<f64>` beside `water_field`; `RIVER_REACH`.
- `domains/terrain/src/lib.rs` / the `GeneratedTerrain` handle (modify) — expose `water_kind` per cell (a `water_kind_at(cell) -> WaterKind` accessor or the globe) so worldgen can build the proximity field.
- `windows/worldgen/src/lib.rs` (modify) — `carrying_inputs_of`: compute `river_proximity` once, re-point line 425's `freshwater`; `MOISTURE_FLOOR_WEIGHT`.
- `windows/worldgen/tests/` (modify/add) — "K spikes near rivers" + "settlements condense near rivers" (emergent, preregistered) + the calibration grounding check.
- `domains/demography/src/carrying_capacity.rs` (modify only if calibration requires) — `FRESHWATER_BONUS` re-fit.
- `windows/vessel/` — flip the Surmise reachability test to success (T4).
- book/docs — settlement gallery re-baseline, chronicle, retrospective, registry (T4).

---

## Task 1: `river_proximity` field builder (terrain)

**Files:**
- Modify: `domains/terrain/src/water.rs`, `domains/terrain/src/lib.rs` (re-export + `GeneratedTerrain` water_kind accessor)
- Test: inline `#[cfg(test)]` in `water.rs`

**Interfaces:**
- Consumes: `Geosphere` (`cells`, `neighbors`, `cell_count`), `CellId`, `CellMap<T>`, `WaterKind` (The Freshet).
- Produces:
  - `pub const RIVER_REACH: u32` (hop radius of the proximity falloff)
  - `pub fn river_proximity(geo: &Geosphere, water_kind: &CellMap<WaterKind>, reach: u32) -> CellMap<f64>`
  - a way for worldgen to get `water_kind` per cell from `GeneratedTerrain` (accessor `water_kind_at(cell) -> WaterKind`, or expose the globe's `water_kind` map).

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn river_proximity_is_one_on_a_river_cell_and_decays_with_hops() {
    // A tiny globe; mark one cell River, rest DryLand; proximity is 1.0 on it,
    // strictly decreasing by hop distance, 0.0 beyond reach.
    let geo = hornvale_kernel::Geosphere::new(3);
    let river = hornvale_kernel::CellId(0);
    let wk = hornvale_kernel::CellMap::from_fn(&geo, |c| {
        if c == river { WaterKind::River } else { WaterKind::DryLand }
    });
    let prox = river_proximity(&geo, &wk, RIVER_REACH);
    assert_eq!(*prox.get(river), 1.0, "on a river cell");
    // an immediate neighbour is high but < 1
    let nb = geo.neighbors(river)[0];
    assert!(*prox.get(nb) > 0.0 && *prox.get(nb) < 1.0, "adjacent is high but < 1");
    assert!(*prox.get(nb) >= *prox.get(&geo.neighbors(nb).iter().copied()
        .find(|n| *n != river).unwrap()) , "monotone non-increasing with hops");
}

#[test]
fn river_proximity_is_zero_with_no_rivers() {
    let geo = hornvale_kernel::Geosphere::new(3);
    let wk = hornvale_kernel::CellMap::from_fn(&geo, |_| WaterKind::DryLand);
    let prox = river_proximity(&geo, &wk, RIVER_REACH);
    for c in geo.cells() {
        assert_eq!(*prox.get(c), 0.0);
    }
}

#[test]
fn river_proximity_is_deterministic_and_reload_stable() {
    let geo = hornvale_kernel::Geosphere::new(3);
    let wk = hornvale_kernel::CellMap::from_fn(&geo, |c| {
        if c.0 % 7 == 0 { WaterKind::River } else { WaterKind::DryLand }
    });
    let a = river_proximity(&geo, &wk, RIVER_REACH);
    let b = river_proximity(&geo, &wk, RIVER_REACH);
    for c in geo.cells() { assert_eq!(a.get(c), b.get(c)); }
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-terrain --lib river_proximity`
Expected: FAIL (`river_proximity`/`RIVER_REACH` undefined).

- [ ] **Step 3: Implement** (in `water.rs`)

```rust
/// The hop radius over which river proximity falls to zero. Tuned (The
/// Confluence) so carrying capacity spikes ADJACENT to rivers — settlements
/// condense a short walk from fresh water, not necessarily on it.
/// type-audit: bare-ok(count: hops)
pub const RIVER_REACH: u32 = 3;

/// Per-cell proximity to fresh flowing water, in `[0, 1]`: `1.0` on a
/// `WaterKind::River` cell, decaying linearly to `0.0` at `reach` hops. A
/// deterministic multi-source BFS outward from all River cells (frontier
/// processed in `CellId` order — no RNG, no HashMap). The carrying-capacity
/// freshwater term (The Confluence) rides this instead of the smooth
/// drainage/moisture proxy, so condensation pulls towns near rivers.
pub fn river_proximity(geo: &Geosphere, water_kind: &CellMap<WaterKind>, reach: u32) -> CellMap<f64> {
    // hop distance to nearest River, capped at reach+1 (unreached).
    let unreached = reach + 1;
    let mut dist: Vec<u32> = vec![unreached; geo.cell_count()];
    let mut frontier: std::collections::BTreeSet<CellId> = std::collections::BTreeSet::new();
    for c in geo.cells() {
        if matches!(*water_kind.get(c), WaterKind::River) {
            dist[c.0 as usize] = 0;
            frontier.insert(c);
        }
    }
    let mut d = 0u32;
    while d < reach && !frontier.is_empty() {
        let mut next: std::collections::BTreeSet<CellId> = std::collections::BTreeSet::new();
        for c in &frontier {
            for &n in geo.neighbors(*c) {
                if dist[n.0 as usize] > d + 1 {
                    dist[n.0 as usize] = d + 1;
                    next.insert(n);
                }
            }
        }
        frontier = next;
        d += 1;
    }
    CellMap::from_fn(geo, |c| {
        let h = dist[c.0 as usize];
        if h > reach { 0.0 } else { 1.0 - (h as f64) / (reach as f64 + 1.0) }
    })
}
```

Then expose `water_kind` to worldgen: add `pub fn water_kind_at(&self, cell: CellId) -> WaterKind { *self.globe().water_kind.get(cell) }` (or equivalent) on `GeneratedTerrain` — match how `drainage_at`/`is_ocean` are exposed. Re-export `river_proximity`, `RIVER_REACH` from `lib.rs`.

- [ ] **Step 4: Run to green**

Run: `cargo test -p hornvale-terrain --lib river_proximity`
Expected: PASS (3 tests).

- [ ] **Step 5: fmt + clippy + type-audit + commit**

```bash
cargo fmt && cargo clippy -p hornvale-terrain --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add domains/terrain/
git commit -m "feat(terrain): river_proximity field over WaterKind::River — the-confluence T1"
```

---

## Task 2: re-point the freshwater term + measure emergence

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (`carrying_inputs_of`, ~line 414-437; add `MOISTURE_FLOOR_WEIGHT`)
- Test: `windows/worldgen/tests/` (a new `confluence.rs`, or extend an existing settlement test)

**Interfaces:**
- Consumes: `river_proximity`, `RIVER_REACH`, `water_kind_at` (T1); `carrying_inputs_of`, `DRAINAGE_REF`, `carrying_capacity`, `CONDENSATION_THRESHOLD` (existing).
- Produces: the re-pointed `freshwater` term; a settlement-near-river measurement helper.

- [ ] **Step 1: Write the failing emergence tests** (`windows/worldgen/tests/confluence.rs`)

```rust
// Helper: fraction of seed-42 settlements whose cell is within RIVER_REACH of a River cell.
fn settlements_near_river_fraction(seed: u64) -> f64 {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    ).unwrap();
    let settlements = hornvale_settlement::all_settlements(&world);
    // map each settlement to its cell and test river_proximity > 0 there
    // (bind the exact settlement->cell + terrain-globe access from the worldgen API).
    /* compute near = settlements with river_proximity(cell) > 0.0 */
    // return near as f64 / settlements.len() as f64
    unimplemented!("bind from the worldgen settlement + terrain API")
}

#[test]
fn k_spikes_near_rivers_on_seed_42() {
    // Mean K in cells within RIVER_REACH of a river is materially higher than the
    // riverless-land mean — the mechanism condensation rides.
    // (build seed-42, carrying_inputs_of, carrying_capacity, partition cells by
    // river_proximity>0, compare means; assert near_mean > far_mean * 1.2)
}

#[test]
fn settlements_condense_near_rivers_emergently() {
    // THE KEYSTONE (emergent, preregistered): the fraction of settlements near
    // fresh water is materially higher than the pre-Confluence baseline. The
    // baseline is measured ONCE (before the re-point, on the same seeds) and
    // frozen as a const in this test; post must clear baseline + a preregistered
    // margin. NOT authored per-town — measured over the whole generated set.
    let frac = settlements_near_river_fraction(42);
    assert!(frac >= PREREGISTERED_MIN, "settlements condense near rivers: {frac} < {PREREGISTERED_MIN}");
}
```

- [ ] **Step 2: Measure the baseline first (preregistration).** BEFORE changing the term, run `settlements_near_river_fraction` on seeds 42 + a small sweep (e.g. 1,7,13,42,99) and record the numbers in the test file as `BASELINE_*` consts + a comment. Set `PREREGISTERED_MIN` from theory (a clear improvement, e.g. baseline + 0.3, or an absolute like 0.7 if the baseline supports it) — frozen before the readout (preregister-on-named-axes discipline). Run the tests to see them FAIL against the current smooth proxy.

- [ ] **Step 3: Re-point the freshwater term.** In `carrying_inputs_of` (worldgen/src/lib.rs), compute the proximity field ONCE and use it. Replace line 425:

```rust
pub fn carrying_inputs_of(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> hornvale_kernel::CellMap<hornvale_demography::CarryingInput> {
    // The Confluence: freshwater rides proximity to the real river network, not a
    // smooth drainage/moisture proxy — so K spikes near rivers and settlements
    // condense there (emergent). A moisture floor keeps riverless-but-wet regions
    // habitable.
    let water_kind = hornvale_kernel::CellMap::from_fn(geo, |c| terrain.water_kind_at(c));
    let river_prox = hornvale_terrain::river_proximity(geo, &water_kind, hornvale_terrain::RIVER_REACH);
    hornvale_kernel::CellMap::from_fn(geo, |cell| {
        let coastal = geo.neighbors(cell).iter().any(|n| terrain.is_ocean(*n));
        let moisture = climate.moisture_at(cell);
        let freshwater = (moisture * MOISTURE_FLOOR_WEIGHT).max(*river_prox.get(cell)).clamp(0.0, 1.0);
        let aridity = ((0.2 - moisture).max(0.0) * 5.0).clamp(0.0, 1.0);
        let hostility = terrain.unrest_at(cell).max(aridity).clamp(0.0, 1.0);
        hornvale_demography::CarryingInput {
            habitable: *climate.habitability().get(cell),
            temperature_c: climate.mean_temperature_at(cell).get(),
            moisture, freshwater, coastal, hostility,
        }
    })
}
```

with `const MOISTURE_FLOOR_WEIGHT: f64 = 0.5;` documented near `DRAINAGE_REF` (the old `drainage_norm`/`DRAINAGE_REF` becomes unused here — remove if no other caller, else leave). Fill in the `settlements_near_river_fraction` helper against the real settlement→cell + `river_proximity` API.

- [ ] **Step 4: Run to green** — `cargo test -p hornvale-worldgen --test confluence`. Expected: PASS (settlements now condense near rivers). If `PREREGISTERED_MIN` isn't cleared, tune `RIVER_REACH`/`MOISTURE_FLOOR_WEIGHT` (T1/here) toward adjacency and re-run — document the tuning.

- [ ] **Step 5: fmt + clippy + type-audit + commit** (SKIP_CENSUS=1 on any worldgen suite run).

```bash
cargo fmt && cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add windows/worldgen/
git commit -m "feat(worldgen): freshwater term rides river proximity; settlements condense near rivers — the-confluence T2"
```

---

## Task 3: re-calibration + the epoch / save-format surface

**Files:**
- Modify: `domains/demography/src/carrying_capacity.rs` (only if re-fit needed), `windows/worldgen/src/lib.rs` (`CONDENSATION_THRESHOLD` if the count shifted; the epoch/stream-label surface)
- Test: the K-grounding calibration check; genesis same-seed identity.

**Interfaces:** Consumes T2's re-pointed field; `FRESHWATER_BONUS`, `CONDENSATION_THRESHOLD`, the calibration harness (`beta_calibration_sweep.rs` pattern).

- [ ] **Step 1: K-grounding check.** Re-run the biomass-by-latitude grounding (the-gathering calibration discipline — the `beta_calibration_sweep.rs` / the demography-report gradient check). Confirm the ~27× tropical:polar gradient still holds within tolerance with the sharper freshwater term. If `K`'s magnitude drifted the gradient out of tolerance, re-fit `FRESHWATER_BONUS` and re-pin it with an updated provenance comment (`carrying_capacity.rs`). Write/keep a test asserting the gradient is in tolerance (SKIP_CENSUS=1; use the fixture-backed calibration, never a live census).

- [ ] **Step 2: Settlement-count check.** `CONDENSATION_THRESHOLD = 10.0` was tuned for ~182 seed-42 settlements. The sharper K may shift the count. Measure the new seed-42 settlement count; if it moved materially (e.g. outside ~[100, 400]), re-fit `CONDENSATION_THRESHOLD` back into the target band and re-pin its provenance comment. Assert the count is in a sane band.

- [ ] **Step 3: The epoch / save-format surface.** Determine whether the settlement seed-derivation's draw ORDER changes (it should not — condensation is deterministic over K, no new draws; settlement *positions* move but the naming/derivation stream is unchanged). Document this in the spec's decisions + a code comment: settlements move as a **derived-formula change** (like a physics-formula change under the save-format contract), not a stream-label epoch, UNLESS a draw-order shift is found — in which case add the epoch suffix (`settlement/...vN`) per the save-format contract. Write a `same seed + pins → byte-identical world` test on the NEW behavior (build seed-42 twice, assert identical serialization).

- [ ] **Step 4: Genesis same-seed identity + commit** (SKIP_CENSUS=1).

```bash
cargo test -p hornvale-worldgen -p hornvale-demography 2>&1 | tail -5   # green
cargo fmt && cargo run --manifest-path tools/type-audit/Cargo.toml -- check
git add domains/demography/ windows/worldgen/
git commit -m "calib(worldgen): re-ground K + settlement count under the confluence freshwater term — the-confluence T3"
```

---

## Task 4: the Surmise payoff + artifact re-baselines + close

**Files:**
- Modify: `windows/vessel/` (the parked Surmise reachability test flips to success), the settlement gallery + Surmise transcript artifacts.
- Create: `book/src/chronicle/the-confluence.md` (+ SUMMARY), `docs/retrospectives/the-confluence.md`.
- Modify: `book/src/frontier/idea-registry.md` (flip SEQ-2, advance MAP-7).

- [ ] **Step 1: Flip the Surmise reachability test to success.** The parked
  `seed_42_home_settlements_real_walk_reachability_is_a_measured_t5_finding` (in `windows/vessel/src/liveness.rs`) asserted `drinks == 0`. Now settlements are near rivers, so the possessed agent's home settlement should reach fresh water. Rewrite it to assert the discovery journey now fires (the agent reaches a `River`/fresh room and drinks within `PLAN_BUDGET`) — the campaign's visible payoff. If it does NOT fire for the seed-42 home settlement specifically (measure it), report loudly and note whether the coarse→fine bridge (followup 1) is needed; do not fake success.

- [ ] **Step 2: Regenerate the drifted committed artifacts** (NOT the census — SKIP_CENSUS=1; census fixtures wait for Nathan's AWS regen). The seed-42 settlement gallery (`book/src/gallery/settlement-seed-42*.md`), the map/almanac if they render settlement positions, and the Surmise possession transcript re-baseline. Use each artifact's documented CLI command; stage only the intended drift.

```bash
bash scripts/regenerate-artifacts.sh   # skips censuses by default
git diff --stat book/   # review: settlements moved; census fixtures NOT touched here
```

- [ ] **Step 3: Chronicle + retrospective + registry.** `book/src/chronicle/the-confluence.md` (the story: settlements were built as attractors over K; K's freshwater term was a smooth proxy; re-pointing it at the real river network pulls towns onto rivers — emergent, historically true; the epoch/census cost; completes The Surmise). Wire into `SUMMARY.md`. `docs/retrospectives/the-confluence.md` (process lessons; promote followups). In `idea-registry.md`: flip **SEQ-2** (settlements now condense near rivers — the first slice shipped) and advance **MAP-7** (freshwater term is real). **Name concepts, not registry IDs, in chronicle/retro prose** (docs_consistency forbids registry IDs outside `book/src/frontier/`).

- [ ] **Step 4: Full gate + STOP for the census + G6.**

```bash
SKIP_CENSUS=1 make gate   # green; heavy/census tier deferred
git add -A && git commit -m "docs(the-confluence): Surmise payoff + settlement re-baseline + chronicle (close) — the-confluence T4"
```

- [ ] **Step 5: STOP — G6 + the census are hard stops the controller/Nathan own.** Present the post-G3 ledger digest (the epoch/census carve-out #4 leading). **The census regen is Nathan's explicit AWS step**, run once pre-merge (`make regen-remote` / `scripts/aws-gate/regen-git.sh`) — never by an implementer, never locally. FF/push/pull are Nathan's calls under `closing-a-campaign`.

---

## Self-review notes (author)

- **Spec coverage:** §3.1 river-proximity term → T1+T2; §3.2 scale resolution → T2/T4 (the Surmise flip measures it); §3.3 re-calibration → T3; §5 acceptance (1 river_proximity → T1; 2 K-spikes → T2; 3 settlements-condense emergent/preregistered → T2; 4 Surmise demo → T4; 5 K calibrated → T3; 6 same-seed identity → T3) covered; §6 save-format (epoch + Nathan-gated census) → T3 + Global Constraints + T4 Step 5; §7 non-goals reserved; §8 constants → T1/T2/T3.
- **Census discipline:** no task runs a census/`make rebaseline`/`make regen-remote`/`HV_CENSUS=1`; all suite runs use `SKIP_CENSUS=1`; the census regen is a named Nathan-gated step (T4 Step 5). This is the campaign's defining constraint.
- **Emergence:** T2's keystone measures the generated settlement set against a preregistered baseline — never authors a town's position.
- **Type consistency:** `river_proximity(geo, water_kind, reach) -> CellMap<f64>`, `RIVER_REACH`, `water_kind_at`, `MOISTURE_FLOOR_WEIGHT` consistent across T1/T2; `carrying_inputs_of`'s signature is unchanged (proximity computed internally).
- **Placeholders:** the two `unimplemented!()`/`/* … */` in T2 Step 1's helper are explicitly flagged "bind from the real API" with the exact data needed (settlement→cell, river_proximity) — the implementer binds them in Step 3; the baseline consts are measured in Step 2. Not silent gaps.
