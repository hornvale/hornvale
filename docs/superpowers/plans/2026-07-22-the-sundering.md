# The Sundering (The Moving Sea) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reroute C1's deep-history bake to follow a **time-varying** connection graph — one geography graph per era, whose ocean is `elevation < era.sea_level` — so glacial low-stands open land bridges to island refugia (the diaspora crosses) and the rising sea drowns them (the peoples sunder).

**Architecture:** An era-aware graph derivation (`connection_graph_at`, marine = `elevation < sea_level`) is added alongside slice-1's present-biome `connection_graph`. The worldgen root builds one graph per era (`CLIMATE_ERAS = 25`) and threads them into `history_bake::bake`; the bake's three site-picking paths iterate `traversable_neighbors(graph_for_era, cell)` (conductance > 0). Derived per era, never committed — a genesis epoch.

**Tech Stack:** Rust 2024, `hornvale-worldgen` (root), `hornvale-topology` (`ConnectionGraph`), `hornvale-history`, kernel `Geosphere`/`CellMap`/`ReferenceElevation`. `cargo nextest` + doctests; `make gate` / `make gate-full`.

## Global Constraints

- **Determinism (constitutional):** same seed + pins ⇒ byte-identical world. `BTreeMap`/`BTreeSet`/`Vec` only — no `HashMap`/`HashSet`. Every float comparison uses `f64::total_cmp`. No RNG beyond the kernel `Seed`/`Stream`; no wall-clock. **No new seed draw** (the graph derivation and the destination selection are deterministic functions of the committed world).
- **Traversability line `conductance > 0.0`** — strictly positive, identical filter at all three site-picking paths.
- **Marine-this-era = `elevation.get(cell).get() < sea_level.get()`** — the single era-aware predicate; drives both the traversal cost (impassable) and the water-route coastal test.
- **Genesis epoch, no save-format change:** no new stream label, no `history/bake/v2`, **no new committed field**. Each era's graph is derived and discarded, like any field.
- **type-audit:** new primitives at a `pub` boundary carry a `type-audit:` verdict tag. `ReferenceElevation`/`ConnectionGraph`/`CellMap` are types, not bare primitives.
- **measure-don't-narrate:** every gate is a real assertion with a mutation-testable failure; thresholds are floors/ceilings clear of the *measured* value. **A phenomenon going inert or the map depopulating ⇒ STOP and surface to Nathan (fidelity carve-out), never a floor.** The displacement gate's re-pin-**or**-re-scope fork (Task 2 Step 12) is the explicit resolution of this campaign's first-design block.
- **Census regen is LOCAL on `lefford` (0063); macOS cannot commit census goldens.** Census regen + keystone refreeze happen at the G6 close (Nathan-authorized), not in these tasks.

---

## File Structure

- `windows/worldgen/src/traversal.rs` — **modified.** Add `pub fn traversal_cost_at(geo, elevation, sea_level)` (marine = `elevation < sea_level`); the existing `traversal_cost` stays for the present graph.
- `windows/worldgen/src/graph_derive.rs` — **modified.** Refactor the water-route helpers (`first_marine_neighbor`, `follow_current`, `add_water_routes`) to take a `marine: &CellMap<bool>` mask instead of `biome: &CellMap<Biome>`; `connection_graph` builds that mask from `biome.is_marine()` (behaviour unchanged); add `pub fn connection_graph_at(geo, elevation, sea_level, current, settlements, cfg)` building the mask from `elevation < sea_level`.
- `windows/worldgen/src/history_bake.rs` — **modified.** Add `traversable_neighbors`; give `Bake` a `graphs: &'a [ConnectionGraph]` + `cur_graph: usize` (remove the now-dead `geo` field); add `era_index_for`; reroute the three site-picking paths; add the trailing `graphs: &[ConnectionGraph]` param to `bake`.
- `windows/worldgen/src/lib.rs` — **modified.** At the bake call site build one graph per era via `connection_graph_at` and pass the slice.
- `windows/worldgen/src/lib.rs` (readback helpers) — **modified (Task 3).** Add `collapse_events` + `sundered_landmasses`/`Landmass`.
- `windows/worldgen/tests/traversal.rs` — **modified (Task 1).** Bridge-appears-at-low-sea-level unit tests.
- `windows/worldgen/tests/history_bake.rs` — **modified (Task 2).** `full_land_graph` helper; per-era graph vecs at the call sites; sunder/leapfrog test.
- `windows/worldgen/tests/history_gates.rs` — **modified (Task 2).** Re-pin (fired) or re-scope (inert) `MIGRATION_FLOOR`/`MIN_RESTACKED_SITES`, labelled.
- `windows/worldgen/tests/history_sundering.rs` — **created (Task 3).** Depopulation ceiling + isolation-divergence.
- `cli/tests/graph_cost.rs` — **modified (Task 4).** 25-era-graph bake wall-time check (`heavy:` tier).

Close (G6, `closing-a-campaign`): census regen on `lefford`, cascade re-pins, keystone refreeze, artifact-drift regen, chronicle, retro, freshness sweep, registry flip.

---

### Task 1: Era-aware graph derivation (additive — the bake is untouched, gates stay green)

**Files:**
- Modify: `windows/worldgen/src/traversal.rs`
- Modify: `windows/worldgen/src/graph_derive.rs`
- Test: `windows/worldgen/tests/traversal.rs`

**Interfaces:**
- Consumes: `traversal::{BASE_COST, SLOPE_SCALE}` (SLOPE_SCALE is module-private — `traversal_cost_at` lives in the same file); `ReferenceElevation::get`; `ConnectionGraph::new`; the existing `add_adjacency_edges`/`add_land_routes`.
- Produces: `pub fn traversal_cost_at(geo, elevation: &CellMap<ReferenceElevation>, sea_level: ReferenceElevation) -> CellMap<u64>`; `pub fn connection_graph_at(geo, elevation: &CellMap<ReferenceElevation>, sea_level: ReferenceElevation, current: &CellMap<[f64;3]>, settlements: &[CellId], cfg: &GraphConfig) -> ConnectionGraph`; water-route helpers now taking `marine: &CellMap<bool>`.

- [ ] **Step 1: Write the bridge unit test (failing).**

Add to `windows/worldgen/tests/traversal.rs`:

```rust
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation};
use hornvale_worldgen::traversal::traversal_cost_at;
fn e(m: f64) -> ReferenceElevation { ReferenceElevation::new(m).unwrap() }

#[test]
fn a_shelf_cell_is_ocean_at_present_but_a_bridge_at_glacial_low_stand() {
    let geo = Geosphere::new(1);
    let shelf = CellId(5);
    // shelf sits at -50 m; everything else is upland at +100 m.
    let elevation = CellMap::from_fn(&geo, |c| if c == shelf { e(-50.0) } else { e(100.0) });
    let present = traversal_cost_at(&geo, &elevation, e(0.0));
    let glacial = traversal_cost_at(&geo, &elevation, e(-120.0));
    assert_eq!(*present.get(shelf), u64::MAX, "shelf is ocean at present sea level");
    assert!(*glacial.get(shelf) < u64::MAX, "shelf is passable land at -120 m (a bridge)");
}
```

- [ ] **Step 2: Run it — expect a compile failure** (`traversal_cost_at` not found).

Run: `cargo test -p hornvale-worldgen --test traversal 2>&1 | tail -15`
Expected: FAIL — unresolved import `traversal_cost_at`.

- [ ] **Step 3: Implement `traversal_cost_at`.**

In `traversal.rs`, mirror `traversal_cost` but swap the marine test for a sea-level comparison (same slope logic, same `f64::total_cmp` fold):

```rust
/// Per-cell land-traversal cost at a given sea level: identical to
/// [`traversal_cost`] except a cell is ocean (`u64::MAX`) iff its elevation is
/// below `sea_level`, rather than by present biome. This is the era-aware cost
/// The Sundering's moving sea plans over — at a glacial low-stand the exposed
/// shelf drops below `u64::MAX` and becomes a passable land bridge.
/// type-audit: bare-ok(count: return)
pub fn traversal_cost_at(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
) -> CellMap<u64> {
    CellMap::from_fn(geo, |cell| {
        let here = elevation.get(cell).get();
        if here < sea_level.get() {
            return u64::MAX;
        }
        let max_gap = geo.neighbors(cell).iter().fold(0.0_f64, |acc, &n| {
            let gap = (elevation.get(n).get() - here).abs();
            if gap.total_cmp(&acc).is_gt() { gap } else { acc }
        });
        BASE_COST.saturating_add((max_gap * SLOPE_SCALE) as u64)
    })
}
```

- [ ] **Step 4: Run — expect PASS.**

Run: `cargo test -p hornvale-worldgen --test traversal 2>&1 | tail -15`
Expected: `a_shelf_cell_is_ocean_at_present_but_a_bridge_at_glacial_low_stand ... ok`.

- [ ] **Step 5: Write the graph-level bridge test (failing).**

Add to `windows/worldgen/tests/graph_derive.rs` (create if absent — it exists on main):

```rust
#[test]
fn a_shelf_joins_two_uplands_only_at_the_glacial_low_stand() {
    use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation};
    use hornvale_worldgen::graph_derive::{connection_graph_at, GraphConfig};
    let e = |m: f64| ReferenceElevation::new(m).unwrap();
    let geo = Geosphere::new(1);
    // Two upland blobs (+100 m) around cells 0 and 30; the two-ring boundary
    // between them sits on a shelf at -50 m (ocean at present, land at -120 m).
    let ring2 = |seed: CellId| {
        let mut s = std::collections::BTreeSet::new();
        s.insert(seed);
        for &n in geo.neighbors(seed) { s.insert(n); }
        for c in s.clone() { for &n in geo.neighbors(c) { s.insert(n); } }
        s
    };
    let up_a = ring2(CellId(0));
    let far = geo.cells().filter(|c| !up_a.contains(c))
        .max_by_key(|&c| geo.hops_between(CellId(0), c, 16).unwrap_or(0)).unwrap();
    let up_b: std::collections::BTreeSet<_> = ring2(far).difference(&up_a).copied().collect();
    let elevation = CellMap::from_fn(&geo, |c| {
        if up_a.contains(&c) || up_b.contains(&c) { e(100.0) } else { e(-50.0) }
    });
    let current = CellMap::from_fn(&geo, |_| [0.0, 0.0, 0.0]); // no lanes — test the bridge
    let cfg = GraphConfig::default();
    let present = connection_graph_at(&geo, &elevation, e(0.0), &current, &[], &cfg);
    let glacial = connection_graph_at(&geo, &elevation, e(-120.0), &current, &[], &cfg);
    // Present: the shelf is ocean, so the two uplands are separate components.
    assert!(present.reachable_regions(1e-9).iter().filter(|c| c.len() > 1).count() >= 2,
        "present sea should leave the uplands sundered");
    // Glacial: the shelf is land, bridging them into one big component.
    let glacial_big = glacial.reachable_regions(1e-9).into_iter().map(|c| c.len()).max().unwrap();
    assert!(glacial_big > present.reachable_regions(1e-9).into_iter().map(|c| c.len()).max().unwrap(),
        "the glacial low-stand must merge the uplands via the exposed shelf");
}
```

- [ ] **Step 6: Run — expect compile failure** (`connection_graph_at` not found).

Run: `cargo test -p hornvale-worldgen --test graph_derive 2>&1 | tail -15`
Expected: FAIL — unresolved `connection_graph_at`.

- [ ] **Step 7: Refactor the water-route helpers to a marine mask, add `connection_graph_at`.**

In `graph_derive.rs`, change `first_marine_neighbor`, `follow_current`, and `add_water_routes` to take `marine: &CellMap<bool>` instead of `biome: &CellMap<Biome>`, replacing each `biome.get(x).is_marine()` with `*marine.get(x)`. Then in the existing `connection_graph`, build the mask and pass it (behaviour identical):

```rust
let marine = CellMap::from_fn(geo, |c| biome.get(c).is_marine());
// add_adjacency_edges(geo, &cost, &mut graph);  (unchanged)
add_water_routes(geo, &marine, current, cfg, &mut graph);
```

Add the era-aware entry point:

```rust
/// The era-aware connection graph: like [`connection_graph`] but a cell is
/// ocean iff `elevation < sea_level`, so a glacial low-stand exposes the shelf
/// as passable land (the land bridges The Sundering's diaspora crosses).
/// Adjacency + sailing lanes only (settlement land-routes stay a present-world
/// read); derived, never committed.
/// type-audit: bare-ok(diagnostic-value: current)
pub fn connection_graph_at(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    current: &CellMap<[f64; 3]>,
    settlements: &[CellId],
    cfg: &GraphConfig,
) -> ConnectionGraph {
    let marine = CellMap::from_fn(geo, |c| elevation.get(c).get() < sea_level.get());
    let cost = crate::traversal::traversal_cost_at(geo, elevation, sea_level);
    let mut graph = ConnectionGraph::new(geo.cell_count());
    add_adjacency_edges(geo, &cost, &mut graph);
    add_water_routes(geo, &marine, current, cfg, &mut graph);
    add_land_routes(geo, &cost, settlements, cfg, &mut graph);
    graph
}
```

Add `ReferenceElevation` to the `use hornvale_kernel::{…}` line and export `connection_graph_at` from `lib.rs`'s `pub use graph_derive::{…}` list.

- [ ] **Step 8: Run both derivation test files — expect PASS.**

Run: `cargo test -p hornvale-worldgen --test traversal --test graph_derive --test graph_byte_identity 2>&1 | tail -25`
Expected: the two new tests PASS; the existing `graph_derive`/`graph_byte_identity`/`traversal` tests still PASS (the present graph is unchanged).

- [ ] **Step 9: Commit.**

```bash
cargo fmt
git add windows/worldgen/src/traversal.rs windows/worldgen/src/graph_derive.rs \
        windows/worldgen/tests/traversal.rs windows/worldgen/tests/graph_derive.rs
git commit -m "feat(worldgen): era-aware graph derivation — sea level opens land bridges (the-sundering T1)"
```

---

### Task 2: The era-threaded moving-sea bake

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs`
- Modify: `windows/worldgen/src/lib.rs` (bake call site, ~3799)
- Test: `windows/worldgen/tests/history_bake.rs`
- Re-pin/re-scope: `windows/worldgen/tests/history_gates.rs`

**Interfaces:**
- Consumes: `hornvale_topology::{ConnectionGraph, Edge, EdgeKind}`; `crate::graph_derive::{connection_graph_at, GraphConfig}` (Task 1); `climate.biome_map()`, `climate.current_at(CellId)->[f64;3]`, `terrain.globe().elevation`; `EraClimate::sea_level`.
- Produces: new `bake` signature `bake(seed, geo, capacity, river_prox, eras, refugia, peoples, cfg, graphs: &[ConnectionGraph]) -> History` with `graphs.len() == eras.len()`.

- [ ] **Step 1: Add the `traversable_neighbors` unit tests (failing).**

At the bottom of `history_bake.rs`, add a `#[cfg(test)] mod tests` block:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};

    #[test]
    fn traversable_neighbors_excludes_ocean_includes_lanes() {
        let mut g = ConnectionGraph::new(4);
        g.add_edge(CellId(0), Edge { to: CellId(1), kind: EdgeKind::Adjacency, conductance: 1.0 });
        g.add_edge(CellId(1), Edge { to: CellId(2), kind: EdgeKind::Adjacency, conductance: 0.0 });
        g.add_edge(CellId(1), Edge { to: CellId(3), kind: EdgeKind::WaterRoute, conductance: 0.5 });
        assert_eq!(traversable_neighbors(&g, CellId(1)), vec![CellId(0), CellId(3)]);
    }

    #[test]
    fn traversable_neighbors_dedups_parallel_edges() {
        let mut g = ConnectionGraph::new(2);
        g.add_edge(CellId(0), Edge { to: CellId(1), kind: EdgeKind::Adjacency, conductance: 1.0 });
        g.add_edge(CellId(0), Edge { to: CellId(1), kind: EdgeKind::WaterRoute, conductance: 0.5 });
        assert_eq!(traversable_neighbors(&g, CellId(0)), vec![CellId(1)]);
    }
}
```

- [ ] **Step 2: Run — expect compile failure** (`traversable_neighbors` not defined).

Run: `cargo test -p hornvale-worldgen --lib traversable_neighbors 2>&1 | tail -15`
Expected: FAIL.

- [ ] **Step 3: Add `traversable_neighbors` + convert `Bake` to per-era graphs.**

Add `use hornvale_topology::ConnectionGraph;` and:

```rust
/// The conductance-positive graph neighbours of `cell` (`conductance > 0.0` —
/// ocean-touching adjacency edges are stored at exactly 0.0). Ascending and
/// deduplicated, matching `Geosphere::neighbors`' contract so the rerouted
/// BFS/scans stay deterministic. On an all-land graph this equals
/// `geo.neighbors(cell)`.
fn traversable_neighbors(graph: &ConnectionGraph, cell: CellId) -> Vec<CellId> {
    let mut ns: Vec<CellId> = graph.edges(cell).iter()
        .filter(|e| e.conductance > 0.0).map(|e| e.to).collect();
    ns.sort();
    ns.dedup();
    ns
}
```

On `Bake<'a>`: **remove the `geo: &'a Geosphere` field** (all three methods that used `self.geo` are rerouted below, leaving it unread; `-D warnings` fails on a dead field). Add `graphs: &'a [ConnectionGraph]` and `cur_graph: usize`, and a helper:

```rust
/// The graph for the era currently being stepped.
fn cur(&self) -> &ConnectionGraph { &self.graphs[self.cur_graph] }
```

Rename `era_for` to also expose the index — add:

```rust
/// The index of the era in force for `year`: the last era whose `day` is at or
/// before `year`, or 0 for years before the first.
fn era_index_for(&self, eras: &[EraClimate], year: f64) -> usize {
    let mut chosen = 0;
    for (i, e) in eras.iter().enumerate() {
        if e.day <= year { chosen = i; }
    }
    chosen
}
```

- [ ] **Step 4: Reroute the three site-picking paths to `self.cur()`.**

`nearest_dest`: `for n in traversable_neighbors(self.cur(), c) {` (replacing `for &n in self.geo.neighbors(c)`).
`raid_target`: `for n in traversable_neighbors(self.cur(), site) {`.
`grow` daughter pick: `traversable_neighbors(self.cur(), site).into_iter().filter(|&n| self.vacant_habitable(era, n))`.
Every tie-break unchanged.

- [ ] **Step 5: Thread the era graph through the epoch loop + `bake` signature.**

Add `graphs: &[ConnectionGraph]` as the trailing param of `bake`; in the `Bake { … }` literal set `graphs`, `cur_graph: 0`, and drop `geo`. Add near the top of `bake`:
`assert_eq!(graphs.len(), eras.len(), "one graph per era");`
In the epoch loop, set the current graph before stepping:

```rust
while year < cfg.end_year {
    let era_idx = bake.era_index_for(eras, year);
    bake.cur_graph = era_idx;
    let era = eras[era_idx].clone();
    let snapshot: Vec<usize> = (0..bake.communities.len()).filter(|&i| bake.communities[i].alive).collect();
    for idx in snapshot { bake.step_community(idx, &era, year); }
    year += cfg.epoch_years;
}
```

(Genesis seeding does not use the graph — it reads capacity/river only — so it needs no era graph.)

- [ ] **Step 6: Add the `full_land_graph` helper + fix existing `bake` call sites.**

In `windows/worldgen/tests/history_bake.rs` add `use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};` and:

```rust
/// A pure-land connection graph over `geo` (unit-conductance adjacency, no water
/// routes). `traversable_neighbors` over this equals `geo.neighbors`, so on an
/// all-land world the bake is byte-identical to the pre-Sundering raw-adjacency
/// bake — the no-op seam.
fn full_land_graph(geo: &Geosphere) -> ConnectionGraph {
    let mut g = ConnectionGraph::new(geo.cell_count());
    for cell in geo.cells() {
        for &n in geo.neighbors(cell) {
            if n.0 > cell.0 {
                g.add_edge(cell, Edge { to: n, kind: EdgeKind::Adjacency, conductance: 1.0 });
            }
        }
    }
    g
}
```

In `same_seed_bakes_byte_identical_history`, `different_seeds_diverge`, and
`the_workload_fires_displacement_at_volume`, build a per-era vec and pass it:
`let graphs: Vec<ConnectionGraph> = eras.iter().map(|_| full_land_graph(&geo)).collect();`
then pass `&graphs` as the trailing `bake` argument. (All-land ⇒ every era's graph is identical ⇒ byte-identity + displacement gates unchanged.)

- [ ] **Step 7: Add the sunder/leapfrog bake test.**

```rust
#[test]
fn ocean_sunders_and_a_lane_leapfrogs() {
    use hornvale_worldgen::history_bake::{BakeConfig, bake, census, History};
    let geo = Geosphere::new(1);
    let ring2 = |seed: CellId| {
        let mut s = BTreeSet::new(); s.insert(seed);
        for &n in geo.neighbors(seed) { s.insert(n); }
        for c in s.clone() { for &n in geo.neighbors(c) { s.insert(n); } }
        s
    };
    let a = ring2(CellId(0));
    let b_seed = geo.cells().filter(|c| !a.contains(c))
        .max_by_key(|&c| geo.hops_between(CellId(0), c, 16).unwrap_or(0)).unwrap();
    let b: BTreeSet<CellId> = ring2(b_seed).difference(&a).copied().collect();
    assert!(a.is_disjoint(&b) && b.len() >= 3, "islands must be disjoint & non-trivial");

    let build_graph = |lane: bool| {
        let mut g = ConnectionGraph::new(geo.cell_count());
        for cell in geo.cells() {
            for &n in geo.neighbors(cell) {
                if n.0 <= cell.0 { continue; }
                let same = (a.contains(&cell) && a.contains(&n)) || (b.contains(&cell) && b.contains(&n));
                if same { g.add_edge(cell, Edge { to: n, kind: EdgeKind::Adjacency, conductance: 1.0 }); }
            }
        }
        if lane {
            let (&fa, &fb) = (a.iter().next().unwrap(), b.iter().next().unwrap());
            g.add_edge(fa, Edge { to: fb, kind: EdgeKind::WaterRoute, conductance: 0.5 });
        }
        g
    };
    assert!(build_graph(false).reachable_regions(1e-9).len() >= 2, "islands must be sundered");

    let refugia = CellMap::from_fn(&geo, |c| b.contains(&c));
    let capacity = CellMap::from_fn(&geo, |c| if a.contains(&c) { 120.0 } else { 60.0 });
    let river = CellMap::from_fn(&geo, |_| 0.0);
    let era = |day: f64, glacial: bool| EraClimate {
        day, ice: CellMap::from_fn(&geo, |_| false),
        habitable: CellMap::from_fn(&geo, |c| if glacial { b.contains(&c) } else { a.contains(&c) }),
        sea_level: e(0.0), ice_fraction: if glacial { 0.6 } else { 0.0 },
    };
    let eras: Vec<EraClimate> = (0..8).map(|i| era(i as f64 * 250.0, i % 2 == 1)).collect();
    let cfg = BakeConfig::default_millennia();
    let people = vec![KindId("goblin")];
    let on_b = |h: &History| h.records.iter().any(|r| b.contains(&r.site));

    let graphs_no = vec![build_graph(false); eras.len()];
    let no_lane = bake(Seed(7), &geo, &capacity, &river, &eras, &refugia, &people, &cfg, &graphs_no);
    assert!(!on_b(&no_lane), "ocean must sunder: crossed with no lane: {:?}", census(&no_lane));

    let graphs_lane = vec![build_graph(true); eras.len()];
    let lane = bake(Seed(7), &geo, &capacity, &river, &eras, &refugia, &people, &cfg, &graphs_lane);
    assert!(on_b(&lane), "a lane must let a people leapfrog to island B: {:?}", census(&lane));
}
```

- [ ] **Step 8: Run the worldgen bake tests — expect PASS.**

Run: `cargo test -p hornvale-worldgen --lib traversable_neighbors && cargo test -p hornvale-worldgen --test history_bake 2>&1 | tail -30`
Expected: unit tests PASS; the three existing tests PASS (all-land no-op); `ocean_sunders_and_a_lane_leapfrogs` PASSES.

- [ ] **Step 9: Wire the 25 per-era graphs at the worldgen root.**

In `lib.rs` at the bake call site (~3796, after `let eras = bake_eras(...)`), before `let history = history_bake::bake(`:

```rust
// The Sundering (the moving sea): one geography graph per era. A cell is ocean
// in era E iff elevation < era.sea_level(E); the glacial low-stand exposes the
// shelf as land bridges to island refugia (the diaspora crosses), the rising
// sea drowns them (the peoples sunder). Adjacency + sailing lanes only, empty
// settlement slice. Derived per era, never committed.
let biome = climate.biome_map();
let current = hornvale_kernel::CellMap::from_fn(geo, |c| climate.current_at(c));
let elevation = &terrain.globe().elevation;
let graphs: Vec<hornvale_topology::ConnectionGraph> = eras.iter().map(|era| {
    crate::graph_derive::connection_graph_at(
        geo, elevation, era.sea_level, &current, &[], &crate::graph_derive::GraphConfig::default(),
    )
}).collect();
```

Add `&graphs` as the trailing argument to `history_bake::bake(…)`. (`biome` is bound only if a later line needs it; if unused, drop it — `connection_graph_at` needs only elevation + sea level + current.)

- [ ] **Step 10: Build the real world; re-measure the gates.**

Run: `cargo test -p hornvale-worldgen --test history_placement --test history_byte_identity 2>&1 | tail -20`
Expected: `history_is_the_sole_settlement_provider`, `emergent_settlement_count_stays_in_the_sane_band` (count in `40..=400`), and both byte-identity tests PASS.

Run: `cargo test -p hornvale-worldgen --test history_gates 2>&1 | tail -30`
Read the measured `migration_events` and `restacked_sites` off the assertion messages.

- [ ] **Step 11: Re-pin (fired) or report (inert) — do not floor.**

- If migration **fires at volume** now (the bridges reached the refugia — e.g. dozens of events) but a floor was crossed: **re-pin** `MIGRATION_FLOOR` / `MIN_RESTACKED_SITES` in `history_gates.rs` clear of the new measured value, with a labelled doc comment: `// The Sundering (moving sea) re-pin: seed-42 now measures <X> (was <Y>)`. `MAX_REGION_OVERLAP` (peoples still separated) should already pass — if not, re-pin it too.
- If migration is **still inert** (bridges did not reach seed-42's refugia — the refugia are deep-ocean islands): do **NOT** re-pin. Reply `DONE_WITH_CONCERNS` with the measured census and the failing gate values, leave `history_gates.rs` untouched, and do not commit. This is the second measured finding (the spec §7 re-scope fork) and the controller brings it to Nathan before Task 2 commits.

- [ ] **Step 12: Commit (only if the gates are green).**

```bash
cargo fmt
git add windows/worldgen/src/history_bake.rs windows/worldgen/src/lib.rs \
        windows/worldgen/tests/history_bake.rs windows/worldgen/tests/history_gates.rs
git commit -m "feat(history): the bake follows the moving sea — glacial bridges cross, the rising sea sunders (the-sundering T2)"
```

---

### Task 3: Depopulation ceiling + isolation-predicts-divergence gates

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (add `collapse_events` + `sundered_landmasses`/`Landmass` next to `migration_events`)
- Create: `windows/worldgen/tests/history_sundering.rs`

**Interfaces:**
- Consumes: `hornvale_settlement::{all_settlements, CELL_ID, IS_SETTLEMENT}`; `hornvale_history::{IS_OCCUPATION, OCC_CAUSE, OCC_PEOPLE}`; `crate::graph_derive::{connection_graph_of, GraphConfig}`; `ConnectionGraph::reachable_regions`; the canonical `KindId` interner used near the existing `OCC_PEOPLE` readback.
- Produces: `pub fn collapse_events(&World) -> u64`; `pub struct Landmass { pub cells: BTreeSet<CellId>, pub peoples: BTreeSet<KindId> }`; `pub fn sundered_landmasses(&World) -> Vec<Landmass>`.

- [ ] **Step 1: Write the depopulation gate (failing).**

Create `windows/worldgen/tests/history_sundering.rs`:

```rust
use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to,
    collapse_events, migration_events, sundered_landmasses,
};

fn build_s(seed: Seed) -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("registries well-formed");
    build_world_to(seed, &SkyPins::default(), SkyChoice::Generated,
        &TerrainPins::default(), &SettlementPins::default(), &wc, BuildDepth::Settlements)
        .expect("seed builds")
}

/// The moving sea must not answer the deep water by starving the map out:
/// collapses (Famine) stay a minority of all occupations. Ceiling set ABOVE the
/// measured seed-42 share in Step 5.
const MAX_COLLAPSE_SHARE: f64 = 0.0; // set in Step 5
#[test]
fn the_map_is_not_depopulated() {
    let w = build_s(Seed(42));
    let collapses = collapse_events(&w) as f64;
    let occupations = w.ledger.find(hornvale_history::IS_OCCUPATION).count() as f64;
    assert!(occupations > 0.0, "no occupations");
    let share = collapses / occupations;
    assert!(share <= MAX_COLLAPSE_SHARE,
        "depopulation: collapse share {share:.4} > ceiling {MAX_COLLAPSE_SHARE} — a fidelity finding for Nathan, not a re-pin.");
    assert!(migration_events(&w) > 0, "no migration — dynamics inert");
}
```

- [ ] **Step 2: Write the isolation-divergence gate (failing).**

```rust
/// Isolation predicts divergence: the present world is genuinely partitioned
/// (≥ MIN_LANDMASSES inhabited land components) and at least one isolated
/// landmass hosts only a proper SUBSET of the world's peoples — a people that
/// could not cross to it. Floors set at/below measured in Step 5.
const MIN_LANDMASSES: usize = 0; // set in Step 5
#[test]
fn isolation_predicts_divergence() {
    let w = build_s(Seed(42));
    let masses = sundered_landmasses(&w);
    assert!(masses.len() >= MIN_LANDMASSES,
        "not sundered: {} inhabited land component(s) (floor {MIN_LANDMASSES})", masses.len());
    let world_peoples: std::collections::BTreeSet<_> =
        masses.iter().flat_map(|m| m.peoples.iter().copied()).collect();
    assert!(world_peoples.len() >= 2, "need ≥2 peoples for a divergence signal");
    let diverged = masses.iter().any(|m| !m.peoples.is_empty() && m.peoples.len() < world_peoples.len());
    assert!(diverged, "no isolated landmass hosts a proper subset of peoples: {:?}",
        masses.iter().map(|m| (m.cells.len(), m.peoples.len())).collect::<Vec<_>>());
}
```

- [ ] **Step 3: Run — expect compile failure** (`collapse_events`, `sundered_landmasses`, `Landmass` undefined).

Run: `cargo test -p hornvale-worldgen --test history_sundering 2>&1 | tail -15`

- [ ] **Step 4: Implement the readback helpers in `lib.rs`.**

Next to `migration_events`:

```rust
/// Occupations that ended in Famine — the collapse signal, mirroring
/// [`migration_events`]. Reads committed `occ-cause` facts; no seed draw.
/// type-audit: bare-ok(count: return)
pub fn collapse_events(world: &World) -> u64 {
    world.ledger.find(hornvale_history::OCC_CAUSE)
        .filter(|f| matches!(&f.object, hornvale_kernel::Value::Text(t) if t == "Famine"))
        .count() as u64
}

/// One inhabited, sea-isolated landmass: a connected component of the PRESENT
/// connection graph (conductance ≥ 1e-6), with the peoples whose alive
/// occupations sit on it.
pub struct Landmass {
    /// The component's cells.
    pub cells: std::collections::BTreeSet<CellId>,
    /// The peoples with an alive occupation on this landmass.
    pub peoples: std::collections::BTreeSet<KindId>,
}

/// The inhabited sea-isolated landmasses of a built world (present graph →
/// components → alive peoples per component). Only inhabited components are
/// returned. Purely derived; the graph is never committed.
pub fn sundered_landmasses(world: &World) -> Vec<Landmass> {
    let graph = crate::graph_derive::connection_graph_of(world, &crate::graph_derive::GraphConfig::default());
    let mut site_people: std::collections::BTreeMap<CellId, KindId> = Default::default();
    for s in hornvale_settlement::all_settlements(world) {
        let (Some(hornvale_kernel::Value::Number(cell)), Some(hornvale_kernel::Value::Text(p))) = (
            world.ledger.value_of(s.id, hornvale_settlement::CELL_ID),
            world.ledger.value_of(s.id, hornvale_history::OCC_PEOPLE),
        ) else { continue };
        // Resolve `p` to the canonical KindId via the SAME interner the existing
        // OCC_PEOPLE readback uses (grep `KindId` near OCC_PEOPLE in this file).
        // Do NOT Box::leak a String.
        if let Some(kind) = resolve_people_kind(p) {
            site_people.insert(CellId(*cell as u32), kind);
        }
    }
    let mut out = Vec::new();
    for comp in graph.reachable_regions(1e-6) {
        let peoples: std::collections::BTreeSet<KindId> =
            comp.iter().filter_map(|c| site_people.get(c).copied()).collect();
        if !peoples.is_empty() { out.push(Landmass { cells: comp, peoples }); }
    }
    out
}
```

Replace `resolve_people_kind(p)` with the file's existing people-string→`KindId` lookup (the naming/culture pass already does this after `emit_history`; reuse it). If there is genuinely no interner, resolve against `WorldComponents`/the registry — never `Box::leak`.

- [ ] **Step 5: Measure and set the thresholds.**

Temporarily set `MAX_COLLAPSE_SHARE = 1.0`, `MIN_LANDMASSES = 1`, run with `--nocapture` (add temporary `eprintln!`s for the measured share, landmass count, and per-mass people counts):

Run: `cargo test -p hornvale-worldgen --test history_sundering -- --nocapture 2>&1 | tail -40`

Set `MAX_COLLAPSE_SHARE` a clear margin above the measured share; `MIN_LANDMASSES` at/just below the measured inhabited-component count. If `isolation_predicts_divergence` cannot pass on seed 42 (no isolated mass hosts a proper subset), report `DONE_WITH_CONCERNS` with the measured masses (the controller decides: widen to a seed sample, or surface). Update both doc comments with the measured values; remove the temporary `eprintln!`s.

- [ ] **Step 6: Run to green, then commit.**

Run: `cargo test -p hornvale-worldgen --test history_sundering 2>&1 | tail -15`

```bash
cargo fmt
git add windows/worldgen/src/lib.rs windows/worldgen/tests/history_sundering.rs
git commit -m "test(history): depopulation ceiling + isolation-predicts-divergence gates (the-sundering T3)"
```

---

### Task 4: The 25-era-graph bake cost check (heavy tier)

**Files:**
- Modify: `cli/tests/graph_cost.rs`

- [ ] **Step 1: Add the cost check (heavy-tier, ignored out of the commit gate).**

Following `graph_cost.rs`'s build helper and the `heavy:` ignore-reason token:

```rust
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn moving_sea_bake_stays_within_budget() {
    let start = std::time::Instant::now();
    let _w = /* build seed-42 to BuildDepth::Settlements — copy graph_cost.rs's build helper */;
    let elapsed = start.elapsed();
    assert!(elapsed.as_secs() < 45,
        "the 25-era-graph bake regressed: {elapsed:?} to build seed-42 settlements (budget 45s)");
}
```

- [ ] **Step 2: Run it (opt-in).**

Run: `cargo test -p hornvale --test graph_cost -- --ignored moving_sea 2>&1 | tail -15`
Expected: PASS within budget. If it is close to 45 s, note it for the controller (the 25 per-era water-route derivations may want the adjacency-only-per-era optimisation — build lanes once from the present coastline, rebuild only adjacency per era).

- [ ] **Step 3: Commit.**

```bash
cargo fmt && git add cli/tests/graph_cost.rs
git commit -m "test(cli): 25-era-graph bake wall-time cost check, heavy tier (the-sundering T4)"
```

---

## Close (G6 — `closing-a-campaign`, Nathan-authorized)

Census regen on `lefford` (`HV_CENSUS=1`, 0063 — macOS cannot commit census goldens); census-close cascade re-pins (`rows.csv` → `golden-pins.sql` + `calibration.rs` via `make census-check`, then `branches_family_calibration.rs`/`gathering_calibration.rs`); seed-42 keystone refreeze from main's tip; artifact-drift regen (almanacs, `connections-seed-42.md`, registry/manifest, lab studies) + `cli/tests/history_battery.rs` report; DoD docs (chronicle, retrospective, freshness sweep of the living-community + connection-graph chapters, Confidence Gradient re-score, MAP-61 slice-2 registry flip); full gate + artifact drift on the merged result; then fast-forward main.

---

## Self-Review

**Spec coverage:** §1/§4 moving sea → Task 1 (`connection_graph_at`, marine = elevation<sea_level) + Task 2 (25 per-era graphs, era-threaded bake). §3 derived-per-era-not-committed + §5 epoch → Tasks 1–2 (no new field; census/keystone at close). §7 gate 1 displacement re-pin-or-re-scope → Task 2 Steps 10–12. §7 gate 2 depopulation → Task 3. §7 gate 3 isolation-divergence → Task 3. §7 cost check → Task 4. §8 non-goals — tectonic coastline, paleo-currents, conductance-weighting, land-routes-in-dynamics all excluded (Global Constraints + Task 1 scope). §9 DoD → Close.

**Placeholder scan:** the two thresholds (`MAX_COLLAPSE_SHARE`, `MIN_LANDMASSES`) are measured-then-set in Task 3 Step 5 (measure-don't-narrate requires this); `resolve_people_kind` is explicitly flagged to replace with the file's canonical interner. No other TBDs.

**Type consistency:** `traversal_cost_at(geo, &CellMap<ReferenceElevation>, ReferenceElevation) -> CellMap<u64>` and `connection_graph_at(…, ReferenceElevation, …)` consistent Task 1 → Task 2 (lib.rs passes `era.sea_level`, a `ReferenceElevation`). `bake(…, graphs: &[ConnectionGraph])` with `graphs.len() == eras.len()` matches every call site (tests build per-era vecs; lib.rs maps `eras.iter()`). `traversable_neighbors(&ConnectionGraph, CellId) -> Vec<CellId>` used identically at all three sites via `self.cur()`. `Landmass { cells, peoples }` produced and consumed with matching fields.
