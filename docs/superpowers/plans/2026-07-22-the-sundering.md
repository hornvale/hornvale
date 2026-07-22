# The Sundering Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reroute C1's deep-history bake so migration, flee-resettle, daughter-founding, and raiding follow the slice-1 geography-stable `ConnectionGraph` (adjacency + sailing lanes) by hop-distance over conductance-positive edges, instead of raw `geo.neighbors` — oceans sunder, sailing lanes leapfrog.

**Architecture:** A geography-stable `ConnectionGraph` (adjacency + water routes, no settlement land-routes) is built once at the `windows/worldgen` composition root and threaded into `history_bake::bake`. The bake's three site-picking paths iterate `traversable_neighbors(graph, cell)` (edges with `conductance > 0.0`, ascending, deduped) rather than `geo.neighbors(cell)`. The graph is derived-never-committed, but the occupation skeleton it produces changes — a genesis epoch (byte-identity breaks, census moves).

**Tech Stack:** Rust (edition 2024), `hornvale-worldgen` (composition root), `hornvale-topology` (`ConnectionGraph`), `hornvale-history` (`OccupationRecord`), kernel `Geosphere`/`CellId`/`CellMap`. `cargo nextest` + doctests; `make gate` / `make gate-full`.

## Global Constraints

- **Determinism (constitutional):** same seed + pins ⇒ byte-identical world. `BTreeMap`/`BTreeSet`/`Vec` only — no `HashMap`/`HashSet`. Every float comparison uses `f64::total_cmp`. No RNG beyond the kernel `Seed`/`Stream`; no wall-clock. The rewire introduces **no new seed draw** (destination selection is deterministic).
- **The traversability line is `conductance > 0.0`** — strictly positive, identical filter at all three site-picking paths. Ocean-touching adjacency edges are stored at conductance `0.0`; sailing-lane edges carry the current magnitude (`> 0.0` for a real current).
- **Geography-stable graph only in the bake:** adjacency + water routes, built from terrain+current with an **empty settlement slice** (`&[]`). No settlement land-routes in the dynamics (they stay a slice-1 present-world read). Built **once**, before the epoch loop.
- **Genesis epoch, but no save-format label change:** no new stream label, no `history/bake/v2` relabel (the labels' draw semantics are unchanged), **no new committed field**. The graph is never committed.
- **type-audit:** any new primitive at a `pub` boundary carries a `type-audit:` verdict tag. `ConnectionGraph` is a type, not a bare primitive — no tag needed on the `bake` param.
- **measure-don't-narrate:** every gate is a real assertion with a mutation-testable failure. A threshold is a floor/ceiling set clear of the *measured* value, never a target tuned to force a pass. **If a phenomenon goes inert or the map depopulates, STOP and surface to Nathan (fidelity carve-out) — never patch with a floor.**
- **Census regen is LOCAL on `lefford` (decision 0063); macOS cannot commit census goldens.** Census regen + keystone refreeze happen at the G6 close (Nathan-authorized), not in these tasks.

---

## File Structure

- `windows/worldgen/src/history_bake.rs` — **modified.** Add the private `traversable_neighbors` helper; add a `graph: &'a ConnectionGraph` field to `Bake`; add a trailing `graph: &ConnectionGraph` param to `bake`; reroute `nearest_dest`, `raid_target`, and the daughter-founding pick in `grow` to `traversable_neighbors(self.graph, …)`.
- `windows/worldgen/src/lib.rs` — **modified.** At the bake call site (~line 3799) build the geography-stable graph from `terrain`/`climate` (already in scope) and pass it into `bake`.
- `windows/worldgen/src/gate_helpers` (existing readback fns live in `lib.rs` / a gates module) — **modified.** Add `collapse_events(&World) -> u64` (mirrors `migration_events`) and `sundered_landmasses(&World) -> Vec<Landmass>` (present connection graph → land components → peoples per component).
- `windows/worldgen/tests/history_bake.rs` — **modified.** Add a `full_land_graph` test helper; pass it to the existing `bake` calls (all-land no-op); add the `traversable_neighbors` unit test and the sunder/leapfrog bake test.
- `windows/worldgen/tests/history_gates.rs` — **modified.** Re-measure and (if moved) re-pin `MIGRATION_FLOOR` / `MAX_REGION_OVERLAP` / `MIN_RESTACKED_SITES`, labelled as the epoch re-pin.
- `windows/worldgen/tests/history_sundering.rs` — **created.** The two new preregistered gates (collapse-share ceiling; isolation-predicts-divergence) on the real seed-42 world.
- `cli/tests/graph_cost.rs` — **modified** (or a sibling heavy test) — add a bake-derivation wall-time check in the `heavy:` tier.

The close (G6, `closing-a-campaign`): census regen on `lefford`, census-close cascade re-pins, seed-42 keystone refreeze, artifact-drift regen (almanac/connections gallery pages), chronicle, retrospective, freshness sweep, registry flip.

---

### Task 1: The graph-following bake (the mechanism + the epoch lands)

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs`
- Modify: `windows/worldgen/src/lib.rs` (bake call site, ~3799)
- Test: `windows/worldgen/tests/history_bake.rs`
- Re-pin (if moved): `windows/worldgen/tests/history_gates.rs`

**Interfaces:**
- Consumes: `hornvale_topology::{ConnectionGraph, Edge, EdgeKind}`; `ConnectionGraph::{new, add_edge, edges, cell_count via node param}`; `Edge { to, kind, conductance }`; `crate::graph_derive::{connection_graph, GraphConfig}`; `Geosphere::{cells, neighbors, cell_count}`; `climate.biome_map()`, `climate.current_at(CellId) -> [f64;3]`, `terrain.globe().elevation` (`&CellMap<ReferenceElevation>`).
- Produces: `fn traversable_neighbors(graph: &ConnectionGraph, cell: CellId) -> Vec<CellId>`; new `bake` signature `bake(seed, geo, capacity, river_prox, eras, refugia, peoples, cfg, graph: &ConnectionGraph) -> History`.

- [ ] **Step 1: Add the `traversable_neighbors` unit test (failing).**

In `windows/worldgen/tests/history_bake.rs`, add at the top `use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};` and this test. It builds a 4-node graph: a passable adjacency edge (0–1), an impassable ocean adjacency edge (1–2, conductance 0), and a sailing lane (1–3, conductance 0.5). `traversable_neighbors(g, 1)` must return exactly `[CellId(0), CellId(3)]` — ocean excluded, lane included, ascending.

```rust
// NOTE: traversable_neighbors is private to history_bake.rs; this test lives
// INSIDE that module's unit tests. Add a `#[cfg(test)] mod` in history_bake.rs
// OR make the test an integration test by exposing the fn `pub(crate)` and
// re-exporting under a test-only path. The plan uses a unit test in
// history_bake.rs (private-fn access); see Step 3.
```

Because `traversable_neighbors` is private, put THIS test in a `#[cfg(test)] mod tests` block at the bottom of `history_bake.rs` (not the integration file):

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_topology::{ConnectionGraph, Edge, EdgeKind};

    #[test]
    fn traversable_neighbors_excludes_ocean_includes_lanes() {
        let mut g = ConnectionGraph::new(4);
        g.add_edge(CellId(0), Edge { to: CellId(1), kind: EdgeKind::Adjacency, conductance: 1.0 });
        g.add_edge(CellId(1), Edge { to: CellId(2), kind: EdgeKind::Adjacency, conductance: 0.0 }); // ocean
        g.add_edge(CellId(1), Edge { to: CellId(3), kind: EdgeKind::WaterRoute, conductance: 0.5 }); // lane
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

- [ ] **Step 2: Run it — expect a compile failure** (`traversable_neighbors` not defined).

Run: `cargo test -p hornvale-worldgen --lib traversable_neighbors 2>&1 | tail -20`
Expected: FAIL — `cannot find function traversable_neighbors`.

- [ ] **Step 3: Implement `traversable_neighbors` + thread the graph into `Bake`/`bake`.**

In `history_bake.rs`, add the import `use hornvale_topology::ConnectionGraph;` and the helper (private — no type-audit tag needed):

```rust
/// The conductance-positive graph neighbours of `cell`: the cells reachable
/// from `cell` over a single edge the land or sea actually permits travelling
/// (`conductance > 0.0` — ocean-touching adjacency edges are stored at exactly
/// 0.0, so this is precisely "you can travel this edge"). Ascending and
/// deduplicated (a cell may be both a mesh neighbour and a sailing-lane
/// destination), matching `Geosphere::neighbors`' ascending-unique contract so
/// the rerouted BFS/scans stay deterministic. On an all-land world this equals
/// `geo.neighbors(cell)` exactly — the rewire is a no-op where there is no sea.
fn traversable_neighbors(graph: &ConnectionGraph, cell: CellId) -> Vec<CellId> {
    let mut ns: Vec<CellId> = graph
        .edges(cell)
        .iter()
        .filter(|e| e.conductance > 0.0)
        .map(|e| e.to)
        .collect();
    ns.sort();
    ns.dedup();
    ns
}
```

Add `graph: &'a ConnectionGraph` to the `Bake<'a>` struct (document it: "The geography-stable transport graph the dynamics follow — adjacency + sailing lanes, conductance 0 across ocean. Built once at the composition root; the bake reads it, never mutates it."). Add it to the `Bake { … }` literal in `bake`. Add a trailing param to `bake`'s signature — `graph: &ConnectionGraph` — after `cfg`, and update the `#[allow(clippy::too_many_arguments)]` doc note to mention it.

- [ ] **Step 4: Reroute the three site-picking paths.**

In `nearest_dest`, replace `for &n in self.geo.neighbors(c) {` with `for n in traversable_neighbors(self.graph, c) {` (the body's `visited.insert(n)` etc. already take `CellId` by value — the `&n` becomes `n`).

In `raid_target`, replace `for &n in self.geo.neighbors(site) {` with `for n in traversable_neighbors(self.graph, site) {`.

In `grow`'s daughter pick, replace
`self.geo.neighbors(site).iter().copied().filter(|&n| self.vacant_habitable(era, n))`
with
`traversable_neighbors(self.graph, site).into_iter().filter(|&n| self.vacant_habitable(era, n))`.

Leave every tie-break (`refugia` > `river_prox` > `CellId`; raid `population` > `CellId`; daughter `capacity*river_factor` > `CellId`) exactly as-is.

- [ ] **Step 5: Add the `full_land_graph` test helper + fix the existing `bake` call sites.**

In `windows/worldgen/tests/history_bake.rs`, add:

```rust
/// A pure-land connection graph over `geo`: every mesh-adjacency pair at unit
/// conductance, no water routes. `traversable_neighbors` over this graph equals
/// `geo.neighbors`, so the bake is byte-identical to the pre-Sundering raw-
/// adjacency bake — the all-land no-op that proves the seam preserves behaviour
/// where there is no sea to sunder.
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

Update the three existing `bake(Seed(..), &geo, &cap, &river, &eras, &refugia, &people, &cfg)` calls (in `same_seed_bakes_byte_identical_history`, `different_seeds_diverge`, `the_workload_fires_displacement_at_volume`) to build `let graph = full_land_graph(&geo);` and pass `&graph` as the trailing argument.

- [ ] **Step 6: Add the sunder/leapfrog bake test (the new-behaviour proof).**

Add this test. It builds two connected land clusters (island A = the two-ring refuge around cell 0, reused from `fixture`; island B = a disjoint connected cluster grown from a far cell by BFS), constructs a graph with **only intra-A and intra-B adjacency edges** (no edge crosses the gap → an ocean strait), and asserts the graph has exactly two positive components as a precondition. Era masks turn island A hostile while island B stays habitable+vacant. Without a lane, A's peoples cannot reach B; adding one `WaterRoute` edge across the gap lets them leapfrog.

```rust
#[test]
fn ocean_sunders_and_a_lane_leapfrogs() {
    use hornvale_worldgen::history_bake::{BakeConfig, bake, census};
    let geo = Geosphere::new(1); // 42 cells

    // Island A: two rings around cell 0 (a connected blob). Island B: two rings
    // around the farthest cell from 0 (also connected, disjoint from A).
    let ring2 = |seed: CellId| -> BTreeSet<CellId> {
        let mut s = BTreeSet::new();
        s.insert(seed);
        for &n in geo.neighbors(seed) { s.insert(n); }
        for c in s.clone() { for &n in geo.neighbors(c) { s.insert(n); } }
        s
    };
    let a = ring2(CellId(0));
    // pick a B-seed not in A, farthest by hop count
    let b_seed = geo.cells().filter(|c| !a.contains(c))
        .max_by_key(|&c| geo.hops_between(CellId(0), c, 16).unwrap_or(0)).unwrap();
    let b = ring2(b_seed);
    // If the two rings overlap, shrink B to b - a so the gap is real.
    let b: BTreeSet<CellId> = b.difference(&a).copied().collect();
    assert!(a.is_disjoint(&b) && !b.is_empty(), "islands must be disjoint & non-empty");

    // Graph: intra-island adjacency only (no cross-gap edge) unless `lane`.
    let build_graph = |lane: bool| -> ConnectionGraph {
        let mut g = ConnectionGraph::new(geo.cell_count());
        for cell in geo.cells() {
            for &n in geo.neighbors(cell) {
                if n.0 <= cell.0 { continue; }
                let same_island = (a.contains(&cell) && a.contains(&n))
                    || (b.contains(&cell) && b.contains(&n));
                if same_island {
                    g.add_edge(cell, Edge { to: n, kind: EdgeKind::Adjacency, conductance: 1.0 });
                }
            }
        }
        if lane {
            let (&fa, &fb) = (a.iter().next().unwrap(), b.iter().next().unwrap());
            g.add_edge(fa, Edge { to: fb, kind: EdgeKind::WaterRoute, conductance: 0.5 });
        }
        g
    };

    // Precondition: without a lane the two islands are separate components.
    assert!(build_graph(false).reachable_regions(1e-9).len() >= 2, "islands must be sundered");

    // Capacity/refugia/eras: seed communities ONLY on island A; island A freezes
    // (glacial), island B is the only habitable land during the glacial era and
    // sits vacant. So an A community must try to migrate; whether it reaches B
    // depends solely on the lane.
    let refugia = CellMap::from_fn(&geo, |c| b.contains(&c)); // B is the refuge
    let capacity = CellMap::from_fn(&geo, |c| if a.contains(&c) { 120.0 } else { 60.0 });
    let river = CellMap::from_fn(&geo, |_| 0.0);
    // era 0 warm (A habitable), era 1 glacial (only B habitable) — repeated.
    let era = |day: f64, glacial: bool| EraClimate {
        day, ice: CellMap::from_fn(&geo, |_| false),
        habitable: CellMap::from_fn(&geo, |c| if glacial { b.contains(&c) } else { a.contains(&c) }),
        sea_level: e(0.0), ice_fraction: if glacial { 0.6 } else { 0.0 },
    };
    let eras: Vec<EraClimate> = (0..8).map(|i| era(i as f64 * 250.0, i % 2 == 1)).collect();
    let cfg = BakeConfig::default_millennia();
    let people = vec![KindId("goblin")];

    let on_b = |h: &hornvale_worldgen::history_bake::History| -> bool {
        h.records.iter().any(|r| b.contains(&r.site))
    };

    // Sundered: no lane ⇒ no A-people ever appears on island B.
    let no_lane = bake(Seed(7), &geo, &capacity, &river, &eras, &refugia, &people, &cfg, &build_graph(false));
    assert!(!on_b(&no_lane), "ocean must sunder: a people crossed with no lane: {:?}", census(&no_lane));

    // Bridged: a single WaterRoute lets a community leapfrog to island B.
    let lane = bake(Seed(7), &geo, &capacity, &river, &eras, &refugia, &people, &cfg, &build_graph(true));
    assert!(on_b(&lane), "sailing lane must let a people leapfrog to island B: {:?}", census(&lane));
}
```

- [ ] **Step 7: Wire the real geography-stable graph at the worldgen root.**

In `windows/worldgen/src/lib.rs` at the bake call site (~3796–3808), immediately before `let history = history_bake::bake(`, build the graph (mirroring `connection_graph_of`'s body — `terrain` and `climate` are already in scope):

```rust
// Slice 2 (The Sundering): the bake follows the geography-stable transport
// graph — adjacency + sailing lanes, built ONCE with an EMPTY settlement slice
// (settlement land-routes depend on who lives where and would cost O(N²) A*
// every epoch; excluded on purpose). Migration/raid/daughter honour the sea:
// oceans block (conductance 0), sailing lanes leapfrog. Derived, never
// committed — but the skeleton it produces changes (a genesis epoch).
let biome = climate.biome_map();
let current = hornvale_kernel::CellMap::from_fn(geo, |c| climate.current_at(c));
let transport = crate::graph_derive::connection_graph(
    geo,
    &terrain.globe().elevation,
    &biome,
    &current,
    &[],
    &crate::graph_derive::GraphConfig::default(),
);
```

and add `&transport` as the trailing argument to `history_bake::bake(…)`.

- [ ] **Step 8: Run the worldgen bake tests.**

Run: `cargo test -p hornvale-worldgen --lib traversable_neighbors && cargo test -p hornvale-worldgen --test history_bake 2>&1 | tail -30`
Expected: the `traversable_neighbors` unit tests PASS; `same_seed_bakes_byte_identical_history`, `different_seeds_diverge`, `the_workload_fires_displacement_at_volume` PASS (all-land no-op); `ocean_sunders_and_a_lane_leapfrogs` PASSES.

- [ ] **Step 9: Re-measure the real-world gates; re-pin only what genuinely moved.**

Run: `cargo test -p hornvale-worldgen --test history_gates --test history_placement --test history_byte_identity 2>&1 | tail -40`

For each failure, print the newly-measured value (the assertion messages already do). Then:
- If a phenomenon **still fires** but crossed a floor/ceiling (e.g. migration dropped from 51 to 34 but stays a real signal; region overlap shifted but stays a genuine separation), **re-pin the constant** in `history_gates.rs` clear of the new measured value and update its doc comment: `// The Sundering (C2 s2) re-pin: seed-42 now measures <X> (was <Y>); …`.
- If a phenomenon **goes inert** (migration collapses toward zero, overlap balloons past ~0.25 meaning peoples re-interleaved, or `emergent_settlement_count_stays_in_the_sane_band` fails low — the map depopulated), **STOP**: this is the spec's §7 fidelity risk. Report `BLOCKED` with the measured census and the failing gate; the controller surfaces it to Nathan (carve-out — never patch with a floor).

- [ ] **Step 10: Commit.**

```bash
cargo fmt
git add windows/worldgen/src/history_bake.rs windows/worldgen/src/lib.rs \
        windows/worldgen/tests/history_bake.rs windows/worldgen/tests/history_gates.rs
git commit -m "feat(history): the bake follows the connection graph — oceans sunder, lanes leapfrog (the-sundering T1)"
```

---

### Task 2: The new preregistered gates — depopulation ceiling + isolation-predicts-divergence

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (add `collapse_events` + `sundered_landmasses` readback helpers, next to `migration_events`/`territories`)
- Create: `windows/worldgen/tests/history_sundering.rs`

**Interfaces:**
- Consumes: `hornvale_settlement::{all_settlements, CELL_ID, IS_SETTLEMENT}`; `hornvale_history::{IS_OCCUPATION, OCC_CAUSE, OCC_PEOPLE, OCC_SITE, IS_RUIN}`; `crate::graph_derive::{connection_graph_of, GraphConfig}`; `ConnectionGraph::reachable_regions`; `migration_events` (as the mirror pattern).
- Produces: `pub fn collapse_events(world: &World) -> u64`; `pub fn sundered_landmasses(world: &World) -> Vec<Landmass>` where `pub struct Landmass { pub cells: BTreeSet<CellId>, pub peoples: BTreeSet<KindId> }`.

- [ ] **Step 1: Write the collapse-share gate (failing).**

Create `windows/worldgen/tests/history_sundering.rs`. First the depopulation ceiling — the bake must not answer the ocean block by starving the map out. (The count-floor is already guarded by `history_placement::emergent_settlement_count_stays_in_the_sane_band`; this adds the complementary share signal.)

```rust
use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to,
    collapse_events, migration_events, sundered_landmasses,
};

fn build_settlements(seed: Seed) -> hornvale_kernel::World {
    let wc = WorldComponents::assemble().expect("registries well-formed");
    build_world_to(seed, &SkyPins::default(), SkyChoice::Generated,
        &TerrainPins::default(), &SettlementPins::default(), &wc, BuildDepth::Settlements)
        .expect("seed builds")
}

/// Gate — **the map is not depopulated.** Blocking ocean crossings must not
/// convert the dynamics into mass extinction: collapses (Famine) stay a
/// minority of all occupations ever opened. Ceiling set ABOVE the measured
/// seed-42 collapse share (fill MEASURED in Step 4), never a target.
const MAX_COLLAPSE_SHARE: f64 = 0.0; // Step 4 sets this above measured
#[test]
fn the_map_is_not_depopulated() {
    let w = build_settlements(Seed(42));
    let collapses = collapse_events(&w) as f64;
    let occupations = w.ledger.find(hornvale_history::IS_OCCUPATION).count() as f64;
    assert!(occupations > 0.0, "no occupations at all");
    let share = collapses / occupations;
    assert!(
        share <= MAX_COLLAPSE_SHARE,
        "depopulation risk fired: collapse share {share:.4} exceeds ceiling \
         {MAX_COLLAPSE_SHARE} — the ocean block is starving the map out, not \
         diverging it. This is a fidelity finding for Nathan, not a re-pin."
    );
    // A live floor too: migration must still fire (the sea did not freeze the
    // dynamics into stasis).
    assert!(migration_events(&w) > 0, "no migration — dynamics went inert");
}
```

- [ ] **Step 2: Write the isolation-divergence gate (failing).**

```rust
/// Gate — **isolation predicts divergence.** The sundered world must be
/// genuinely partitioned (≥ 2 land components), and at least one isolated
/// landmass must host only a SUBSET of the world's peoples — a people that
/// could not cross the sea to reach it. That proper-subset is the structural,
/// un-tunable signature of divergence-by-isolation. Floors set below measured
/// (fill in Step 4).
const MIN_LANDMASSES: usize = 0; // Step 4 sets this at/below measured
#[test]
fn isolation_predicts_divergence() {
    let w = build_settlements(Seed(42));
    let masses = sundered_landmasses(&w);
    assert!(
        masses.len() >= MIN_LANDMASSES,
        "world is not sundered: only {} inhabited land component(s) (floor {MIN_LANDMASSES})",
        masses.len()
    );
    // The union of all peoples present anywhere.
    let world_peoples: std::collections::BTreeSet<_> =
        masses.iter().flat_map(|m| m.peoples.iter().copied()).collect();
    // Guard against a false pass: a single-people world has no subset to find.
    assert!(world_peoples.len() >= 2, "need ≥2 peoples for a divergence signal");
    let diverged = masses.iter().any(|m| {
        !m.peoples.is_empty() && m.peoples.len() < world_peoples.len()
    });
    assert!(
        diverged,
        "no isolated landmass hosts a proper subset of peoples — every mass has \
         everyone, so the sea is not diverging anyone. Masses: {:?}",
        masses.iter().map(|m| (m.cells.len(), m.peoples.len())).collect::<Vec<_>>()
    );
}
```

- [ ] **Step 3: Run — expect compile failures** (`collapse_events`, `sundered_landmasses`, `Landmass` undefined).

Run: `cargo test -p hornvale-worldgen --test history_sundering 2>&1 | tail -20`
Expected: FAIL — unresolved imports.

- [ ] **Step 4: Implement the readback helpers.**

In `windows/worldgen/src/lib.rs`, next to `migration_events`, add (all reading the committed ledger; `sundered_landmasses` also derives the present graph):

```rust
/// The number of occupations that ended in Famine (a community starved out) —
/// the collapse signal, mirroring [`migration_events`]. Reads the committed
/// `occ-cause` facts; a purely derived count, no seed draw.
/// type-audit: bare-ok(count: return)
pub fn collapse_events(world: &World) -> u64 {
    world.ledger.find(hornvale_history::OCC_CAUSE)
        .filter(|f| matches!(&f.object, hornvale_kernel::Value::Text(t) if t == "Famine"))
        .count() as u64
}

/// One inhabited, sea-isolated landmass: a connected component of the present
/// transport graph (conductance ≥ 1e-6, matching the almanac's isolation line),
/// with the set of peoples whose alive occupations sit on it.
pub struct Landmass {
    /// The component's cells.
    pub cells: std::collections::BTreeSet<CellId>,
    /// The peoples with an alive occupation on this landmass.
    pub peoples: std::collections::BTreeSet<KindId>,
}

/// The inhabited sea-isolated landmasses of a built world: present connection
/// graph → land components (conductance ≥ 1e-6) → each component's alive
/// peoples. Only components carrying at least one alive settlement are
/// returned. Purely derived — the graph is never committed.
pub fn sundered_landmasses(world: &World) -> Vec<Landmass> {
    let graph = crate::graph_derive::connection_graph_of(world, &crate::graph_derive::GraphConfig::default());
    // site cell -> people, for alive settlements only.
    let mut site_people: std::collections::BTreeMap<CellId, KindId> = Default::default();
    for s in hornvale_settlement::all_settlements(world) {
        let (Some(hornvale_kernel::Value::Number(cell)), Some(hornvale_kernel::Value::Text(p))) = (
            world.ledger.value_of(s.id, hornvale_settlement::CELL_ID),
            world.ledger.value_of(s.id, hornvale_history::OCC_PEOPLE),
        ) else { continue };
        site_people.insert(CellId(*cell as u32), KindId(kind_leak(p)));
    }
    let mut out = Vec::new();
    for comp in graph.reachable_regions(1e-6) {
        let peoples: std::collections::BTreeSet<KindId> =
            comp.iter().filter_map(|c| site_people.get(c).copied()).collect();
        if !peoples.is_empty() {
            out.push(Landmass { cells: comp, peoples });
        }
    }
    out
}
```

`KindId` holds a `&'static str`; deriving it from a ledger `Text` needs the canonical interned kind. Use the existing worldgen mechanism for turning a people string back into a `KindId` (the same one `emit_history`/the placement pass uses — grep `KindId(` near `OCC_PEOPLE` readback in `lib.rs`; reuse that, do NOT leak a `String`). Replace `kind_leak(p)` with that canonical lookup. If none exists, resolve via the world's registry/`WorldComponents`, not `Box::leak`.

- [ ] **Step 5: Measure and set the thresholds.**

Temporarily set `MAX_COLLAPSE_SHARE = 1.0`, `MIN_LANDMASSES = 1` and run:

Run: `cargo test -p hornvale-worldgen --test history_sundering -- --nocapture 2>&1 | tail -40`

Read the measured collapse share and landmass count from the output (add a temporary `eprintln!` if needed). Set `MAX_COLLAPSE_SHARE` a clear margin **above** the measured share (e.g. measured 0.34 → ceiling 0.55) and `MIN_LANDMASSES` at or just below the measured inhabited-component count. If the isolation-divergence assertion cannot pass on seed 42 (no isolated landmass hosts a proper people-subset — the sea is not diverging anyone), that is a real finding: report `DONE_WITH_CONCERNS` with the measured masses so the controller can decide whether to widen to a seed sample or surface to Nathan. Update both doc comments with the measured values.

- [ ] **Step 6: Run to green.**

Run: `cargo test -p hornvale-worldgen --test history_sundering 2>&1 | tail -20`
Expected: both gates PASS.

- [ ] **Step 7: Commit.**

```bash
cargo fmt
git add windows/worldgen/src/lib.rs windows/worldgen/tests/history_sundering.rs
git commit -m "test(history): depopulation ceiling + isolation-predicts-divergence gates (the-sundering T2)"
```

---

### Task 3: The bake-derivation cost check (heavy tier)

**Files:**
- Modify: `cli/tests/graph_cost.rs` (or create `cli/tests/history_cost.rs` following its pattern)

**Interfaces:**
- Consumes: `std::time::Instant`; `build_world_to` at `BuildDepth::Settlements`; the `heavy:` ignore-reason convention (`cli/tests/heavy_tier.rs`).

- [ ] **Step 1: Add the cost check (heavy-tier, ignored out of the commit gate).**

Following `graph_cost.rs`'s shape and the `heavy:` ignore-reason token, add a test that builds the seed-42 world to `BuildDepth::Settlements` (which now derives the geography-stable graph and runs the graph-following bake) and asserts the wall-time stays under a generous budget. Measure-don't-narrate: the geography-stable graph is one cheap derivation and graph-BFS is ~mesh-BFS cost, but this bounds it rather than asserting by hand.

```rust
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn sundering_bake_stays_within_budget() {
    let start = std::time::Instant::now();
    let _w = /* build seed-42 to BuildDepth::Settlements (copy graph_cost.rs's build helper) */;
    let elapsed = start.elapsed();
    assert!(
        elapsed.as_secs() < 30,
        "graph-following bake regressed: {elapsed:?} to build seed-42 settlements (budget 30s)"
    );
}
```

- [ ] **Step 2: Run it (heavy tests are opt-in).**

Run: `cargo test -p hornvale --test graph_cost -- --ignored sundering_bake 2>&1 | tail -20`
Expected: PASS well under budget.

- [ ] **Step 3: Commit.**

```bash
cargo fmt
git add cli/tests/graph_cost.rs
git commit -m "test(cli): bake-derivation wall-time cost check, heavy tier (the-sundering T3)"
```

---

## Close (G6 — `closing-a-campaign`, Nathan-authorized)

Not TDD tasks — the merge ritual, run after the final whole-branch review:

- **Census regen on `lefford`** (`HV_CENSUS=1`, decision 0063 — macOS cannot commit census goldens). **Explicit Nathan authorization required.**
- **Census-close cascade re-pins:** `rows.csv` goldens → `golden-pins.sql` + `windows/lab/tests/calibration.rs` (`make census-check`, column order = live-computed, pinned-literal) → `branches_family_calibration.rs`, `gathering_calibration.rs` (fixture-backed re-pins are macOS-safe).
- **Seed-42 keystone refreeze** from main's tip (`cli/tests/fixtures/world-seed-42.json`).
- **Artifact drift regen:** the three seed-42 almanacs, the connections gallery (`book/src/gallery/connections-seed-42.md`), registry/manifest dumps, lab studies — via the CI artifact commands / `make rebaseline`; commit the drift.
- **Heavy report artifact:** `cli/tests/history_battery.rs` regenerates the committed history report — run and commit its drift.
- **DoD docs:** chronicle (`book/src/chronicle/the-sundering.md`), retrospective (`docs/retrospectives/the-sundering.md`), book freshness sweep (the living-community + connection-graph chapters), Confidence Gradient re-score if a bet moved, registry flip (MAP-61 slice-2 → shipped; repoint Where).
- **Full gate + artifact drift on the merged result**, then fast-forward main.

---

## Self-Review

**Spec coverage:** §1 payoff + §4 mechanism → Task 1 (three site swaps + geography-stable graph). §3 derived-not-committed + §5 epoch → Task 1 (no new committed field; census/keystone at close). §7 gate 1 (displacement fires) → existing `history_gates`/`history_bake` tests re-pinned in Task 1 Step 9. §7 gate 2 (depopulation) → Task 2 collapse-share + existing band test. §7 gate 3 (isolation-divergence) → Task 2. §7 cost check → Task 3. §8 non-goals — no land-routes/conductance-weighting/new field introduced (Global Constraints). §9 DoD → Close section.

**Placeholder scan:** the two threshold constants (`MAX_COLLAPSE_SHARE`, `MIN_LANDMASSES`) are deliberately measured-then-set in Task 2 Step 5 (measure-don't-narrate requires this — the value cannot be known before the epoch runs), with the exact procedure specified; `kind_leak` is explicitly flagged as a placeholder to replace with the canonical KindId lookup. No other TBDs.

**Type consistency:** `traversable_neighbors(&ConnectionGraph, CellId) -> Vec<CellId>` used identically at all three sites; `bake`'s trailing `graph: &ConnectionGraph` param matches the call sites in tests (via `full_land_graph`) and lib.rs (via `connection_graph(…, &[], …)`); `Landmass { cells, peoples }` produced by `sundered_landmasses` and consumed by the divergence gate with matching field names; `collapse_events`/`migration_events` share the ledger-count shape.
