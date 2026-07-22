# The Connection Graph — Campaign 2, Slice 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Derive the world's transport graph — cell adjacency + sailing lanes (ocean currents) + natural land routes (least-cost paths over a terrain traversal-cost field) — as a byte-identical *derived* structure, and make it legible (scene edges + an almanac connections/isolation readout).

**Architecture:** A new kernel-only `domains/topology` crate owns the `ConnectionGraph` type and its pure ops (graph-distance, connected-components/reachability, and a `SearchSpace` for least-cost cell routing via `hornvale_kernel::astar`). The cross-domain *derivation* runs at `windows/worldgen`, reading the ocean-current field and a new terrain traversal-cost field. The graph is DERIVED, never committed — worlds stay byte-identical (no epoch).

**Tech Stack:** Rust 2024; `hornvale-kernel` only in the domain (`CellId`, `CellMap`, `Geosphere`, `astar`/`SearchSpace`, `BTreeMap`/`BTreeSet`); `hornvale-climate`/`hornvale-terrain` consumed only at the worldgen root.

## Global Constraints

- Kernel `Seed`/`Stream` only (the graph is derived — likely draws nothing; if a draw is needed, `range_u32(lo,hi)` is INCLUSIVE of hi).
- No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. Float ordering via `f64::total_cmp`.
- `astar` costs are **u64 integers** (no float non-determinism); the traversal-cost field is `CellMap<u64>`.
- No wall-clock time in the derivation; `std::time::Instant` only in the cost-gate benchmark under a scoped `#[allow(clippy::disallowed_types)]`.
- **DERIVED, never committed** — the graph is not written to the ledger; worlds remain byte-identical (no epoch, no census regen). Any legibility emit quantizes floats at the boundary (`hornvale_kernel::quantize`).
- **Determinism:** same seed + pins → byte-identical graph; `astar` is deterministic (BTreeSet frontier, total order, no RNG — The Foresight's keystone).
- These are **natural routes** (terrain/sea traversability), NOT built roads. Name types/prose accordingly (`WaterRoute`, `LandRoute`, never `Road`).
- Every crate `#![warn(missing_docs)]`; item-level `type-audit:` tags on every pub-boundary primitive (`cargo run --manifest-path tools/type-audit/Cargo.toml -- check`).
- Dependencies: `serde` + `serde_json` only; a domain serializes no domain enum (serialize at the window via `serialize_with` if an edge type must cross the scene boundary).
- `cargo fmt` last before every commit. Gate: `make gate`.

---

### Task 1: `domains/topology` — the graph type + pure ops

**Files:**
- Create: `domains/topology/Cargo.toml`, `domains/topology/src/lib.rs`, `domains/topology/src/graph.rs`
- Modify: root `Cargo.toml` (add member), `windows/worldgen/Cargo.toml` (add dep)
- Test: `domains/topology/tests/graph.rs`

**Interfaces — Produces:**
- `pub enum EdgeKind { Adjacency, WaterRoute, LandRoute }`
- `pub struct Edge { pub to: CellId, pub kind: EdgeKind, pub conductance: f64 }` (conductance: dimensionless ease-of-travel, higher = easier)
- `pub struct ConnectionGraph { … }` with `pub fn new(node_count: usize) -> Self`, `pub fn add_edge(&mut self, from: CellId, edge: Edge)` (undirected: stores both directions), `pub fn edges(&self, from: CellId) -> &[Edge]`, `pub fn nodes(&self) -> impl Iterator<Item = CellId>`. Backed by `BTreeMap<CellId, Vec<Edge>>` (deterministic; NO HashMap).
- `pub fn reachable_regions(&self, min_conductance: f64) -> Vec<BTreeSet<CellId>>` — connected components over edges with `conductance >= min_conductance`, each component a `BTreeSet`, the `Vec` ordered by each component's min `CellId` (deterministic).

- [ ] **Step 1: failing test** — `tests/graph.rs`: build a 4-cell graph (0-1 adjacency, 1-2 water route, 3 isolated), assert `edges(1)` has both kinds, and `reachable_regions(0.0)` returns 2 components `{0,1,2}` and `{3}` (3 alone). Use `CellId(u32)`.
- [ ] **Step 2: run — fails** (`cargo test -p hornvale-topology --test graph`).
- [ ] **Step 3: implement** `graph.rs` (BTreeMap adjacency list; `reachable_regions` = a deterministic BFS/union over BTreeSet-ordered nodes, edges filtered by conductance). Mirror `domains/settlement/Cargo.toml`'s shape; `#![warn(missing_docs)]`; type-audit tags on `conductance: f64` etc.
- [ ] **Step 4: run — passes.** Step 5: fmt + clippy + type-audit, commit `feat(topology): ConnectionGraph type + reachability (T1)`.

---

### Task 2: The least-cost cell `SearchSpace` (topology, kernel-only)

**Files:** Create `domains/topology/src/route.rs`; Modify `lib.rs` (`pub mod route;`); Test `domains/topology/tests/route.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::{astar, SearchSpace, Geosphere, CellId, CellMap}` (confirm the exact `SearchSpace` trait shape in `kernel/src/astar.rs` — `successors(&s) -> impl IntoIterator<Item=(Action, State, u64)>`, `heuristic(&s) -> u64`, plus a goal test).
- Produces: `pub struct CellRoute<'a> { geo: &'a Geosphere, cost: &'a CellMap<u64>, goal: CellId }` impl `SearchSpace` (State = `CellId`, Action = `CellId`, step cost = `cost[next]`, heuristic = an admissible integer great-circle hop estimate to `goal` — e.g. 0 for admissibility if unsure, or a floor). `pub fn least_cost(geo, cost, from, to, budget: usize) -> Option<(Vec<CellId>, u64)>` — returns the path + total cost, or None if unreachable within `budget`.

- [ ] **Step 1: failing test** — a 3×3-ish hand-built geosphere + `CellMap<u64>` cost (a cheap corridor vs an expensive wall); assert `least_cost` picks the corridor and its total cost, and returns `None` when the budget is too small. Determinism: same inputs → identical path.
- [ ] **Step 2: run — fails.** Step 3: implement `CellRoute` + `least_cost` over `astar`. Heuristic must be admissible (never overestimate) or `astar` returns non-optimal paths — if unsure, use `0` (Dijkstra) and note it. Step 4: run — passes. Step 5: fmt/clippy/type-audit, commit `feat(topology): least-cost cell routing over astar (T2)`.

---

### Task 3: The terrain traversal-cost field (worldgen)

**Files:** Create `windows/worldgen/src/traversal.rs`; Modify `lib.rs`; Test `windows/worldgen/tests/traversal.rs`

**Interfaces:**
- Consumes: the committed elevation `CellMap<ReferenceElevation>` (worldgen builds it; see `lib.rs:1185`), `biome_at` (ocean detection), `geo.neighbors`.
- Produces: `pub fn traversal_cost(geo: &Geosphere, elevation: &CellMap<ReferenceElevation>, biome: &CellMap<Biome>) -> CellMap<u64>` — per-cell integer land-traversal cost: a base cost + a slope term (max elevation gradient to neighbours, scaled), very high on peaks, `u64::MAX` (impassable) on ocean cells. Reuses committed elevation — NO new seed draw.

- [ ] **Step 1: failing test** — a hand-built elevation/biome fixture: a flat lowland cell reads a low cost, a steep peak reads a high cost, an ocean cell reads `u64::MAX`. Deterministic.
- [ ] **Step 2-4:** implement `traversal_cost` (base + slope; ocean = impassable). Quantize NOT needed (integers). Step 5: fmt/clippy/type-audit, commit `feat(worldgen): terrain traversal-cost field (T3)`.

---

### Task 4: Sailing-lane + land-route derivation (worldgen)

**Files:** Create `windows/worldgen/src/graph_derive.rs`; Modify `lib.rs`; Test `windows/worldgen/tests/graph_derive.rs`

**Interfaces:**
- Consumes: `ConnectionGraph`/`Edge`/`EdgeKind` (T1), `least_cost` (T2), `traversal_cost` (T3), the current field (`current_at(cell) -> [f64;3]`), `biome_at`, the settlement cells (`hornvale_settlement::all_settlements`).
- Produces: `pub fn connection_graph(geo, elevation, biome, current, settlements: &[CellId], cfg: &GraphConfig) -> ConnectionGraph`:
  - **Adjacency:** for every cell, an `Adjacency` edge to each neighbour, conductance from the (inverse) traversal cost.
  - **Water routes:** for each coastal cell (a non-ocean cell adjacent to ocean), follow the `current_at` vector across ocean cells (bounded steps) to the next coastal cell it reaches → a `WaterRoute` edge, conductance from current strength. Deterministic (fixed step count; `total_cmp` tie-breaks on direction).
  - **Land routes:** for each pair of settlements within a bounded hop radius (`cfg.land_route_radius`), run `least_cost` over the traversal-cost field with `cfg.astar_budget`; if the path cost is below `cfg.corridor_max_cost`, add a `LandRoute` edge, conductance from the cost. **Bounded** — only nearby pairs, capped budget.
- `pub struct GraphConfig { pub land_route_radius: u32, pub astar_budget: usize, pub corridor_max_cost: u64, pub water_route_max_steps: u32 }` with a `default()`.

- [ ] **Step 1: failing test** — a small fixture world (a bay with two coasts + two settlements across a low pass and one behind a peak); assert a water route links the two coasts, a land route links the pass-connected settlements, and NO land route crosses the peak. Determinism.
- [ ] **Step 2-4:** implement. Keep all candidate ordering `total_cmp`/BTree-deterministic. Step 5: commit `feat(worldgen): derive water + land routes into the ConnectionGraph (T4)`.

---

### Task 5: The cost gate (the size risk — measure-don't-narrate)

**Files:** Create `cli/tests/graph_cost.rs` (heavy `#[ignore]`d full-world battery); light in-gate assertion in `windows/worldgen/tests/graph_derive.rs`
**Interfaces:** Consumes `connection_graph` on a real seed-42 world (build via the worldgen API to `BuildDepth::Settlements`).

- [ ] **Step 1: preregister the gate** — measure on seed-42: graph-derivation wall-time and the total land-route `astar` attempt count; assert wall-time < a budget and attempts < a bound (bounded by nearby-pairs). The heavy battery carries the verbatim ignore reason `"heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"`. A light in-gate check runs the derivation on a small pinned world and asserts the attempt count is bounded.
- [ ] **Step 2-4:** run once, record the numbers; if attempts/time blow past budget, TIGHTEN `land_route_radius`/`astar_budget` (do not widen the gate). Step 5: commit `test(worldgen): connection-graph cost gate (T5)`.

---

### Task 6: Legibility — scene edges + almanac connections/isolation + gallery

**Files:** the scene emit (`windows/scene/…` — follow an existing per-cell/edge emission; a WINDOW may `serialize_with` the edge kinds); `windows/almanac/src/…` a `render_connections(world, site) -> String`; a committed seed-42 gallery page in `book/src/gallery/`; Test the almanac render + a scene fixture.

- [ ] **Step 1: failing acceptance test** — `render_connections` on a site with a known water + land route contains "sea-lane"/"by land" and, for an isolated region's site, an isolation line ("cut off … by the peaks"). Deterministic.
- [ ] **Step 2-4:** implement `render_connections` (query the derived graph + `reachable_regions`); wire the scene edge emission; generate the seed-42 gallery page (add the regen to CI's "Artifacts are current" step). **Visual pass:** read the generated page — does the transport topology read clearly? Step 5: commit `feat(almanac,scene): render the transport topology + isolation (T6)`.

---

### Task 7: Determinism + byte-identity + DoD

**Files:** `windows/worldgen/tests/graph_byte_identity.rs`; the stream manifest (if any label added — likely none, derived); the DoD artifacts.

- [ ] **Step 1: byte-identity** — derive the seed-42 graph twice; assert identical (serialize the edge lists and compare). Assert C1's world serialization is UNCHANGED (no epoch — the graph is derived, not committed). Run `make gate` + `make rebaseline` + `git diff --exit-code` — the ONLY new committed artifact is the gallery page (no census/world drift).
- [ ] **Step 2: DoD** — chronicle `book/src/chronicle/the-connection-graph.md` (+ SUMMARY; name concepts, NO registry IDs outside `book/src/frontier/`); retrospective `docs/retrospectives/`; registry flip `MAP-61` → slice-1-shipped + repoint Where; book freshness sweep. **No census regen** (byte-identical). Re-run the FULL `make gate` after the DoD commit.
- [ ] **Step 3:** commit `docs(the-connection-graph): DoD — chronicle, retro, MAP-61 flip (T7)`.

---

## Self-Review

**Spec coverage:** §3 architecture → T1/T2 (domain) + T3/T4 (worldgen derivation); §4 model (adjacency+water+land, conductance) → T1/T4; §4.1 traversal-cost → T3; §4.2 reachability/isolation → T1 (`reachable_regions`) + T6 (render); §5 cost bound → T5; §6 determinism → T2/T7; §7 non-goals honored (no built roads/portals/gating/dynamics/diffuse — only derivation+legibility); §8 DoD → T7. No gaps.

**Placeholder scan:** signatures + test intent concrete; the one open detail (the `astar` heuristic admissibility) is flagged in T2 with a safe default (0/Dijkstra). No "TBD".

**Type consistency:** `ConnectionGraph`/`Edge`/`EdgeKind{Adjacency,WaterRoute,LandRoute}`/`conductance` used identically T1→T6; `least_cost`/`traversal_cost`/`connection_graph`/`GraphConfig` signatures match across T2-T5; "natural routes / WaterRoute / LandRoute", never "Road".
