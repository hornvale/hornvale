# The Tumult (The Sandpile) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make conflict emerge from crowding: when a displaced community finds no vacant cell reachable over the era graph, it displaces the nearest *occupied* cell and the evicted occupant cascades — the Sea-Peoples avalanche — and the cascade-size distribution is measured against a power law.

**Architecture:** A recursive `relocate` in `history_bake.rs` replaces the two no-vacant dead-ends (`step_community`'s Famine, `raid`'s "lost"); a `nearest_occupied` helper mirrors `nearest_dest`'s graph BFS; the `BakeCensus` gains a log-binned cascade-size histogram; a worldgen entry point exposes the baked `History` per seed; a heavy-tier test adjudicates the histogram's shape (power law vs bell/spike).

**Tech Stack:** Rust 2024, `hornvale-worldgen` (`history_bake.rs`), `hornvale-history`, kernel `CellId`/`CellMap`, `hornvale-topology`. `cargo nextest` + doctests; `make gate` / `make gate-full`.

## Global Constraints

- **Determinism (constitutional):** same seed + pins ⇒ byte-identical skeleton. `BTreeMap`/`BTreeSet`/`Vec` only — no `HashMap`/`HashSet`. Every float compare via `f64::total_cmp`. No RNG beyond the kernel `Seed`/`Stream`; no wall-clock. **No new seed draw** beyond the existing raid path.
- **Lorenz-safe:** triggers are the seed-replayed committed climate eras (`eff == 0`) and deterministic over-pressure (`pressure >= 1.0`) — NEVER a stochastic forward-integration of a chaotic pressure variable. The cascade READS the frozen epoch state; it does not integrate an ODE.
- **Bounded cascade depth:** a `const CASCADE_DEPTH_CAP: u32` guards non-termination and the size-risk; measured by the cost gate, high enough not to clip real avalanches.
- **No new committed field, predicate, or stream label.** A cascade is a chain of the existing `CauseOfEnd::Fled` occupation records with `ended_by = Ended::By(displacer)`. The cascade histogram lives in the (uncommitted, diagnostic) `BakeCensus`.
- **type-audit:** new primitives at a `pub` boundary carry a `type-audit:` verdict tag.
- **measure-don't-narrate:** every gate is a real assertion with a mutation-testable failure; thresholds are floors/ceilings clear of the measured value. The power-law metric is a falsification headline — **either outcome ships** (power law = SOC confirmed; bell/spike/no-cascades = documented falsification). Depopulation or an inert world is a fidelity finding for Nathan, never a floor.
- **Census regen is LOCAL on `lefford` (0063); macOS cannot commit census goldens** — census regen + keystone refreeze happen at the G6 close.

---

## File Structure

- `windows/worldgen/src/history_bake.rs` — **modified.** Add `const CASCADE_DEPTH_CAP`; add `nearest_occupied`; add the recursive `relocate` (returns cascade size); reroute `step_community`'s `eff == 0` branch and `raid`'s evicted-community branch through it; add a `cascade_hist: [u64; CASCADE_BINS]` (log-binned) to `BakeCensus` + a `record_cascade(size)` tally + a `pub fn cascade_sizes()` reader.
- `windows/worldgen/src/lib.rs` — **modified.** Extract the bake-input assembly + `bake()` call at the settlement stage into `pub fn history_for(seed, sky, terrain_pins, settlement_pins, wc) -> Result<History, BuildError>` (returning the diagnostic `History`); `build_world_to` calls it. This is the measurement entry point (no committed field).
- `windows/worldgen/tests/history_bake.rs` — **modified.** Unit tests: `nearest_occupied`; the cascade fires + terminates + tallies on a crowded fixture; the all-vacant case is unchanged (no cascade).
- `windows/worldgen/tests/history_tumult.rs` — **created.** The preregistered gates: conflict-fires-at-volume; not-depopulated; the power-law falsification metric (heavy-tier over a seed sample).
- `windows/worldgen/tests/history_gates.rs` — **modified (if drift).** Re-pin `MIGRATION_FLOOR`/others if the epoch moves them (labelled), per the census-close discipline at G6; light re-measure here.
- `cli/tests/graph_cost.rs` — **modified.** A heavy-tier cost gate bounding the cascade bake wall-time + the max cascade depth actually reached.

Close (G6, `closing-a-campaign`): census regen on lefford, cascade re-pins, keystone refreeze, artifact drift, chronicle, retrospective, Confidence Gradient re-score (SOC bet moves from `raw`), registry flip (SOC-criticality → elaborated/slice-1), full gate.

---

### Task 1: `nearest_occupied` + the cascade histogram field

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs`
- Test: `windows/worldgen/tests/history_bake.rs`

**Interfaces:**
- Consumes: `traversable_neighbors(&ConnectionGraph, CellId) -> Vec<CellId>` (existing); `self.node_index: BTreeMap<CellId, usize>`; `self.cur() -> &ConnectionGraph`.
- Produces: `fn nearest_occupied(&self, from: CellId) -> Option<usize>` (community index of the nearest occupied cell over the era graph, excluding `from`, BFS layers, lowest-`CellId` tie-break); `const CASCADE_DEPTH_CAP: u32 = 256`; `const CASCADE_BINS: usize = 12`; `BakeCensus.cascade_hist: [u64; CASCADE_BINS]`; `BakeCensus::record_cascade(&mut self, size: u32)`; `pub fn cascade_sizes(&self) -> [u64; CASCADE_BINS]`.

- [ ] **Step 1: Write the `nearest_occupied` unit test (failing).**

Add to `windows/worldgen/tests/history_bake.rs` a test that builds a small graph + a `Bake` with two occupied cells and asserts `nearest_occupied` returns the nearer one, excluding the origin. Since `Bake` is private, put this in a `#[cfg(test)] mod tests` block at the bottom of `history_bake.rs` (like `traversable_neighbors`' tests):

```rust
#[test]
fn nearest_occupied_finds_the_closest_occupied_cell_over_the_graph() {
    // full-land graph over Geosphere::new(1); occupy cells 3 and 20; from cell 0,
    // whichever is fewer graph-hops away wins (lowest CellId breaks a tie).
    let geo = Geosphere::new(1);
    let graph = full_land_graph(&geo); // test helper already in the integration file — mirror it here
    // ... construct a minimal Bake with node_index = {CellId(3)->0, CellId(20)->1} ...
    // assert nearest_occupied(CellId(0)) is the index whose cell is fewer hops from 0.
}
```

(The implementer constructs the minimal `Bake` the same way the file's other private-method tests do; if no such harness exists, assert `nearest_occupied`'s BFS/tie-break logic against a hand-built `node_index` and graph. Keep the assertion on graph-hop distance + lowest-`CellId` tie-break.)

- [ ] **Step 2: Run — expect a compile failure** (`nearest_occupied` not defined).

Run: `cargo test -p hornvale-worldgen --lib nearest_occupied 2>&1 | tail -15`
Expected: FAIL — `cannot find function/method nearest_occupied`.

- [ ] **Step 3: Implement `nearest_occupied` + the histogram field.**

In `history_bake.rs`, add near `nearest_dest`:

```rust
/// The nearest OCCUPIED cell to `from` (excluding `from`), by breadth-first
/// graph-hop distance over the era graph; within the nearest layer, lowest
/// `CellId` — total & deterministic. Returns the occupying community's index,
/// or `None` if no occupied cell is reachable. The Tumult's Sea-Peoples
/// cascade displaces this cell when no vacant land is reachable.
fn nearest_occupied(&self, from: CellId) -> Option<usize> {
    let mut visited: BTreeSet<CellId> = BTreeSet::new();
    visited.insert(from);
    let mut frontier: Vec<CellId> = vec![from];
    while !frontier.is_empty() {
        let mut next: Vec<CellId> = Vec::new();
        let mut hits: Vec<(CellId, usize)> = Vec::new();
        for &c in &frontier {
            for n in traversable_neighbors(self.cur(), c) {
                if visited.insert(n) {
                    next.push(n);
                    if let Some(&idx) = self.node_index.get(&n) {
                        hits.push((n, idx));
                    }
                }
            }
        }
        if !hits.is_empty() {
            hits.sort_by(|a, b| a.0.cmp(&b.0)); // lowest CellId in the nearest layer
            return Some(hits[0].1);
        }
        frontier = next;
    }
    None
}
```

Add the constants and the histogram. `CASCADE_BINS = 12` covers sizes 1, 2, 3-4, 5-8, … up to 2^10+ (log2 bins). Add to `BakeCensus`:

```rust
/// Log-binned histogram of cascade sizes (# displacements in one relaxation):
/// bin i counts cascades whose size falls in [2^i, 2^(i+1)). The raw material
/// of The Tumult's power-law falsification metric. Not committed (diagnostic).
/// type-audit: bare-ok(count: cascade_hist)
pub cascade_hist: [u64; CASCADE_BINS],
```

(Add `CASCADE_BINS` to the `BakeCensus` derive's `Default` — `[u64; N]` is `Default`/`Copy` for `N <= 32` via const generics; if the derive rejects it, implement `Default` by hand.) Add methods:

```rust
impl BakeCensus {
    /// Record one completed cascade of `size` displacements into the log-binned
    /// histogram (size 0 — a relocation that reached vacant land — is not a
    /// cascade and is not recorded).
    fn record_cascade(&mut self, size: u32) {
        if size == 0 { return; }
        let bin = (31 - size.leading_zeros()).min(CASCADE_BINS as u32 - 1) as usize;
        self.cascade_hist[bin] += 1;
    }
}

/// The cascade-size histogram off a baked history (bin i = sizes [2^i, 2^(i+1))).
pub fn cascade_sizes(h: &History) -> [u64; CASCADE_BINS] {
    h.tally.cascade_hist
}
```

- [ ] **Step 4: Run — expect PASS.**

Run: `cargo test -p hornvale-worldgen --lib nearest_occupied 2>&1 | tail -15`
Expected: the `nearest_occupied` test PASSES; the crate compiles (histogram field added).

- [ ] **Step 5: Commit.**

```bash
cargo fmt
git add windows/worldgen/src/history_bake.rs windows/worldgen/tests/history_bake.rs
git commit -m "feat(history): nearest_occupied + cascade-size histogram scaffolding (the-tumult T1)"
```

---

### Task 2: The cascade — `relocate` replaces the no-vacant dead-ends

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs`
- Test: `windows/worldgen/tests/history_bake.rs`

**Interfaces:**
- Consumes: `nearest_dest`, `nearest_occupied`, `open`, `close`, `touch`, `record_cascade` (Task 1); `MIGRATE_SURVIVAL`, `RAID_SEIZE` (existing consts); `CauseOfEnd::{Fled, Migrated}`, `Ended::By`, `Founding::From`.
- Produces: `fn relocate(&mut self, people: KindId, pop: f64, lineage: EntityId, offset: f64, from: CellId, era: &EraClimate, year: f64, depth: u32) -> u32` (returns the cascade size = number of displacements caused).

- [ ] **Step 1: Write the cascade test (failing).**

Add to `windows/worldgen/tests/history_bake.rs` a test on a FULLY-OCCUPIED small world: seed communities on every habitable cell, then an era turns one cell hostile so its community must relocate — with no vacant land it must displace, cascading. Assert (a) a cascade fires (`census(&h).cascade_hist` has a nonzero bin, or `raided`/`fled` rise above the all-vacant baseline), (b) the bake terminates (no hang — the depth cap holds), (c) determinism (same seed → identical records).

```rust
#[test]
fn a_full_world_cascades_when_a_cell_turns_hostile() {
    // Geosphere::new(1); all cells habitable+vacant at genesis, seed MANY
    // communities so the graph saturates; then a glacial era makes one cell
    // hostile with NO vacant refuge, forcing displacement onto occupied land.
    // (construct via the fixture pattern in this file; capacity/eras chosen so
    // the world fills and one cell then evicts with nowhere vacant.)
    // asserts: some cascade_hist bin > 0; bake returns (terminates); byte-identical twice.
}
```

- [ ] **Step 2: Run — expect FAIL** (no cascade yet: the community collapses/is-lost instead of displacing).

Run: `cargo test -p hornvale-worldgen --test history_bake a_full_world_cascades 2>&1 | tail -20`
Expected: FAIL — cascade histogram is all-zero (current code collapses at the dead-end).

- [ ] **Step 3: Implement `relocate` + reroute the two dead-ends.**

Add the recursive helper:

```rust
/// Relocate a homeless people (evicted by climate or raid) to a new home,
/// cascading when there is no vacant land. Returns the cascade size — the
/// number of OCCUPIED cells this relocation displaced (0 if it reached vacant
/// land directly). The Sea-Peoples avalanche: no vacant cell ⇒ take the nearest
/// occupied cell (raid it), and its evicted occupant relocates in turn.
/// Bounded by `CASCADE_DEPTH_CAP` (a truncated cascade drops the last remnant).
#[allow(clippy::too_many_arguments)]
fn relocate(
    &mut self, people: KindId, pop: f64, lineage: EntityId, offset: f64,
    from: CellId, era: &EraClimate, year: f64, depth: u32,
) -> u32 {
    if depth >= CASCADE_DEPTH_CAP {
        return 0; // truncated — the last remnant is lost (bounded-size guard)
    }
    // Vacant land reachable? Then no conflict — settle there.
    if let Some(dest) = self.nearest_dest(era, from) {
        let new_idx = self.open(people, dest, year, pop, Founding::From(lineage), Some(lineage), offset);
        self.touch(new_idx, year);
        return 0;
    }
    // No vacant land — displace the nearest occupied cell (the avalanche).
    let Some(victim) = self.nearest_occupied(from) else {
        return 0; // nothing vacant AND nothing occupied reachable — lost
    };
    let victim_site = self.communities[victim].site;
    let (v_people, v_pop, v_lineage, v_offset) = {
        let c = &self.communities[victim];
        (self.records[c.record].people, c.population, c.lineage, c.tech_offset)
    };
    // The homeless people takes the victim's site (open BEFORE close so
    // node_index[victim_site] points at the new occupant; close then sees the
    // cell already re-indexed and does not free it).
    let new_idx = self.open(people, victim_site, year, pop, Founding::From(lineage), Some(lineage), offset);
    let displacer_id = self.communities[new_idx].id;
    self.close(victim, year, CauseOfEnd::Fled, Ended::By(displacer_id));
    self.touch(new_idx, year);
    self.tally.raided += 1;
    self.tally.fled += 1;
    // The evicted occupant cascades onward.
    1 + self.relocate(v_people, v_pop * MIGRATE_SURVIVAL, v_lineage, v_offset, victim_site, era, year, depth + 1)
}
```

In `step_community`, the `eff == 0` branch (currently lines ~475-502) becomes:

```rust
if eff == 0.0 {
    let (record, pop, lineage, offset) = {
        let c = &self.communities[idx];
        (c.record, c.population, c.lineage, c.tech_offset)
    };
    let people = self.records[record].people;
    self.close(idx, year, CauseOfEnd::Migrated, Ended::Nature);
    let size = self.relocate(people, pop * MIGRATE_SURVIVAL, lineage, offset, site, era, year, 0);
    if size == 0 { self.tally.migrated += 1; } else { self.tally.record_cascade(size); }
    return;
}
```

In `raid`, the evicted-community refound (currently lines ~544-558, `if let Some(dest) = nearest_dest ... resettled ... else lost`) becomes:

```rust
let size = self.relocate(people, remaining, lineage, offset, flee_site, era, year, 0);
if size == 0 { self.tally.resettled += 1; } else { self.tally.record_cascade(size); }
```

(Remove the old `open`/`nearest_dest` block that `relocate` now subsumes; keep the seize + close-target-Fled that precedes it.)

- [ ] **Step 4: Run — expect PASS.**

Run: `cargo test -p hornvale-worldgen --test history_bake 2>&1 | tail -25`
Expected: `a_full_world_cascades_when_a_cell_turns_hostile` PASSES; the existing all-land no-op / byte-identity / displacement tests still PASS.

- [ ] **Step 5: Commit.**

```bash
cargo fmt
git add windows/worldgen/src/history_bake.rs windows/worldgen/tests/history_bake.rs
git commit -m "feat(history): the Sea-Peoples cascade — displace occupied land when none is vacant (the-tumult T2)"
```

---

### Task 3: The measurement entry point + the falsification gates

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (extract `history_for`)
- Create: `windows/worldgen/tests/history_tumult.rs`
- Re-measure: `windows/worldgen/tests/history_gates.rs` (light — re-pin if the epoch moved a floor)

**Interfaces:**
- Consumes: `bake`, `census`, `cascade_sizes`, `History`, `BakeCensus` (worldgen); `Seed`, `SkyPins`, `TerrainPins`, `SettlementPins`, `SkyChoice`, `WorldComponents`, `build_world_to`, `BuildDepth`.
- Produces: `pub fn history_for(seed: Seed, sky: &SkyPins, sky_choice: SkyChoice, terrain: &TerrainPins, settlement: &SettlementPins, wc: &WorldComponents) -> Result<History, BuildError>` — assembles the bake inputs (the same terrain/climate/paleoclimate/capacity/river/eras/refugia/graphs the bake call site builds) and returns the diagnostic `History`.

- [ ] **Step 1: Extract `history_for` from the bake call site.**

In `lib.rs`, factor the bake-input assembly + `history_bake::bake(...)` call (the block that produces `let history = ...` at the settlement stage) into `pub fn history_for(...) -> Result<History, BuildError>`, and have the existing call site call it. Verify byte-identity is preserved: `cargo test -p hornvale-worldgen --test history_byte_identity` stays green.

- [ ] **Step 2: Write the conflict-fires + not-depopulated gates (failing until measured).**

Create `windows/worldgen/tests/history_tumult.rs`:

```rust
use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{SettlementPins, SkyChoice, WorldComponents, cascade_sizes, census, history_for};

fn hist(seed: Seed) -> [u64; 12] {
    let wc = WorldComponents::assemble().expect("registries");
    let h = history_for(seed, &SkyPins::default(), SkyChoice::Generated,
        &TerrainPins::default(), &SettlementPins::default(), &wc).expect("bakes");
    cascade_sizes(&h)
}

/// Gate — conflict FIRES. With crowding, cascades occur (raids rise from the
/// pre-Tumult zero). If the world never saturates, this is a density-calibration
/// finding for Nathan, not a floor. Floor set below the measured value (Step 4).
const MIN_CASCADES: u64 = 0; // set in Step 4
#[test]
fn conflict_fires_at_volume() {
    let total: u64 = hist(Seed(42)).iter().sum();
    assert!(total >= MIN_CASCADES,
        "conflict inert: only {total} cascades on seed 42 (floor {MIN_CASCADES}) — the world is not \
         saturating; a density-calibration finding, not a floor to lower.");
}
```

And the depopulation gate (reuse `census(&h)` for alive/collapsed, or the existing `emergent_settlement_count_stays_in_the_sane_band` in `history_placement.rs` — assert the cascade does not empty the map).

- [ ] **Step 3: Write the power-law falsification metric (heavy-tier, the headline).**

```rust
/// The falsification HEADLINE (heavy: pools cascades over a seed sample and
/// adjudicates the size distribution). A power law (roughly linear log-count vs
/// log-size with negative slope over ≥ the middle bins) confirms self-organized
/// criticality; a bell/spike/empty distribution FALSIFIES the bare sandpile
/// (recorded as the honest result motivating cohesion/grievance). Either ships.
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn cascade_sizes_are_measured_and_the_shape_adjudicated() {
    let mut agg = [0u64; 12];
    for s in 1..=30u64 {
        let h = hist(Seed(s));
        for (a, b) in agg.iter_mut().zip(h.iter()) { *a += b; }
    }
    let total: u64 = agg.iter().sum();
    assert!(total > 0, "no cascades across the sample — the world never saturates (falsified/inert)");
    // Adjudicate: fit log-count vs log-bin over the populated bins; a power law
    // reads a negative slope with a heavy tail (fill the exact test + the pinned
    // slope/decades in Step 4 from the MEASURED distribution — power law OR the
    // documented bell/spike falsification; do NOT tune to force "power law").
    eprintln!("SUNDER-TUMULT cascade histogram (pooled 1..=30): {agg:?}");
}
```

- [ ] **Step 4: Measure and set thresholds / adjudicate the shape.**

Build seed-42 (and the 1..=30 sample) with `--nocapture`; read the cascade counts. Set `MIN_CASCADES` clear below the measured seed-42 total. Inspect the pooled histogram: if it is heavy-tailed (a power law), pin the shape assertion (e.g. monotone-decreasing log-counts with a negative log-log slope over the middle bins) and record the measured slope; **if it is a bell/spike/empty, do NOT force it — assert the measured reality and record the falsification** (the bare sandpile is insufficient; motivates cohesion/grievance). If seed-42 shows NO cascades at all (the world never saturates), reply `DONE_WITH_CONCERNS` with the census — a density-calibration decision for Nathan (founding density / capacity / bake span), which is a fidelity carve-out.

- [ ] **Step 5: Re-measure the existing seed-42 gates; re-pin if a floor moved.**

Run `cargo test -p hornvale-worldgen --test history_gates --test history_placement`. If the epoch moved `MIGRATION_FLOOR`/`MAX_REGION_OVERLAP`/the band and a phenomenon still fires, re-pin (labelled `// The Tumult (the sandpile) re-pin: …`). If a phenomenon goes inert or the map depopulates, STOP and report (fidelity carve-out).

- [ ] **Step 6: Run to green, commit.**

```bash
cargo fmt
git add windows/worldgen/src/lib.rs windows/worldgen/tests/history_tumult.rs windows/worldgen/tests/history_gates.rs
git commit -m "test(history): cascade-fires + not-depopulated + power-law falsification gates (the-tumult T3)"
```

---

### Task 4: The cascade cost gate (heavy tier)

**Files:**
- Modify: `cli/tests/graph_cost.rs`

- [ ] **Step 1: Add the cost + max-depth check (heavy-tier).**

Following `graph_cost.rs`'s build helper and the `heavy:` ignore-reason token, build seed-42 to `BuildDepth::Settlements` (which now runs the cascading bake), time it, and assert the wall-time stays under budget. If `history_for` exposes the max cascade depth reached (add a `max_cascade_depth: u32` to `BakeCensus`, tallied in `relocate`), also assert it stays below `CASCADE_DEPTH_CAP` (a real avalanche is not being clipped).

```rust
#[test]
#[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
fn tumult_cascade_bake_stays_within_budget() {
    let start = std::time::Instant::now();
    let _w = /* build seed-42 to BuildDepth::Settlements — copy graph_cost.rs's build helper */;
    let elapsed = start.elapsed();
    assert!(elapsed.as_secs() < 60,
        "the cascading bake regressed: {elapsed:?} to build seed-42 settlements (budget 60s)");
}
```

- [ ] **Step 2: Run it (opt-in).**

Run: `cargo test -p hornvale --test graph_cost -- --ignored tumult_cascade 2>&1 | tail -15`
Expected: PASS within budget; note the max cascade depth if instrumented (if it hits `CASCADE_DEPTH_CAP`, the cap is clipping real avalanches — raise it, or report if that signals a runaway).

- [ ] **Step 3: Commit.**

```bash
cargo fmt && git add cli/tests/graph_cost.rs windows/worldgen/src/history_bake.rs
git commit -m "test(cli): cascading-bake wall-time + max-depth cost gate, heavy tier (the-tumult T4)"
```

---

## Close (G6 — `closing-a-campaign`, Nathan-authorized)

Census regen on `lefford` (0063); census-close cascade re-pins (`rows.csv` → `golden-pins.sql` + `calibration.rs` via `make census-check`, then `branches_family`/`gathering`); seed-42 keystone refreeze from main's tip; artifact-drift regen; DoD docs (chronicle, retrospective, freshness sweep, Confidence Gradient re-score — the SOC bet moves from `raw`, registry flip SOC-criticality → elaborated/slice-1-shipped with the measured power-law-or-falsification result); full gate + artifact drift; then fast-forward main.

---

## Self-Review

**Spec coverage:** §4.2 cascade → Task 2 (`relocate` at both dead-ends). §4.1 drive/crowding → measured (Task 3 gates). §4.3 boundedness/determinism → Task 1/2 (`CASCADE_DEPTH_CAP`, deterministic BFS/tie-breaks) + Task 4 (max-depth). §4.4 tally → Task 1 (histogram). §5 falsification metric → Task 3. §8 gates (fires / not-depopulated / power-law / cost) → Task 3 + Task 4. §6 epoch (no new committed field) → the cascade uses existing `Fled` records; census/keystone at close. §9 non-goals — no cohesion/grievance/shock/roads introduced.

**Placeholder scan:** `MIN_CASCADES` and the power-law shape assertion are measured-then-set in Task 3 Step 4 (measure-don't-narrate requires it — the epoch's values can't be known before it runs), with the adjudication procedure specified. The `history_for` extraction and the "copy graph_cost.rs's build helper" reference concrete existing code. No other TBDs.

**Type consistency:** `relocate(KindId, f64, EntityId, f64, CellId, &EraClimate, f64, u32) -> u32` used at both call sites; `nearest_occupied(CellId) -> Option<usize>` consistent; `cascade_hist: [u64; CASCADE_BINS]` with `CASCADE_BINS = 12` matched by the `[u64; 12]` test signatures; `history_for(...) -> Result<History, BuildError>` produced in Task 3 Step 1 and consumed by the gates. `record_cascade(u32)` / `cascade_sizes(&History) -> [u64; 12]` consistent.
